#include "lexer.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error(const char *fmt, ...) {
	va_list ap;

	(void)fprintf(stderr, "docu: error: ");

	va_start(ap, fmt);
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);

	(void)fputc('\n', stderr);

	exit(1);
}

char *substr(const char *src, int start, int end) {
	int len = end - start;
	char *sub = (char *)malloc(len + 1);
	if (sub == NULL)
		error("something happened while allocating data at %d", start);

	strncpy(sub, src + start, len);
	sub[len] = '\0';
	return sub;
}

token_vector *vec_init() {
	token_vector *v = (token_vector *)malloc(sizeof(token_vector));
	v->count = 0;
	v->capacity = 60;
	v->items = (struct token_t **)malloc(sizeof(struct token_t *) * v->capacity);

	return v;
}

inline struct token_t *vec_last(token_vector *vec) {
	return vec->items[vec->count - 1];
}

void vec_grow(token_vector *vec) {
	vec->capacity *= 2;
	vec->items = (struct token_t **)realloc(vec->items, sizeof(struct token_t *) * vec->capacity);
	if (vec->items == NULL)
		error("something happened while growing the items vector"); // improve error message
}

void vec_add(token_vector *vec, struct token_t *token) {
	if (vec->count >= vec->capacity)
		vec_grow(vec);

	vec->items[vec->count++] = token;
}

void vec_free(token_vector *vec) {
	for (size_t i = 0; i < vec->count; i++) {
		free(vec->items[i]->lexeme);
		free(vec->items[i]);
	}
	free(vec->items);
	free(vec);
}

struct token_t *token_new(enum TokenType type, const char *lexeme) {
	struct token_t *token = (struct token_t *)malloc(sizeof(struct token_t));
	token->type = type;
	token->lexeme = strdup(lexeme);
	return token;
}

struct token_t *token_make_from_keyword(const char *lexeme) {
	if (strcmp(lexeme, "@desc") == 0) {
		return token_new(TOK_DESC, lexeme);
	} else if (strcmp(lexeme, "@author") == 0) {
		return token_new(TOK_AUTHOR, lexeme);
	} else if (strcmp(lexeme, "@version") == 0) {
		return token_new(TOK_VERSION, lexeme);
	} else if (strcmp(lexeme, "@tags") == 0) {
		return token_new(TOK_TAGS, lexeme);
	} else if (strcmp(lexeme, "@param") == 0) {
		return token_new(TOK_PARAM, lexeme);
	} else if (strcmp(lexeme, "@return") == 0) {
		return token_new(TOK_RETURN, lexeme);
	} else if (strcmp(lexeme, "@example") == 0) {
		return token_new(TOK_EXAMPLE, lexeme);
	} else if (strcmp(lexeme, "@warning") == 0) {
		return token_new(TOK_WARNING, lexeme);
	} else if (strcmp(lexeme, "@todo") == 0) {
		return token_new(TOK_TODO, lexeme);
	}
	return token_new(TOK_STRING, lexeme);
}

void token_print(struct token_t *token) {
	const char *typeName;
	switch (token->type) {
	case TOK_TITLE:
		typeName = "TITLE";
		break;
	case TOK_SUBTITLE:
		typeName = "SUBTITLE";
		break;
	case TOK_DESC:
		typeName = "DESC";
		break;
	case TOK_AUTHOR:
		typeName = "AUTHOR";
		break;
	case TOK_VERSION:
		typeName = "VERSION";
		break;
	case TOK_TAGS:
		typeName = "TAGS";
		break;
	case TOK_CODEBLOCK:
		typeName = "CODEBLOCK";
		break;
	case TOK_LIST_ITEM:
		typeName = "LIST_ITEM";
		break;
	case TOK_PARAM:
		typeName = "PARAM";
		break;
	case TOK_RETURN:
		typeName = "RETURN";
		break;
	case TOK_EXAMPLE:
		typeName = "EXAMPLE";
		break;
	case TOK_WARNING:
		typeName = "WARNING";
		break;
	case TOK_TODO:
		typeName = "TODO";
		break;
	case TOK_STRING:
		typeName = "STRING";
		break;
	case TOK_NEWLINE:
		typeName = "NEWLINE";
		break;
	case TOK_EOF:
		typeName = "EOF";
		break;
	default:
		typeName = "UNKNOWN";
		break;
	}
	printf("<%s, \"%s\">\n", typeName, token->lexeme);
}

void lexer_next(struct lexer_t *lexer) {
	while (lexer->pos < lexer->length && (lexer->src[lexer->pos] == ' ' || lexer->src[lexer->pos] == '\t'))
		lexer->pos++;

	if (lexer->pos >= lexer->length) {
		vec_add(lexer->tokens, token_new(TOK_EOF, "EOF"));
		return;
	}

	char c = lexer->src[lexer->pos];
	switch (c) {
	case '\n':
		lexer->pos++;
		vec_add(lexer->tokens, token_new(TOK_NEWLINE, "\n"));
		break;

	case '#': {
		int start = lexer->pos;
		int hash_count = 0;
		while (lexer->pos < lexer->length && lexer->src[lexer->pos] == '#') {
			hash_count++;
			lexer->pos++;
		}

		if (hash_count == 1)
			vec_add(lexer->tokens, token_new(TOK_TITLE, "#"));
		else if (hash_count == 2)
			vec_add(lexer->tokens, token_new(TOK_SUBTITLE, "##"));
		else {
			char *sub = substr(lexer->src, start, lexer->pos);
			vec_add(lexer->tokens, token_new(TOK_UNKNOWN, sub));
			free(sub);
		}

		break;
	}

	case '@': {
		int start = lexer->pos;
		while (lexer->pos < lexer->length && !isspace(lexer->src[lexer->pos])) {
			lexer->pos++;
		}
		char *at_word = substr(lexer->src, start, lexer->pos);
		struct token_t *token = token_make_from_keyword(at_word);
		free(at_word);
		vec_add(lexer->tokens, token);
		break;
	}

	case '`': {
		if (lexer->pos + 2 < lexer->length && lexer->src[lexer->pos] == '`' &&
		    lexer->src[lexer->pos + 1] == '`' && lexer->src[lexer->pos + 2] == '`') {
			lexer->pos += 3;
			vec_add(lexer->tokens, token_new(TOK_CODEBLOCK, "```"));
		} else {
			lexer->pos++;
			vec_add(lexer->tokens, token_new(TOK_UNKNOWN, "`"));
		}

		break;
	}

	case '-': {
		lexer->pos++;
		char buf[2] = {c, '\0'};
		vec_add(lexer->tokens, token_new(TOK_LIST_ITEM, buf));
	}

	default: {
		int start = lexer->pos;
		while (lexer->pos < lexer->length && !isspace(lexer->src[lexer->pos])) {
			lexer->pos++;
		}

		char *sub = substr(lexer->src, start, lexer->pos);
		vec_add(lexer->tokens, token_new(TOK_STRING, sub));
		free(sub);
		break;
	}
	}

	return;
}
