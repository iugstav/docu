#pragma once

#include <stdlib.h>

enum TokenType {
	TOK_TITLE,
	TOK_SUBTITLE,
	TOK_DESC,
	TOK_AUTHOR,
	TOK_VERSION,
	TOK_LIST_ITEM,
	TOK_TAGS,
	TOK_WARNING,
	TOK_PARAM,
	TOK_RETURN,
	TOK_TODO,
	TOK_CODEBLOCK,
	TOK_EXAMPLE,

	TOK_STRING,
	TOK_NEWLINE,
	TOK_EOF,

	TOK_UNKNOWN
};

struct token_t {
	enum TokenType type;
	char *lexeme;
};

typedef struct {
	struct token_t **items;
	size_t count;
	size_t capacity;

} token_vector;

struct lexer_t {
	const char *src;
	int pos;
	int length;
	token_vector *tokens;
};

char *substr(const char *src, int start, int end);

// token vector
token_vector *vec_init();
void vec_add(token_vector *vec, struct token_t *token);
struct token_t *vec_last(token_vector *vec);
void vec_free(token_vector *vec);

// token
struct token_t *token_new(enum TokenType type, const char *lexeme);

// lexer
void lexer_next(struct lexer_t *lexer);
void token_print(struct token_t *token);
