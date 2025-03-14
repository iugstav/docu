#include "parser.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void error(const char *fmt, ...);

void ast_add_child(struct node *parent, struct node *child) {
	if (!parent->n1) {
		parent->n1 = child;
	} else {
		struct node *tmp = parent->n1;
		while (tmp->n2)
			tmp = tmp->n2;
		tmp->n2 = child;
	}
}

void ast_print(struct node *node, int depth) {
	if (!node)
		return;
	for (int i = 0; i < depth; i++)
		printf("  ");
	const char *typeName;
	switch (node->kind) {
	case NODE_DOCUMENT:
		typeName = "DOCUMENT";
		break;
	case NODE_TITLE:
		typeName = "TITLE";
		break;
	case NODE_SUBTITLE:
		typeName = "SUBTITLE";
		break;
	case NODE_DESCRIPTION:
		typeName = "DESCRIPTION";
		break;
	case NODE_AUTHOR:
		typeName = "AUTHOR";
		break;
	case NODE_VERSION:
		typeName = "VERSION";
		break;
	case NODE_TAGS:
		typeName = "TAGS";
		break;
	case NODE_CODEBLOCK:
		typeName = "CODEBLOCK";
		break;
	case NODE_LIST:
		typeName = "LIST";
		break;
	case NODE_PARAM:
		typeName = "PARAM";
		break;
	case NODE_RETURN:
		typeName = "RETURN";
		break;
	case NODE_EXAMPLE:
		typeName = "EXAMPLE";
		break;
	case NODE_WARNING:
		typeName = "WARNING";
		break;
	case NODE_TODO:
		typeName = "TODO";
		break;
	case NODE_TEXT:
		typeName = "TEXT";
		break;
	default:
		typeName = "UNKNOWN";
		break;
	}

	printf("%s: %s\n", typeName, node->value ? node->value : "");
	ast_print(node->n1, depth + 1);
	ast_print(node->n2, depth);
}

void ast_free(struct node *node) {
	if (!node)
		return;
	ast_free(node->n1);
	ast_free(node->n2);
	free(node->value);
	free(node);
}

struct node *node_new(enum NodeKind kind, const char *value) {
	struct node *n = (struct node *)malloc(sizeof(struct node));
	if (!n) {
		error("could not allocate a node for ast");
	}

	n->kind = kind;
	n->value = strdup(value);
	n->n1 = NULL;
	n->n2 = NULL;
	return n;
}

struct parser *parser_new(token_vector *vec) {
	struct parser *p = (struct parser *)malloc(sizeof(struct parser));
	p->tokens = vec;
	p->index = 0;
	return p;
}

struct token_t *parser_current(struct parser *parser) {
	struct token_t *token;
	if (parser->index < parser->tokens->count)
		token = parser->tokens->items[parser->index];
	else
		token = token_new(TOK_EOF, "EOF");
	return token;
}

void advance(struct parser *parser) {
	if (parser->index < parser->tokens->count)
		parser->index++;
}

char *go_until_newline(struct parser *parser);
struct node *parse_element(struct parser *parser);

struct node *parser_parse_document(struct parser *parser) {
	struct node *doc = node_new(NODE_DOCUMENT, "Document");
	while (parser_current(parser)->type != TOK_EOF) {
		struct node *element = parse_element(parser);
		if (element)
			ast_add_child(doc, element);
	}
	return doc;
}

char *go_until_newline(struct parser *parser) {
	int buf_size = sizeof(char) * 100;
	char *buffer = (char *)malloc(buf_size);
	if (!buffer)
		error("could not allocate data for parsing the document");
	buffer[0] = '\0';

	struct token_t *token = parser_current(parser);
	while (token->type != TOK_NEWLINE && token->type != TOK_EOF) {
		token = parser_current(parser);
		size_t size_needed = strlen(buffer) + strlen(token->lexeme);
		if (size_needed > buf_size) {
			buf_size *= 2;
			buffer = (char *)realloc(buffer, buf_size);
			if (!buffer)
				error("could not reallocate data for parsing the document");
		}
		strcat(buffer, token->lexeme);
		strcat(buffer, " ");
		advance(parser);
	}

	size_t len = strlen(buffer);
	if (len > 0 && buffer[len - 1] == ' ')
		buffer[len - 1] = '\0';
	if (token->type == TOK_NEWLINE)
		advance(parser);
	return buffer;
}

struct node *parse_element(struct parser *parser) {
	struct token_t *tok = parser_current(parser);
	switch (tok->type) {
	case TOK_TITLE: {
		// Consume the title token and then parse its text.
		advance(parser);
		char *value = go_until_newline(parser);
		struct node *node = node_new(NODE_TITLE, value);
		free(value);
		return node;
	}
	case TOK_SUBTITLE: {
		advance(parser);
		char *value = go_until_newline(parser);
		struct node *node = node_new(NODE_SUBTITLE, value);
		free(value);
		return node;
	}
	case TOK_DESC:
	case TOK_AUTHOR:
	case TOK_VERSION:
	case TOK_TAGS:
	case TOK_PARAM:
	case TOK_RETURN:
	case TOK_EXAMPLE:
	case TOK_WARNING:
	case TOK_TODO: {
		// Map keyword token to corresponding node type.
		enum NodeKind type;
		switch (tok->type) {
		case TOK_DESC:
			type = NODE_DESCRIPTION;
			break;
		case TOK_AUTHOR:
			type = NODE_AUTHOR;
			break;
		case TOK_VERSION:
			type = NODE_VERSION;
			break;
		case TOK_TAGS:
			type = NODE_TAGS;
			break;
		case TOK_PARAM:
			type = NODE_PARAM;
			break;
		case TOK_RETURN:
			type = NODE_RETURN;
			break;
		case TOK_EXAMPLE:
			type = NODE_EXAMPLE;
			break;
		case TOK_WARNING:
			type = NODE_WARNING;
			break;
		case TOK_TODO:
			type = NODE_TODO;
			break;
		default:
			type = NODE_TEXT;
			break;
		}
		advance(parser);
		char *value = go_until_newline(parser);
		struct node *node = node_new(type, value);
		free(value);
		return node;
	}
	case TOK_LIST_ITEM: {
		// Parse a sequence of list items.
		struct node *listNode = node_new(NODE_LIST, "List");
		while (parser_current(parser)->type == TOK_LIST_ITEM) {
			// Skip the list marker.
			advance(parser);
			char *value = go_until_newline(parser);
			struct node *item = node_new(NODE_TEXT, value);
			ast_add_child(listNode, item);
			free(value);
		}
		return listNode;
	}
	case TOK_CODEBLOCK: {
		// Parse a code block: skip opening marker, accumulate value until closing marker.
		advance(parser);
		size_t bufSize = 256;
		char *buffer = (char *)malloc(bufSize);
		if (!buffer) {
			error("malloc failed");
		}
		buffer[0] = '\0';

		struct token_t *ct = parser_current(parser);
		while (ct->type != TOK_CODEBLOCK && ct->type != TOK_EOF) {
			ct = parser_current(parser);
			size_t needed = strlen(buffer) + strlen(ct->lexeme) + 2;
			if (needed > bufSize) {
				bufSize *= 2;
				buffer = (char *)realloc(buffer, bufSize);
				if (!buffer) {
					error("realloc failed");
				}
			}
			strcat(buffer, ct->lexeme);
			strcat(buffer, " ");
			advance(parser);
		}
		if (ct->type == TOK_CODEBLOCK)
			advance(parser);
		struct node *node = node_new(NODE_CODEBLOCK, buffer);
		free(buffer);
		return node;
	}
	case TOK_STRING: {
		// Plain text line.
		char *value = go_until_newline(parser);
		struct node *node = node_new(NODE_TEXT, value);
		free(value);
		return node;
	}
	case TOK_NEWLINE: {
		advance(parser);
		return NULL;
	}
	default:
		advance(parser);
		return NULL;
	}
}
