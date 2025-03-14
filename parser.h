#pragma once

#include "lexer.h"
#include <stdlib.h>

enum NodeKind {
	NODE_DOCUMENT,
	NODE_TITLE,
	NODE_SUBTITLE,
	NODE_DESCRIPTION,
	NODE_AUTHOR,
	NODE_VERSION,
	NODE_TAGS,
	NODE_CODEBLOCK,
	NODE_LIST,
	NODE_PARAM,
	NODE_RETURN,
	NODE_EXAMPLE,
	NODE_WARNING,
	NODE_TODO,
	NODE_TEXT
};

struct node {
	enum NodeKind kind;
	char *value;
	struct node *n1;
	struct node *n2;
};

struct node *node_new(enum NodeKind kind, const char *value);
void ast_print(struct node *node, int depth);
void ast_free(struct node *node);

struct parser {
	token_vector *tokens;
	size_t index;
};

struct parser *parser_new(token_vector *vec);
struct node* parser_parse_document(struct parser *parser);
