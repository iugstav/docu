#include "html.h"
#include "lexer.h"
#include "parser.h"

void ast_to_html(struct node *node, FILE *out) {
	if (!node)
		return;

	char *value;
	if (node->kind != NODE_TAG_VALUE) {
		value = node->value.str;
		switch (node->kind) {
		case NODE_TITLE:
			fprintf(out, "<h1>%s</h1>\n", value);
			break;
		case NODE_SUBTITLE:
			fprintf(out, "<h2>%s</h2>\n", value);
			break;
		case NODE_TAGS:
			fprintf(out, "<p><strong>Tags:</strong> %s</p>\n", value);
			break;
		case NODE_CODEBLOCK:
			fprintf(out, "<pre><code>%s</code></pre>\n", value);
			break;
		case NODE_LIST: {
			fprintf(out, "<ul>\n");
			struct node *item = node->n1;
			while (item) {
				fprintf(out, "  <li>%s</li>\n", item->value.str);
				item = item->n2;
			}
			fprintf(out, "</ul>\n");
			break;
		}
		case NODE_TEXT:
			fprintf(out, "<p>%s</p>\n", value);
			break;
		default:
			break;
		}
	} else {
		value = node->value.tag_content->value;
		switch (node->value.tag_content->owner_tag) {
		case TOK_DESC:
			fprintf(out, "<p>%s</p>\n", value);
			break;
		case TOK_EXAMPLE:
			fprintf(out, "<p>%s</p>\n", value);
			break;
		case TOK_AUTHOR:
			fprintf(out, "<p><strong>Author:</strong> %s</p>\n", value);
			break;
		case TOK_VERSION:
			fprintf(out, "<p><strong>Version:</strong> %s</p>\n", value);
			break;
		default:
			break;
		}
	}

	ast_to_html(node->n1, out);
	ast_to_html(node->n2, out);
}

void html_to_file(struct node *node, FILE *out) {
	fprintf(out, "<!DOCTYPE html>\n<html>\n<head>\n");
	fprintf(out, "  <meta charset=\"UTF-8\">\n");
	fprintf(out, "  <title>Generated Document</title>\n");
	fprintf(out, "</head>\n<body>\n\n");

	ast_to_html(node, out);

	fprintf(out, "\n</body>\n</html>\n");
}
