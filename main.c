#include "html.h"
#include "lexer.h"
#include "parser.h"
#include <stdio.h>
#include <string.h>

int main(void) {
	const char *input = "# MyProject\n"
			    "@desc A lightweight API for user authentication.\n"
			    "@author John Doe\n"
			    "@version 1.0.3\n"
			    "\n"
			    "## Features\n"
			    "- Secure authentication\n"
			    "- Token-based sessions\n"
			    "- Scalable API design\n"
			    "\n"
			    "@example\n"
			    "```bash\n"
			    "curl -X POST https://api.example.com/users \\\n"
			    "     -d \"username=johndoe\" \\\n"
			    "     -d \"password=securepass\"\n"
			    "```\n";

	struct lexer_t *lexer = (struct lexer_t *)malloc(sizeof(struct lexer_t));
	lexer->src = input;
	lexer->pos = 0;
	lexer->length = strlen(input); // switch when using file
	lexer->tokens = vec_init();

	printf("input length: %lu\n", strlen(input));
	do {
		lexer_next(lexer);
	} while (vec_last(lexer->tokens)->type != TOK_EOF);

	struct parser *parser = parser_new(lexer->tokens);
	struct node *document = parser_parse_document(parser);

	FILE *result = fopen("output.html", "w+");
	if (!result) {
		fprintf(stderr, "could not open output file to generate documentation...\n");
		exit(69);
	}

	html_to_file(document, result);

	fclose(result);
	ast_free(document);
	vec_free(lexer->tokens);
	free(lexer);
	free(parser);
}
