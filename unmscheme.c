/* unmscheme
 *
 *	Copyright (C) 2007-2009		Lance R. Williams
 *				     			University of New Mexico
 *				     			Matthew J. Barrick
 *	All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation version
 * 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

#define GL 1

#include <ctype.h>
#include <float.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <setjmp.h>
#include <complex.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <libgen.h>
#include <assert.h>
#include <limits.h>

#ifdef GL

#endif


#ifdef WIN32
#include <windows.h>
#endif

#include "complex.c"
static fcomplex zero = { 0.0, 0.0 };

#include "svd.c"
#include "four1.c"

#include "sexpr_types.h"
#include "unmscheme.h"
#include "macros.h"
#include "defines.h"

typedef struct procs {
	pid_t pid;
	struct procs* next;
} procs;

static sexpr *global_vars;
static sexpr *global_vals;
static sexpr *global_env_vars;
static sexpr *global_env_vals;
static sexpr *macros;
static sexpr *nil;
static sexpr *eof_object;
static sexpr *undefined;
static sexpr *true;
static sexpr *false;
static sexpr *current_input_port;
static sexpr *current_output_port;
static sexpr *number_zero;
static sexpr *symbol_nil;
static sexpr *symbol_straight;
static sexpr *symbol_spot;
static sexpr *symbol_transparent;
static sexpr *symbol_bend;
static sexpr *symbol_adjoin;
static sexpr *symbol_adorn;
static sexpr *symbol_text;
static sexpr *graphic_default_color;
static sexpr *graphic_nil;
static sexpr *bytecode_halt;
static sexpr *bytecode_local_refer;
static sexpr *bytecode_global_refer;
static sexpr *bytecode_refer;
static sexpr *bytecode_constant;
static sexpr *bytecode_close;
static sexpr *bytecode_return;
static sexpr *bytecode_test;
static sexpr *bytecode_local_assign;
static sexpr *bytecode_global_assign;
static sexpr *bytecode_assign;
static sexpr *bytecode_define;
static sexpr *bytecode_funcall;
static sexpr *bytecode_apply;
static sexpr *bytecode_frame;
static sexpr *bytecode_argument;
static sexpr *bytecode_conti;
static sexpr *bytecode_nuate;
static sexpr *bytecode_throw;
static sexpr *bytecode_catch;
static sexpr *symbol_lambda;
static sexpr *symbol_begin;
static sexpr *symbol_apply;
static sexpr *symbol_define;
static sexpr *symbol_define_macro;
static sexpr *symbol_if;
static sexpr *symbol_callcc;
static sexpr *symbol_callec;
static sexpr *primitive_append;
static sexpr *primitive_cons;
static sexpr *symbol_let;
static sexpr *symbol_let_star;
static sexpr *symbol_letrec;
static sexpr *symbol_set_bang;
static sexpr *symbol_quote;
static sexpr *symbol_quasiquote;
static sexpr *symbol_unquote;
static sexpr *symbol_unquote_splicing;
static sexpr *symbol_hyphen;
static sexpr *token_left;
static sexpr *token_right;
static sexpr *token_dot;
static sexpr *token_quote;
static sexpr *token_quasiquote;
static sexpr *token_unquote;
static sexpr *token_unquote_splicing;
static sexpr *token_pound_sign;
static node *hashtab[HASHSIZE];
static int gargc;
static char **gargv;
static int gensyms;
static int gc;
static sexpr ***head, ***tail;
static long unsigned smalloc;
static sexpr smalloc0[SEXPRS];
static sexpr *tail0;
static sexpr smalloc1[SEXPRS];
static sexpr *tail1;
static int loads;
static jmp_buf esc;
static procs *process_list;
static char* unmscheme_directory;



static double image_display_scale = 1.0;

void cull_zombie_hordes() {
	procs* p = process_list;
	procs* prev = process_list;
	int status;
	while (p != NULL) {
		//	printf("\nwaiting on %i\n", p->pid);
		if (waitpid(p->pid, &status, WNOHANG | WUNTRACED) > 0) {
			//		printf("Removing process %i, exit status %i \n", p->pid, status);
			if (p == process_list) {
				process_list = NULL;
			} else {
				prev->next = p->next;
			}
		}
		prev = p;
		p = p->next;
	}
}


void load_from(char* dir, char* filename) {
	static char path_to_load[PATH_MAX];
	snprintf(path_to_load,PATH_MAX,"%s/%s",dir,filename);
	path_to_load[PATH_MAX-1] = '\0';
	load(str2exp(path_to_load));
}

int main(int argc, char **argv) {
	int i;
	sexpr *input, *result;
	// Locate the directory that contains unmscheme's sorted bits and
	// chdir() into it.
	unmscheme_directory = strdup(dirname(argv[0]));

	gargc = argc;
	gargv = argv;

	char c;

	gc = 0;

	smalloc = (long unsigned) smalloc0;

	process_list = NULL;
	for (i = 0; i < HASHSIZE; i++)
		hashtab[i] = NULL;
	gensyms = 0;
	define_scheme_constants();

	make_global_env();

	load_from(unmscheme_directory,"compiler-basics.scm");
	load_from(unmscheme_directory,"plumbing.scm");
	load_from(unmscheme_directory,"boldt.scm");

	printf(
			"Welcome to UNM Scheme 2.6.3 Copyright (c) 2009 The University of New Mexico\n");

	setjmp(esc);
	while (loads < argc)
		load(str2exp(argv[loads++]));
	printf("> ");
	while ((c = getc(stdin)) != EOF) {
		ungetc(c, stdin);
		input = parse(stdin, '\n');
		if (!null(input)) {
			result = eval(car(input));
			while (!null(input = cdr(input))) {
				print(result);
				printf("> ");
				result = eval(car(input));

			}
			print(result);
			garbage_collect();
			cull_zombie_hordes();
			printf("> ");
		}

	}

	exit(0);
}

void print_value_escape(char *msg, struct sexpr *sp) {
	printf("%s", msg);
	if (undefined(sp))
		printf("\n");
	else
		print(sp);
	longjmp(esc, 1);
}

int isfalse(sexpr *sp) {
	return (boolean(sp) && (sp->u.i == 0));
}

sexpr *color(double r, double g, double b) {
	return cons(num2exp(r), cons(num2exp(g), cons(num2exp(b), nil)));
}

sexpr *make_bytecode(char *name, int i) {
	sexpr *sp = malloc(sizeof(sexpr));
	sp->type = i;
	symbol$name(sp) = malloc((strlen(name) + 1) * sizeof(char));
	strcpy(symbol$name(sp), name);
	return sp;
}

void define_scheme_constants() {

	/* Scheme constants */
	loads = 1;
	nil = malloc(sizeof(sexpr));
	nil->type = NIL;
	eof_object = malloc(sizeof(sexpr));
	eof_object->type = EOF_OBJECT;
	current_input_port = malloc(sizeof(sexpr));
	current_input_port->type = INPUT_PORT;
	current_input_port->u.file = stdin;
	current_output_port = malloc(sizeof(sexpr));
	current_output_port->type = OUTPUT_PORT;
	current_output_port->u.file = stdout;
	undefined = malloc(sizeof(sexpr));
	undefined->type = UNDEFINED;
	true = malloc(sizeof(sexpr));
	true->type = BOOLEAN;
	true->u.i = 1;
	false = malloc(sizeof(sexpr));
	false->type = BOOLEAN;
	false->u.i = 0;
	number_zero = malloc(sizeof(sexpr));
	number_zero->type = NUMBER;
	number_zero->u.x = 0.0;
	global_vars = make_extensible_vector(0, 1000);
	global_vals = make_extensible_vector(0, 1000);
	global_env_vars = cons(global_vars, nil);
	global_env_vals = cons(global_vals, nil);
	macros = nil;

	/* Plumbing Graphics constants */
	symbol_nil = str2symbol("nil");
	symbol_straight = str2symbol("straight");
	symbol_spot = str2symbol("spot");
	symbol_transparent = str2symbol("transparent");
	symbol_bend = str2symbol("bend");
	symbol_adjoin = str2symbol("adjoin");
	symbol_adorn = str2symbol("adorn");
	symbol_text = str2symbol("text");

	graphic_default_color = color(255.0, 255.0, 255.0);
	graphic_nil = malloc(sizeof(sexpr));
	graphic_nil->type = GRAPHIC;
	graphic$sexpr(graphic_nil) = nil;

	/* Constants needed for compilation */
	symbol_quote = str2symbol("quote");
	symbol_quasiquote = str2symbol("quasiquote");
	symbol_unquote = str2symbol("unquote");
	symbol_unquote_splicing = str2symbol("unquote-splicing");
	primitive_cons = make_primitive(cons, 2, "cons");
	primitive_append = make_primitive(append, 2, "scheme:append");
	symbol_lambda = str2symbol("lambda");
	symbol_begin = str2symbol("scheme:begin");
	symbol_apply = str2symbol("apply");
	symbol_define = str2symbol("scheme:define");
	symbol_define_macro = str2symbol("define-macro");
	symbol_if = str2symbol("if");
	symbol_callcc = str2symbol("call/cc");
	symbol_callec = str2symbol("call/ec");
	symbol_let = str2symbol("let");
	symbol_let_star = str2symbol("let*");
	symbol_letrec = str2symbol("letrec");
	symbol_set_bang = str2symbol("set!");

	/* Scanner constants */
	token_left = char2token('(');
	token_right = char2token(')');
	token_dot = char2token('.');
	token_quote = char2token('\'');
	token_quasiquote = char2token('`');
	token_unquote = char2token(',');
	token_unquote_splicing = char2token('@');
	token_pound_sign = char2token('#');
	symbol_hyphen = str2symbol("-");

	/* Virtual machine constants */
	bytecode_halt = make_bytecode("halt", HALT);
	bytecode_local_refer = make_bytecode("local-refer", LOCAL_REFER);
	bytecode_global_refer = make_bytecode("global-refer", GLOBAL_REFER);
	bytecode_refer = make_bytecode("refer", REFER);
	bytecode_constant = make_bytecode("constant", CONSTANT);
	bytecode_close = make_bytecode("close", CLOSE);
	bytecode_return = make_bytecode("return", RETURN);
	bytecode_test = make_bytecode("test", TEST);
	bytecode_local_assign = make_bytecode("local-assign", LOCAL_ASSIGN);
	bytecode_global_assign = make_bytecode("global-assign", GLOBAL_ASSIGN);
	bytecode_assign = make_bytecode("assign", ASSIGN);
	bytecode_define = make_bytecode("define", DEFINE);
	bytecode_apply = make_bytecode("apply", APPLY);
	bytecode_funcall = make_bytecode("funcall", FUNCALL);
	bytecode_frame = make_bytecode("frame", FRAME);
	bytecode_argument = make_bytecode("argument", ARGUMENT);
	bytecode_conti = make_bytecode("conti", CONTI);
	bytecode_nuate = make_bytecode("nuate", NUATE);
	bytecode_throw = make_bytecode("throw", THROW);
	bytecode_catch = make_bytecode("catch", CATCH);
}

sexpr *load(sexpr *filename) {

	FILE *file;
	sexpr *input, *result;

	if (!string(filename)) {
		printf("load: Illegal filename.\n");
		longjmp(esc, 1);
	}

	file = fopen(filename->u.text, "r");
	if (file) {

		input = parse(file, EOF);
		if (!pair(input))
			longjmp(esc, 1);

		result = nil;

		while (!null(input)) {
			result = eval(car(input));
			input = cdr(input);
		}

		fclose(file);
		return result;
	} else {
		printf("load: File not found.\n");
		longjmp(esc, 1);
	}
}

void eat_line(FILE *file, char c) {
	while (c != EOF && c != '\n')
		c = getc(file);
	return;
}

sexpr *open_input_file(sexpr *filename) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = INPUT_PORT;

	if (!string(filename)) {
		printf("open-input-file: Illegal filename.\n");
		longjmp(esc, 1);
	}

	sp->u.file = fopen(filename->u.text, "r");
	if (sp->u.file)
		return sp;
	else {
		printf("open-input-file: Attempt to open input file failed.\n");
		longjmp(esc, 1);
	}
}

sexpr *open_output_file(sexpr *filename) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = OUTPUT_PORT;

	if (!string(filename)) {
		printf("open-output-file: Illegal filename.\n");
		longjmp(esc, 1);
	}

	sp->u.file = fopen(filename->u.text, "w");

	if (sp->u.file)
		return sp;

	printf("open-output-file: Attempt to open output file failed.\n");
	longjmp(esc, 1);
}

sexpr *close_input_port(sexpr *port) {

	if (!input_port(port)) {
		print_value_escape("close-input-port: Argument is not an input-port: ",
				port);
	}
	fclose(port->u.file);
	return undefined;
}

sexpr *close_output_port(sexpr *port) {

	if (!output_port(port)) {
		print_value_escape(
				"close-output-port: Argument is not an output-port: ", port);
	}
	fclose(port->u.file);
	return undefined;
}

sexpr *read_char(sexpr *port) {
	char c;

	if (!input_port(port)) {
		print_value_escape("read-char: Argument is not an input-port: ", port);
		return NULL;
	} else {
		c = getc(port->u.file);
		if (c != EOF)
			return char2exp(c);
		else
			return eof_object;
	}
}

sexpr *write_char(sexpr *c, sexpr *port) {

	if (!output_port(port)) {
		print_value_escape("write-char: Argument is not an output-port: ", port);
	}
	putc(c->u.c, port->u.file);
	return undefined;
}

sexpr *peek_char(sexpr *port) {
	char c;

	if (!input_port(port)) {
		print_value_escape("peek-char: Argument is not an input-port: ", port);
	}

	c = getc(port->u.file);
	ungetc(c, port->u.file);
	if (c != EOF)
		return char2exp(c);
	else
		return eof_object;
}

int isspecialer(char c) {
	return (c == '~' || c == '!' || c == '#' || c == '$' || c == '%' || c
			== '^' || c == '&' || c == '*' || c == '-' || c == '_' || c == '+'
			|| c == '=' || c == ':' || c == '<' || c == '>' || c == '/' || c
			== '?' || c == '@');
}

int isletter(char c) {
	return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
}

int islegal1st(char c) {
	return (isletter(c) || isspecialer(c));
}

int islegal2nd(char c) {
	return (isletter(c) || isspecialer(c) || isdigit(c));
}

int isdigit_or_pt(char c) {
	return (isdigit(c) || c == '.');
}

sexpr *lex_error(FILE *file, char c) {
	printf("Lexical error reading character '%c'\n", c);
	eat_line(file, c);
	longjmp(esc, 1);
	return nil;
}

int eatit(int(*func)(char), char *name, FILE *file, char stop) {

	char c;
	int m = 0;

	c = getc(file);
	while (func(c) && (c != stop) && (c != EOF)) {
		if (m >= STRLEN)
			lex_error(file, c);
		name[m++] = c;
		c = getc(file);
	}
	name[m] = '\0';
	ungetc(c, file);
	return 1;
}

double eat_number(FILE *file, char stop) {
	char c = getc(file);
	int m = 0;
	double x;
	int state = 1;
	char *name = malloc(STRLEN * sizeof(char));

	while ((c != stop) && (c != EOF)) {
		switch (state) {
		case 1:
			if (isdigit(c))
				state = 2;
			else if (c == '.')
				state = 3;
			else {
				printf("Malformed number.\n");
				longjmp(esc, 1);
			}
			break;
		case 2:
			if (isdigit(c))
				state = 2;
			else if (c == '.')
				state = 3;
			else {
				ungetc(c, file);
				name[m] = '\0';
				sscanf(name, "%lg", &x);
				free(name);
				return x;
			}
			break;
		case 3:
			if (isdigit(c))
				state = 3;
			else {
				ungetc(c, file);
				name[m] = '\0';
				sscanf(name, "%lg", &x);
				free(name);
				return x;
			}
			break;
		}
		name[m++] = c;
		c = getc(file);
	}
	if (state == 2 || state == 3) {
		name[m] = '\0';
		ungetc(c, file);
		sscanf(name, "%lg", &x);
		free(name);
		return x;
	}
	printf("Malformed number.\n");
	longjmp(esc, 1);
}

sexpr *eat_complex(int sign, FILE *file, char stop) {
	double x = sign * eat_number(file, stop), y;
	char c = getc(file), d;
	switch (c) {
	case 'i':
		return cons(complex2exp(Complex(0.0, x)), lex(file, stop));
	case '+':
		y = eat_number(file, stop);
		if (getc(file) == 'i')
			return cons(complex2exp(Complex(x, y)), lex(file, stop));
		break;
	case '@':
		d = getc(file);
		if (d == '-') {
			y = -eat_number(file, stop);
		} else {
			ungetc(d, file);
			y = eat_number(file, stop);
		}
		return cons(polar2complex(num2exp(x), num2exp(y)), lex(file, stop));
	case '-':
		y = eat_number(file, stop);
		if (getc(file) == 'i')
			return cons(complex2exp(Complex(x, -y)), lex(file, stop));
		break;
	default:
		ungetc(c, file);
		return cons(num2exp(x), lex(file, stop));
	}
	printf("Malformed number.\n");
	longjmp(esc, 1);
}

int notquote(char c) {
	return (c != '\"');
}

int notstile(char c) {
	return (c != '|');
}

sexpr *lex(FILE *file, char stop) {

	char c, d;

	while (isspace(c = getc(file))) {
		if (stop == ' ' || c == stop || c == EOF)
			return nil;
	}

	if (c == stop)
		return nil;

	switch (c) {
	case '(':
		return cons(token_left, lex(file, stop));
	case ')':
		return cons(token_right, lex(file, stop));
	case '\'':
		return cons(token_quote, lex(file, stop));
	case '`':
		return cons(token_quasiquote, lex(file, stop));
	case ',':
		d = getc(file);
		if (d == '@')
			return cons(token_unquote_splicing, lex(file, stop));
		ungetc(d, file);
		return cons(token_unquote, lex(file, stop));
	case '#':
		switch (d = getc(file)) {
		case '!':
			eat_line(file, c);
			return lex(file, stop);
		case 't':
			return cons(true, lex(file, stop));
		case 'f':
			return cons(false, lex(file, stop));
		case '\\':
			return backslash(file, stop);
		case '(':
			ungetc(d, file);
			return cons(token_pound_sign, lex(file, stop));
		default:
			printf("Illegal character, '%c', following '#'.\n", d);
			eat_line(file, d);
			longjmp(esc, 1);
		}
	case ';':
		eat_line(file, c);
		return lex(file, stop);
	case '\"': {
		char *name = malloc(STRLEN * sizeof(char));
		sexpr *sp;
		eatit(notquote, name, file, '\"');
		d = getc(file);
		sp = str2exp(name);
		free(name);
		return cons(sp, lex(file, stop));
	}
	case '|': {
		char *between = malloc(STRLEN * sizeof(char));
		char *name = malloc(STRLEN * sizeof(char));
		sexpr *sp;
		eatit(notstile, between, file, '|');
		d = getc(file);
		sprintf(name, "|%s|", between);
		sp = str2symbol(name);
		free(between);
		free(name);
		return cons(sp, lex(file, stop));
	}
	case '.':
		d = getc(file);
		if (!isdigit(d)) {
			ungetc(d, file);
			return cons(token_dot, lex(file, stop));
		}
		ungetc(d, file);
		ungetc(c, file);
		return eat_complex(1, file, stop);
	case '-':
		d = getc(file);
		if (isdigit_or_pt(d)) {
			ungetc(d, file);
			return eat_complex(-1, file, stop);
		} else if (islegal1st(d)) {
			char *name = malloc(STRLEN * sizeof(char));
			sexpr *sp;
			ungetc(d, file);
			ungetc(c, file);
			eatit(islegal2nd, name, file, stop);
			sp = str2symbol(name);
			free(name);
			return cons(sp, lex(file, stop));
		} else {
			ungetc(d, file);
			return cons(symbol_hyphen, lex(file, stop));
		}
	default:
		if (isdigit(c)) {
			ungetc(c, file);
			return eat_complex(1, file, stop);
		} else if (islegal1st(c)) {
			char *name = malloc(STRLEN * sizeof(char));
			sexpr *sp;
			ungetc(c, file);
			eatit(islegal2nd, name, file, stop);
			sp = str2symbol(name);
			free(name);
			return cons(sp, lex(file, stop));
		}
	}
	longjmp(esc, 1);
}

sexpr *read_newline(FILE *file, int stop) {
	char c2 = getc(file), c3, c4, c5, c6, c7;
	if (c2 != 'e') {
		ungetc(c2, file);
		return (cons(char2exp('n'), lex(file, stop)));
	} else {
		c3 = getc(file);
		if (c3 != 'w')
			return lex_error(file, c3);
		else {
			c4 = getc(file);
			if (c4 != 'l')
				return lex_error(file, c4);
			else {
				c5 = getc(file);
				if (c5 != 'i')
					return lex_error(file, c5);
				else {
					c6 = getc(file);
					if (c6 != 'n')
						return lex_error(file, c6);
					else {
						c7 = getc(file);
						if (c7 == 'e')
							return (cons(char2exp('\n'), lex(file, stop)));
						else
							return lex_error(file, c7);
					}
				}
			}
		}
	}
}

sexpr *read_space(FILE *file, int stop) {
	char c2 = getc(file), c3, c4, c5;
	if (c2 != 'p') {
		ungetc(c2, file);
		return (cons(char2exp('s'), lex(file, stop)));
	} else {
		c3 = getc(file);
		if (c3 != 'a')
			return lex_error(file, c3);
		else {
			c4 = getc(file);
			if (c4 != 'c')
				return lex_error(file, c4);
			else {
				c5 = getc(file);
				if (c5 == 'e')
					return (cons(char2exp(' '), lex(file, stop)));
				else
					return lex_error(file, c5);
			}
		}
	}
}

sexpr *read_tab(FILE *file, int stop) {
	char c2 = getc(file), c3;
	if (c2 != 'a') {
		ungetc(c2, file);
		return (cons(char2exp('t'), lex(file, stop)));
	} else {
		c3 = getc(file);
		if (c3 == 'b')
			return (cons(char2exp('\t'), lex(file, stop)));
		else
			return lex_error(file, c3);
	}
}

sexpr *backslash(FILE *file, int stop) {
	char c1 = getc(file);
	switch (c1) {
	case 'n':
		return read_newline(file, stop);
	case 's':
		return read_space(file, stop);
	case 't':
		return read_tab(file, stop);
	default:
		if (c1 != stop)
			return (cons(char2exp(c1), lex(file, stop)));
		else
			return lex_error(file, c1);
	}
}

sexpr *cons(sexpr *car, sexpr *cdr) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(pair);
	sp->type = PAIR;
	pair$car(sp) = car;
	pair$cdr(sp) = cdr;
	return sp;
}

sexpr *car(sexpr *sp) {
	if (pair(sp))
		return pair$car(sp);
	print_value_escape("Attempt to compute car of non-pair: ", sp);
	return NULL;
}

sexpr *cdr(sexpr *sp) {
	if (pair(sp))
		return pair$cdr(sp);
	print_value_escape("Attempt to compute cdr of non-pair: ", sp);
	return NULL;
}

sexpr *caar(sexpr *sp) {
	return car(car(sp));
}

sexpr *cadr(sexpr *sp) {
	return car(cdr(sp));
}

sexpr *cdar(sexpr *sp) {
	return cdr(car(sp));
}

sexpr *cddr(sexpr *sp) {
	return cdr(cdr(sp));
}

sexpr *caddr(sexpr *sp) {
	return car(cdr(cdr(sp)));
}

sexpr *cadddr(sexpr *sp) {
	return car(cdr(cdr(cdr(sp))));
}

double remainder(double x, double y) {
	double ratio = x / y;
	return y * (ratio - floor(ratio));
}

double deg2rad(double deg) {
	return (remainder(deg, 360) / 180.0) * PI;
}


double angle_difference(double rad1, double rad2) {
	return remainder(rad1 - rad2, TWOPI);
}


float bhot(float xmin, float xmax, float x) {

	x = (x - xmin) / (xmax - xmin);

	if (x < 0.666666667)
		return 0.0;
	else
		return (x - 0.666666667) * 3.0;
}

float ghot(float xmin, float xmax, float x) {

	x = (x - xmin) / (xmax - xmin);

	if (x < 0.333333333)
		return 0.0;
	else if (x < 0.666666667)
		return (x - 0.333333333) * 3.0;
	else
		return 1.0;
}

float rhot(float xmin, float xmax, float x) {

	x = (x - xmin) / (xmax - xmin);

	if (x < 0.333333333)
		return x * 3.0;
	else
		return 1.0;
}

#define DISPLAY 0
#define PRINT 1

#define EMBEDDED 0
#define TOPLEVEL 1

int spawn_subprocess() {
	pid_t pid;
	procs* p;
	int pipefd[2];

	if (pipe(pipefd) == -1) {
		perror("pipe");
		exit(EXIT_FAILURE);
	}

	switch (pid = fork()) {
	case -1:
		fprintf(stderr, "fork failed");
		break;
	case 0:
		close(pipefd[1]);
		dup2(pipefd[0], STDIN_FILENO);
		close(pipefd[0]);

		/* Just pass on the environment to the child
		 * process in case the underlying OpenGL/GLUT implementation
		 * needs it.
		 */
		chdir(unmscheme_directory);
		if (execl("./unm_viewer", "unm_viewer", "-gldebug", NULL) < 0) {
			if (execlp("unm_viewer", "unm_viewer", "-gldebug", NULL) < 0) {
				fprintf(stderr, "Error launching viewer\n");
				exit(EXIT_FAILURE);
			}
		}

		break;
	default:
		close(pipefd[0]);
		p = (procs*) (malloc(sizeof(procs)));
		p->pid = pid;
		p->next = process_list;
		process_list = p;
		return pipefd[1];
	}
	return -1;
}

void print_line(line* l) {
	fprintf(stderr, "line: x(%f,%f) y(%f,%f) %f %f %f %f l(%i,%i) %i %i %i\n",
			l->x[0], l->x[1], l->y[0], l->y[1], l->theta, l->contrast,
			l->length, l->coverage, length(l->links[0]), length(l->links[1]),
			l->replaced, l->merged, l->age);

}
void serialize_line(line* l, FILE* pipe_file) {
	fwrite(l, sizeof(line), 1, pipe_file);
}

void serialize_generic_sexpr(sexpr* sp, FILE* pipe_file) {
	int type;

	type = sp->type;
	fwrite(&type, sizeof(int), 1, pipe_file);

	switch (type) {
	case NIL:
		break;
	case NUMBER:
		fwrite(&(sp->u.x), sizeof(double), 1, pipe_file);
		break;
	case CHARACTER:
		fwrite(&(sp->u.c), sizeof(char), 1, pipe_file);
		break;
	case STRING:
		type = strlen(sp->u.text) + 1;
		fwrite(&type, sizeof(int), 1, pipe_file);
		fwrite(sp->u.text, sizeof(char) * type, 1, pipe_file);
		break;
	case BOOLEAN:
		fwrite(&(sp->u.i), sizeof(int), 1, pipe_file);
		break;
	case PAIR:
		serialize_generic_sexpr(pair$car(sp),pipe_file);
		serialize_generic_sexpr(pair$cdr(sp),pipe_file);
		break;
	case SYMBOL:
		type = strlen(((symbol*) sp)->name) + 1;
		fwrite(&type, sizeof(int), 1, pipe_file);
		fwrite(((symbol*) sp)->name, sizeof(char) * type, 1, pipe_file);
		break;
	case GRAPHIC:
		sp->type = GRAPHIC;
		serialize_generic_sexpr(((graphic*) sp)->sexpr, pipe_file);

		break;

	default:
		fprintf(stderr, "bad sexpr type in marshalling\n");
		exit(-1);

	}

}

#ifdef GL
sexpr* image_sp;
#endif

void serialize_sexpr(sexpr *sp, FILE *file, int link_graph) {


#ifdef GL
	int fileno;
	image img;
	fileno = spawn_subprocess();
	FILE* pipe_file;
	pipe_file = fdopen(fileno, "w");
	sketch sk;
	sexpr* current;
	sexpr* current_link;
	double color_array[3];
	int len;
	int r, c, j, i ;
	fwrite(&image_display_scale, sizeof(double), 1, pipe_file);
	fwrite(sp, sizeof(sexpr), 1, pipe_file);
	//write(fileno,sp,sizeof(sexpr));

	switch (sp->type) {

	case IMAGE:

		image_sp = image_normalize(sp);
		c = image$cols(image_sp);
		r = image$rows(image_sp);

		img.type = IMAGE;
		img.rows = r;
		img.cols = c;
		img.data = image$data(image_sp);

		fwrite(&img, sizeof(image), 1, pipe_file);
		fwrite(img.data, sizeof(float), img.rows * img.cols, pipe_file);

		break;
	case COMPLEX_IMAGE:
		image_sp = color_image_normalize(complex2color(sp));

		c = image$cols(image_sp);
		r = image$rows(image_sp);

		img.type = COMPLEX_IMAGE;
		img.rows = r;
		img.cols = c;
		img.data = image$data(image_sp);

		fwrite(&img, sizeof(image), 1, pipe_file);
		fwrite(img.data, sizeof(float), img.rows * img.cols * 3, pipe_file);

		break;
	case COLOR_IMAGE:
		image_sp = color_image_normalize(sp);
		int c, r;
		c = image$cols(image_sp);
		r = image$rows(image_sp);

		img.type = COLOR_IMAGE;
		img.rows = r;
		img.cols = c;
		img.data = image$data(image_sp);

		fwrite(&img, sizeof(image), 1, pipe_file);
		fwrite(img.data, sizeof(float), img.rows * img.cols * 3, pipe_file);

		break;
	case SKETCH:
		fwrite(&link_graph, sizeof(int), 1, pipe_file);
		color_array[0] = car(graphic_default_color)->u.x;
		color_array[1] = cadr(graphic_default_color)->u.x;
		color_array[2] = caddr(graphic_default_color)->u.x;
		fwrite(color_array, sizeof(double), 3, pipe_file);

		sk.bixel_cols = sketch$bixel_cols(sp);
		sk.bixel_dx = sketch$bixel_dx(sp);
		sk.bixel_dy = sketch$bixel_dy(sp);
		sk.bixel_rows = sketch$bixel_rows(sp);
		sk.cols = sketch$cols(sp);
		sk.count = sketch$count(sp);
		assert(sk.count > -1);
		//sk.grid = sketch$grid(sp);
		sk.grid = NULL;
		//	assert(sk.grid==NULL);
		sk.rows = sketch$rows(sp);
		sk.type = SKETCH;
		sk.ls = sketch$ls(sp);

		fwrite(&sk, sizeof(sketch), 1, pipe_file);


		current = sk.ls;
		for (i = 0; i < sk.count; i++) {
			serialize_line((line*) car(current), pipe_file);
			current = cdr(current);
		}

		current = sk.ls;
		for (i = 0; i < sk.count; i++) {
			len = length((sexpr*) ((line*) (car(current)))->links[0]);
			fwrite(&len, sizeof(int), 1, pipe_file);
			current_link = (sexpr*) ((line*) (car(current)))->links[0];
			for (j = 0; j < len; j++) {
				serialize_line((line*) car(current_link), pipe_file);
				current_link = cdr(current_link);
			}

			len = length((sexpr*) ((line*) (car(current)))->links[1]);
			fwrite(&len, sizeof(int), 1, pipe_file);
			current_link = (sexpr*) ((line*) (car(current)))->links[1];
			for (j = 0; j < len; j++) {
				serialize_line((line*) car(current_link), pipe_file);
				current_link = cdr(current_link);
			}
			current = cdr(current);
		}

		break;
	case GRAPHIC:
		color_array[0] = car(graphic_default_color)->u.x;
		color_array[1] = cadr(graphic_default_color)->u.x;
		color_array[2] = caddr(graphic_default_color)->u.x;
		fwrite(color_array, sizeof(double), 3, pipe_file);
		serialize_generic_sexpr(sp, pipe_file);

		break;
	default:
		fprintf(file, "\nUnknown type for serialization\n");
	}

	fclose(pipe_file);
	close(fileno);

#endif //GL
	return;
}

sexpr *display_or_print(sexpr *sp, int print_flag, int toplevel_flag,
		FILE *file) {

	char text[STRLEN];
	int r, c;
	float x0, y0, x1, y1;

	switch (sp->type) {
	case NIL:
		fprintf(file, "()");
		break;
	case PAIR:
		pair_print(sp, print_flag, file);
		break;
	case NUMBER:
		fprintf(file, "%g", sp->u.x);
		break;
	case TOKEN:
		if (eq(sp, token_unquote_splicing))
			fprintf(file, ",@");
		else
			fprintf(file, "%c", sp->u.c);
		break;
	case CHARACTER:
		if (print_flag) {
			switch (sp->u.c) {
			case '\n':
				fprintf(file, "#\\newline");
				break;
			case ' ':
				fprintf(file, "#\\space");
				break;
			case '\t':
				fprintf(file, "#\\tab");
				break;
			default:
				fprintf(file, "#\\%c", sp->u.c);
			}
		} else
			fprintf(file, "%c", sp->u.c);
		break;
	case STRING:
		if (print_flag) {
			fprintf(file, "\"%s\"", sp->u.text);
		} else {
			fprintf(file, "%s", sp->u.text);
		}
		break;
	case SYMBOL:
		fprintf(file, "%s", symbol$name(sp));
		break;
	case VIRGIN:
	case CLOSURE:
		fprintf(file, "#<procedure>");
		break;
	case PRIMITIVE:
		fprintf(file, "#<primitive:%s>", primitive$name(sp));
		break;
	case BOOLEAN:
		fprintf(file, "%s", sp->u.i ? "#t" : "#f");
		break;
	case VECTOR:
		vector_print(sp, print_flag, file);
		break;
	case INPUT_PORT:
		fprintf(file, "#<input-port>");
		break;
	case OUTPUT_PORT:
		fprintf(file, "#<output-port>");
		break;
	case EOF_OBJECT:
		fprintf(file, "#<eof-object>");
		break;
	case GRAPHIC:
		sprintf(text, "#<graphic:%s>", symbol$name(gtype(sp)));
		if (toplevel_flag) {
			serialize_sexpr(sp, file, 0);
		}
		/*
		 #ifdef GL
		 if (toplevel_flag) {
		 switch (pid = fork()) {
		 case -1:
		 printf("fork failed");
		 break;
		 case 0:
		 glutInit(&gargc,gargv);
		 glutInitWindowSize(700,700);
		 glutCreateWindow(text);
		 graphic_sp = sp;
		 glutDisplayFunc(display_graphic);
		 glutMainLoop();
		 exit(0);
		 }
		 }
		 #endif
		 */
		fprintf(file, "%s", text);
		return undefined;
	case PLUMBER:
		x0 = plumber$x(sp);
		y0 = plumber$y(sp);
		x1 = plumber$heading(sp);
		fprintf(file, "#<plumber: x = %g y = %g heading = %g", x0, y0, x1);
		break;
	case IMAGE:
		r = image$rows(sp);
		c = image$cols(sp);
		sprintf(text, "#<image: rows = %d cols = %d>", r, c);
		fprintf(file, "%s", text);
		if (toplevel_flag) {
			serialize_sexpr(sp, file, 0);
		}
		return undefined;

	case COMPLEX_IMAGE:
		r = complex_image$rows(sp);
		c = complex_image$cols(sp);
		sprintf(text, "#<complex-image: rows = %d cols = %d>", r, c);
		if (toplevel_flag) {
			serialize_sexpr(sp, file, 0);
		}
		fprintf(file, "%s", text);
		return undefined;
	case COLOR_IMAGE:
		r = image$rows(sp);
		c = image$cols(sp);
		sprintf(text, "#<color-image: rows = %d cols = %d>", r, c);
		if (toplevel_flag) {
			serialize_sexpr(sp, file, 0);
		}
		fprintf(file, "%s", text);
		return undefined;
	case LINE:
		x0 = line$x(sp)[0];
		y0 = line$y(sp)[0];
		x1 = line$x(sp)[1];
		y1 = line$y(sp)[1];
		fprintf(file, "#<line: x0 = %g y0 = %g x1 = %g y1 = %g>", x0, y0, x1,
				y1);
		break;
	case SKETCH:
		r = sketch$rows(sp);
		c = sketch$cols(sp);
		sprintf(text, "#<sketch: rows = %d cols = %d count = %d>", r, c,
				sketch$count(sp));
		if (toplevel_flag) {
			serialize_sexpr(sp, file, 0);
		}
		/*#ifdef GL
		 if (toplevel_flag) {
		 switch (pid = fork()) {
		 case -1:
		 printf("fork failed");
		 break;
		 case 0:
		 glutInit(&gargc,gargv);
		 glutInitWindowSize(c,r);
		 glutInitWindowPosition(0,0);
		 glutCreateWindow(text);
		 sketch_sp = sp;
		 glutDisplayFunc(display_sketch);
		 glutMainLoop();
		 exit(0);
		 }
		 }
		 #endif
		 */
		fprintf(file, "%s", text);
		return undefined;
	case CONTINUATION:
		fprintf(file, "#<continuation>");
		break;
	case UNDEFINED:
		fprintf(file, "#<void>");
		break;
	case COMPLEX:
		if (sp->u.z.i < 0)
			fprintf(file, "%g-%gi", (double) sp->u.z.r,
					(double) fabs(sp->u.z.i));
		else
			fprintf(file, "%g+%gi", (double) sp->u.z.r, (double) sp->u.z.i);
		break;
	default:
		fprintf(file, "%s", symbol$name(sp));
	}
	return undefined;
}

void ReadString(FILE *ff, char *str) {
	char ch;
	int i;

	for (i = 0, ch = fgetc(ff); ch != EOF && ch != 10 && ch != 13; i++) {
		str[i] = ch;
		ch = fgetc(ff);
	}

	str[i] = 0;
}

int ReadCommentedString(FILE *ff, char *str) {
	char trash[256];
	int i;

	if (0 == fscanf(ff, "%s", str))
		return (3); /* EOF */
	for (i = 0; str[i] != '#' && str[i] != 0; i++)
		;
	if (str[i] == '#')
		ReadString(ff, trash);
	str[i] = 0;
	return 0;
}

int ReadNextNumber(FILE *ff, unsigned *x) {
	int res;
	char str[256];
	for (res = ReadCommentedString(ff, str); !res && sscanf(str, "%u", x) != 1; res
			= ReadCommentedString(ff, str))
		;
	return res;
}

sexpr *read_image_helper(FILE *file, int image_type) {

	unsigned x = 0, y = 0, greylevels = 0;
	unsigned i, n, data;

	sexpr *sp;

	if (ReadNextNumber(file, &x) || ReadNextNumber(file, &y) || ReadNextNumber(
			file, &greylevels)) {
		printf("read-image: Bad image header.\n");
		longjmp(esc, 1);
	}

	if (image_type == IMAGE) {
		sp = make_image((int) y, (int) x);
		n = y * x;
	} else {
		sp = make_color_image((int) y, (int) x);
		n = y * x * 3;
	}

	for (i = 0; i < n; i++) {
		if (ReadNextNumber(file, &data)) {
			printf("read-image: Bad data.\n");
			longjmp(esc, 1);
		}
		image$data(sp)[i] = (float) data;
	}

	return sp;
}

sexpr *readpgm(sexpr *filename) {

	FILE *file;
	sexpr *sp;

	if (!string(filename)) {
		printf("read-image: Illegal filename.\n");
		longjmp(esc, 1);
	}

	file = fopen(filename->u.text, "r");
	if (!file) {
		printf("read-image: File not found.\n");
		longjmp(esc, 1);
	}

	if (getc(file) != 'P' || getc(file) != '2') {
		printf("read-image: Image file must be ASCII .pgm format.\n");
		longjmp(esc, 1);
	}

	sp = read_image_helper(file, IMAGE);

	fclose(file);
	return sp;
}

sexpr *readppm(sexpr *filename) {

	FILE *file;
	sexpr *sp;

	if (!string(filename)) {
		printf("read-color-image: Illegal filename.\n");
		longjmp(esc, 1);
	}

	file = fopen(filename->u.text, "r");
	if (!file) {
		printf("read-color-image: File not found.\n");
		longjmp(esc, 1);
	}

	if (getc(file) != 'P' || getc(file) != '3') {
		printf("read-color-image: Image file must be ASCII .ppm format.\n");
		longjmp(esc, 1);
	}

	sp = read_image_helper(file, COLOR_IMAGE);

	fclose(file);
	return sp;
}

sexpr *writepgm(sexpr *sp, sexpr *filename) {

	FILE *file;
	int i, j, n, grey_value, max_grey_value, rows, cols;

	if (!image(sp)) {
		printf("write-image: First argument must be an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp);
	cols = image$cols(sp);
	n = rows * cols;

	if (!string(filename)) {
		printf("write-image: Illegal filename.\n");
		longjmp(esc, 1);
	}

	max_grey_value = 0;
	for (i = 0; i < n; i++) {
		grey_value = image$data(sp)[i];
		if (grey_value < 0) {
			print_value_escape("write-image: Negative grey value: ", num2exp(
					grey_value));
		}
		if (grey_value > max_grey_value)
			max_grey_value = grey_value;
	}

	file = fopen(filename->u.text, "w");

	fprintf(file, "P2\n");
	fprintf(file, "# Creator: University of New Mexico Scheme 2.5\n");
	fprintf(file, "%d %d\n", cols, rows);
	fprintf(file, "%d\n", max_grey_value);
	j = 1;
	for (i = 0; i < n; i++) {
		fprintf(file, "%5d ", (int) floor(image$data(sp)[i] + 0.5));
		if (j++ == 15) {
			j = 1;
			fprintf(file, "\n");
		}
	}
	fprintf(file, "\n");
	fclose(file);
	return undefined;
}

sexpr *writeppm(sexpr *sp, sexpr *filename) {

	FILE *file;
	int i, j, n, grey_value, max_grey_value, rows, cols;

	if (!color_image(sp)) {
		printf("write-color-image: First argument must be a color image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp);
	cols = image$cols(sp);
	n = rows * cols;

	if (!string(filename)) {
		printf("write-color-image: Illegal filename.\n");
		longjmp(esc, 1);
	}

	max_grey_value = 0;
	for (i = 0; i < 3 * n; i++) {
		grey_value = image$data(sp)[i];
		if (grey_value < 0) {
			print_value_escape("write-color-image: Negative grey value: ",
					num2exp(grey_value));
		}
		if (grey_value > max_grey_value)
			max_grey_value = grey_value;
	}

	file = fopen(filename->u.text, "w");

	fprintf(file, "P3\n");
	fprintf(file, "# Creator: University of New Mexico Scheme 2.5\n");
	fprintf(file, "%d %d\n", cols, rows);
	fprintf(file, "%d\n", max_grey_value);
	j = 1;
	for (i = 0; i < n; i++) {
		fprintf(file, "%5d ", (int) floor(image$data(sp)[i * 3] + 0.5));
		fprintf(file, "%5d ", (int) floor(image$data(sp)[i * 3 + 1] + 0.5));
		fprintf(file, "%5d ", (int) floor(image$data(sp)[i * 3 + 2] + 0.5));
		if (j++ == 5) {
			j = 1;
			fprintf(file, "\n");
		}
	}
	fprintf(file, "\n");
	fclose(file);
	return undefined;
}

sexpr *image_ref(sexpr *sp1, sexpr *sp2, sexpr *sp3) {
	int i, j, rows, cols;

	float x, y;

	float f00, f01, f10, f11, *data;

	fcomplex c00, c01, c10, c11, *cdata;

	if ((!image(sp1) && !complex_image(sp1)) || !number(sp2) || !number(sp3)) {
		printf("image-ref: Illegal argument.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	i = (int) floor((float) sp2->u.x);
	j = (int) floor((float) sp3->u.x);

	if (i < 0 || i >= rows || j < 0 || j >= cols) {
		return number_zero;
	}

	y = sp2->u.x - (float) i;
	x = sp3->u.x - (float) j;

	if (x > 0 || y > 0) {
		switch (sp1->type) {
		case IMAGE:
			data = image$data(sp1);
			f00 = data[i * cols + j];
			f01 = data[i * cols + j + 1];
			f10 = data[(i + 1) * cols + j];
			f11 = data[(i + 1) * cols + j + 1];
			return num2exp((f01 - f00) * x + (f10 - f00) * y + (f11 + f00 - f10
					- f01) * x * y + f00);
		case COMPLEX_IMAGE:
			cdata = complex_image$data(sp1);
			c00 = cdata[i * cols + j];
			c01 = cdata[i * cols + j + 1];
			c10 = cdata[(i + 1) * cols + j];
			c11 = cdata[(i + 1) * cols + j + 1];
			return complex2exp(Cadd(RCmul(x, Csub(c01, c00)), Cadd(RCmul(y,
					Csub(c10, c00)), Cadd(RCmul(x * y, Cadd(c11, Csub(c00,
					Cadd(c10, c01)))), c00))));
		default:
			print_value_escape("image-ref: Argument is not an image: ", sp1);
		}
	} else {
		switch (sp1->type) {
		case IMAGE:
			data = image$data(sp1);
			return num2exp(data[i * cols + j]);
		case COMPLEX_IMAGE:
			cdata = complex_image$data(sp1);
			return complex2exp(cdata[i * cols + j]);
		default:
			print_value_escape("image-ref: Argument is not an image: ", sp1);

		}
	}
	return NULL;
}

sexpr *image_set(sexpr *sp1, sexpr *sp2, sexpr *sp3, sexpr *sp4) {
	int i, j, index;

	if ((!image(sp1) && !complex_image(sp1)) || !number(sp2) || !number(sp3)) {
		printf("image-set!: Illegal argument.\n");
		longjmp(esc, 1);
	}

	i = (int) floor((float) sp2->u.x);
	j = (int) floor((float) sp3->u.x);

	if (i < 0 || i >= image$rows(sp1)) {
		printf("image-set!: Row index out of range: %d\n", i);
		longjmp(esc, 1);
	}

	if (j < 0 || j >= image$cols(sp1)) {
		printf("image-set!: Column index out of range: %d\n", j);
		longjmp(esc, 1);
	}

	switch (sp1->type) {
	case IMAGE:
		if (number(sp4)) {
			index = i * image$cols(sp1) + j;
			image$data(sp1)[index] = (float) sp4->u.x;
			return undefined;
		}
		print_value_escape("image-set!: Argument is not a real number: ", sp4);
	case COMPLEX_IMAGE:
		switch (sp4->type) {
		case NUMBER:
			index = i * image$cols(sp1) + j;
			complex_image$data(sp1)[index] = Complex(sp4->u.x, 0.0);
			return undefined;
		case COMPLEX:
			index = i * image$cols(sp1) + j;
			complex_image$data(sp1)[index] = sp4->u.z;
			return undefined;
		default:
			print_value_escape("image-set!: Argument is not a number: ", sp4);
		}
	default:
		print_value_escape("image-set!: Argument is not an image: ", sp1);
	}
	return NULL;
}

sexpr *color_image_set(sexpr *sp1, sexpr *sp2, sexpr *sp3, sexpr *sp4,
		sexpr *sp5, sexpr *sp6) {
	int i, j, index;

	if (!color_image(sp1) || !number(sp2) || !number(sp3) || !number(sp4) || !number(sp5) || !number(sp6)) {

		printf("color-image-set!: Illegal argument.\n");
		longjmp(esc, 1);
	}

	i = (int) floor((float) sp2->u.x);
	j = (int) floor((float) sp3->u.x);

	if (i < 0 || i >= image$rows(sp1)) {
		printf("image-set!: Row index out of range: %d\n", i);
		longjmp(esc, 1);
	}

	if (j < 0 || j >= image$cols(sp1)) {
		printf("image-set!: Column index out of range: %d\n", j);
		longjmp(esc, 1);
	}

	index = 3 * (i * image$cols(sp1) + j);
	image$data(sp1)[index] = (float) sp4->u.x;
	image$data(sp1)[index + 1] = (float) sp5->u.x;
	image$data(sp1)[index + 2] = (float) sp6->u.x;
	return undefined;
}

int member(sexpr *item, sexpr *ls) {
	while (!null(ls)) {
		if (item == car(ls))
			return 1;
		ls = cdr(ls);
	}
	return 0;
}

void check_image_sizes(char *name, int row, int col, sexpr *sp) {
	if (row != image$rows(sp) && row != -1) {
		printf("%s: Row size %d not equal to %d.\n", name, row, image$rows(sp));
		longjmp(esc, 1);
	}
	if (col != image$cols(sp) && col != -1) {
		printf("%s: Column size %d not equal to %d.\n", name, col, image$cols(sp));
		longjmp(esc, 1);
	}
	return;
}

sexpr *image2array(sexpr *sp1) {

	int i, j, rows, cols, index;
	sexpr *sp2, **col_data, **row_data;

	if (!image(sp1) && !complex_image(sp1)) {
		printf("image->array: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	sp2 = make_vector(rows);
	col_data = vector$data(sp2);

	for (i = 0; i < rows; i++) {
		col_data[i] = make_vector(cols);
		row_data = vector$data(col_data[i]);
		if (image(sp1)) {
			for (j = 0; j < cols; j++) {
				index = cols * i + j;
				row_data[j] = num2exp(image$data(sp1)[index]);
			}
		} else {
			for (j = 0; j < cols; j++) {
				index = cols * i + j;
				row_data[j] = complex2exp(complex_image$data(sp1)[index]);
			}
		}
	}
	return sp2;
}

sexpr *make_image(int rows, int cols) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(image);
	sp->type = IMAGE;
	image$rows(sp) = rows;
	image$cols(sp) = cols;
	image$data(sp) = (float *) smalloc;
	smalloc += cols * rows * sizeof(float);
	return sp;
}

sexpr *make_color_image(int rows, int cols) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(image);
	sp->type = COLOR_IMAGE;
	image$rows(sp) = rows;
	image$cols(sp) = cols;
	image$data(sp) = (float *) smalloc;
	smalloc += 3 * cols * rows * sizeof(float);
	return sp;
}

sexpr *make_complex_image(int rows, int cols) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(complex_image);
	sp->type = COMPLEX_IMAGE;
	complex_image$rows(sp) = rows;
	complex_image$cols(sp) = cols;
	complex_image$data(sp) = (fcomplex *) smalloc;
	smalloc += cols * rows * sizeof(fcomplex);
	return sp;
}

sexpr *make_covariance_matrix(sexpr *sp1) {

	int i, j, k, rows, cols, n, m;
	float x, y;

	float **images;

	float *data3;

	sexpr *sp2, *sp3;

	if (!vector(sp1)) {
		printf("make-covariance-matrix: Illegal argument.\n");
		longjmp(esc, 1);
	}

	/* find size */
	m = vector$length(sp1);
	sp2 = vector$data(sp1)[0];
	if (!image(sp2)) {
		printf("make-covariance-matrix: Illegal argument.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp2);
	cols = image$cols(sp2);

	images = (float **) malloc(m * sizeof(float *));
	images[0] = image$data(sp2);

	/* check sizes */
	for (j = 1; j < m; j++) {
		sp2 = vector$data(sp1)[j];
		if (!image(sp2)) {
			printf("make-covariance-matrix: Illegal argument.\n");
			free(images);
			longjmp(esc, 1);
		}
		check_image_sizes("make-covariance-matrix", rows, cols, sp2);
		images[j] = image$data(sp2);
	}

	/* make image */
	sp3 = make_image(m, m);

	data3 = image$data(sp3);

	/* for every pixel */
	n = rows * cols;
	for (i = 0; i < n; i++) {

		/* for every image */
		for (j = 0; j < m; j++) {
			x = images[j][i];
			for (k = 0; k < m; k++) {
				y = images[k][i];
				data3[j * m + k] += x * y;
			}
		}
	}

	/* normalize */
	for (j = 0; j < m; j++) {
		for (k = 0; k < m; k++) {
			data3[j * m + k] /= n;
		}
	}

	/* return matrix */
	free(images);

	return sp3;
}

sexpr *test_svd(sexpr *sp) {

	int i, j, rows;
	sexpr *singular_vectors, *singular_values;

	double **A, *s;

	if (!image(sp)) {
		printf("svd: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp);

	if (rows != image$cols(sp)) {
		printf("svd: Argument is not a square image.\n");
		longjmp(esc, 1);
	}

	A = (double **) malloc(2 * rows * sizeof(double *));
	s = (double *) malloc(rows * sizeof(double));

	for (i = 0; i < 2 * rows; i++)
		A[i] = malloc(rows * sizeof(double));

	for (i = 0; i < rows; i++) {
		for (j = 0; j < rows; j++)
			A[i][j] = (double) image$data(sp)[i * rows + j];
	}

	svd(A, s, rows);

	singular_vectors = make_image(rows, rows);
	singular_values = make_vector(rows);

	for (i = 0; i < rows; i++) {
		vector$data(singular_values)[i] = num2exp(s[i]);
		for (j = 0; j < rows; j++) {
			image$data(singular_vectors)[i * rows + j] = (float) A[rows + i][j];
		}
	}

	for (i = 0; i < 2 * rows; i++)
		free(A[i]);
	free(A);
	free(s);

	return list2(singular_vectors, singular_values);
}

float proj_pt_on_line_rel_dist(sexpr *ln, int end, float x, float y) {

	float x0 = line$x(ln)[end];
	float y0 = line$y(ln)[end];
	float x1 = line$x(ln)[opposite(end)];
	float y1 = line$y(ln)[opposite(end)];
	float length = line$length(ln);

	float dx = x1 - x0;
	float dy = y1 - y0;

	return ((x - x0) * dx + (y - y0) * dy) / (length * length);
}

float pt_line_dist_sq(float x0, float y0, float x1, float y1, float x, float y) {

	float dx = x1 - x0;
	float dy = y1 - y0;
	float length_squared = dx * dx + dy * dy;

	float dot = ((x - x0) * dx + (y - y0) * dy) / length_squared;

	float u = x0 + dx * dot;
	float v = y0 + dy * dot;

	return (u - x) * (u - x) + (v - y) * (v - y);
}

int line_intersects_circle(float x0, float y0, float x1, float y1, float x,
		float y, float r) {

	float dx = x1 - x0;
	float dy = y1 - y0;

	float u, v;

	float m, b, d, f, g, h;

	if (fabs(dx) >= fabs(dy) || dy == 0) {
		m = dy / dx;
		b = -m * (x0 - x) + (y0 - y);
		f = -2 * m * b;
		g = 2 * (m * m + 1);
		h = f * f + 2 * g * r * r;
		if (h < 0)
			return 0;
		d = sqrt(h);
		u = (f + d) / g + x;
		v = (f - d) / g + x;
		return x1 >= u && u >= x0 && x1 >= v && v >= x0;
	} else {
		m = dx / dy;
		b = -m * (y0 - y) + (x0 - x);
		f = -2 * m * b;
		g = 2 * (m * m + 1);
		h = f * f + 2 * g * r * r;
		if (h < 0)
			return 0;
		d = sqrt(h);
		u = (f + d) / g + x;
		v = (f - d) / g + x;
		return x1 >= u && u >= x0 && x1 >= v && v >= x0;
	}
}

sexpr *get_all_lns_end_in_circle(sexpr *sp, float x, float y, int end, float r) {

	int index;

	float x0, y0;

	float rr = r * r;

	float bixel_dx = sketch$bixel_dx(sp);
	float bixel_dy = sketch$bixel_dy(sp);

	float bixel_rows = (float) sketch$bixel_rows(sp);
	float bixel_cols = (float) sketch$bixel_cols(sp);

	int bixel_i0 = (int) floor((y - r) / bixel_dy);
	int bixel_j0 = (int) floor((x - r) / bixel_dx);

	int bixel_i1 = (int) ceil((y + r) / bixel_dy);
	int bixel_j1 = (int) ceil((x + r) / bixel_dx);

	int bixel_i, bixel_j;

	sexpr *lns_in_bixel, *ln;

	sexpr *lns_in_circle = nil;

	sexpr **grid = sketch$grid(sp);

	bixel_i0 = max(0,bixel_i0);
	bixel_j0 = max(0,bixel_j0);
	bixel_i1 = min(bixel_i1,bixel_rows);
	bixel_j1 = min(bixel_j1,bixel_cols);

	for (bixel_i = bixel_i0; bixel_i < bixel_i1; bixel_i++) {
		for (bixel_j = bixel_j0; bixel_j < bixel_j1; bixel_j++) {

			index = bixel_i * bixel_cols + bixel_j;
			lns_in_bixel = grid[index];

			while (!null(lns_in_bixel)) {

				ln = pair$car(lns_in_bixel);
				x0 = line$x(ln)[end];
				y0 = line$y(ln)[end];

				if (((x0 - x) * (x0 - x) + (y0 - y) * (y0 - y) < rr)
						&& !member(ln, lns_in_circle))
					lns_in_circle = cons(ln, lns_in_circle);

				lns_in_bixel = pair$cdr(lns_in_bixel);
			}
		}
	}
	return lns_in_circle;
}

sexpr *get_all_lns_in_circle(sexpr *sp, float x, float y, float r) {

	int index;

	float x0, y0, x1, y1;

	float rr = r * r;

	float bixel_dx = sketch$bixel_dx(sp);
	float bixel_dy = sketch$bixel_dy(sp);

	float bixel_rows = (float) sketch$bixel_rows(sp);
	float bixel_cols = (float) sketch$bixel_cols(sp);

	int bixel_i0 = (int) floor((y - r) / bixel_dy);
	int bixel_j0 = (int) floor((x - r) / bixel_dx);

	int bixel_i1 = (int) ceil((y + r) / bixel_dy);
	int bixel_j1 = (int) ceil((x + r) / bixel_dx);

	int bixel_i, bixel_j;

	sexpr *lns_in_bixel, *ln;

	sexpr *lns_in_circle = nil;

	sexpr **grid = sketch$grid(sp);

	bixel_i0 = max(0,bixel_i0);
	bixel_j0 = max(0,bixel_j0);
	bixel_i1 = min(bixel_i1,bixel_rows);
	bixel_j1 = min(bixel_j1,bixel_cols);

	for (bixel_i = bixel_i0; bixel_i < bixel_i1; bixel_i++) {
		for (bixel_j = bixel_j0; bixel_j < bixel_j1; bixel_j++) {

			index = bixel_i * bixel_cols + bixel_j;
			lns_in_bixel = grid[index];

			while (!null(lns_in_bixel)) {

				ln = pair$car(lns_in_bixel);
				x0 = line$x(ln)[0];
				y0 = line$y(ln)[0];
				x1 = line$x(ln)[1];
				y1 = line$y(ln)[1];

				if (((x0 - x) * (x0 - x) + (y0 - y) * (y0 - y) < rr || (x1 - x)
						* (x1 - x) + (y1 - y) * (y1 - y) < rr
						|| line_intersects_circle(x0, y0, x1, y1, x, y, r))
						&& !member(ln, lns_in_circle))
					lns_in_circle = cons(ln, lns_in_circle);

				lns_in_bixel = pair$cdr(lns_in_bixel);
			}
		}
	}
	return lns_in_circle;
}

sexpr *po_ln_filter_magnitude(sexpr *ln, sexpr *lns, float contrast_ratio_min,
		float contrast_ratio_max) {

	float contrast = line$contrast(ln);
	float contrast_ratio;
	sexpr *previous;
	sexpr *current = lns;

	previous = nil;

	while (!null(current)) {
		contrast_ratio = line$contrast(pair$car(current)) / contrast;

		if ((contrast_ratio < contrast_ratio_min) || (contrast_ratio
				> contrast_ratio_max)) {
			if (null(previous)) {
				current = lns = pair$cdr(lns);
				continue;
			} else {
				pair$cdr(previous) = current = pair$cdr(current);
				continue;
			}
		}

		previous = current;
		current = pair$cdr(current);
	}

	return lns;
}

sexpr *po_ln_filter_direction(sexpr *ln, sexpr *lns, float delta_theta_max) {

	float theta0 = line$theta(ln);
	float theta1;
	float delta_theta;

	sexpr *previous;
	sexpr *current = lns;

	previous = nil;

	while (!null(current)) {

		theta1 = line$theta(pair$car(current));

		delta_theta = (float) fabs(angle_difference((double) theta0,
				(double) theta1));

		delta_theta = (float) min(delta_theta, TWOPI-delta_theta);

		if (delta_theta > delta_theta_max) {
			if (null(previous)) {
				current = lns = pair$cdr(lns);
				continue;
			} else {
				pair$cdr(previous) = current = pair$cdr(current);
				continue;
			}
		}
		previous = current;
		current = pair$cdr(current);
	}

	return lns;
}

sexpr *po_ln_filter_end_project(sexpr *ln, int end, sexpr *lns,
		float end_proj_rel_max) {

	sexpr *previous;
	sexpr *current = lns;
	sexpr *first;

	float d1, d2;

	previous = nil;

	while (!null(current)) {

		first = pair$car(current);

		/* No need to divide by length! */
		d1 = proj_pt_on_line_rel_dist(ln, end, line$x(first)[opposite(end)],
		line$y(first)[opposite(end)]);
		d2 = proj_pt_on_line_rel_dist(ln, end, line$x(first)[end],
				line$y(first)[end]);

		if (d1> end_proj_rel_max || d1 <= d2) {
			if (null(previous)) {
				current = lns = pair$cdr(lns);
				continue;
			} else {
				pair$cdr(previous) = current = pair$cdr(current);
				continue;
			}
		}
		previous = current;
		current = pair$cdr(current);
	}

	return lns;
}

sexpr *po_ln_filter_lateral_distance(sexpr *ln, sexpr *lns,
		float lateral_dist_abs_max_sq) {

	sexpr *previous;
	sexpr *current = lns;
	sexpr *first;

	float xm, ym;

	float x0 = line$x(ln)[0];
	float y0 = line$y(ln)[0];

	float x1 = line$x(ln)[1];
	float y1 = line$y(ln)[1];

	previous = nil;

	while (!null(current)) {

		first = pair$car(current);

		xm = (line$x(first)[0] + line$x(first)[1]) / 2.0;
		ym = (line$y(first)[0] + line$y(first)[1]) / 2.0;

		if (pt_line_dist_sq(x0, y0, x1, y1, xm, ym) > lateral_dist_abs_max_sq) {
			if (null(previous)) {
				current = lns = pair$cdr(lns);
				continue;
			} else {
				pair$cdr(previous) = current = pair$cdr(current);
				continue;
			}
		}
		previous = current;
		current = pair$cdr(current);
	}

	return lns;
}

sexpr *delete(sexpr *item, sexpr *ls) {

	sexpr *previous;
	sexpr *current = ls;

	previous = nil;

	while (!null(current)) {
		if (eq(item, car(current))) {
			if (null(previous)) {
				current = ls = pair$cdr(ls);
				continue;
			} else {
				pair$cdr(previous) = current = pair$cdr(current);
				continue;
			}
		}
		previous = current;
		current = pair$cdr(current);
	}
	return ls;
}

void po_initialize_grid_for_ln_ends(sexpr *sp, int bixel_rows, int bixel_cols) {

	int index, bixels;
	int rows = sketch$rows(sp);
	int cols = sketch$cols(sp);
	sexpr *ls = sketch$ls(sp);
	sexpr *ln, **grid;
	float bixel_dx, bixel_dy;
	float x0, y0, x1, y1;

	bixels = bixel_rows * bixel_cols;
	bixel_dx = (float) cols / (float) bixel_cols;
	bixel_dy = (float) rows / (float) bixel_rows;

	grid = (sexpr **) smalloc;
	smalloc += bixels * sizeof(sexpr *);

	for (index = 0; index < bixels; index++)
		grid[index] = nil;

	while (!null(ls)) {
		ln = pair$car(ls);
		x0 = line$x(ln)[0];
		y0 = line$y(ln)[0];
		x1 = line$x(ln)[1];
		y1 = line$y(ln)[1];

		index = (int) floor(y0 / bixel_dy) * bixel_cols + floor(x0 / bixel_dx);
		if (index >= 0 && index < bixels)
			grid[index] = cons(ln, grid[index]);

		index = (int) floor(y1 / bixel_dy) * bixel_cols + floor(x1 / bixel_dx);
		if (index >= 0 && index < bixels)
			grid[index] = cons(ln, grid[index]);

		ls = pair$cdr(ls);
	}

	sketch$bixel_rows(sp) = bixel_rows;
	sketch$bixel_cols(sp) = bixel_cols;
	sketch$bixel_dx(sp) = bixel_dx;
	sketch$bixel_dy(sp) = bixel_dy;
	sketch$grid(sp) = grid;
}

void po_initialize_grid_for_all_lns(sexpr *sp, int bixel_rows, int bixel_cols) {

	int index, bixels;
	int rows = sketch$rows(sp);
	int cols = sketch$cols(sp);
	sexpr *ls = sketch$ls(sp);
	sexpr *ln, **grid;
	float dx, dy, bixel_dx, bixel_dy;
	float x, y, x0, y0, x1, y1;

	bixels = bixel_rows * bixel_cols;
	bixel_dx = (float) cols / (float) bixel_cols;
	bixel_dy = (float) rows / (float) bixel_rows;

	grid = (sexpr **) smalloc;
	smalloc += bixels * sizeof(sexpr *);

	for (index = 0; index < bixels; index++)
		grid[index] = nil;

	while (!null(ls)) {
		ln = pair$car(ls);
		x0 = line$x(ln)[0];
		y0 = line$y(ln)[0];
		x1 = line$x(ln)[1];
		y1 = line$y(ln)[1];
		dy = y1 - y0;
		dx = x1 - x0;

		x = x0;
		y = y0;

		if (fabs(dx) > fabs(dy)) {

			if (x <= x1) {
				while (x <= x1) {
					index = (int) floor(y / bixel_dy) * bixel_cols + floor(x
							/ bixel_dx);
					if (index >= 0 && index < bixels)
						grid[index] = cons(ln, grid[index]);

					x += bixel_dx;
					y = y0 + (x - x0) * (dy / dx);
				}
			} else {
				while (x >= x1) {
					index = (int) floor(y / bixel_dy) * bixel_cols + floor(x
							/ bixel_dx);
					if (index >= 0 && index < bixels)
						grid[index] = cons(ln, grid[index]);
					x -= bixel_dx;
					y = y0 + (x - x0) * (dy / dx);
				}
			}
		} else {
			if (y <= y1) {
				while (y <= y1) {
					index = (int) floor(y / bixel_dy) * bixel_cols + floor(x
							/ bixel_dx);
					if (index >= 0 && index < bixels)
						grid[index] = cons(ln, grid[index]);

					y += bixel_dy;
					x = x0 + (y - y0) * (dx / dy);
				}
			} else {
				while (y >= y1) {
					index = (int) floor(y / bixel_dy) * bixel_cols + floor(x
							/ bixel_dx);
					if (index >= 0 && index < bixels)
						grid[index] = cons(ln, grid[index]);

					y -= bixel_dy;
					x = x0 + (y - y0) * (dx / dy);
				}
			}
		}

		index = (int) floor(y1 / bixel_dy) * bixel_cols + floor(x1 / bixel_dx);
		if (index >= 0 && index < bixels)
			grid[index] = cons(ln, grid[index]);

		ls = pair$cdr(ls);
	}

	sketch$bixel_rows(sp) = bixel_rows;
	sketch$bixel_cols(sp) = bixel_cols;
	sketch$bixel_dx(sp) = bixel_dx;
	sketch$bixel_dy(sp) = bixel_dy;
	sketch$grid(sp) = grid;
}

sexpr *po_link(sexpr *sp1, sexpr *sp2) {

	sexpr *ls = sketch$ls(sp1);
	sexpr **link_parameters = vector$data(sp2);

	int end;

	sexpr *ln, *lns;

	int rows, cols, get_lns_mode;

	float link_radius, contrast_ratio_min, contrast_ratio_max;
	float delta_theta_max, end_proj_rel_max, lateral_dist_abs_max_sq;

	float x, y;

	if (!sketch(sp1)) {
		printf("po-link!: First argument must be sketch.\n");
		longjmp(esc, 1);
	}

	rows = sketch$rows(sp1);
	cols = sketch$cols(sp1);

	if (!vector(sp2) || vector$length(sp2) != 7) {
		printf("po-link!: Link-parameters must be length seven vector.\n");
		longjmp(esc, 1);
	}

	if (!boolean(link_parameters[0])) {
		printf("po-link!: Get-lines-mode parameter must be boolean.\n");
		longjmp(esc, 1);
	}

	get_lns_mode = link_parameters[0]->u.i;

	if (!number(link_parameters[1])) {
		printf("po-link!: Link-radius must be number.\n");
		longjmp(esc, 1);
	}

	link_radius = link_parameters[1]->u.x;

	if (!number(link_parameters[2])) {
		printf("po-link!: Contrast-ratio-minimum parameter must be number.\n");
		longjmp(esc, 1);
	}

	contrast_ratio_min = link_parameters[2]->u.x;

	if (!number(link_parameters[3])) {
		printf("po-link!: Contrast-ratio-maximum parameter must be number.\n");
		longjmp(esc, 1);
	}

	contrast_ratio_max = link_parameters[3]->u.x;

	if (!number(link_parameters[4])) {
		printf("po-link!: Delta-theta-maximum parameter must be number.\n");
		longjmp(esc, 1);
	}

	delta_theta_max = link_parameters[4]->u.x;

	if (!number(link_parameters[5])) {
		printf(
				"po-link!: End-project-relative-maximum parameter must be number.\n");
		longjmp(esc, 1);
	}

	end_proj_rel_max = link_parameters[5]->u.x;

	if (!number(link_parameters[6])) {
		printf(
				"po-link!: Lateral-distance-absolute-maximum-squared parameter must be number.\n");
		longjmp(esc, 1);
	}

	lateral_dist_abs_max_sq = link_parameters[6]->u.x;

	if (get_lns_mode == 1)
		po_initialize_grid_for_all_lns(sp1, rows / (link_radius * 2.0), cols
				/ (link_radius * 2.0));
	else
		po_initialize_grid_for_ln_ends(sp1, rows / (link_radius * 2.0), cols
				/ (link_radius * 2.0));

	while (!null(ls)) {

		ln = pair$car(ls);
		for (end = 0; end <= 1; end++) {

			x = line$x(ln)[end];
			y = line$y(ln)[end];

			if (get_lns_mode == 1)
				lns = get_all_lns_in_circle(sp1, x, y, link_radius);
			else
				lns = get_all_lns_end_in_circle(sp1, x, y, opposite(end),
				link_radius);

			/* delete yourself */
			lns = delete(ln, lns);

			lns = po_ln_filter_magnitude(ln, lns, contrast_ratio_min,
					contrast_ratio_max);

			lns = po_ln_filter_direction(ln, lns, delta_theta_max);

			lns = po_ln_filter_end_project(ln, end, lns, end_proj_rel_max);

			lns = po_ln_filter_lateral_distance(ln, lns,
					lateral_dist_abs_max_sq);

			line$links(ln)[end] = lns;
		}
		ls = pair$cdr(ls);
	}
	return sp1;
}

sexpr *make_sketch(sexpr *rows, sexpr *cols, sexpr *ls) {

	sexpr *result;
	sexpr *ln;

	if (!number(rows) || !number(cols)) {
		printf("make-sketch: First and second arguments must be numbers.\n");
		longjmp(esc, 1);
	}

	result = (sexpr *) smalloc;
	smalloc += sizeof(sketch);
	result->type = SKETCH;
	sketch$rows(result) = rows->u.x;
	sketch$cols(result) = cols->u.x;
	sketch$bixel_rows(result) = 0;
	sketch$bixel_cols(result) = 0;
	sketch$count(result) = 0;
	sketch$grid(result) = NULL;
	sketch$ls(result) = nil;

	while (!null(ls)) {
		if (!pair(ls)) {
			printf("make-sketch: Third argument must be list.\n");
			longjmp(esc, 1);
		}
		ln = pair$car(ls);
		if (!line(ln)) {
			printf("make-sketch: Third argument must be list of lines.\n");
			longjmp(esc, 1);
		}
		sketch$ls(result) = cons(ln, sketch$ls(result));
		sketch$count(result)++;
		ls = pair$cdr(ls);
	}

	return result;
}

void add_new_line(float x0, float y0, float x1, float y1, float contrast,
		float coverage, sexpr *sp1) {
	sexpr *sp2;
	float dx = x1 - x0;
	float dy = y1 - y0;
	sp2 = (sexpr *) smalloc;
	smalloc += sizeof(line);
	sp2->type = LINE;
	line$x(sp2)[0] = x0;
	line$y(sp2)[0] = y0;
	line$x(sp2)[1] = x1;
	line$y(sp2)[1] = y1;
	line$theta(sp2) = atan2(dy, dx);
	line$contrast(sp2) = contrast;
	line$length(sp2) = sqrt(dx * dx + dy * dy);
	line$coverage(sp2) = coverage;
	line$age(sp2) = 0;
	line$replaced(sp2) = 0;
	line$merged(sp2) = 0;
	line$links(sp2)[0] = nil;
	line$links(sp2)[1] = nil;
	sketch$count(sp1)++;
	sketch$ls(sp1) = cons(sp2, sketch$ls(sp1));
}

void add_old_line(sexpr *sp0, sexpr *sp1) {
	line$age(sp0)++;
	line$links(sp0)[0] = nil;
	line$links(sp0)[1] = nil;
	sketch$count(sp1)++;
	sketch$ls(sp1) = cons(sp0, sketch$ls(sp1));
}

sexpr *po_merge(sexpr *sp1, sexpr *sp2) {

	sexpr *ls, *ln;

	if (!sketch(sp1) || !sketch(sp2) || sketch$rows(sp1) != sketch$rows(sp2) || sketch$cols(sp1) != sketch$cols(sp2)) {
		printf("po-merge!: Arguments must be sketches with equal dimensions.\n");
		longjmp(esc, 1);
	}

	/* so grids will be gc'd */
	sketch$rows(sp2) = 0;

	ls = sketch$ls(sp2);

	while (!null(ls)) {
		ln = pair$car(ls);
		if (line$replaced(ln) == 0 && line$merged(ln) == 0 && !member(ln, sketch$ls(sp1))) {
			line$merged(ln) = 1;
			sketch$count(sp1)++;
			sketch$ls(sp1) = cons(ln, sketch$ls(sp1));
		}
		ls = pair$cdr(ls);
	}

	return sp1;
}

sexpr *sketch_union(sexpr *sp1, sexpr *sp2) {

	sexpr *result, *ls1, *ls2, *ln;

	if (!sketch(sp1) || !sketch(sp2) || sketch$rows(sp1) != sketch$rows(sp2) || sketch$cols(sp1) != sketch$cols(sp2)) {
		printf(
				"sketch-union: Arguments must be sketches with equal dimensions.\n");
		longjmp(esc, 1);
	}

	result = make_sketch(num2exp(sketch$rows(sp1)), num2exp(sketch$cols(sp1)), nil);

	ls1 = sketch$ls(sp1);
	ls2 = sketch$ls(sp2);

	while (!null(ls1)) {
		ln = pair$car(ls1);
		sketch$count(result)++;
		sketch$ls(result) = cons(ln, sketch$ls(result));
		ls1 = pair$cdr(ls1);
	}

	while (!null(ls2)) {
		ln = pair$car(ls2);
		if (!member(ln, sketch$ls(result))) {
			sketch$count(result)++;
			sketch$ls(result) = cons(ln, sketch$ls(result));
		}
		ls2 = pair$cdr(ls2);
	}

	return result;
}

sexpr *sketch_lines(sexpr *sp) {
	if (!sketch(sp)) {
		printf("sketch-lines: Argument is not a sketch.\n");
		longjmp(esc, 1);
	}
	return sketch$ls(sp);
}

sexpr *sketch_rows(sexpr *sp) {
	if (!sketch(sp)) {
		printf("sketch-rows: Argument is not a sketch.\n");
		longjmp(esc, 1);
	}
	return num2exp(sketch$rows(sp));
}

sexpr *sketch_cols(sexpr *sp) {
	if (!sketch(sp)) {
		printf("sketch-cols: Argument is not a sketch.\n");
		longjmp(esc, 1);
	}
	return num2exp(sketch$cols(sp));
}

sexpr *sketch_count(sexpr *sp) {
	if (!sketch(sp)) {
		printf("sketch-count: Argument is not a sketch.\n");
		longjmp(esc, 1);
	}
	return num2exp(sketch$count(sp));
}

sexpr *line_x0(sexpr *sp) {
	if (!line(sp)) {
		printf("line-x0: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$x(sp)[0]);
}

sexpr *line_y0(sexpr *sp) {
	if (!line(sp)) {
		printf("line-y0: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$y(sp)[0]);
}

sexpr *line_x1(sexpr *sp) {
	if (!line(sp)) {
		printf("line-x1: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$x(sp)[1]);
}

sexpr *line_y1(sexpr *sp) {
	if (!line(sp)) {
		printf("line-y1: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$y(sp)[1]);
}

sexpr *line_contrast(sexpr *sp) {
	if (!line(sp)) {
		printf("line-contrast: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$contrast(sp));
}

sexpr *line_theta(sexpr *sp) {
	if (!line(sp)) {
		printf("line-theta: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$theta(sp));
}

sexpr *line_length(sexpr *sp) {
	if (!line(sp)) {
		printf("line-length: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return num2exp(line$length(sp));
}

sexpr *line_links(sexpr *sp) {
	if (!line(sp)) {
		printf("line-links: Argument is not a line.\n");
		longjmp(esc, 1);
	}
	return list2(line$links(sp)[0], line$links(sp)[1]);
}

sexpr *set_line_contrast(sexpr *sp1, sexpr *sp2) {
	if (!line(sp1)) {
		printf("set-line-contrast!: First argument is not a line.\n");
		longjmp(esc, 1);
	}

	if (!number(sp2)) {
		printf("set-line-contrast!: Second argument is not a number.\n");
		longjmp(esc, 1);
	}

	line$contrast(sp1) = sp2->u.x;

	return undefined;

}

sexpr *po_replace(sexpr *sp1, sexpr *sp2) {

	sexpr **replace_parameters = vector$data(sp2);

	int i;

	float x, y, xm, ym, dx, dy;
	float x0, y0, x1, y1, u0, v0, u1, v1;
	float x00, y00, x01, y01, x10, y10, x11, y11;
	float u00, v00, u01, v01, u10, v10, u11, v11;
	float dot00, dot01, dot0, dot1, dot10, dot11;
	float cumulative_coverage, coverage;
	float scale, mse, best_mse;
	float best_avg_contrast = 0, best_cumulative_coverage = 0;
	float c, c0, c1, l, l0, l1, ll;
	float m, m0, m1, n0, n1, o0, o1;
	float k, k0, k1;

	sexpr *best_ln = nil, *best_ln0 = nil, *best_ln1 = nil;

	float best_u00 = 0, best_v00 = 0, best_u0 = 0, best_v0 = 0;
	float best_u1 = 0, best_v1 = 0, best_u11 = 0, best_v11 = 0;

	int new, copied, paths, path_histogram[10];

	sexpr *ls, *ls0, *ls1;
	sexpr *ln, *ln0, *ln1;
	double **A, *s;
	sexpr *result;

	float replace_radius;
	float straightness;
	int coverage_filter_on;
	float min_cumulative_coverage;
	float min_coverage;
	int delta_abst_lev_max;
	int replace_straightest_path;
	float rr;

	if (!sketch(sp1)) {
		printf("po-replace!: First argument must be sketch.\n");
		longjmp(esc, 1);
	}

	result = make_sketch(num2exp(sketch$rows(sp1)), num2exp(sketch$cols(sp1)), nil);

	if (!vector(sp2) || vector$length(sp2) != 7) {
		printf("po-replace!: Replace parameters must be length seven vector.\n");
		longjmp(esc, 1);
	}

	if (!number(replace_parameters[0])) {
		printf("po-replace!: Replace-radius must be number.\n");
		longjmp(esc, 1);
	}

	replace_radius = replace_parameters[0]->u.x;

	if (!number(replace_parameters[1])) {
		printf("po-replace!: Straightness-threshold must be number.\n");
		longjmp(esc, 1);
	}

	straightness = replace_parameters[1]->u.x;

	if (!boolean(replace_parameters[2])) {
		printf("po-replace!: Coverage-filter-on parameter must be boolean.\n");
		longjmp(esc, 1);
	}

	coverage_filter_on = replace_parameters[2]->u.i;

	if (!number(replace_parameters[3])) {
		printf(
				"po-replace!: Minimum-cumulative-coverage parameter must be number.\n");
		longjmp(esc, 1);
	}

	min_cumulative_coverage = replace_parameters[3]->u.x;

	if (!number(replace_parameters[4])) {
		printf("po-replace!: Minimum-coverage-parameter must be number.\n");
		longjmp(esc, 1);
	}

	min_coverage = replace_parameters[4]->u.x;

	if (!number(replace_parameters[5])) {
		printf(
				"po-replace!: Delta-abstraction-level-maximum parameter must be number.\n");
		longjmp(esc, 1);
	}

	delta_abst_lev_max = (int) replace_parameters[5]->u.x;

	if (!boolean(replace_parameters[6])) {
		printf(
				"po-replace!: replace-straightest-path parameter must be boolean.\n");
		longjmp(esc, 1);
	}

	replace_straightest_path = replace_parameters[6]->u.i;

	rr = replace_radius * replace_radius;

	A = (double **) malloc(4 * sizeof(double *));
	s = (double *) malloc(2 * sizeof(double));

	new = copied = 0;

	for (i = 0; i < 4; i++)
		A[i] = malloc(2 * sizeof(double));

	for (i = 0; i <= 9; i++)
		path_histogram[i] = 0;

	/* Enumerate length three paths */
	ls = sketch$ls(sp1);

	while (!null(ls)) {

		paths = 0;
		best_mse = FLT_MAX;

		ln = pair$car(ls);

		if (line$replaced(ln)) {
			ls = pair$cdr(ls);
			continue;
		}

		x0 = line$x(ln)[0];
		y0 = line$y(ln)[0];

		x1 = line$x(ln)[1];
		y1 = line$y(ln)[1];

		x = (x0 + x1) / 2.0;
		y = (y0 + y1) / 2.0;

		ls0 = line$links(ln)[0];
		while (!null(ls0)) {

			ln0 = pair$car(ls0);

			if (line$replaced(ln0)) {
				ls0 = pair$cdr(ls0);
				continue;
			}

			x00 = line$x(ln0)[0];
			y00 = line$y(ln0)[0];
			if (((x - x00) * (x - x00) + (y - y00) * (y - y00)) > rr) {
				ls0 = pair$cdr(ls0);
				continue;
			}

			x01 = line$x(ln0)[1];
			y01 = line$y(ln0)[1];

			ls1 = line$links(ln)[1];
			while (!null(ls1)) {

				ln1 = pair$car(ls1);

				if (line$replaced(ln1)) {
					ls1 = pair$cdr(ls1);
					continue;
				}

				x11 = line$x(ln1)[1];
				y11 = line$y(ln1)[1];

				if (((x - x11) * (x - x11) + (y - y11) * (y - y11)) > rr) {
					ls1 = pair$cdr(ls1);
					continue;
				}

				x10 = line$x(ln1)[0];
				y10 = line$y(ln1)[0];

				xm = (x00 + x01 + x0 + x1 + x10 + x11) / 6.0;
				ym = (y00 + y01 + y0 + y1 + y10 + y11) / 6.0;

				A[0][0] = (x00 - xm) * (x00 - xm) + (x01 - xm) * (x01 - xm)
						+ (x0 - xm) * (x0 - xm) + (x1 - xm) * (x1 - xm) + (x10
						- xm) * (x10 - xm) + (x11 - xm) * (x11 - xm);

				A[1][1] = (y00 - ym) * (y00 - ym) + (y01 - ym) * (y01 - ym)
						+ (y0 - ym) * (y0 - ym) + (y1 - ym) * (y1 - ym) + (y10
						- ym) * (y10 - ym) + (y11 - ym) * (y11 - ym);

				A[0][1] = A[1][0] = (x00 - xm) * (y00 - ym) + (x01 - xm) * (y01
						- ym) + (x0 - xm) * (y0 - ym) + (x1 - xm) * (y1 - ym)
						+ (x10 - xm) * (y10 - ym) + (x11 - xm) * (y11 - ym);

				svd(A, s, 2);

				l = A[2][0] * A[2][0] + A[2][1] * A[2][1];

				dx = -A[2][0] / l;
				dy = A[2][1] / l;

				dot00 = dx * (x00 - xm) + dy * (y00 - ym);

				u00 = xm + dot00 * dx;
				v00 = ym + dot00 * dy;

				dot01 = dx * (x01 - xm) + dy * (y01 - ym);

				u01 = xm + dot01 * dx;
				v01 = ym + dot01 * dy;

				dot0 = dx * (x0 - xm) + dy * (y0 - ym);

				u0 = xm + dot0 * dx;
				v0 = ym + dot0 * dy;

				dot1 = dx * (x1 - xm) + dy * (y1 - ym);

				u1 = xm + dot1 * dx;
				v1 = ym + dot1 * dy;

				dot10 = dx * (x10 - xm) + dy * (y10 - ym);

				u10 = xm + dot10 * dx;
				v10 = ym + dot10 * dy;

				dot11 = dx * (x11 - xm) + dy * (y11 - ym);

				u11 = xm + dot11 * dx;
				v11 = ym + dot11 * dy;

				l0 = line$length(ln0);
				l = line$length(ln);
				l1 = line$length(ln1);

				scale = ((x00 - x11) * (x00 - x11) + (y00 - y11) * (y00 - y11))
						* 4.0;

				mse = (pt_line_dist_sq(u00, v00, u11, v11, x00, y00) * l0
						+ pt_line_dist_sq(u00, v00, u11, v11, x01, y01) * l0
						+ pt_line_dist_sq(u00, v00, u11, v11, x0, y0) * l
						+ pt_line_dist_sq(u00, v00, u11, v11, x1, y1) * l
						+ pt_line_dist_sq(u00, v00, u11, v11, x10, y10) * l1
						+ pt_line_dist_sq(u00, v00, u11, v11, x11, y11) * l1)
						/ scale;

				paths++;

				if (mse < best_mse && mse < straightness) {

					m0 = sqrt((u01 - u00) * (u01 - u00) + (v01 - v00) * (v01
							- v00));
					m = sqrt((u1 - u0) * (u1 - u0) + (v1 - v0) * (v1 - v0));
					m1 = sqrt((u11 - u10) * (u11 - u10) + (v11 - v10) * (v11
							- v10));

					n0
							= sqrt((u00 - u0) * (u00 - u0) + (v00 - v0) * (v00
									- v0));
					n1
							= sqrt((u11 - u1) * (u11 - u1) + (v11 - v1) * (v11
									- v1));

					if (n0 < m0)
						o0 = m0 - n0;
					else
						o0 = 0.0;

					if (n1 < m1)
						o1 = m1 - n1;
					else
						o1 = 0.0;

					k0 = line$coverage(ln0);
					k = line$coverage(ln);
					k1 = line$coverage(ln1);

					ll = sqrt((u11 - u00) * (u11 - u00) + (v11 - v00) * (v11
							- v00));

					cumulative_coverage = (m0 * k0 + m * k + m1 * k1 - o0 - o1)
							/ ll;
					coverage = (m0 + m + m1 - o0 - o1) / ll;

					if (coverage_filter_on == 0 || (coverage_filter_on == 1
							&& cumulative_coverage > min_cumulative_coverage
							&& coverage > min_coverage)) {

						best_mse = mse;

						c0 = line$contrast(ln0);
						c = line$contrast(ln);
						c1 = line$contrast(ln1);

						best_avg_contrast = (c0 * l0 + c * l + c1 * l1) / (l0
								+ l + l1);
						best_cumulative_coverage = cumulative_coverage;

						best_u00 = u00;
						best_v00 = v00;
						best_u11 = u11;
						best_v11 = v11;

						best_ln0 = ln0;
						best_ln = ln;
						best_ln1 = ln1;

						if (!replace_straightest_path)
							goto break_break;
					}
				}
				ls1 = pair$cdr(ls1);
			}
			ls0 = pair$cdr(ls0);
		}

		break_break:

		if (best_mse < FLT_MAX) {

			new++;

			add_new_line(best_u00, best_v00, best_u11, best_v11,
					best_avg_contrast, best_cumulative_coverage, result);

			line$replaced(best_ln0) = 1;
			line$replaced(best_ln) = 1;
			line$replaced(best_ln1) = 1;

			/*
			 if (paths < 9)
			 path_histogram[paths]++;
			 else
			 path_histogram[10]++;
			 */
		}

		ls = pair$cdr(ls);
	}

	/*
	 printf("length three path histogram: \n");
	 for (i = 0; i < 10; i++) printf("%d(%d) ",i,path_histogram[i]);
	 printf("\n");
	 */

	for (i = 0; i <= 9; i++)
		path_histogram[i] = 0;

	/* Enumerate length two left-paths */
	ls = sketch$ls(sp1);

	while (!null(ls)) {

		paths = 0;
		best_mse = FLT_MAX;

		ln = pair$car(ls);

		if (line$replaced(ln)) {
			ls = pair$cdr(ls);
			continue;
		}

		x0 = line$x(ln)[0];
		y0 = line$y(ln)[0];

		x1 = line$x(ln)[1];
		y1 = line$y(ln)[1];

		x = (x0 + x1) / 2.0;
		y = (y0 + y1) / 2.0;

		ls0 = line$links(ln)[0];
		while (!null(ls0)) {

			ln0 = pair$car(ls0);

			if (line$replaced(ln0)) {
				ls0 = pair$cdr(ls0);
				continue;
			}

			x00 = line$x(ln0)[0];
			y00 = line$y(ln0)[0];
			if (((x - x00) * (x - x00) + (y - y00) * (y - y00)) > rr) {
				ls0 = pair$cdr(ls0);
				continue;
			}

			x01 = line$x(ln0)[1];
			y01 = line$y(ln0)[1];

			xm = (x00 + x01 + x0 + x1) / 4.0;
			ym = (y00 + y01 + y0 + y1) / 4.0;

			A[0][0] = (x00 - xm) * (x00 - xm) + (x01 - xm) * (x01 - xm) + (x0
					- xm) * (x0 - xm) + (x1 - xm) * (x1 - xm);
			A[1][1] = (y00 - ym) * (y00 - ym) + (y01 - ym) * (y01 - ym) + (y0
					- ym) * (y0 - ym) + (y1 - ym) * (y1 - ym);
			A[0][1] = A[1][0] = (x00 - xm) * (y00 - ym) + (x01 - xm) * (y01
					- ym) + (x0 - xm) * (y0 - ym) + (x1 - xm) * (y1 - ym);

			svd(A, s, 2);

			l = A[2][0] * A[2][0] + A[2][1] * A[2][1];

			dx = -A[2][0] / l;
			dy = A[2][1] / l;

			dot00 = dx * (x00 - xm) + dy * (y00 - ym);

			u00 = xm + dot00 * dx;
			v00 = ym + dot00 * dy;

			dot01 = dx * (x01 - xm) + dy * (y01 - ym);

			u01 = xm + dot01 * dx;
			v01 = ym + dot01 * dy;

			dot0 = dx * (x0 - xm) + dy * (y0 - ym);

			u0 = xm + dot0 * dx;
			v0 = ym + dot0 * dy;

			dot1 = dx * (x1 - xm) + dy * (y1 - ym);

			u1 = xm + dot1 * dx;
			v1 = ym + dot1 * dy;

			l0 = line$length(ln0);
			l = line$length(ln);

			scale = ((x00 - x1) * (x00 - x1) + (y00 - y1) * (y00 - y1)) * 2.0;

			mse = (pt_line_dist_sq(u00, v00, u1, v1, x00, y00) * l0
					+ pt_line_dist_sq(u00, v00, u1, v1, x01, y01) * l0
					+ pt_line_dist_sq(u00, v00, u1, v1, x0, y0) * l
					+ pt_line_dist_sq(u00, v00, u1, v1, x1, y1) * l) / scale;

			paths++;

			if (mse < best_mse && mse < straightness) {

				m0
						= sqrt((u01 - u00) * (u01 - u00) + (v01 - v00) * (v01
								- v00));
				m = sqrt((u1 - u0) * (u1 - u0) + (v1 - v0) * (v1 - v0));

				n0 = sqrt((u00 - u0) * (u00 - u0) + (v00 - v0) * (v00 - v0));

				if (n0 < m0)
					o0 = m0 - n0;
				else
					o0 = 0.0;

				k0 = line$coverage(ln0);
				k = line$coverage(ln);

				ll = sqrt((u1 - u00) * (u1 - u00) + (v1 - v00) * (v1 - v00));

				cumulative_coverage = (m0 * k0 + m * k - o0) / ll;
				coverage = (m0 + m - o0) / ll;

				if (coverage_filter_on == 0 || (coverage_filter_on == 1
						&& cumulative_coverage > min_cumulative_coverage
						&& coverage > min_coverage)) {

					best_mse = mse;

					c0 = line$contrast(ln0);
					c = line$contrast(ln);

					best_avg_contrast = (c0 * l0 + c * l) / (l0 + l);
					best_cumulative_coverage = cumulative_coverage;

					best_u00 = u00;
					best_v00 = v00;
					best_u1 = u1;
					best_v1 = v1;

					best_ln0 = ln0;
					best_ln = ln;

					if (!replace_straightest_path)
						break;
				}
			}
			ls0 = pair$cdr(ls0);
		}

		if (best_mse < FLT_MAX) {

			new++;

			add_new_line(best_u00, best_v00, best_u1, best_v1,
					best_avg_contrast, best_cumulative_coverage, result);

			line$replaced(best_ln0) = 1;
			line$replaced(best_ln) = 1;

			/*
			 if (paths < 9)
			 path_histogram[paths]++;
			 else
			 path_histogram[10]++;
			 */
		}

		ls = pair$cdr(ls);
	}

	/* Enumerate length two right-paths */
	ls = sketch$ls(sp1);

	while (!null(ls)) {

		paths = 0;
		best_mse = FLT_MAX;

		ln = pair$car(ls);

		if (line$replaced(ln)) {
			ls = pair$cdr(ls);
			continue;
		}

		x0 = line$x(ln)[0];
		y0 = line$y(ln)[0];

		x1 = line$x(ln)[1];
		y1 = line$y(ln)[1];

		x = (x0 + x1) / 2.0;
		y = (y0 + y1) / 2.0;

		ls1 = line$links(ln)[1];
		while (!null(ls1)) {

			ln1 = pair$car(ls1);

			if (line$replaced(ln1)) {
				ls1 = pair$cdr(ls1);
				continue;
			}

			x11 = line$x(ln1)[1];
			y11 = line$y(ln1)[1];

			if (((x - x11) * (x - x11) + (y - y11) * (y - y11)) > rr) {
				ls1 = pair$cdr(ls1);
				continue;
			}

			x10 = line$x(ln1)[0];
			y10 = line$y(ln1)[0];

			xm = (x0 + x1 + x10 + x11) / 4.0;
			ym = (y0 + y1 + y10 + y11) / 4.0;

			A[0][0] = (x0 - xm) * (x0 - xm) + (x1 - xm) * (x1 - xm)
					+ (x10 - xm) * (x10 - xm) + (x11 - xm) * (x11 - xm);
			A[1][1] = (y0 - ym) * (y0 - ym) + (y1 - ym) * (y1 - ym)
					+ (y10 - ym) * (y10 - ym) + (y11 - ym) * (y11 - ym);
			A[0][1] = A[1][0] = (x0 - xm) * (y0 - ym) + (x1 - xm) * (y1 - ym)
					+ (x10 - xm) * (y10 - ym) + (x11 - xm) * (y11 - ym);

			svd(A, s, 2);

			l = A[2][0] * A[2][0] + A[2][1] * A[2][1];

			dx = -A[2][0] / l;
			dy = A[2][1] / l;

			dot0 = dx * (x0 - xm) + dy * (y0 - ym);

			u0 = xm + dot0 * dx;
			v0 = ym + dot0 * dy;

			dot1 = dx * (x1 - xm) + dy * (y1 - ym);

			u1 = xm + dot1 * dx;
			v1 = ym + dot1 * dy;

			dot10 = dx * (x10 - xm) + dy * (y10 - ym);

			u10 = xm + dot10 * dx;
			v10 = ym + dot10 * dy;

			dot11 = dx * (x11 - xm) + dy * (y11 - ym);

			u11 = xm + dot11 * dx;
			v11 = ym + dot11 * dy;

			l = line$length(ln);
			l1 = line$length(ln1);

			scale = ((x0 - x11) * (x0 - x11) + (y0 - y11) * (y0 - y11)) * 2.0;

			mse = (pt_line_dist_sq(u0, v0, u11, v11, x0, y0) * l
					+ pt_line_dist_sq(u0, v0, u11, v11, x1, y1) * l
					+ pt_line_dist_sq(u0, v0, u11, v11, x10, y10) * l1
					+ pt_line_dist_sq(u0, v0, u11, v11, x11, y11) * l1) / scale;

			paths++;

			if (mse < best_mse && mse < straightness) {

				m = sqrt((u1 - u0) * (u1 - u0) + (v1 - v0) * (v1 - v0));
				m1
						= sqrt((u11 - u10) * (u11 - u10) + (v11 - v10) * (v11
								- v10));

				n1 = sqrt((u11 - u1) * (u11 - u1) + (v11 - v1) * (v11 - v1));

				if (n1 < m1)
					o1 = m1 - n1;
				else
					o1 = 0.0;

				k = line$coverage(ln);
				k1 = line$coverage(ln1);

				ll = sqrt((u11 - u0) * (u11 - u0) + (v11 - v0) * (v11 - v0));

				cumulative_coverage = (m * k + m1 * k1 - o1) / ll;
				coverage = (m + m1 - o1) / ll;

				if (coverage_filter_on == 0 || (coverage_filter_on == 1
						&& cumulative_coverage > min_cumulative_coverage
						&& coverage > min_coverage)) {

					best_mse = mse;

					c = line$contrast(ln);
					c1 = line$contrast(ln1);

					best_avg_contrast = (c * l + c1 * l1) / (l + l1);
					best_cumulative_coverage = cumulative_coverage;

					best_u0 = u0;
					best_v0 = v0;
					best_u11 = u11;
					best_v11 = v11;

					best_ln = ln;
					best_ln1 = ln1;

					if (!replace_straightest_path)
						break;
				}
			}

			ls1 = pair$cdr(ls1);
		}

		if (best_mse < FLT_MAX) {

			new++;

			add_new_line(best_u0, best_v0, best_u11, best_v11,
					best_avg_contrast, best_cumulative_coverage, result);

			line$replaced(best_ln) = 1;
			line$replaced(best_ln1) = 1;

			/*
			 if (paths < 9)
			 path_histogram[paths]++;
			 else
			 path_histogram[10]++;
			 */
		}

		ls = pair$cdr(ls);
	}

	/*
	 printf("length two path histogram: \n");
	 for (i = 0; i < 10; i++) printf("%d(%d) ",i,path_histogram[i]);
	 printf("\n");
	 */

	/* Copy non-replaced lines */
	ls = sketch$ls(sp1);
	while (!null(ls)) {
		ln = pair$car(ls);
		if (line$replaced(ln) == 0 && line$age(ln) < delta_abst_lev_max) {
			add_old_line(ln, result);
			copied++;
		}
		ls = pair$cdr(ls);
	}

	/*
	 printf("new: %d copied: %d total: %d\n",new,copied,new+copied);
	 */

	return result;
}

void add_zc0(float x0, float y0, float x1, float y1, float contrast, sexpr *sp1) {
	sexpr *sp2;
	float dx = x1 - x0;
	float dy = y1 - y0;
	sp2 = (sexpr *) smalloc;
	smalloc += sizeof(line);
	sp2->type = LINE;
	line$x(sp2)[0] = x0;
	line$y(sp2)[0] = y0;
	line$x(sp2)[1] = x1;
	line$y(sp2)[1] = y1;
	line$theta(sp2) = atan2(dy, dx);
	line$contrast(sp2) = contrast;
	line$length(sp2) = sqrt(dx * dx + dy * dy);
	line$coverage(sp2) = 1.0;
	line$age(sp2) = 0;
	line$replaced(sp2) = 0;
	line$merged(sp2) = 0;
	line$links(sp2)[0] = nil;
	line$links(sp2)[1] = nil;
	sketch$count(sp1)++;
	sketch$ls(sp1) = cons(sp2, sketch$ls(sp1));
}

void add_zc1(float x, float y, float theta, float contrast, sexpr *sp1) {
	sexpr *sp2;
	float dx = cos(theta) / 2.0;
	float dy = sin(theta) / 2.0;
	sp2 = (sexpr *) smalloc;
	smalloc += sizeof(line);
	sp2->type = LINE;
	line$x(sp2)[0] = x - dx;
	line$y(sp2)[0] = y - dy;
	line$x(sp2)[1] = x + dx;
	line$y(sp2)[1] = y + dy;
	line$theta(sp2) = theta;
	line$contrast(sp2) = contrast;
	line$length(sp2) = 1.0;
	line$coverage(sp2) = 1.0;
	line$age(sp2) = 0;
	line$replaced(sp2) = 0;
	line$merged(sp2) = 0;
	line$links(sp2)[0] = nil;
	line$links(sp2)[1] = nil;
	sketch$count(sp1)++;
	sketch$ls(sp1) = cons(sp2, sketch$ls(sp1));
}

sexpr *make_line(sexpr *x0, sexpr *y0, sexpr *x1, sexpr *y1, sexpr *contrast) {

	sexpr *result;

	if (!number(x0) || !number(y0) || !number(x1) || !number(y1) || !number(contrast)) {
		printf("make-line: Illegal argument.\n");
		longjmp(esc, 1);
	}

	result = (sexpr *) smalloc;
	smalloc += sizeof(line);
	result->type = LINE;
	line$x(result)[0] = x0->u.x;
	line$y(result)[0] = y0->u.x;
	line$x(result)[1] = x1->u.x;
	line$y(result)[1] = y1->u.x;
	line$theta(result) = atan2(y1->u.x - y0->u.x, x1->u.x - x0->u.x);
	line$contrast(result) = contrast->u.x;
	line$length(result) = sqrt((x1->u.x - x0->u.x) * (x1->u.x - x0->u.x) + (y1->u.x
			- y0->u.x) * (y1->u.x - y0->u.x));
	line$coverage(result) = 1.0;
	line$age(result) = 0;
	line$replaced(result) = 0;
	line$links(result)[0] = nil;
	line$links(result)[1] = nil;

	return result;
}

#define interpolate(f00,f01,f10,f11,x,y) (f01-f00)*x+(f10-f00)*y+(f11+f00-f10-f01)*x*y+f00;

sexpr *zerocrossings(sexpr *sp1, sexpr *sp2, sexpr *sp3, sexpr *sp4) {

	int i, j, rows, cols;

	unsigned code;

	float *lapl, *dx, *dy;
	float contrast;
	float lapl00, lapl01, lapl10, lapl11;
	float dx00, dx01, dx10, dx11;
	float dy00, dy01, dy10, dy11;
	float x0, y0, x1, y1, dxc, dyc, xc, yc;

	int method;
	sexpr *result;

	x0 = y0 = x1 = y1 = 0.0;

	if (!image(sp1) || !image(sp2) || !image(sp3)) {
		printf("zero-crossings: First three arguments must be images.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	check_image_sizes("zero-crossings", image$rows(sp1), image$cols(sp1), sp2);
	check_image_sizes("zero-crossings", image$rows(sp1), image$cols(sp1), sp3);

	if (!number(sp4)) {
		printf("zero-crossings: Method parameter must be number.\n");
		longjmp(esc, 1);
	}

	method = sp4->u.x;

	if (method != 0 && method != 1 && method != 2) {
		printf("zero-crossings: Method parameter must equal 0, 1, or 2.\n");
		longjmp(esc, 1);
	}

	result = (sexpr *) smalloc;
	smalloc += sizeof(sketch);
	result->type = SKETCH;
	sketch$rows(result) = rows;
	sketch$cols(result) = cols;
	sketch$bixel_rows(result) = 0;
	sketch$bixel_cols(result) = 0;
	sketch$count(result) = 0;
	sketch$grid(result) = NULL;
	sketch$ls(result) = nil;

	lapl = image$data(sp1);
	dx = image$data(sp2);
	dy = image$data(sp3);

	for (i = 0; i < rows - 1; i++) {
		for (j = 0; j < cols - 1; j++) {

			/* 00 -- 01 */
			/* |     |  */
			/* 10 -- 11 */
			lapl00 = lapl[i * cols + j];
			lapl01 = lapl[i * cols + j + 1];
			lapl10 = lapl[(i + 1) * cols + j];
			lapl11 = lapl[(i + 1) * cols + j + 1];

			code = 0;

			if (lapl00 * lapl01 * lapl10 * lapl11 != 0) {
				if (lapl00> 0)
				code += 1;
				if (lapl01> 0)
				code += 2;
				if (lapl10> 0)
				code += 4;
				if (lapl11> 0)
				code += 8;
			}

			/* + to the right */
			switch (code) {
				case 0:
				/* - - */
				/* - - */
				continue;
				case 1:
				/* + - */
				/* - - */
				x0 = j + lapl00 / (lapl00 - lapl01);
				y0 = i;
				x1 = j;
				y1 = i + lapl00 / (lapl00 - lapl10);
				break;
				case 2:
				/* - + */
				/* - - */
				x0 = j + 1;
				y0 = i + lapl01 / (lapl01 - lapl11);
				x1 = j + lapl00 / (lapl00 - lapl01);
				y1 = i;
				break;
				case 3:
				/* + + */
				/* - - */
				x0 = j + 1;
				y0 = i + lapl01 / (lapl01 - lapl11);
				x1 = j;
				y1 = i + lapl00 / (lapl00 - lapl10);
				break;
				case 4:
				/* - - */
				/* + - */
				x0 = j;
				y0 = i + lapl00 / (lapl00 - lapl10);
				x1 = j + lapl10 / (lapl10 - lapl11);
				y1 = i + 1;
				break;
				case 5:
				/* + - */
				/* + - */
				x0 = j + lapl00 / (lapl00 - lapl01);
				y0 = i;
				x1 = j + lapl10 / (lapl10 - lapl11);
				y1 = i + 1;
				break;
				case 6:
				/* - + */
				/* + - */
				break;
				case 7:
				/* + + */
				/* + - */
				x0 = j + 1;
				y0 = i + lapl01 / (lapl01 - lapl11);
				x1 = j + lapl10 / (lapl10 - lapl11);
				y1 = i + 1;
				break;
				case 8:
				/* - - */
				/* - + */
				x0 = j + lapl10 / (lapl10 - lapl11);
				y0 = i + 1;
				x1 = j + 1;
				y1 = i + lapl01 / (lapl01 - lapl11);
				break;
				case 9:
				/* + - */
				/* - + */
				continue;
				case 10:
				/* - + */
				/* - + */
				x0 = j + lapl10 / (lapl10 - lapl11);
				y0 = i + 1;
				x1 = j + lapl00 / (lapl00 - lapl01);
				y1 = i;
				break;
				case 11:
				/* + + */
				/* - + */
				x0 = j + lapl10 / (lapl10 - lapl11);
				y0 = i + 1;
				x1 = j;
				y1 = i + lapl00 / (lapl00 - lapl10);
				break;
				case 12:
				/* - - */
				/* + + */
				x0 = j;
				y0 = i + lapl00 / (lapl00 - lapl10);
				x1 = j + 1;
				y1 = i + lapl01 / (lapl01 - lapl11);
				break;
				case 13:
				/* + - */
				/* + + */
				x0 = j + lapl00 / (lapl00 - lapl01);
				y0 = i;
				x1 = j + 1;
				y1 = i + lapl01 / (lapl01 - lapl11);
				break;
				case 14:
				/* - + */
				/* + + */
				x0 = j;
				y0 = i + lapl00 / (lapl00 - lapl10);
				x1 = j + lapl00 / (lapl00 - lapl01);
				y1 = i;
				break;
				case 15:
				/* + + */
				/* + + */
				continue;
				default:
				continue;
			}

			xc = (x0 + x1) / 2 - j;
			yc = (y0 + y1) / 2 - i;

			dx00 = dx[i * cols + j];
			dx01 = dx[i * cols + j + 1];
			dx10 = dx[(i + 1) * cols + j];
			dx11 = dx[(i + 1) * cols + j + 1];

			dy00 = dy[i * cols + j];
			dy01 = dy[i * cols + j + 1];
			dy10 = dy[(i + 1) * cols + j];
			dy11 = dy[(i + 1) * cols + j + 1];

			dxc = interpolate(dx00,dx01,dx10,dx11,xc,yc);
			dyc = interpolate(dy00,dy01,dy10,dy11,xc,yc);
			contrast = sqrt(dxc * dxc + dyc * dyc);

			switch (method) {
				case 0:
				add_zc0(x0, y0, x1, y1, contrast, result);
				break;
				case 1:
				add_zc1((x0 + x1) / 2, (y0 + y1) / 2, atan2(-dxc, dyc),
						contrast, result);
				break;
				case 2:
				add_zc1(x0, y0, atan2(-dxc, dyc), contrast, result);
			}
		}
	}
	return result;
}

sexpr *filter_on_contrast(sexpr *sp1, sexpr *sp2, sexpr *sp3) {

	sexpr *result;
	sexpr *ls1, *ls, *first;

	float lower, upper, contrast;
	int count;

	if (!sketch(sp1) || !number(sp2) || !number(sp3)) {
		printf("filter-on-contrast: Illegal arguments.\n");
		longjmp(esc, 1);
	}

	lower = sp2->u.x;
	upper = sp3->u.x;

	result = (sexpr *) smalloc;
	smalloc += sizeof(sketch);
	result->type = SKETCH;
	sketch$rows(result) = sketch$rows(sp1);
	sketch$cols(result) = sketch$cols(sp1);
	sketch$bixel_rows(result) = 0;
	sketch$bixel_cols(result) = 0;
	sketch$grid(result) = NULL;

	count = 0;
	ls = nil;

	ls1 = sketch$ls(sp1);
	while (!null(ls1)) {
		first = pair$car(ls1);;
		contrast = line$contrast(first);
		if (contrast > lower && contrast < upper) {
			count++;
			ls = cons(first, ls);
		}
		ls1 = pair$cdr(ls1);
	}
	sketch$ls(result) = ls;
	sketch$count(result) = count;
	return result;
}

sexpr *filter_on_length(sexpr *sp1, sexpr *sp2, sexpr *sp3) {

	sexpr *result;
	sexpr *ls1, *ls, *first;
	float lower, upper, length;
	int count;

	if (!sketch(sp1) || !number(sp2) || !number(sp3)) {
		printf("filter-on-length: Illegal arguments.\n");
		longjmp(esc, 1);
	}

	lower = sp2->u.x;
	upper = sp3->u.x;

	result = (sexpr *) smalloc;
	smalloc += sizeof(sketch);
	result->type = SKETCH;
	sketch$rows(result) = sketch$rows(sp1);
	sketch$cols(result) = sketch$cols(sp1);
	sketch$bixel_rows(result) = 0;
	sketch$bixel_cols(result) = 0;
	sketch$grid(result) = NULL;

	count = 0;
	ls = nil;

	ls1 = sketch$ls(sp1);
	while (!null(ls1)) {
		first = pair$car(ls1);
		length = line$length(first);
		if (length > lower && length < upper) {
			count++;
			ls = cons(first, ls);
		}
		ls1 = pair$cdr(ls1);
	}
	sketch$ls(result) = ls;
	sketch$count(result) = count;
	return result;
}

sexpr *filter_on_angle(sexpr *sp1, sexpr *sp2, sexpr *sp3) {

	sexpr *result;
	sexpr *ls1, *ls, *first;
	double lower, upper, theta;
	int count;

	if (!sketch(sp1) || !number(sp2) || !number(sp3)) {
		printf("filter-on-angle: Illegal arguments.\n");
		longjmp(esc, 1);
	}

	lower = sp2->u.x;
	upper = sp3->u.x;

	result = (sexpr *) smalloc;
	smalloc += sizeof(sketch);
	result->type = SKETCH;
	sketch$rows(result) = sketch$rows(sp1);
	sketch$cols(result) = sketch$cols(sp1);
	sketch$bixel_rows(result) = 0;
	sketch$bixel_cols(result) = 0;
	sketch$grid(result) = NULL;

	count = 0;
	ls = nil;

	ls1 = sketch$ls(sp1);
	while (!null(ls1)) {
		first = pair$car(ls1);
		theta = (double) line$theta(first);
		if (angle_difference(theta, lower) <= PI && angle_difference(theta,
				upper) >= PI) {
			count++;
			ls = cons(first, ls);
		}
		ls1 = pair$cdr(ls1);
	}
	sketch$ls(result) = ls;
	sketch$count(result) = count;
	return result;
}

void generate_postscript_header(FILE *file, float ratio, char *title) {
	fprintf(file, "%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(file, "%%%%Title: %s\n", title);
	fprintf(file, "%%%%Creator: University of New Mexico Scheme 2.5\n");
	if (ratio < 1.0)
		fprintf(file, "%%%%BoundingBox: 0 %g 432 432\n", 432 * (1.0 - ratio));
	else
		fprintf(file, "%%%%BoundingBox: 0 0 %g 432\n", 432 / ratio);
	fprintf(file, "%%%%Pages: 1\n");
	fprintf(file, "%%%%EndComments\n");
	fprintf(file, "save\n");
	fprintf(file, "/inch {72 mul} def\n");
	fprintf(file,
			"/displine { /y2 exch def /x2 exch def /y1 exch def /x1 exch def\n");
	fprintf(file, "x1 y1 moveto x2 y2 lineto stroke } def\n");
	fprintf(file, "%%%%EndProlog\n");
	fprintf(file, "%%%%%%Page: 1 1\n");
	fprintf(file, "0 0 translate\n");
	fprintf(file, "432 432 scale\n");
	fprintf(file, "0.00115741 setlinewidth\n");
	fprintf(file, "0.5 dup translate -90 rotate -0.5 dup translate\n");
	if (ratio < 1.0)
		fprintf(file,
				"0 0 moveto 0 1 lineto %f 1 lineto %f 0 lineto closepath\n",
				ratio, ratio);
	else
		fprintf(file,
				"0 0 moveto 0 %f lineto 1 %f lineto 1 0 lineto closepath\n",
				1.0 / ratio, 1.0 / ratio);
	fprintf(file, "stroke\n");
}

sexpr *sketch2postscript(sexpr *sp1, sexpr *sp2) {

	sexpr *ls, *ln, *port;

	int rows, cols, count;

	char text[STRLEN];

	float x0, y0, x1, y1, ratio, scale;

	FILE *file;

	if (!sketch(sp1)) {
		printf("sketch->postscript: First argument must be sketch.\n");
		longjmp(esc, 1);
	}

	if ((!string(sp2))) {
		printf("sketch->postscript: Illegal filename.\n");
		longjmp(esc, 1);
	}

	ls = sketch$ls(sp1);
	rows = sketch$rows(sp1);
	cols = sketch$cols(sp1);
	count = sketch$count(sp1);

	port = open_output_file(sp2);

	file = port->u.file;

	ratio = max((float) rows/cols,(float) cols/rows);

	sprintf(text, "#<sketch: rows = %d cols = %d count = %d>", rows, cols,
			count);
	generate_postscript_header(file, (float) rows / cols, text);
	fprintf(file, "0.00115741 setlinewidth\n");

	scale = min(rows*ratio,cols*ratio);

	while (!null(ls)) {

		ln = pair$car(ls);

		x0 = line$x(ln)[0] / scale;
		y0 = line$y(ln)[0] / scale;
		x1 = line$x(ln)[1] / scale;
		y1 = line$y(ln)[1] / scale;

		fprintf(file, "%f %f %f %f displine\n", y0, x0, y1, x1);

		ls = pair$cdr(ls);

	}

	fprintf(file, "showpage\n");

	close_output_port(port);

	return undefined;
}

char *convert_escape_sequences(char *text) {

	int i = 1, j = 0;

	char *converted_text = malloc(STRLEN * sizeof(char));

	while (text[i] != '\0') {
		if (text[i - 1] == '\\') {
			switch (text[i]) {
			case 'n':
				converted_text[j] = '\n';
				i++;
				break;
			case 't':
				converted_text[j] = '\t';
				i++;
				break;
			case 'v':
				converted_text[j] = '\v';
				i++;
				break;
			case 'b':
				converted_text[j] = '\b';
				i++;
				break;
			case 'r':
				converted_text[j] = '\r';
				i++;
				break;
			case 'f':
				converted_text[j] = '\f';
				i++;
				break;
			case 'a':
				converted_text[j] = '\a';
				i++;
				break;
			case '\\':
				converted_text[j] = '\\';
				i++;
				break;
			case '\?':
				converted_text[j] = '\?';
				i++;
				break;
			case '\'':
				converted_text[j] = '\'';
				i++;
				break;
			default:
				converted_text[j] = text[i - 1];
				break;
			}
		} else
			converted_text[j] = text[i - 1];
		i++;
		j++;
	}
	converted_text[j++] = text[i - 1];
	converted_text[j] = '\0';
	return converted_text;
}

sexpr *scheme_fprintf(sexpr *sp1, sexpr *sp2, sexpr *sp3) {

	char *format;
	char prefix[STRLEN];
	sexpr *arg;

	int i, j, k;

	if (!output_port(sp1) || (!string(sp2)) || !vector(sp3)) {
		printf("fprintf: Illegal argument.\n");
		longjmp(esc, 1);
	}

	format = convert_escape_sequences(sp2->u.text);

	i = k = 0;
	while (format[i] != '\0') {
		j = 0;
		while (format[i] != '%' && format[i] != '\0')
			prefix[j++] = format[i++];

		while (format[i] != 'e' && format[i] != 'f' && format[i] != 'g'
				&& format[i] != 'c' && format[i] != 's' && format[i] != '\0')
			prefix[j++] = format[i++];

		if (format[i] != '\0')
			prefix[j++] = format[i++];

		prefix[j] = '\0';

		if (k < vector$length(sp3)) {
			arg = vector$data(sp3)[k++];

			switch (arg->type) {
			case NUMBER:
				if ('e' <= prefix[j - 1] && prefix[j - 1] <= 'g') {
					fprintf(sp1->u.file, prefix, arg->u.x);
				} else {
					printf("fprintf: %c%c format used with number.\n", '%',
							prefix[j - 1]);
					longjmp(esc, 1);
				}
				break;
			case CHARACTER:
				if (prefix[j - 1] == 'c') {
					fprintf(sp1->u.file, prefix, arg->u.c);
				} else {
					printf("fprintf: %c%c format used with character.\n", '%',
							prefix[j - 1]);
					longjmp(esc, 1);
				}
				break;
			case STRING:
				if (prefix[j - 1] == 's') {
					fprintf(sp1->u.file, prefix, arg->u.text);
				} else {
					printf("fprintf: %c%c format used with string.\n", '%',
							prefix[j - 1]);
					longjmp(esc, 1);
				}
				break;
			default:
				printf("fprintf: No format for expression of this type.\n");
				longjmp(esc, 1);
			}
		} else
			fprintf(sp1->u.file, "%s", prefix);
	}

	if (k < vector$length(sp3)) {
		printf("fprintf: Too many arguments for format string.\n");
		longjmp(esc, 1);
	}

	free(format);
	return undefined;
}

sexpr *scheme_fscanf(sexpr *sp1, sexpr *sp2) {

	float x;
	char *format, c;
	char prefix[STRLEN], text[STRLEN];
	sexpr *result = nil;

	int i, j, k, status;

	if (!input_port(sp1) || (!string(sp2))) {
		printf("fscanf: Illegal argument.\n");
		longjmp(esc, 1);
	}

	format = convert_escape_sequences(sp2->u.text);

	i = k = 0;
	while (format[i] != '\0') {
		j = 0;
		while (format[i] != '%' && format[i] != '\0')
			prefix[j++] = format[i++];

		while (format[i] != 'e' && format[i] != 'f' && format[i] != 'g'
				&& format[i] != 'c' && format[i] != 's' && format[i] != '\0')
			prefix[j++] = format[i++];

		if (format[i] == '\0') {
			free(format);
			return result;
		}

		prefix[j++] = format[i++];
		prefix[j] = '\0';

		switch (prefix[j - 1]) {
		case 'e':
		case 'f':
		case 'g':
			status = fscanf(sp1->u.file, prefix, &x);
			if (status) {
				if (status != EOF)
					result = append(result, cons(num2exp((double) x), nil));
				else {
					free(format);
					return eof_object;
				}
			}
			break;
		case 'c':
			status = fscanf(sp1->u.file, prefix, &c);
			if (status) {
				if (status != EOF)
					result = append(result, cons(char2exp(c), nil));
				else {
					free(format);
					return eof_object;
				}
			}
			break;
		case 's':
			status = fscanf(sp1->u.file, prefix, text);
			if (status) {
				if (status != EOF)
					result = append(result, cons(str2exp(text), nil));
				else {
					free(format);
					return eof_object;
				}
			}
			break;
		default:
			printf("fscanf: Unrecognized format: %c", prefix[j - 1]);
			longjmp(esc, 1);
		}
	}
	getc(sp1->u.file);

	free(format);
	return result;
}

sexpr *image2complex(sexpr *sp0) {

	int i, rows, cols, n;
	sexpr *sp1;

	if (!image(sp0)) {
		print_value_escape("image->complex_image: Argument is not an image: ",
				sp0);
	}

	rows = image$rows(sp0);
	cols = image$cols(sp0);
	n = rows * cols;

	sp1 = make_complex_image(rows, cols);

	for (i = 0; i < n; i++) {
		complex_image$data(sp1)[i] = Complex(image$data(sp0)[i], 0.0);
	}
	return sp1;
}

sexpr *register_images(sexpr *im1, sexpr *im2, sexpr *dy, sexpr *dx, sexpr *y0,
		sexpr *x0) {

	int i1, j1, i2, j2, index1, index2, count;
	int rows1, cols1, rows2, cols2, r0, c0, dr, dc;
	int u, v, max_u, max_v;
	int p1[256], p2[256], p12[256][256];
	float p, h1, h2, h12, i12, max_i12;
	float *data1, *data2;
	float overlap;

	if (!image(im1) || !image(im2)) {
		printf("register-images: First two arguments must be images.\n");
		longjmp(esc, 1);
	}

	if (!number(y0) || !number(x0) || !number(dy) || !number(dx)) {
		printf("register-images: Search widths and offsets must be numbers.\n");
		longjmp(esc, 1);
	}

	im1 = image_normalize(im1);
	im2 = image_normalize(im2);

	data1 = image$data(im1);
	data2 = image$data(im2);
	rows1 = image$rows(im1);
	cols1 = image$cols(im1);
	rows2 = image$rows(im2);
	cols2 = image$cols(im2);

	for (i1 = 0; i1 < rows1; i1++) {
		index1 = i1 * cols1;
		for (j1 = 0; j1 < cols1; j1++) {
			data1[index1 + j1] *= 255;
		}
	}

	for (i2 = 0; i2 < rows2; i2++) {
		index2 = i2 * cols2;
		for (j2 = 0; j2 < cols2; j2++) {
			data2[index2 + j2] *= 255;
		}
	}

	r0 = (int) y0->u.x;
	c0 = (int) x0->u.x;
	dr = (int) dy->u.x;
	dc = (int) dx->u.x;

	if ((dr < 0) || (dc < 0)) {
		printf("register-images: Search widths must be positive.\n");
		longjmp(esc, 1);
	}

	max_u = max_v = 0;
	max_i12 = 0.0;

	for (u = r0 - dr; u <= r0 + dr; u++) {
		for (v = c0 - dc; v <= c0 + dc; v++) {

			for (i1 = 0; i1 < 256; i1++) {
				p1[i1] = p2[i1] = 0;
				for (j1 = 0; j1 < 256; j1++) {
					p12[i1][j1] = 0;
				}
			}

			count = 0;
			for (i1 = 0; i1 < rows1; i1++) {
				for (j1 = 0; j1 < cols1; j1++) {
					i2 = i1 + u;
					j2 = j1 + v;
					if ((i2 > 0) && (i2 < rows2) && (j2 > 0) && (j2 < cols2)) {
						index1 = i1 * cols1 + j1;
						index2 = i2 * cols2 + j2;
						p1[(int) data1[index1]]++;
						p2[(int) data2[index2]]++;
						p12[(int) data1[index1]][(int) data2[index2]]++;
						count++;
					}
				}
			}

			overlap = (float) count;

			h1 = h2 = h12 = 0.0;
			for (i1 = 0; i1 < 256; i1++) {
				h1 += (((p = p1[i1] / overlap) > 0) ? -p * log(p) : 0.0);
				h2 += (((p = p2[i1] / overlap) > 0) ? -p * log(p) : 0.0);
				for (i2 = 0; i2 < 256; i2++) {
					h12 += (((p = p12[i1][i2] / overlap) > 0) ? -p * log(p)
							: 0.0);
				}
			}

			i12 = h1 + h2 - h12;

			if (i12 > max_i12) {
				max_i12 = i12;
				max_u = u;
				max_v = v;
			}
		}
	}

	return cons(num2exp((float) max_u), cons(num2exp((float) max_v), nil));
}

int power_of_two(int i) {
	int j;
	for (j = 1; j < 2 * i; j *= 2)
		if (j == i)
			return 1;
	return 0;
}

sexpr *fft(sexpr *sp0, sexpr *direction) {

	int i;
	int N;

	float *data;
	float rootN;

	sexpr *sp1, *component;

	if (!vector(sp0) || !number(direction) || fabs(direction->u.x) != 1) {
		printf("fft: Illegal argument.\n");
		longjmp(esc, 1);
	}

	N = vector$length(sp0);

	if (!power_of_two(N)) {
		printf("fft: Length of vector must be integral power of two.\n");
		longjmp(esc, 1);
	}

	rootN = sqrt(N);
	data = malloc((2 * N + 1) * sizeof(float));

	sp1 = make_vector(N);

	for (i = 0; i < N; i++) {
		component = vector$data(sp0)[i];
		if (komplex(component)) {
			data[2 * i + 1] = component->u.z.r;
			data[2 * i + 2] = component->u.z.i;
		} else if (number(component)) {
			data[2 * i + 1] = component->u.x;
			data[2 * i + 2] = 0.0;
		} else
			print_value_escape("fft: Vector component not a number: ",
					component);
	}

	four1(data, N, (int) direction->u.x);

	for (i = 0; i < N; i++) {
		if (direction->u.x == 1)
			vector$data(sp1)[i] = complex2exp(Complex(data[2 * i + 1] / rootN,
					data[2 * i + 2] / rootN));
		else
			vector$data(sp1)[i] = complex2exp(Complex(data[2 * i + 1] / rootN,
					data[2 * i + 2] / rootN));
	}

	free(data);

	return sp1;
}

sexpr *image_fft(sexpr *sp0, sexpr* direction) {

	int i, j;
	int rows, cols, index;

	float *data_row, *data_col;

	float rootNM;

	sexpr *sp1;

	if ((!image(sp0) && !complex_image(sp0)) || !number(direction) || fabs(direction->u.x) != 1) {
		printf("fft-image: Illegal argument.\n");
		longjmp(esc, 1);
	}

	if (image(sp0))
		sp0 = image2complex(sp0);

	cols = complex_image$cols(sp0);
	rows = complex_image$rows(sp0);

	if (!power_of_two(cols) || !power_of_two(rows)) {
		printf("fft-image: Image dimensions must be integral power of two.\n");
		longjmp(esc, 1);
	}

	rootNM = sqrt(rows * cols);

	data_row = malloc((2 * cols + 1) * sizeof(float));
	data_col = malloc((2 * rows + 1) * sizeof(float));

	sp1 = make_complex_image(rows, cols);

	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols; j++) {
			index = cols * i + j;
			data_row[2 * j + 1] = complex_image$data(sp0)[index].r;
			data_row[2 * j + 2] = complex_image$data(sp0)[index].i;
		}

		four1(data_row, cols, (int) floor((float) direction->u.x));

		for (j = 0; j < cols; j++) {
			index = cols * i + j;
			complex_image$data(sp1)[index].r = data_row[2 * j + 1];
			complex_image$data(sp1)[index].i = data_row[2 * j + 2];
		}
	}

	for (j = 0; j < cols; j++) {
		for (i = 0; i < rows; i++) {
			index = cols * i + j;
			data_col[2 * i + 1] = complex_image$data(sp1)[index].r;
			data_col[2 * i + 2] = complex_image$data(sp1)[index].i;
		}

		four1(data_col, rows, (int) floor((float) direction->u.x));

		for (i = 0; i < rows; i++) {
			index = cols * i + j;
			complex_image$data(sp1)[index].r = data_col[2 * i + 1] / rootNM;
			complex_image$data(sp1)[index].i = data_col[2 * i + 2] / rootNM;
		}
	}

	free(data_row);
	free(data_col);

	return sp1;
}

sexpr *image_sum(sexpr *sp0) {

	int i, n;

	switch (sp0->type) {
	case IMAGE: {
		float *data, sum = 0;
		data = image$data(sp0);
		n = image$rows(sp0) * image$cols(sp0);
		for (i = 0; i < n; i++)
			sum += data[i];
		return num2exp(sum);
	}
	case COMPLEX_IMAGE: {
		fcomplex *cdata, csum = zero;
		cdata = complex_image$data(sp0);
		n = complex_image$rows(sp0) * complex_image$cols(sp0);
		for (i = 0; i < n; i++)
			csum = Cadd(csum, cdata[i]);
		return complex2exp(csum);
	}
	default:
		print_value_escape("image-sum: Argument is not an image: ", sp0);
	}
	return NULL;
}

sexpr *image_min(sexpr *sp0) {

	int i, n;

	float x, *data, minimum = FLT_MAX;

	if (!image(sp0)) {
		print_value_escape("image-min: Argument is not an image: ", sp0);
	}

	data = image$data(sp0);
	n = image$rows(sp0) * image$cols(sp0);
	for (i = 0; i < n; i++) {
		x = data[i];
		if (x < minimum)
			minimum = x;
	}
	return num2exp(minimum);
}

sexpr *image_max(sexpr *sp0) {

	int i, n;

	float x, *data, maximum = -FLT_MAX;

	if (!image(sp0)) {
		print_value_escape("image-max: Argument is not an image: ", sp0);
	}

	data = image$data(sp0);
	n = image$rows(sp0) * image$cols(sp0);
	for (i = 0; i < n; i++) {
		x = data[i];
		if (x > maximum)
			maximum = x;
	}
	return num2exp(maximum);
}

sexpr *image_normalize(sexpr *sp1) {

	int i, rows, cols, n;
	float value, scale;
	sexpr *sp2;
	float *data1, *data2;
	fcomplex *cdata1, *cdata2;

	float maximum = -FLT_MAX;
	float minimum = FLT_MAX;

	switch (sp1->type) {
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);

		sp2 = make_image(rows, cols);

		data1 = image$data(sp1);
		data2 = image$data(sp2);

		n = rows * cols;
		for (i = 0; i < n; i++) {
			value = data1[i];
			if (value > maximum)
				maximum = value;
			if (value < minimum)
				minimum = value;
		}

		scale = maximum - minimum;
		if (scale < 1.0)
			scale = 1.0;

		for (i = 0; i < n; i++)
			data2[i] = (data1[i] - minimum) / scale;

		return sp2;
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);

		sp2 = make_complex_image(rows, cols);

		cdata1 = complex_image$data(sp1);
		cdata2 = complex_image$data(sp2);

		n = rows * cols;
		for (i = 0; i < n; i++) {
			value = Cabs(cdata1[i]);
			if (value > maximum)
				maximum = value;
		}

		if (maximum < 1.0)
			maximum = 1.0;
		scale = 1.0 / maximum;

		for (i = 0; i < n; i++)
			cdata2[i] = RCmul(scale, cdata1[i]);

		return sp2;
	default:
		printf("image-normalize: Argument must be image.\n");
		longjmp(esc, 1);
	}
}

sexpr *color_image_normalize(sexpr *sp1) {

	int i, rows, cols, n;
	float value, maximum, minimum, scale;
	sexpr *sp2;

	if (!color_image(sp1)) {
		printf("color-image-normalize: Argument must be color-image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	sp2 = make_color_image(rows, cols);

	maximum = -FLT_MAX;
	minimum = FLT_MAX;

	n = rows * cols;
	for (i = 0; i < n * 3; i++) {
		value = image$data(sp1)[i];
		if (value > maximum)
			maximum = value;
		if (value < minimum)
			minimum = value;
	}

	scale = maximum - minimum;
	if (scale < 1.0)
		scale = 1.0;

	for (i = 0; i < n * 3; i++)
		image$data(sp2)[i] = (image$data(sp1)[i] - minimum) / scale;

	return sp2;
}

sexpr *shrink(sexpr *sp1, sexpr *sp2) {

	int i, rows, cols, n;
	sexpr *sp3;
	float magnitude, threshold;
	fcomplex z;

	if (!number(sp2)) {
		printf("shrink: Second argument must be a number.\n");
		longjmp(esc, 1);
	}

	threshold = (float) sp2->u.x;

	switch (sp1->type) {
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);

		sp3 = make_image(rows, cols);

		n = rows * cols;
		for (i = 0; i < n; i++) {
			magnitude = image$data(sp1)[i];
			if (fabs(magnitude) < threshold)
				image$data(sp3)[i] = 0;
			else
				image$data(sp3)[i] = (magnitude > 0) ? magnitude - threshold
						: magnitude + threshold;
		}
		return sp3;
	case COMPLEX_IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);

		sp3 = make_complex_image(rows, cols);

		n = rows * cols;
		for (i = 0; i < n; i++) {
			z = complex_image$data(sp1)[i];
			magnitude = Cmag(z);
			if (magnitude > 0.0) {
				if (magnitude > threshold)
					complex_image$data(sp3)[i] = RCmul(
							(magnitude - threshold) / magnitude, z);
				else
					complex_image$data(sp3)[i] = zero;
			}
		}
		return sp3;
	default:
		printf("shrink: First argument must be an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *image_rows(sexpr *sp) {
	switch (sp->type) {
	case IMAGE:
	case COLOR_IMAGE:
		return num2exp(image$rows(sp));
	case COMPLEX_IMAGE:
		return num2exp(complex_image$rows(sp));
	default:
		print_value_escape("image-rows: Argument is not an image: ", sp);
	}
	return NULL;
}

sexpr *image_cols(sexpr *sp) {
	switch (sp->type) {
	case IMAGE:
	case COLOR_IMAGE:
		return num2exp(image$cols(sp));
	case COMPLEX_IMAGE:
		return num2exp(complex_image$cols(sp));
	default:
		print_value_escape("image-cols: Argument is not an image: ", sp);
	}
	return NULL;
}

sexpr *real_image_complex_helper(fcomplex( func)(float, float), sexpr *sp1,
		sexpr *sp2) {
	int rows = image$rows(sp2);
	int cols = image$cols(sp2);
	int i, n = rows * cols;
	float x = (float) sp1->u.x;
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_complex_image(rows, cols);
	fcomplex *cdata3 = complex_image$data(sp3);
	for (i = 0; i < n; i++)
		cdata3[i] = func((float) x, data2[i]);
	return sp3;
}

sexpr *image_real_complex_helper(fcomplex( func)(float, float), sexpr *sp1,
		sexpr *sp2) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	float x = (float) sp2->u.x;
	sexpr *sp3 = make_complex_image(rows, cols);
	fcomplex *cdata3 = complex_image$data(sp3);
	for (i = 0; i < n; i++)
		cdata3[i] = func(data1[i], x);
	return sp3;
}

sexpr *image_image_complex_helper(fcomplex( func)(float, float), sexpr *sp1,
		sexpr *sp2) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_complex_image(rows, cols);
	fcomplex *cdata3 = complex_image$data(sp3);
	check_image_sizes("complex-helper", rows, cols, sp2);
	for (i = 0; i < n; i++)
		cdata3[i] = func(data1[i], data2[i]);
	return sp3;
}

sexpr *complex_helper(fcomplex( func)(float, float), sexpr *sp1, sexpr *sp2) {
	switch (sp1->type) {
	case NUMBER:
		switch (sp2->type) {
		case NUMBER:
			return complex2exp(func((float) sp1->u.x, (float) sp2->u.x));
		case IMAGE:
			return real_image_complex_helper(func, sp1, sp2);
		default:
			printf("complex-helper: Argument is not a number or image.\n");
			longjmp(esc, 1);
		}
	case IMAGE:
		switch (sp2->type) {
		case NUMBER:
			return real_image_complex_helper(func, sp1, sp2);
		case IMAGE:
			return image_image_complex_helper(func, sp1, sp2);
		default:
			printf("complex-helper: Incompatible argument types.\n");
			longjmp(esc, 1);
		}
	default:
		printf("complex-helper: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
	return NULL;
}

fcomplex Polar(float rho, float theta) {
	return Complex(rho * cos(theta), rho * sin(theta));
}

sexpr *polar2complex(sexpr *sp1, sexpr *sp2) {
	return complex_helper(Polar, sp1, sp2);
}

sexpr *make_complex(sexpr *sp1, sexpr *sp2) {
	return complex_helper(Complex, sp1, sp2);
}

sexpr *complex_image_real(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!complex_image(sp1)) {
		printf("complex-image-real-part: Argument is not a complex image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = (float) complex_image$data(sp1)[i].r;

	return sp2;
}

sexpr *complex_image_imag(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!complex_image(sp1)) {
		printf("complex-image-imag-part: Argument is not a complex image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = (float) complex_image$data(sp1)[i].i;

	return sp2;
}

sexpr *complex_image_angle(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!complex_image(sp1)) {
		printf("complex-image-angle: Argument is not a complex image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = atan2(complex_image$data(sp1)[i].i, complex_image$data(sp1)[i].r);

	return sp2;
}

sexpr *complex_image_magnitude(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!complex_image(sp1)) {
		printf("complex-image-magnitude: Argument is not a complex image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = (float) Cmag(complex_image$data(sp1)[i]);

	return sp2;
}

sexpr *image_cat_helper_helper(sexpr *(*func)(), sexpr *sp1, sexpr *sp2) {
	return make_complex(func(complex_image_real(sp1), complex_image_real(sp2)),
			func(complex_image_imag(sp1), complex_image_imag(sp2)));
}

sexpr *image_cat_helper(sexpr *(*func)(), sexpr *sp1, sexpr *sp2) {
	switch (sp1->type) {
	case IMAGE:
		switch (sp2->type) {
		case IMAGE:
			return func(sp1, sp2);
		case COMPLEX_IMAGE:
			return image_cat_helper_helper(func, image2complex(sp1), sp2);
		default:
			return undefined;
		}
	case COMPLEX_IMAGE:
		switch (sp2->type) {
		case IMAGE:
			return image_cat_helper_helper(func, sp1, image2complex(sp2));
		case COMPLEX_IMAGE:
			return image_cat_helper_helper(func, sp1, sp2);
		default:
			return undefined;
		}
	default:
		return undefined;
	}
}

sexpr *top2bottom(sexpr *sp1, sexpr *sp2) {

	int i, j, rows1, rows2, rows3, cols, size1, index;
	sexpr *sp3;

	cols = image$cols(sp1);
	check_image_sizes("top-to-bottom", -1, cols, sp2);
	rows1 = image$rows(sp1);
	rows2 = image$rows(sp2);
	rows3 = rows1 + rows2;
	sp3 = make_image(rows3, cols);
	size1 = rows1 * cols;
	for (j = 0; j < cols; j++) {
		for (i = 0; i < rows1; i++) {
			index = cols * i + j;
			image$data(sp3)[index] = image$data(sp1)[index];
		}
		for (i = 0; i < rows2; i++) {
			index = cols * i + j;
			image$data(sp3)[index + size1] = image$data(sp2)[index];
		}
	}

	return sp3;
}

sexpr *left2right(sexpr *sp1, sexpr *sp2) {

	int i, j, cols1, cols2, cols3, rows, index1, index2, index3;

	sexpr *sp3;

	rows = image$rows(sp1);
	check_image_sizes("left-to-right", rows, -1, sp2);

	cols1 = image$cols(sp1);
	cols2 = image$cols(sp2);
	cols3 = cols1 + cols2;

	sp3 = make_image(rows, cols3);

	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols1; j++) {
			index1 = cols1 * i + j;
			index3 = cols3 * i + j;
			image$data(sp3)[index3] = image$data(sp1)[index1];
		}
		for (j = 0; j < cols2; j++) {
			index2 = cols2 * i + j;
			index3 = cols3 * i + cols1 + j;
			image$data(sp3)[index3] = image$data(sp2)[index2];
		}
	}

	return sp3;
}

sexpr *scheme_top2bottom(sexpr *sp1, sexpr *sp2) {
	sexpr *sp3 = image_cat_helper(top2bottom, sp1, sp2);
	if (undefined(sp3)) {
		printf("top-to-bottom: Argument is not an image.\n");
		longjmp(esc, 1);
	}
	return sp3;
}

sexpr *scheme_left2right(sexpr *sp1, sexpr *sp2) {
	sexpr *sp3 = image_cat_helper(left2right, sp1, sp2);
	if (undefined(sp3)) {
		printf("left-to-right: Argument is not an image.\n");
		longjmp(esc, 1);
	}
	return sp3;
}

sexpr *matrix_product(sexpr *sp1, sexpr *sp2) {

	int i1, j1, j2, rows1, cols1, cols2;

	float sum, *data1, *data2, *data3;

	fcomplex csum, *cdata1, *cdata2, *cdata3;

	sexpr *sp3;

	switch (sp1->type) {
	case IMAGE:
		rows1 = image$rows(sp1);
		cols1 = image$cols(sp1);
		data1 = image$data(sp1);
		switch (sp2->type) {
		case IMAGE:
			check_image_sizes("matrix-product", cols1, -1, sp2);
			cols2 = image$cols(sp2);
			data2 = image$data(sp2);
			sp3 = make_image(rows1, cols2);
			data3 = image$data(sp3);
			for (i1 = 0; i1 < rows1; i1++) {
				for (j2 = 0; j2 < cols2; j2++) {
					sum = 0.0;
					for (j1 = 0; j1 < cols1; j1++) {
						sum += data1[i1 * cols1 + j1] * data2[j1 * cols2 + j2];
					}
					data3[i1 * cols2 + j2] = sum;
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("matrix-product", cols1, -1, sp2);
			cols2 = complex_image$cols(sp2);
			cdata2 = complex_image$data(sp2);
			sp3 = make_complex_image(rows1, cols2);
			cdata3 = complex_image$data(sp3);
			for (i1 = 0; i1 < rows1; i1++) {
				for (j2 = 0; j2 < cols2; j2++) {
					csum = zero;
					for (j1 = 0; j1 < cols1; j1++) {
						csum = Cadd(csum, RCmul(data1[i1 * cols1 + j1],
								cdata2[j1 * cols2 + j2]));
					}
					cdata3[i1 * cols2 + j2] = csum;
				}
			}
			return sp3;
		default:
			print_value_escape("matrix-product: Argument is not an image: ",
					sp2);
		}
	case COMPLEX_IMAGE:
		rows1 = complex_image$rows(sp1);
		cols1 = complex_image$cols(sp1);
		cdata1 = complex_image$data(sp1);
		switch (sp2->type) {
		case IMAGE:
			check_image_sizes("matrix-product", cols1, -1, sp2);
			cols2 = image$cols(sp2);
			data2 = image$data(sp2);
			sp3 = make_complex_image(rows1, cols2);
			cdata3 = complex_image$data(sp3);
			for (i1 = 0; i1 < rows1; i1++) {
				for (j2 = 0; j2 < cols2; j2++) {
					csum = zero;
					for (j1 = 0; j1 < cols1; j1++) {
						csum = Cadd(csum, RCmul(data2[j1 * cols2 + j2],
								cdata1[i1 * cols1 + j1]));
					}
					cdata3[i1 * cols2 + j2] = csum;
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("matrix-product", cols1, -1, sp2);
			cols2 = complex_image$cols(sp2);
			cdata2 = complex_image$data(sp2);
			sp3 = make_complex_image(rows1, cols2);
			cdata3 = complex_image$data(sp3);
			for (i1 = 0; i1 < rows1; i1++) {
				for (j2 = 0; j2 < cols2; j2++) {
					csum = zero;
					for (j1 = 0; j1 < cols1; j1++) {
						csum = Cadd(csum, Cmul(cdata1[i1 * cols1 + j1],
								cdata2[j1 * cols2 + j2]));
					}
					cdata3[i1 * cols2 + j2] = csum;
				}
			}
			return sp3;
		default:
			print_value_escape("matrix-product: Argument is not an image: ",
					sp2);
		}
	default:
		print_value_escape("matrix-product: Argument is not an image: ", sp1);
	}
	return NULL;
}

sexpr *times(sexpr *sp1, sexpr *sp2) {
	int i, n, rows, cols;
	float x;
	fcomplex z;
	sexpr *sp3;

	switch (sp1->type) {
	case NUMBER:
		x = sp1->u.x;
		switch (sp2->type) {
		case NUMBER:
			return num2exp(x * sp2->u.x);
		case COMPLEX:
			return complex2exp(RCmul(x, sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp2)[i] * x;
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(x, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("*: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX:
		z = sp1->u.z;
		switch (sp2->type) {
		case NUMBER:
			return complex2exp(RCmul(sp2->u.x, z));
		case COMPLEX:
			return complex2exp(Cmul(z, sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(image$data(sp2)[i], z);
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cmul(z, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("*: Argument is not a number or image: ", sp2);
			break;
		}
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_image(rows, cols);
			x = sp2->u.x;
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] * x;
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(image$data(sp1)[i], z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("*", rows, cols, sp2);
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] * image$data(sp2)[i];
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("*", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(image$data(sp1)[i], complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("*: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX_IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_complex_image(rows, cols);
			x = sp2->u.x;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(x, complex_image$data(sp1)[i]);
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cmul(complex_image$data(sp1)[i], z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("*", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(image$data(sp2)[i], complex_image$data(sp1)[i]);
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("*", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cmul(complex_image$data(sp1)[i], complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("*: Argument is not a number or image: ", sp2);
			break;
		}
	default:
		print_value_escape("*: Argument is not a number or image: ", sp1);
		break;
	}
	return NULL;
}

sexpr *plus(sexpr *sp1, sexpr *sp2) {
	int i, n, rows, cols;
	float x;
	fcomplex z;
	sexpr *sp3;

	switch (sp1->type) {
	case NUMBER:
		x = sp1->u.x;
		switch (sp2->type) {
		case NUMBER:
			return num2exp(x + sp2->u.x);
		case COMPLEX:
			return complex2exp(Cadd(Complex((float) x, 0.0), sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = x + image$data(sp2)[i];
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			z = Complex((float) x, 0.0);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(z, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("+: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX:
		z = sp1->u.z;
		switch (sp2->type) {
		case NUMBER:
			return complex2exp(Cadd(z, Complex((float) sp2->u.x, 0.0)));
		case COMPLEX:
			return complex2exp(Cadd(z, sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(z, Complex(image$data(sp2)[i], 0.0));
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(z, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("+: Argument is not a number or image: ", sp2);
			break;
		}
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_image(rows, cols);
			x = sp2->u.x;
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] + x;
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(Complex(image$data(sp1)[i], 0.0), z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("+", rows, cols, sp2);
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] + image$data(sp2)[i];
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("+", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(Complex(image$data(sp1)[i], 0.0), complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("+: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_complex_image(rows, cols);
			z = Complex((float) sp2->u.x, 0.0);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(complex_image$data(sp1)[i], z);
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(complex_image$data(sp1)[i], z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("+", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(complex_image$data(sp1)[i], Complex(image$data(sp2)[i], 0.0));
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("+", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cadd(complex_image$data(sp1)[i], complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("+: Argument is not a number or image: ", sp2);
			break;
		}
	default:
		print_value_escape("+: Argument is not a number or image: ", sp1);
		break;
	}
	return NULL;
}

sexpr *minus(sexpr *sp1, sexpr *sp2) {
	int i, n, rows, cols;
	float x;
	fcomplex z;
	sexpr *sp3;

	switch (sp1->type) {
	case NUMBER:
		x = sp1->u.x;
		switch (sp2->type) {
		case NUMBER:
			return num2exp(x - sp2->u.x);
		case COMPLEX:
			return complex2exp(Csub(Complex((float) x, 0.0), sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = x - image$data(sp2)[i];
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			z = Complex((float) x, 0.0);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(z, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("-: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX:
		z = sp1->u.z;
		switch (sp2->type) {
		case NUMBER:
			return complex2exp(Csub(z, Complex((float) sp2->u.x, 0.0)));
		case COMPLEX:
			return complex2exp(Csub(z, sp2->u.z));
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(z, Complex(image$data(sp2)[i], 0.0));
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(z, complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("-: Argument is not a number or image: ", sp2);
			break;
		}
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_image(rows, cols);
			x = sp2->u.x;
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] - x;
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(Complex(image$data(sp1)[i], 0.0), z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("-", rows, cols, sp2);
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] - image$data(sp2)[i];
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("-", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(Complex(image$data(sp1)[i], 0.0), complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("-: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_complex_image(rows, cols);
			z = Complex((float) sp2->u.x, 0.0);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(complex_image$data(sp1)[i], z);
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z = sp2->u.z;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(complex_image$data(sp1)[i], z);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("-", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(complex_image$data(sp1)[i], Complex(image$data(sp2)[i], 0.0));
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("-", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Csub(complex_image$data(sp1)[i], complex_image$data(sp2)[i]);
			}
			return sp3;
		default:
			print_value_escape("-: Argument is not a number or image: ", sp2);
			break;
		}
	default:
		print_value_escape("-: Argument is not a number or image: ", sp1);
		break;
	}
	return NULL;
}

sexpr *divide(sexpr *sp1, sexpr *sp2) {
	int i, n, rows, cols;
	float x1, x2;
	fcomplex z1, z2;
	sexpr *sp3;

	switch (sp1->type) {
	case NUMBER:
		x1 = sp1->u.x;
		switch (sp2->type) {
		case NUMBER:
			x2 = sp2->u.x;
			if (x2 != 0.0)
				return num2exp(x1 / x2);
			printf("/: Attempt to divide by zero.\n");
			longjmp(esc, 1);
		case COMPLEX:
			z1 = sp2->u.z;
			if (Cmag(z1) != 0.0)
				return complex2exp(Cdiv(Complex((float) x1, 0.0), z1));
			printf("/: Attempt to divide by zero.\n");
			longjmp(esc, 1);
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				x2 = image$data(sp2)[i];
				if (x2 != 0) {
					image$data(sp3)[i] = x1 / x2;
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				z1 = complex_image$data(sp2)[i];
				if (Cmag(z1) != 0.0) {
					complex_image$data(sp3)[i] = Cdiv(Complex((float) x1, 0.0), z1);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		default:
			print_value_escape("/: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX:
		z1 = sp1->u.z;
		switch (sp2->type) {
		case NUMBER:
			x2 = sp2->u.x;
			if (x2 != 0.0)
				return complex2exp(RCmul(1.0 / x2, z1));
			printf("/: Attempt to divide by zero.\n");
			longjmp(esc, 1);
		case COMPLEX:
			z2 = sp2->u.z;
			if (Cmag(z2) != 0.0)
				return complex2exp(Cdiv(z1, z2));
			printf("/: Attempt to divide by zero.\n");
			longjmp(esc, 1);
		case IMAGE:
			rows = image$rows(sp2);
			cols = image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				x2 = image$data(sp2)[i];
				if (x2 != 0.0) {
					complex_image$data(sp3)[i] = RCmul(1.0 / x2, z1);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			rows = complex_image$rows(sp2);
			cols = complex_image$cols(sp2);
			n = rows * cols;
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				z2 = complex_image$data(sp2)[i];
				if (Cmag(z2) != 0.0) {
					complex_image$data(sp3)[i] = Cdiv(z1, z2);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		default:
			print_value_escape("/: Argument is not a number or image: ", sp2);
			break;
		}
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_image(rows, cols);
			x2 = sp2->u.x;
			if (x2 == 0.0) {
				printf("/: Attempt to divide by zero.\n");
				longjmp(esc, 1);
			}
			for (i = 0; i < n; i++) {
				image$data(sp3)[i] = image$data(sp1)[i] / x2;
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z2 = sp2->u.z;
			if (Cmag(z2) == 0.0) {
				printf("/: Attempt to divide by zero.\n");
				longjmp(esc, 1);
			}
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cdiv(Complex((float) image$data(sp1)[i], 0.0), z2);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("/", rows, cols, sp2);
			sp3 = make_image(rows, cols);
			for (i = 0; i < n; i++) {
				x2 = image$data(sp2)[i];
				if (x2 != 0.0) {
					image$data(sp3)[i] = image$data(sp1)[i] / x2;
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("/", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				z2 = complex_image$data(sp2)[i];
				if (Cmag(z2) != 0.0) {
					complex_image$data(sp3)[i] = Cdiv(Complex((float) image$data(sp1)[i], 0.0), z2);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		default:
			print_value_escape("/: Argument is not a number or image: ", sp2);
			break;
		}
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);
		n = rows * cols;
		switch (sp2->type) {
		case NUMBER:
			sp3 = make_complex_image(rows, cols);
			x2 = sp2->u.x;
			if (x2 == 0.0) {
				printf("/: Attempt to divide by zero.\n");
				longjmp(esc, 1);
			}
			x2 = 1.0 / x2;
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = RCmul(x2, complex_image$data(sp1)[i]);
			}
			return sp3;
		case COMPLEX:
			sp3 = make_complex_image(rows, cols);
			z2 = sp2->u.z;
			if (Cmag(z2) == 0.0) {
				printf("/: Attempt to divide by zero.\n");
				longjmp(esc, 1);
			}
			for (i = 0; i < n; i++) {
				complex_image$data(sp3)[i] = Cdiv(complex_image$data(sp1)[i], z2);
			}
			return sp3;
		case IMAGE:
			check_image_sizes("/", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				x2 = image$data(sp2)[i];
				if (x2 != 0.0) {
					complex_image$data(sp3)[i] = RCmul(1.0 / x2, complex_image$data(sp1)[i]);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			check_image_sizes("/", rows, cols, sp2);
			sp3 = make_complex_image(rows, cols);
			for (i = 0; i < n; i++) {
				z2 = complex_image$data(sp2)[i];
				if (Cmag(z2) != 0.0) {
					complex_image$data(sp3)[i] = Cdiv(complex_image$data(sp1)[i], z2);
				} else {
					printf("/: Attempt to divide by zero.\n");
					longjmp(esc, 1);
				}
			}
			return sp3;
		default:
			print_value_escape("/: Argument is not a number or image: ", sp2);
			break;
		}
	default:
		print_value_escape("/: Argument is not a number or image: ", sp1);
		break;
	}
	return NULL;
}

sexpr *setcar(sexpr *s1, sexpr *s2) {
	if (!pair(s1)) {
		print_value_escape("Attempt to set-car! of non-pair: ", s1);
	}
	pair$car(s1) = s2;
	return undefined;
}

sexpr *setcdr(sexpr *s1, sexpr *s2) {
	if (!pair(s1)) {
		print_value_escape("Attempt to set-cdr! of non-pair: ", s2);
	}
	pair$cdr(s1) = s2;
	return undefined;
}

sexpr *make_number_list(int n) {
	if (n == 0)
		return nil;
	return cons(num2exp((double) n), make_number_list(n - 1));
}

sexpr *make_complex_number_list(int n) {
	if (n == 0)
		return nil;
	return cons(complex2exp(Complex(0.0, (float) n)), make_complex_number_list(
			n - 1));
}

sexpr *compile_app(sexpr *sp) {
	return cps2vector(car(
			compile(sp, cons(bytecode_halt, nil), global_env_vars)));
}

sexpr *image2list(sexpr *sp0) {

	int i, n;
	float *data;
	fcomplex *cdata;

	sexpr *sp1 = nil;

	switch (sp0->type) {
	case IMAGE:
		n = image$rows(sp0) * image$cols(sp0);
		data = image$data(sp0);
		for (i = 0; i < n; i++)
			sp1 = cons(num2exp(data[i]), sp1);
		return sp1;
	case COMPLEX_IMAGE:
		n = complex_image$rows(sp0) * complex_image$cols(sp0);
		cdata = complex_image$data(sp0);
		for (i = 0; i < n; i++)
			sp1 = cons(complex2exp(cdata[i]), sp1);
		return sp1;
	default:
		printf("image->list: Illegal argument.\n");
		longjmp(esc, 1);
	}
}

sexpr *scheme_image_map(sexpr *sp0, sexpr *sp1) {

	int i, j, k, n;
	int rows, cols, index, cflag;

	float **images;
	fcomplex **complex_images;

	float *data2;
	fcomplex *cdata3;

	sexpr *code, *result, *args, *sp2, *sp3, *sp4, *sp5;

	if (!vector(sp1)) {
		printf("image-map: Illegal argument.\n");
		longjmp(esc, 1);
	}

	cflag = 0;

	sp4 = vector$data(sp1)[0];

	n = vector$length(sp1);

	switch (sp0->type) {
	case PRIMITIVE:
		if (primitive$arity(sp0) != n) {
			printf(
					"image-map: First argument must be a procedure of %d arguments.\n",
					n);
			longjmp(esc, 1);
		}
		break;
	case VIRGIN:
	case CLOSURE:
		if (closure$arity(sp0) != n && abs(closure$arity(sp0) + 1) > n) {
			printf(
					"image-map: First argument must be a procedure of %d arguments.\n",
					n);
			longjmp(esc, 1);
		}
		break;
	default:
		printf("image-map: First argument must be a procedure.\n");
		longjmp(esc, 1);
	}

	images = (float **) malloc(n * sizeof(float *));
	complex_images = (fcomplex **) malloc(n * sizeof(fcomplex *));

	args = make_complex_number_list(n);
	sp5 = list2vector(args);

	switch (sp4->type) {
	case IMAGE:
		rows = image$rows(sp4);
		cols = image$cols(sp4);
		images[0] = image$data(sp4);
		vector$data(sp5)[0]->type = NUMBER;
		break;
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp4);
		cols = complex_image$cols(sp4);
		complex_images[0] = complex_image$data(sp4);
		break;
	default:
		printf("image-map: Illegal argument.\n");
		free(images);
		longjmp(esc, 1);
	}

	for (i = 1; i < n; i++) {
		sp4 = vector$data(sp1)[i];

		switch (sp4->type) {
		case IMAGE:
			check_image_sizes("image-map", rows, cols, sp4);
			images[i] = image$data(sp4);
			vector$data(sp5)[i]->type = NUMBER;
			break;
		case COMPLEX_IMAGE:
			check_image_sizes("image-map", rows, cols, sp4);
			complex_images[i] = complex_image$data(sp4);
			break;
		default:
			printf("image-map: Illegal argument.\n");
			free(images);
			longjmp(esc, 1);
		}
	}

	sp2 = make_image(rows, cols);
	data2 = image$data(sp2);

	sp3 = make_complex_image(rows, cols);
	cdata3 = complex_image$data(sp3);

	code = compile_app(cons(sp0, args));

	index = 0;
	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols; j++) {
			for (k = 0; k < n; k++) {
				if (number(vector$data(sp5)[k])) {
					vector$data(sp5)[k]->u.x = images[k][index];
				} else {
					vector$data(sp5)[k]->u.z = complex_images[k][index];
				}
			}
			result = virtual_machine(nil, code, global_env_vals, nil);
			switch (result->type) {
			case NUMBER:
				data2[index] = result->u.x;
				cdata3[index++] = Complex((float) result->u.x, 0.0);
				break;
			case COMPLEX:
				cflag = 1;
				cdata3[index++] = result->u.z;
				break;
			default:
				printf("image-map: Error at location (%d,%d).\n", i, j);
				longjmp(esc, 1);
			}
		}
	}

	free(images);

	return cflag ? sp3 : sp2;
}

sexpr *image_crop(sexpr *sp0, sexpr *row0, sexpr *col0, sexpr *rows,
		sexpr *cols) {
	int input_cols, i, j, output_rows, output_cols, r0, c0, n, index;
	sexpr *sp1;

	if (!image(sp0)) {
		printf("image-crop: First argument must be an image.\n");
		longjmp(esc, 1);
	}

	if (!number(row0) || !number(col0) || !number(rows) || !number(cols)) {
		printf("image-crop: Second through fifth arguments must be numbers.\n");
		longjmp(esc, 1);
	}

	input_cols = image$cols(sp0);
	n = input_cols * image$rows(sp0);

	r0 = row0->u.x;
	c0 = col0->u.x;
	output_rows = (int) floor((float) rows->u.x);
	output_cols = (int) floor((float) cols->u.x);

	if ((r0 < 0) || (c0 < 0) || (output_rows <= 0) || (output_cols <= 0)) {
		printf("image-crop: Number arguments must be positive.\n");
		longjmp(esc, 1);
	}

	sp1 = make_image(output_rows, output_cols);

	for (i = 0; i < output_rows; i++) {
		for (j = 0; j < output_cols; j++) {
			index = (i + r0) * input_cols + j + c0;
			if ((index >= 0) && (index < n)) {
				image$data(sp1)[i * output_cols + j] = image$data(sp0)[index];
			} else {
				printf("image-crop: Incompatible output dimensions.\n");
				longjmp(esc, 1);
			}
		}
	}
	return sp1;
}

sexpr *image_pad(sexpr *sp0, sexpr *rows, sexpr *cols) {
	int input_rows, input_cols, i, j, output_rows, output_cols, n;
	sexpr *sp1;

	if (!image(sp0)) {
		printf("image-pad: First argument must be an image.\n");
		longjmp(esc, 1);
	}

	if (!number(rows) || !number(cols)) {
		printf("image-pad: Row and column size must be numbers.\n");
		longjmp(esc, 1);
	}

	input_rows = image$rows(sp0);
	input_cols = image$cols(sp0);

	output_rows = (int) floor((float) rows->u.x);
	output_cols = (int) floor((float) cols->u.x);
	n = output_rows * output_cols;

	if ((input_rows < 0) || (input_cols < 0) || (output_rows <= 0)
			|| (output_cols <= 0)) {
		printf("image-pad: Number arguments must be positive.\n");
		longjmp(esc, 1);
	}

	if (output_rows < input_rows || output_cols < input_cols) {
		printf("image-pad: Output sizes must exceed input sizes.\n");
		longjmp(esc, 1);
	}

	sp1 = make_image(output_rows, output_cols);

	for (i = 0; i < n; i++)
		image$data(sp1)[i] = 0.0;

	for (i = 0; i < input_rows; i++) {
		for (j = 0; j < input_cols; j++) {
			image$data(sp1)[i * output_cols + j] = image$data(sp0)[i * input_cols + j];
		}
	}
	return sp1;
}

sexpr *scheme_make_image(sexpr *sp1, sexpr *sp2, sexpr *sp3, sexpr *sp4) {
	int i, j, cflag, not_filter;
	int rows, cols, fold_rows, fold_cols;
	int n, index;
	sexpr *sp5, *sp6, *code, *result;

	sexpr *value1 = number_zero;
	sexpr *value2 = num2exp(0.0); /* number_zero; */

	cflag = 0;

	if (!number(sp1) || !number(sp2)) {
		printf("make-image: First two arguments must be numbers.\n");
		longjmp(esc, 1);
	}

	if (!primitive(sp3) && !virgin(sp3) && !closure(sp3)) {
		print_value_escape("make-image: Third argument must be a procedure: ",
				sp3);
	}

	if (!boolean(sp4)) {
		print_value_escape("make-image: Fourth argument must be boolean: ", sp4);
	}

	not_filter = sp4->u.i;

	rows = (int) floor((float) sp1->u.x);
	cols = (int) floor((float) sp2->u.x);

	sp5 = make_image(rows, cols);
	sp6 = make_complex_image(rows, cols);

	n = rows * cols;
	fold_rows = rows / 2;
	fold_cols = cols / 2;

	if (primitive(sp3)) {
		sexpr *(*func)() = primitive$func(sp3);
		if (primitive$arity(sp3) != 2) {
			printf(
					"make-image: Third argument must be a procedure of two numbers.\n");
			longjmp(esc, 1);
		}
		index = 0;
		for (i = 0; i < rows; i++) {
			value1->u.x = (not_filter || i <= fold_rows) ? i : i - rows;
			for (j = 0; j < cols; j++) {
				value2->u.x = (not_filter || j <= fold_cols) ? j : j - cols;
				result = func(value1, value2);
				switch (result->type) {
				case NUMBER:
					image$data(sp5)[index] = result->u.x;
					complex_image$data(sp6)[index++] = Complex(result->u.x, 0.0);
					break;
				case COMPLEX:
					cflag = 1;
					complex_image$data(sp6)[index++] = result->u.z;
					break;
				default:
					printf("make-image: Error at location (%d,%d).\n", i, j);
					longjmp(esc, 1);
				}
			}
		}
	} else {
		if (closure$arity(sp3) != 2 && closure$arity(sp3) >= 0) {
			printf(
					"make-image: Third argument must be a procedure of two arguments.\n");
			longjmp(esc, 1);
		}
		code = compile_app(cons(sp3, list2(value1, value2)));

		index = 0;
		for (i = 0; i < rows; i++) {
			value1->u.x = (not_filter || i <= fold_rows) ? i : i - rows;
			for (j = 0; j < cols; j++) {
				value2->u.x = (not_filter || j <= fold_cols) ? j : j - cols;

				// 	if (not_filter) printf("* ");
				// 	printf("i: %i  j: %i  ", i, j);
				// 	printf("row: %1.2f  col: %1.2f", value1->u.x, value2->u.x);
				// 	printf("rowaddr: %x   coladdr: %x", (int)value1, (int)value2);

				result = virtual_machine(nil, code, global_env_vals, nil);

				// 	printf("  val: %1.2f", result->u.x);
				// 	printf("\n");

				switch (result->type) {
				case NUMBER:
					image$data(sp5)[index] = result->u.x;
					complex_image$data(sp6)[index++] = Complex(result->u.x, 0.0);
					break;
				case COMPLEX:
					cflag = 1;
					complex_image$data(sp6)[index++] = result->u.z;
					break;
				default:
					printf("make-image: Error at location (%d,%d).\n", i, j);
					longjmp(esc, 1);
				}
			}
		}
	}
	return cflag ? sp6 : sp5;
}

sexpr *rgb2color(sexpr *r, sexpr* g, sexpr *b) {

	int i, rows, cols, n;
	sexpr *sp;

	if (!image(r) || !image(g) || !image(b)) {
		printf("rgb->color-image: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(r);
	cols = image$cols(r);

	check_image_sizes("rgb->color-image", rows, cols, g);
	check_image_sizes("rgb->color-image", rows, cols, b);

	sp = make_color_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++) {
		image$data(sp)[i * 3 + 0] = image$data(r)[i];
		image$data(sp)[i * 3 + 1] = image$data(g)[i];
		image$data(sp)[i * 3 + 2] = image$data(b)[i];
	}

	return sp;
}

sexpr *make_hot_image(sexpr *sp1) {

	int i, rows, cols, n;
	float value, maximum, minimum;
	sexpr *sp2;

	if (!image(sp1)) {
		printf("make-hot-image: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	sp2 = make_color_image(rows, cols);

	maximum = -FLT_MAX;
	minimum = FLT_MAX;

	n = rows * cols;
	for (i = 0; i < n; i++) {
		value = image$data(sp1)[i];
		if (value > maximum)
			maximum = value;
		if (value < minimum)
			minimum = value;
	}

	for (i = 0; i < n; i++) {
		value = image$data(sp1)[i];
		image$data(sp2)[i * 3 + 0] = rhot(minimum, maximum, value);
		image$data(sp2)[i * 3 + 1] = ghot(minimum, maximum, value);
		image$data(sp2)[i * 3 + 2] = bhot(minimum, maximum, value);
	}

	return sp2;
}

/*
 Richardson, John L., Visualizing quantum scattering on the CM-2 supercomputer, Computer Physics Communications 63 (1991), pp. 84-94.
 http://www.hallym.ac.kr/~physics/education/TIPTOP/VLAB/QmSct/complex.html
 */

sexpr *complex2color(sexpr *sp1) {

	int i, rows, cols, n;
	float x, y, radius, d, a, b, R, G, B, scale, maximum, minimum;
	sexpr *sp2;

	if (!complex_image(sp1)) {
		printf("complex-image->color-image: Argument is not a complex image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);

	sp2 = make_color_image(rows, cols);

	maximum = -FLT_MAX;
	minimum = FLT_MAX;

	n = rows * cols;
	for (i = 0; i < n; i++) {
		x = complex_image$data(sp1)[i].r;
		y = complex_image$data(sp1)[i].i;
		if (x > maximum)
			maximum = x;
		if (x < minimum)
			minimum = x;
		if (y > maximum)
			maximum = y;
		if (y < minimum)
			minimum = y;
	}

	scale = 2.0 / (maximum - minimum);

	for (i = 0; i < n; i++) {
		x = scale * (complex_image$data(sp1)[i].r);
		y = scale * (complex_image$data(sp1)[i].i);
		radius = sqrt(x * x + y * y);
		a = 0.40824829046386301636 * x;
		b = 0.70710678118654752440 * y;
		d = 1.0 / (1.0 + radius * radius);
		R = 0.5 + 0.81649658092772603273 * x * d;
		G = 0.5 - d * (a - b);
		B = 0.5 - d * (a + b);
		d = 0.5 - radius * d;
		if (radius < 1)
			d = -d;
		image$data(sp2)[i * 3 + 0] = R + d;
		image$data(sp2)[i * 3 + 1] = G + d;
		image$data(sp2)[i * 3 + 2] = B + d;
	}

	return sp2;
}

sexpr *color_image_red(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!color_image(sp1)) {
		printf("color-image-red: Argument is not a color image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = image$data(sp1)[i * 3];
	return sp2;
}

sexpr *color_image_green(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!color_image(sp1)) {
		printf("color-image-green: Argument is not a color image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = image$data(sp1)[i * 3 + 1];

	return sp2;
}

sexpr *color_image_blue(sexpr *sp1) {
	int i, n, rows, cols;
	sexpr *sp2;

	if (!color_image(sp1)) {
		printf("color-image-blue: Argument is not a color image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	sp2 = make_image(rows, cols);

	n = rows * cols;
	for (i = 0; i < n; i++)
		image$data(sp2)[i] = image$data(sp1)[i * 3 + 2];

	return sp2;
}

sexpr *rgb2hsi(sexpr *spr, sexpr *spg, sexpr *spb) {

	sexpr *sph, *sps, *spi;

	float v1, v2, r, g, b;
	int rows, cols, j, n;

	if (!image(spr) || !image(spg) || !image(spb)) {
		printf("rgb->hsi: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(spr);
	cols = image$cols(spr);

	check_image_sizes("rgb->hsi", rows, cols, spg);
	check_image_sizes("rgb->hsi", rows, cols, spb);

	sph = make_image(rows, cols);
	sps = make_image(rows, cols);
	spi = make_image(rows, cols);

	n = rows * cols;
	for (j = 0; j < n; j++) {
		r = image$data(spr)[j];
		g = image$data(spg)[j];
		b = image$data(spb)[j];

		/*
		 v1 = (2.0*b-r-g)/2.449489742783178;
		 v2 = (r-g)/2.449489742783178;
		 */

		v1 = (2.0 * r - g - b) / 2.449489742783178;
		v2 = (g - b) / 2.449489742783178;

		image$data(sph)[j] = (v1 != 0) ? atan2(v2, v1) : 0.0;
		image$data(sps)[j] = sqrt(v1 * v1 + v2 * v2);
		image$data(spi)[j] = (r + g + b) / 3;
	}
	return cons(sph, list2(sps, spi));
}

sexpr *hsi2rgb(sexpr *sph, sexpr *sps, sexpr *spi) {

	sexpr *spr, *spg, *spb;

	float v1, v2, h, s, i;
	int rows, cols, j, n;

	if (!image(sph) || !image(sps) || !image(spi)) {
		printf("hsi->rgb: Argument is not an image.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sph);
	cols = image$cols(sph);

	check_image_sizes("hsi->rgb", rows, cols, sps);
	check_image_sizes("hsi->rgb", rows, cols, spi);

	spr = make_image(rows, cols);
	spg = make_image(rows, cols);
	spb = make_image(rows, cols);

	n = rows * cols;
	for (j = 0; j < n; j++) {
		h = image$data(sph)[j];
		s = image$data(sps)[j];
		i = image$data(spi)[j];

		v1 = 2.449489742783178 * s * cos(h) / 3;
		v2 = 2.449489742783178 * s * sin(h) / 2;

		/*
		 image$data(spr)[j] = i - v1/2 + v2;
		 image$data(spg)[j] = i - v1/2 - v2;
		 image$data(spb)[j] = i + v1;
		 */

		image$data(spg)[j] = i - v1 / 2 + v2;
		image$data(spb)[j] = i - v1 / 2 - v2;
		image$data(spr)[j] = i + v1;
	}
	return cons(spr, list2(spg, spb));
}

sexpr *upsample_cols(sexpr *sp1) {

	int i, j, cols;
	int rows1, rows2, index1, index2;

	sexpr *sp2;

	switch (sp1->type) {
	case IMAGE:
		cols = image$cols(sp1);
		rows1 = image$rows(sp1);
		rows2 = rows1 * 2;
		sp2 = make_image(rows2, cols);

		for (i = 0; i < rows2; i++) {
			for (j = 0; j < cols; j++) {
				index2 = cols * i + j;
				if (even(i)) {
					index1 = cols * i / 2 + j;
					image$data(sp2)[index2] = image$data(sp1)[index1];
				} else
					image$data(sp2)[index2] = 0;
			}
		}
		return sp2;
	case COMPLEX_IMAGE:
		cols = complex_image$cols(sp1);
		rows1 = complex_image$rows(sp1);
		rows2 = rows1 * 2;
		sp2 = make_complex_image(rows2, cols);

		for (i = 0; i < rows2; i++) {
			for (j = 0; j < cols; j++) {
				index2 = cols * i + j;
				if (even(i)) {
					index1 = cols * i / 2 + j;
					complex_image$data(sp2)[index2] = complex_image$data(sp1)[index1];
				} else
					complex_image$data(sp2)[index2] = zero;
			}
		}
		return sp2;
	default:
		printf("upsample-cols: Argument must be an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *upsample_rows(sexpr *sp1) {

	int i, j, rows;
	int cols1, cols2, index1, index2;

	sexpr *sp2;

	switch (sp1->type) {
	case IMAGE:
		rows = image$rows(sp1);
		cols1 = image$cols(sp1);
		cols2 = cols1 * 2;

		sp2 = make_image(rows, cols2);

		for (j = 0; j < cols2; j++) {
			for (i = 0; i < rows; i++) {
				index2 = cols2 * i + j;
				if (even(j)) {
					index1 = cols1 * i + j / 2;
					image$data(sp2)[index2] = image$data(sp1)[index1];
				} else
					image$data(sp2)[index2] = 0;
			}
		}
		return sp2;
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols1 = complex_image$cols(sp1);
		cols2 = cols1 * 2;

		sp2 = make_complex_image(rows, cols2);

		for (j = 0; j < cols2; j++) {
			for (i = 0; i < rows; i++) {
				index2 = cols2 * i + j;
				if (even(j)) {
					index1 = cols1 * i + j / 2;
					complex_image$data(sp2)[index2] = complex_image$data(sp1)[index1];
				} else
					complex_image$data(sp2)[index2] = zero;
			}
		}
		return sp2;
	default:
		printf("upsample-rows: Argument must be an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *downsample_cols(sexpr *sp1) {

	int i1, i2, j, cols;
	int rows1, rows2, index1, index2;

	sexpr *sp2;

	switch (sp1->type) {
	case IMAGE:
		cols = image$cols(sp1);
		rows1 = image$rows(sp1);

		if (rows1 <= 1) {
			printf("downsample-cols: Too few rows.\n");
			longjmp(esc, 1);
		}

		rows2 = even(rows1) ? rows1 / 2 : rows1 / 2 + 1;

		sp2 = make_image(rows2, cols);

		for (i1 = 0, i2 = 0; i1 < rows1; i1 += 2, i2++) {
			for (j = 0; j < cols; j++) {
				index1 = cols * i1 + j;
				index2 = cols * i2 + j;
				image$data(sp2)[index2] = image$data(sp1)[index1];
			}
		}
		return sp2;
	case COMPLEX_IMAGE:
		cols = complex_image$cols(sp1);
		rows1 = complex_image$rows(sp1);

		if (rows1 <= 1) {
			printf("downsample-cols: Too few rows.\n");
			longjmp(esc, 1);
		}

		rows2 = even(rows1) ? rows1 / 2 : rows1 / 2 + 1;

		sp2 = make_complex_image(rows2, cols);

		for (i1 = 0, i2 = 0; i1 < rows1; i1 += 2, i2++) {
			for (j = 0; j < cols; j++) {
				index1 = cols * i1 + j;
				index2 = cols * i2 + j;
				complex_image$data(sp2)[index2] = complex_image$data(sp1)[index1];
			}
		}
		return sp2;
	default:
		printf("downsample-cols: Argument is not an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *downsample_rows(sexpr *sp1) {

	int i, j1, j2, rows;
	int cols1, cols2, index1, index2;

	sexpr *sp2;

	switch (sp1->type) {
	case IMAGE:
		cols1 = image$cols(sp1);
		rows = image$rows(sp1);

		if (cols1 <= 1) {
			printf("downsample-rows: Too few columns.\n");
			longjmp(esc, 1);
		}

		cols2 = even(cols1) ? cols1 / 2 : cols1 / 2 + 1;

		sp2 = make_image(rows, cols2);

		for (j1 = 0, j2 = 0; j1 < cols1; j1 += 2, j2++) {
			for (i = 0; i < rows; i++) {
				index1 = cols1 * i + j1;
				index2 = cols2 * i + j2;
				image$data(sp2)[index2] = image$data(sp1)[index1];
			}
		}
		return sp2;
	case COMPLEX_IMAGE:
		cols1 = complex_image$cols(sp1);
		rows = complex_image$rows(sp1);

		if (cols1 <= 1) {
			printf("downsample-rows: Too few columns.\n");
			longjmp(esc, 1);
		}

		cols2 = even(cols1) ? cols1 / 2 : cols1 / 2 + 1;

		sp2 = make_complex_image(rows, cols2);

		for (j1 = 0, j2 = 0; j1 < cols1; j1 += 2, j2++) {
			for (i = 0; i < rows; i++) {
				index1 = cols1 * i + j1;
				index2 = cols2 * i + j2;
				complex_image$data(sp2)[index2] = complex_image$data(sp1)[index1];
			}
		}
		return sp2;
	default:
		printf("downsample-rows: Argument is not an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *image_transpose(sexpr *sp1) {

	int i, j, rows, cols;
	int index1, index2;
	sexpr *sp2;

	switch (sp1->type) {
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		sp2 = make_image(cols, rows);

		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				index1 = cols * i + j;
				index2 = rows * j + i;
				image$data(sp2)[index2] = image$data(sp1)[index1];
			}
		}
		return sp2;
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);
		sp2 = make_complex_image(cols, rows);

		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				index1 = cols * i + j;
				index2 = rows * j + i;
				complex_image$data(sp2)[index2] = complex_image$data(sp1)[index1];
			}
		}
		return sp2;
	default:
		printf("image-transpose: Argument is not an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *array2complex(sexpr *sp1) {

	int i, j, rows, cols, index;
	sexpr *row, *sp2;

	rows = vector$length(sp1);
	cols = vector$length(vector$data(sp1)[0]);

	sp2 = make_complex_image(rows, cols);

	for (i = 0; i < rows; i++) {
		row = vector$data(sp1)[i];

		/* Need to check row length */
		for (j = 0; j < cols; j++) {
			index = cols * i + j;
			switch (vector$data(row)[j]->type) {
			case NUMBER:
				complex_image$data(sp2)[index] = Complex(vector$data(row)[j]->u.x, 0.0);
				break;
			case COMPLEX:
				complex_image$data(sp2)[index] = vector$data(row)[j]->u.z;
				break;
			default:
				printf("array->complex: Non-number at location (%d, %d).\n", i,
						j);
				longjmp(esc, 1);
			}
		}
	}
	return sp2;
}

sexpr *array2image(sexpr *sp1) {

	int i, j, rows, cols, index;
	sexpr *row, *sp2;

	if (!vector(sp1) || !vector(vector$data(sp1)[0])) {
		print_value_escape("array->image: Argument is not an array: ", sp1);
	}

	rows = vector$length(sp1);
	cols = vector$length(vector$data(sp1)[0]);
	for (i = 1; i < rows; i++) {
		if (!vector(vector$data(sp1)[i]) || vector$length(vector$data(sp1)[i]) != cols) {
			print_value_escape("array->image: Argument is not an array: ", sp1);
		}
	}

	sp2 = make_image(rows, cols);

	for (i = 0; i < rows; i++) {
		row = vector$data(sp1)[i];
		for (j = 0; j < cols; j++) {
			index = cols * i + j;
			switch (vector$data(row)[j]->type) {
			case NUMBER:
				image$data(sp2)[index] = (float) vector$data(row)[j]->u.x;
				break;
			case COMPLEX:
				return array2complex(sp1);
			default:
				printf("array->image: Non-number at location (%d, %d).\n", i, j);
				longjmp(esc, 1);
			}
		}
	}
	return sp2;
}

sexpr *scheme_list2vector(sexpr *ls) {
	int i, len;
	sexpr *sp, **data;
	if (!pair(ls) && !null(ls)) {
		print_value_escape("list->vector: Argument is not a list: ", ls);
	}

	len = length(ls);
	sp = make_vector(len);
	data = vector$data(sp);

	for (i = 0; i < len; i++) {
		data[i] = car(ls);
		ls = cdr(ls);
	}
	return sp;
}

sexpr *list2vector(sexpr *ls) {
	int i, len;
	sexpr *sp, **data;

	/*
	 if (!pair(ls) && !null(ls)) {
	 print_value_escape("list->vector: Argument is not a list: ",ls);
	 }
	 */

	len = length(ls);
	sp = make_vector(len);
	data = vector$data(sp);

	for (i = 0; i < len; i++) {
		data[i] = pair$car(ls);
		ls = pair$cdr(ls);
	}
	return sp;
}

sexpr *vector2list(sexpr *sp1) {
	int i, len;
	sexpr *sp2;

	if (!vector(sp1)) {
		printf("vector->list: Argument is not a vector.\n");
		longjmp(esc, 1);
	}

	len = vector$length(sp1);

	sp2 = nil;
	for (i = len - 1; i >= 0; i--)
		sp2 = cons(vector$data(sp1)[i], sp2);

	return sp2;
}

sexpr *convolve(sexpr *sp1, sexpr *sp2) {

	int i1, j1, i2, j2, mn;
	int rows, cols, m, n, m2, n2, index1, index2;
	float sum, *data1, *data2, *data3;
	fcomplex csum, *cdata1, *cdata2, *cdata3;
	sexpr *sp3;

	switch (sp1->type) {
	case IMAGE:
		rows = image$rows(sp1);
		cols = image$cols(sp1);
		data1 = image$data(sp1);

		switch (sp2->type) {
		case VECTOR:
			return convolve(sp1, array2image(sp2));
		case IMAGE:
			m = image$rows(sp2);
			n = image$cols(sp2);
			data2 = image$data(sp2);

			m2 = even(m) ? m / 2 - 1 : (int) floor(m / 2);
			n2 = even(n) ? n / 2 - 1 : (int) floor(n / 2);

			sp3 = make_image(rows, cols);
			data3 = image$data(sp3);

			for (i1 = 0; i1 < rows; i1++) {
				for (j1 = 0; j1 < cols; j1++) {
					sum = 0;
					for (i2 = 0; i2 < m; i2++) {
						for (j2 = 0; j2 < n; j2++) {
							index1 = cols * mod(i1+i2-m2,rows) + mod(j1+j2-n2,cols);
							index2 = n * (m - i2 - 1) + (n - j2 - 1);
							sum += data1[index1] * data2[index2];
						}
					}
					data3[cols * i1 + j1] = sum;
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			m = complex_image$rows(sp2);
			n = complex_image$cols(sp2);
			mn = m * n;
			cdata2 = complex_image$data(sp2);

			m2 = even(m) ? m / 2 - 1 : (int) floor(m / 2);
			n2 = even(n) ? n / 2 - 1 : (int) floor(n / 2);

			sp3 = make_complex_image(rows, cols);
			cdata3 = complex_image$data(sp3);

			for (i1 = 0; i1 < rows; i1++) {
				for (j1 = 0; j1 < cols; j1++) {
					csum = zero;
					for (i2 = 0; i2 < m; i2++) {
						for (j2 = 0; j2 < n; j2++) {
							index1 = cols * mod(i1+i2-m2,rows) + mod(j1+j2-n2,cols);
							index2 = n * (m - i2 - 1) + (n - j2 - 1);
							csum = Cadd(csum, RCmul(data1[index1], Conjg(
									cdata2[index2])));
						}
					}
					cdata3[cols * i1 + j1] = csum;
				}
			}
			return sp3;
		default:
			printf("convolve: Incompatible kernel.\n");
			longjmp(esc, 1);
		}
	case COMPLEX_IMAGE:
		rows = complex_image$rows(sp1);
		cols = complex_image$cols(sp1);
		cdata1 = complex_image$data(sp1);

		switch (sp2->type) {
		case VECTOR:
			return convolve(sp1, array2image(sp2));
		case IMAGE:
			m = image$rows(sp2);
			n = image$cols(sp2);
			data2 = image$data(sp2);

			m2 = even(m) ? m / 2 - 1 : (int) floor(m / 2);
			n2 = even(n) ? n / 2 - 1 : (int) floor(n / 2);

			sp3 = make_complex_image(rows, cols);
			cdata3 = complex_image$data(sp3);

			for (i1 = 0; i1 < rows; i1++) {
				for (j1 = 0; j1 < cols; j1++) {
					csum = zero;
					for (i2 = 0; i2 < m; i2++) {
						for (j2 = 0; j2 < n; j2++) {
							index1 = cols * mod(i1+i2-m2,rows) + mod(j1+j2-n2,cols);
							index2 = n * (m - i2 - 1) + (n - j2 - 1);
							csum = Cadd(csum, RCmul(data2[index2],
									cdata1[index1]));
						}
					}
					cdata3[cols * i1 + j1] = csum;
				}
			}
			return sp3;
		case COMPLEX_IMAGE:
			m = complex_image$rows(sp2);
			n = complex_image$cols(sp2);
			mn = m * n;
			cdata2 = complex_image$data(sp2);

			m2 = even(m) ? m / 2 - 1 : (int) floor(m / 2);
			n2 = even(n) ? n / 2 - 1 : (int) floor(n / 2);

			sp3 = make_complex_image(rows, cols);
			cdata3 = complex_image$data(sp3);

			for (i1 = 0; i1 < rows; i1++) {
				for (j1 = 0; j1 < cols; j1++) {
					csum = zero;
					for (i2 = 0; i2 < m; i2++) {
						for (j2 = 0; j2 < n; j2++) {
							index1 = cols * mod(i1+i2-m2,rows) + mod(j1+j2-n2,cols);
							index2 = n * (m - i2 - 1) + (n - j2 - 1);
							csum = Cadd(csum, Cmul(cdata1[index1], Conjg(
									cdata2[index2])));
						}
					}
					cdata3[cols * i1 + j1] = csum;
				}
			}
			return sp3;
		default:
			printf("convolve: Incompatible kernel.\n");
			longjmp(esc, 1);
		}
	default:
		printf("convolve: First argument must be an image.\n");
		longjmp(esc, 1);
	}
}

sexpr *outline(sexpr *sp1, sexpr *sp2, sexpr *sp3) {

	int i, j, index0, index1;
	int rows, cols, edge, non_edge;

	float *data1, *data4;

	sexpr *sp4;

	if (!image(sp1)) {
		print_value_escape("outline: First argument must be image: ", sp1);
	}

	if (!number(sp2) || !number(sp3)) {
		printf("outline: Second and third arguments must be numbers.\n");
		longjmp(esc, 1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data1 = image$data(sp1);

	edge = sp2->u.x;
	non_edge = sp3->u.x;

	sp4 = make_image(rows, cols);
	data4 = image$data(sp4);

	for (i = 0; i < rows; i++) {
		index0 = i * cols;
		for (j = 0; j < cols; j++) {
			data4[index0 + j] = non_edge;
		}
	}

	for (i = 0; i < rows - 1; i++) {
		index0 = i * cols;
		index1 = (i + 1) * cols;
		for (j = 0; j < cols; j++) {
			if (data1[index0 + j] + data1[index1 + j] == 1) {
				data4[index0 + j] = edge;
			}
		}
	}

	index0 = (rows - 1) * cols;
	for (j = 0; j < cols; j++) {
		if (data1[index0 + j] + data1[j] == 1) {
			data4[index0 + j] = edge;
		}
	}

	for (i = 0; i < rows; i++) {
		index0 = i * cols;
		for (j = 1; j < cols - 1; j++) {
			if (data1[index0 + j] + data1[index0 + j + 1] == 1) {
				data4[index0 + j] = edge;
			}
		}
	}

	for (i = 0; i < rows; i++) {
		index0 = i * cols;
		if (data1[index0 + cols - 1] + data1[index0] == 1) {
			data4[index0 + cols - 1] = edge;
		}
	}

	return sp4;
}

sexpr *perimeters(sexpr *sp1) {
	int i, j, index0, index1, index2;
	int rows, cols, maximum, n;
	float *data1, *data2;
	sexpr *sp2;

	if (!image(sp1)) {
		print_value_escape("perimeters: Argument must be image: ", sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data1 = image$data(sp1);
	n = rows * cols;

	maximum = 0;
	for (i = 0; i < n; i++)
		if (data1[i] > maximum)
			maximum = data1[i];

	sp2 = make_image(1, maximum + 1);
	data2 = image$data(sp2);
	for (i = 0; i <= maximum; i++)
		data2[i] = 0.0;

	for (i = 0; i < rows - 1; i++) {
		index0 = i * cols;
		index1 = (i + 1) * cols;
		for (j = 0; j < cols; j++) {

			index2 = (int) data1[index0 + j];
			if (index2 > 0) {
				if (data1[index1 + j] == 0) {
					data2[index2] = data2[index2] + 1;
				}
			}

			index2 = (int) data1[index1 + j];
			if (index2 > 0) {
				if (data1[index0 + j] == 0) {
					data2[index2] = data2[index2] + 1;
				}
			}
		}
	}

	index0 = (rows - 1) * cols;
	for (j = 0; j < cols; j++) {

		index2 = (int) data1[index0 + j];
		if (index2 > 0) {
			if (data1[j] == 0) {
				data2[index2] = data2[index2] + 1;
			}
		}

		index2 = (int) data1[j];
		if (index2 > 0) {
			if (data1[index0 + j] == 0) {
				data2[index2] = data2[index2] + 1;
			}
		}
	}

	for (i = 0; i < rows; i++) {
		index0 = i * cols;
		for (j = 1; j < cols - 1; j++) {

			index2 = data1[index0 + j];
			if (index2 > 0) {
				if (data1[index0 + j + 1] == 0) {
					data2[index2] = data2[index2] + 1;
				}
			}

			index2 = data1[index0 + j + 1];
			if (index2 > 0) {
				if (data1[index0 + j] == 0) {
					data2[index2] = data2[index2] + 1;
				}
			}
		}
	}

	for (i = 0; i < rows; i++) {
		index0 = i * cols;

		index2 = (int) data1[index0 + cols - 1];
		if (index2 > 0) {
			if (data1[index0 + cols] == 0) {
				data2[index2] = data2[index2] + 1;
			}
		}

		index2 = (int) data1[index0 + cols];
		if (index2 > 0) {
			if (data1[index0 + cols - 1] == 0) {
				data2[index2] = data2[index2] + 1;
			}
		}
	}

	return vector$data(image2array(sp2))[0];
}

sexpr *areas(sexpr *sp1) {
	int i, index;
	int rows, cols, maximum, n;
	float *data1, *data2;
	sexpr *sp2;

	if (!image(sp1)) {
		print_value_escape("areas: Argument must be image: ", sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data1 = image$data(sp1);
	n = rows * cols;

	maximum = 0;
	for (i = 0; i < n; i++)
		if (data1[i] > maximum)
			maximum = data1[i];

	sp2 = make_image(1, maximum + 1);
	data2 = image$data(sp2);
	for (i = 0; i <= maximum; i++)
		data2[i] = 0.0;

	for (i = 0; i < n; i++) {
		index = (int) data1[i];
		if (index >= 0) {
			data2[index] = data2[index] + 1;
		}
	}

	return vector$data(image2array(sp2))[0];
}

sexpr *centers_of_mass(sexpr *sp1) {
	int i, j, index;
	int rows, cols, maximum, n;
	int *r, *c, *a;
	float *data;
	sexpr *sp2;

	if (!image(sp1)) {
		print_value_escape("centers-of-mass: Argument must be image: ", sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data = image$data(sp1);
	n = rows * cols;

	maximum = 0;
	for (i = 0; i < n; i++)
		if (data[i] > maximum)
			maximum = data[i];
	maximum++;

	r = (int *) malloc(maximum * sizeof(int));
	c = (int *) malloc(maximum * sizeof(int));
	a = (int *) malloc(maximum * sizeof(int));

	for (i = 0; i < maximum; i++) {
		r[i] = 0;
		c[i] = 0;
		a[i] = 0;
	}

	sp2 = make_vector(maximum);

	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols; j++) {
			index = (int) data[i * cols + j];
			r[index] += i;
			c[index] += j;
			a[index]++;
		}
	}

	for (i = 0; i < maximum; i++) {
		vector$data(sp2)[i] = cons(num2exp((float) r[i] / (float) a[i]), cons(
				num2exp((float) c[i] / (float) a[i]), nil));
	}

	free(r);
	free(c);
	free(a);

	return sp2;
}

sexpr *bounding_boxes(sexpr *sp1) {
	int i, j, index;
	int rows, cols, maximum, n;
	int *rmin, *rmax, *cmin, *cmax;
	float *data;
	sexpr *sp2;

	if (!image(sp1)) {
		print_value_escape("bounding-boxes: Argument must be image: ", sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data = image$data(sp1);
	n = rows * cols;

	maximum = 0;
	for (i = 0; i < n; i++)
		if (data[i] > maximum)
			maximum = data[i];
	maximum++;

	rmin = (int *) malloc(maximum * sizeof(int));
	rmax = (int *) malloc(maximum * sizeof(int));
	cmin = (int *) malloc(maximum * sizeof(int));
	cmax = (int *) malloc(maximum * sizeof(int));

	for (i = 0; i < maximum; i++) {
		rmin[i] = 0;
		rmax[i] = 0;
		cmin[i] = 0;
		cmax[i] = 0;
	}

	sp2 = make_vector(maximum);

	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols; j++) {
			index = (int) data[i * cols + j];
			rmax[index] = i;
			cmax[index] = j;
		}
	}

	for (i = rows - 1; i >= 0; i--) {
		for (j = cols - 1; j >= 0; j--) {
			index = (int) data[i * cols + j];
			rmin[index] = i;
			cmin[index] = j;
		}
	}

	for (i = 0; i < maximum; i++) {
		vector$data(sp2)[i] = cons(num2exp((float) rmin[i]), cons(num2exp(
				(float) cmin[i]), cons(num2exp((float) rmax[i]), cons(num2exp(
				(float) cmax[i]), nil))));
	}

	free(rmin);
	free(cmin);
	free(rmax);
	free(cmax);

	return sp2;
}

sexpr *label(sexpr *sp1) {
	int i, j, index0, index1, n;
	int rows, cols;
	int a, b, c;
	float *data1, *data2;
	int *alias;
	sexpr *sp2;

	if (!image(sp1)) {
		print_value_escape("label: First argument must be image: ", sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	data1 = image$data(sp1);

	c = 1;

	sp2 = make_image(rows, cols);
	data2 = image$data(sp2);

	n = rows * cols;
	for (i = 0; i < n; i++)
		data2[i] = 0.0;

	alias = malloc(n * sizeof(int));
	for (i = 0; i < n; i++)
		alias[i] = i;

	for (i = 0; i < rows; i++) {
		for (j = 0; j < cols; j++) {
			if (data1[cols * i + j] > 0) {
				a = (int) data2[cols * mod(i-1,rows) + j];
				b = (int) data2[cols * i + mod(j-1,cols)];
				if (!a && !b) {
					data2[cols * i + j] = (float) c++;
				} else if (a && b) {
					if (alias[a] == alias[b]) {
						data2[cols * i + j] = (float) alias[a];
					} else {
						/* use lower and set higher alias to lower */
						if (alias[a] < alias[b]) {
							data2[cols * i + j] = (float) alias[a];
							alias[b] = alias[a];
						} else {
							data2[cols * i + j] = (float) alias[b];
							alias[a] = alias[b];
						}
					}
				} else if (a) {
					data2[cols * i + j] = (float) alias[a];
				} else {
					data2[cols * i + j] = (float) alias[b];
				}
			}
		}
	}

	/* find lowest label in each equivalence class */
	for (i = 0; i < n; i++) {
		index0 = (int) data2[i];
		if (index0 > 0) {
			index1 = alias[index0];
			while (index0 != index1) {
				index0 = index1;
				index1 = alias[index0];
			}
			data2[i] = (float) index1;
		}
	}

	/* renumber labels */
	for (i = 1, j = 1; i < (int) c; i++) {
		if (alias[i] == i)
			alias[i] = j++;
	}

	/* re-label image */
	for (i = 0; i < n; i++) {
		data2[i] = (float) alias[(int) data2[i]];
	}

	free(alias);

	return sp2;
}

/*

 sexpr *median_filter(sexpr *sp1, sexpr *sp2, sexpr *sp3) {
 int rows, cols;
 float *data;

 if (!image(sp1)) {
 print_value_escape("median-filter: First argument must be image: ",sp1);
 }

 if (!number(sp2)) {
 print_value_escape("median-filter: Second argument must be number: ",sp2);
 }

 if (!number(sp3)) {
 print_value_escape("median-filter: Third argument must be number: ",sp3);
 }

 rows = image$rows(sp1);
 cols = image$cols(sp1);
 data1 = image$data(sp1);

 for (i = 0; i < rows; i++) {
 for (j = 0; j < cols; j++) {
 }
 }

 sp4 = make_image(rows,cols);
 data4 = image$data(sp4);

 return sp4;
 }

 */

sexpr *distance_transform(sexpr *sp1) {

	int i, j, k, l, index0, index1;
	int rows, cols, m, n;
	float sum, x, *data1, *data2, *kdata1, *kdata2;
	sexpr *sp2;

	sexpr *kernel1 = make_image(5, 5);
	sexpr *kernel2 = make_image(5, 5);

	if (!image(sp1)) {
		print_value_escape("distance-transform: Argument must be an image: ",
				sp1);
	}

	rows = image$rows(sp1);
	cols = image$cols(sp1);
	n = (int) sqrt(rows * rows + cols * cols) / 2.0;
	data1 = image$data(sp1);

	kdata1 = image$data(kernel1);
	kdata2 = image$data(kernel2);

	kdata1[0] = kdata2[24] = 2.8284;
	kdata1[1] = kdata2[23] = 2.2;
	kdata1[2] = kdata2[22] = 2.0;
	kdata1[3] = kdata2[21] = 2.2;
	kdata1[4] = kdata2[20] = 2.8284;
	kdata1[5] = kdata2[19] = 2.2;
	kdata1[6] = kdata2[18] = 1.4;
	kdata1[7] = kdata2[17] = 1.0;
	kdata1[8] = kdata2[16] = 1.4;
	kdata1[9] = kdata2[15] = 2.2;
	kdata1[10] = kdata2[14] = 2.0;
	kdata1[11] = kdata2[13] = 1.0;
	kdata1[12] = kdata2[12] = 0.0;

	sp2 = outline(sp1, number_zero, num2exp(10e7));
	data2 = image$data(sp2);

	for (m = 0; m < n; m++) {

		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				sum = 10e7;
				for (k = 2; k < 5; k++) {
					index0 = cols * mod(i+k-2,rows);
					index1 = 5 * k;
					for (l = 0; l < 5; l++) {
						if (index1 + l >= 12) {
							x = data2[index0 + mod(j+l-2,cols)] + kdata2[index1 + l];
							sum = min(sum,x);
						}
					}
				}
				data2[cols * i + j] = sum;
			}
		}

		for (i = rows - 1; i >= 0; i--) {
			for (j = cols - 1; j >= 0; j--) {
				sum = 10e7;
				for (k = 0; k < 3; k++) {
					index0 = cols * mod(i+k-2,rows);
					index1 = 5 * k;
					for (l = 0; l < 5; l++) {
						if (index1 + l <= 12) {
							x = data2[index0 + mod(j+l-2,cols)] + kdata1[index1 + l];
							sum = min(sum,x);
						}
					}
				}
				data2[cols * i + j] = sum;
			}
		}
	}

	n = rows * cols;
	for (i = 0; i < n; i++)
		data2[i] *= data1[i];

	return sp2;
}

sexpr *set_image_display_scale(sexpr *scale) {
	if (!number(scale)) {
		printf("set-image-scale!: Illegal argument.\n");
		longjmp(esc, 1);
	}

	image_display_scale = scale->u.x;
	return undefined;
}

sexpr *set_graphic_default_color(sexpr *color) {
	if (!pair(color)) {
		printf("set-graphic-color!: Illegal argument.\n");
		longjmp(esc, 1);
	}

	graphic_default_color = color;
	return undefined;
}

sexpr *graphic2postscript(sexpr *sp1, sexpr *sp2) {

	sexpr *port, *p;

	char text[STRLEN];

	if (!graphic(sp1)) {
		printf("graphic->postscript: First argument must be graphic object.\n");
		longjmp(esc, 1);
	}

	if ((!string(sp2))) {
		printf("graphic->postscript: Illegal filename.\n");
		longjmp(esc, 1);
	}

	port = open_output_file(sp2);

	sprintf(text, "#<graphic:%s>", symbol$name(gtype(sp1)));
	generate_postscript_header(port->u.file, 1.0, text);

	p = make_plumber(1.0, -0.6667, HALFPI);

	draw_plumber_postscript(p, graphic_default_color, port->u.file);

	p = draw_graphic_postscript(p, sp1, port->u.file);

	draw_plumber_postscript(p, graphic_default_color, port->u.file);

	fprintf(port->u.file, "showpage\n");

	close_output_port(port);

	return undefined;
}

sexpr *draw_transparent(sexpr *p1, sexpr *sp) {

	sexpr *len = cadr(graphic$sexpr(sp));
	sexpr *p2;

	double x, y, heading;

	heading = plumber$heading(p1);
	x = plumber$x(p1) + (len->u.x / 300.0) * cos(heading);
	y = plumber$y(p1) + (len->u.x / 300.0) * sin(heading);
	p2 = make_plumber(x, y, heading);

	return p2;
}

sexpr *draw_bend(sexpr *p1, sexpr *sp) {

	sexpr *second = cadr(graphic$sexpr(sp));
	sexpr *p2;

	double x, y, heading;

	x = plumber$x(p1);
	y = plumber$y(p1);
	heading = plumber$heading(p1);
	p2 = make_plumber(x, y, angle_difference(heading, deg2rad(second->u.x)));
	return p2;
}

sexpr *draw_graphic_postscript(sexpr *p, sexpr *sp, FILE *file) {
	sexpr *g, *first;
	g = graphic$sexpr(sp);

	if (null(g))
		return p;

	first = car(g);
	if (eq(first, symbol_straight))
		return draw_straight_postscript(p, sp, file);
	else if (eq(first, symbol_spot))
		return draw_spot_postscript(p, sp, file);
	else if (eq(first, symbol_transparent))
		return draw_transparent(p, sp);
	else if (eq(first, symbol_adjoin))
		return draw_adjoin_postscript(p, sp, file);
	else if (eq(first, symbol_adorn)) {
		draw_adorn_postscript(p, sp, file);
		return p;
	} else if (eq(first, symbol_bend))
		return draw_bend(p, sp);
	else if (eq(first, symbol_text))
		return draw_text_postscript(p, sp, file);
	else {
		printf("Unrecognized graphic type.\n");
		return p;
	}
}

sexpr *draw_text_postscript(sexpr *p, sexpr *sp, FILE *file) {

	sexpr *text = cadr(graphic$sexpr(sp));
	sexpr *color = cadr(cdr(graphic$sexpr(sp)));
	sexpr *size = cadr(cddr(graphic$sexpr(sp)));

	sexpr *r = car(color);
	sexpr *g = cadr(color);
	sexpr *b = car(cddr(color));

	if (!string(text)) {
		printf("graphic->postscript: Illegal text argument.\n");
		longjmp(esc, 1);
	}

	if (!number(r) || !number(g) || !number(b)) {
		printf("graphic->postscript: Illegal color argument.\n");
		longjmp(esc, 1);
	}

	if (!number(size)) {
		printf("graphic->postscript: Illegal size argument.\n");
		longjmp(esc, 1);
	}

	fprintf(file, "%f %f moveto\n", (1.0 - plumber$y(p)) / 2.0, plumber$x(p)
	/ 2.0);
	fprintf(file, "90 rotate\n");
	fprintf(file, "/Helvetica findfont\n");
	fprintf(file, "%f scalefont\n", size->u.x * 0.02);
	fprintf(file, "setfont\n");

	if (r->u.x + g->u.x + b->u.x > 764)
		fprintf(file, "%f %f %f setrgbcolor\n", 0.0, 0.0, 0.0);
	else
		fprintf(file, "%f %f %f setrgbcolor\n", r->u.x / 128, g->u.x / 128,
				b->u.x / 128);
	fprintf(file, "(%s) show\n", text->u.text);
	fprintf(file, "-90 rotate\n");

	return p;
}

sexpr *draw_plumber_postscript(sexpr *p, sexpr *color, FILE *file) {

	sexpr *r, *g, *b;

	double c, s;

	float x0, y0, x1, y1, x2, y2;

	c = cos(plumber$heading(p)- PI / 2.0) / 60.0;
	s = sin(plumber$heading(p)- PI / 2.0) / 60.0;

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("graphic->postscript: Illegal color argument.\n");
		longjmp(esc, 1);
	}

	x0 = plumber$x(p) - C30 * c + S30 * s;
	y0 = 1.0 - plumber$y(p) + C30 * s + S30 * c;

	x1 = plumber$x(p) - s;
	y1 = 1.0 - plumber$y(p) - c;

	x2 = plumber$x(p) + C30 * c + S30 * s;
	y2 = 1.0 - plumber$y(p) - C30 * s + S30 * c;

	fprintf(file, "%f setlinewidth\n", 1 / 864.0);
	if (r->u.x + g->u.x + b->u.x > 764)
		fprintf(file, "%f %f %f setrgbcolor\n", 0.0, 0.0, 0.0);
	else
		fprintf(file, "%f %f %f setrgbcolor\n", r->u.x / 128, g->u.x / 128,
				b->u.x / 128);
	fprintf(file, "%f %f moveto\n", y0 / 2.0, x0 / 2.0);
	fprintf(file, "%f %f lineto\n", y1 / 2.0, x1 / 2.0);
	fprintf(file, "%f %f lineto\n", y2 / 2.0, x2 / 2.0);
	fprintf(file, "%f %f lineto\n", y0 / 2.0, x0 / 2.0);
	fprintf(file, "stroke\n");

	return p;
}

sexpr *draw_straight_postscript(sexpr *p1, sexpr *sp, FILE *file) {

	sexpr *len = cadr(graphic$sexpr(sp));
	sexpr *color = car(cddr(graphic$sexpr(sp)));
	sexpr *width = cadr(cddr(graphic$sexpr(sp)));

	sexpr *p2;
	sexpr *r, *g, *b;
	double x, y, heading;

	heading = plumber$heading(p1);
	x = plumber$x(p1) + (len->u.x / 300.0) * cos(heading);
	y = plumber$y(p1) + (len->u.x / 300.0) * sin(heading);
	p2 = make_plumber(x, y, heading);

	if (!number(len)) {
		printf("graphic->postscript: Illegal length argument.\n");
		longjmp(esc, 1);
	}

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("graphic->postscript: Illegal color argument.\n");
		longjmp(esc, 1);
	}

	if (!number(width)) {
		printf("graphic->postscript: Illegal width argument.\n");
		longjmp(esc, 1);
	}

	fprintf(file, "%f setlinewidth\n", width->u.x / 864.0);

	if (r->u.x + g->u.x + b->u.x > 764)
		fprintf(file, "%f %f %f setrgbcolor\n", 0.0, 0.0, 0.0);
	else
		fprintf(file, "%f %f %f setrgbcolor\n", r->u.x / 128, g->u.x / 128,
				b->u.x / 128);

	fprintf(file, "%f %f %f %f displine\n", (1.0 - plumber$y(p1)) / 2.0, plumber$x(p1) / 2.0, (1.0 - y) / 2.0, x / 2.0);

	return p2;
}

sexpr *draw_spot_postscript(sexpr *p, sexpr *sp, FILE *file) {

	sexpr *width = cadr(graphic$sexpr(sp));
	sexpr *color = car(cddr(graphic$sexpr(sp)));

	sexpr *r, *g, *b;

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("graphic->postscript: Illegal color argument.\n");
		longjmp(esc, 1);
	}

	if (!number(width)) {
		printf("graphic->postscript: Illegal width argument.\n");
		longjmp(esc, 1);
	}

	fprintf(file, "newpath\n");
	if (r->u.x + g->u.x + b->u.x > 764)
		fprintf(file, "%f %f %f setrgbcolor\n", 0.0, 0.0, 0.0);
	else
		fprintf(file, "%f %f %f setrgbcolor\n", r->u.x / 128, g->u.x / 128,
				b->u.x / 128);

	fprintf(file, "%f %f %f 0.0 360.0 arc\n", (1.0 - plumber$y(p)) / 2.0, plumber$x(p) / 2.0, width->u.x / 432.0);
	fprintf(file, "fill\n");

	return p;
}

sexpr *draw_adjoin_postscript(sexpr *p, sexpr *sp, FILE *file) {
	sexpr *g = graphic$sexpr(sp);
	if (null(g))
		return p;
	return draw_graphic_postscript(draw_graphic_postscript(p, gcar(sp), file),
			gcdr(sp), file);
}

sexpr *draw_adorn_postscript(sexpr *p, sexpr *sp, FILE *file) {
	sexpr *g = graphic$sexpr(sp);
	if (null(g))
		return p;
	draw_graphic_postscript(p, gcar(sp), file);
	draw_graphic_postscript(p, gcdr(sp), file);
	return p;
}

/*

 sexpr *draw_graphic(sexpr *p, sexpr *sp) {
 sexpr  *first, *g = graphic$sexpr(sp);

 if (null(g)) return p;

 first = car(g);
 if (eq(first,symbol_straight))
 return draw_straight(p,sp);
 else if (eq(first,symbol_spot))
 return draw_spot(p,sp);
 else if (eq(first,symbol_transparent))
 return draw_transparent(p,sp);
 else if (eq(first,symbol_adjoin))
 return draw_adjoin(p,sp);
 else if (eq(first,symbol_adorn)) {
 draw_adorn(p,sp);
 return p;
 } else if (eq(first,symbol_bend))
 return draw_bend(p,sp);
 else if (eq(first,symbol_text))
 return draw_text(p,sp);
 else {
 printf("Unrecognized graphic type.\n");
 return p;
 }
 }

 sexpr *draw_text(sexpr *p, sexpr *sp) {
 char *c;
 sexpr *text = cadr(graphic$sexpr(sp));
 sexpr *color = cadr(cdr(graphic$sexpr(sp)));
 sexpr *size = cadr(cddr(graphic$sexpr(sp)));

 sexpr *r, *g, *b;

 double s;

 if (!string(text)) {
 printf("draw-text: Illegal text argument.\n");
 longjmp(esc,1);
 }

 r = car(color);
 g = cadr(color);
 b = car(cddr(color));

 if (!number(r) || !number(g) || !number(b)) {
 printf("draw-text: Illegal color argument.\n");
 longjmp(esc,1);
 }

 if (!number(size)) {
 printf("draw-text: Illegal size argument.\n");
 longjmp(esc,1);
 }

 if (size->u.x < 2.0) {
 glColor3f(r->u.x/128,g->u.x/128,b->u.x/128);
 glRasterPos2f(plumber$x(p), plumber$y(p));
 for (c = text->u.text; *c; c++) glutBitmapCharacter(GLUT_BITMAP_8_BY_13, *c);
 } else {
 s = size->u.x*0.0003;
 glPushMatrix();
 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 glEnable(GL_BLEND);
 glEnable(GL_LINE_SMOOTH);
 glLineWidth(1.0);
 glTranslatef(plumber$x(p), plumber$y(p), 0.0);
 glScalef(s, s, s);
 glColor3f(r->u.x/128,g->u.x/128,b->u.x/128);
 for (c = text->u.text; *c; c++) glutStrokeCharacter(GLUT_STROKE_ROMAN, *c);
 glPopMatrix();
 }
 return p;
 }

 sexpr *draw_plumber(sexpr *p, sexpr *color) {

 sexpr *r, *g, *b;

 double x, y, c, s;

 c = cos(plumber$heading(p)-PI/2.0);
 s = sin(plumber$heading(p)-PI/2.0);
 x = plumber$x(p);
 y = plumber$y(p);

 r = car(color);
 g = cadr(color);
 b = car(cddr(color));

 if (!number(r) || !number(g) || !number(b)) {
 printf("draw-plumber: Illegal color argument.\n");
 longjmp(esc,1);
 }

 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 glEnable(GL_BLEND);
 glEnable(GL_LINE_SMOOTH);
 glLineWidth(1.0);
 glBegin(GL_LINE_LOOP);
 glColor3f(r->u.x/128,g->u.x/128,b->u.x/128);
 glVertex2f(x-(C30*c - S30*s)/60.0,y+(-C30*s - S30*c)/60.0);
 glVertex2f(x-s/60.0,y+c/60.0);
 glVertex2f(x-(-C30*c - S30*s)/60.0,y+(C30*s - S30*c)/60.0);
 glEnd();

 return p;
 }

 sexpr *draw_straight(sexpr *p1, sexpr *sp) {

 sexpr *len = cadr(graphic$sexpr(sp));
 sexpr *color = car(cddr(graphic$sexpr(sp)));
 sexpr *width = cadr(cddr(graphic$sexpr(sp)));

 sexpr *p2;
 sexpr *r, *g, *b;
 double x, y, heading;

 if (!number(len)) {
 printf("draw-straight: Illegal length argument.\n");
 longjmp(esc,1);
 }

 heading = plumber$heading(p1);
 x = plumber$x(p1) + (len->u.x/300.0)*cos(heading);
 y = plumber$y(p1) + (len->u.x/300.0)*sin(heading);
 p2 = make_plumber(x, y, heading);

 r = car(color);
 g = cadr(color);
 b = car(cddr(color));

 if (!number(r) || !number(g) || !number(b)) {
 printf("draw-straight: Illegal color argument.\n");
 longjmp(esc,1);
 }

 if (!number(width)) {
 printf("draw-straight: Illegal width argument.\n");
 longjmp(esc,1);
 }

 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 glEnable(GL_BLEND);
 glEnable(GL_LINE_SMOOTH);
 glLineWidth(width->u.x);
 glBegin(GL_LINES);
 glColor3f(r->u.x/128,g->u.x/128,b->u.x/128);
 glVertex2f(plumber$x(p1),plumber$y(p1));
 glVertex2f(plumber$x(p2),plumber$y(p2));
 glEnd();
 return p2;
 }

 sexpr *draw_spot(sexpr *p, sexpr *sp) {

 sexpr *width = cadr(graphic$sexpr(sp));
 sexpr *color = car(cddr(graphic$sexpr(sp)));

 sexpr *r, *g, *b;

 if (!number(width)) {
 printf("draw-spot: Illegal width argument.\n");
 longjmp(esc,1);
 }

 r = car(color);
 g = cadr(color);
 b = car(cddr(color));

 if (!number(r) || !number(g) || !number(b)) {
 printf("draw-spot: Illegal color argument.\n");
 longjmp(esc,1);
 }

 glPointSize(width->u.x);
 glBegin(GL_POINTS);
 glColor3f(r->u.x/128,g->u.x/128,b->u.x/128);
 glVertex2f(plumber$x(p),plumber$y(p));
 glEnd();
 return p;
 }

 sexpr *draw_adjoin(sexpr *p, sexpr *sp) {
 sexpr *g = graphic$sexpr(sp);
 if (null(g)) return p;
 return draw_graphic(draw_graphic(p,gcar(sp)),gcdr(sp));
 }

 sexpr *draw_adorn(sexpr *p, sexpr *sp) {
 sexpr *g = graphic$sexpr(sp);
 if (null(g)) return p;
 draw_graphic(p,gcar(sp));
 draw_graphic(p,gcdr(sp));
 return p;
 }
 */

sexpr *scheme_read(sexpr *port) {

	char c;
	sexpr *input;

	if (!input_port(port)) {
		print_value_escape("read: Argument is not an input-port: ", port);
	}

	if ((c = getc(port->u.file)) != EOF) {
		ungetc(c, port->u.file);
		input = parse(port->u.file, ' ');
		if (pair(input))
			return car(input);
	}

	return undefined;
}

sexpr *scheme_write(sexpr *sp, sexpr *port) {

	if (!output_port(port)) {
		print_value_escape("write: Argument is not an output-port: ", port);
	}

	display_or_print(sp, PRINT, TOPLEVEL, port->u.file);

	return undefined;

}

int eq(sexpr *s1, sexpr *s2) {
	if (s1->type == s2->type) {
		switch (s1->type) {
		case NIL:
			return 1;
		case NUMBER:
			return (s1->u.x == s2->u.x);
		case TOKEN:
		case CHARACTER:
			return (s1->u.c == s2->u.c);
		case STRING:
			return !strcmp(s1->u.text, s2->u.text);
		case BOOLEAN:
			return (s1->u.i == s2->u.i);
		case VIRGIN:
		case CLOSURE:
		case PRIMITIVE:
		case VECTOR:
		case INPUT_PORT:
		case OUTPUT_PORT:
		case PAIR:
		case SYMBOL:
		case GRAPHIC:
		case IMAGE:
		case LINE:
		case SKETCH:
			return (s1 == s2);
		case COMPLEX:
			return (s1->u.z.r == s2->u.z.r && s1->u.z.i == s2->u.z.i);
		default:
			printf("eq: Illegal type: %d\n", s1->type);
			return 0;
		}
	} else
		return 0;
}
sexpr *image_map(double(*func)(double), sexpr *sp1) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	sexpr *sp2 = make_image(rows, cols);
	float *data2 = image$data(sp2);
	for (i = 0; i < n; i++)
		data2[i] = (float) func((double) data1[i]);
	return sp2;
}

sexpr *complex_image_map(fcomplex(*func)(fcomplex), sexpr *sp1) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	fcomplex *cdata1 = complex_image$data(sp1);
	sexpr *sp2 = make_complex_image(rows, cols);
	fcomplex *cdata2 = complex_image$data(sp2);
	for (i = 0; i < n; i++)
		cdata2[i] = func(cdata1[i]);
	return sp2;
}

sexpr *complex_image_map_real_result(float(*func)(fcomplex), sexpr *sp1) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	fcomplex *cdata1 = complex_image$data(sp1);
	sexpr *sp2 = make_image(rows, cols);
	float *data2 = image$data(sp2);
	for (i = 0; i < n; i++)
		data2[i] = func(cdata1[i]);
	return sp2;
}

sexpr *sine(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(sin(s->u.x));
	case COMPLEX:
		return complex2exp(Csin(s->u.z));
	case IMAGE:
		return image_map(sin, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Csin, s);
	default:
		printf("sin: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *cosine(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(cos(s->u.x));
	case COMPLEX:
		return complex2exp(Ccos(s->u.z));
	case IMAGE:
		return image_map(cos, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Ccos, s);
	default:
		printf("cos: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *tangent(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(tan(s->u.x));
	case IMAGE:
		return image_map(tan, s);
	case COMPLEX:
	case COMPLEX_IMAGE:
		return divide(sine(s), cosine(s));
	default:
		printf("tan: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *arcsine(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(asin(s->u.x));
	case COMPLEX:
		return complex2exp(Casin(s->u.z));
	case IMAGE:
		return image_map(asin, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Casin, s);
	default:
		printf("asin: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *arccosine(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(acos(s->u.x));
	case COMPLEX:
		return complex2exp(Cacos(s->u.z));
	case IMAGE:
		return image_map(acos, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Cacos, s);
	default:
		printf("acos: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *logarithm(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(log(s->u.x));
	case COMPLEX:
		return complex2exp(Clog(s->u.z));
	case IMAGE:
		return image_map(log, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Clog, s);
	default:
		printf("log: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *exponential(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(exp(s->u.x));
	case COMPLEX:
		return complex2exp(Cexp(s->u.z));
	case IMAGE:
		return image_map(exp, s);
	case COMPLEX_IMAGE:
		return complex_image_map(Cexp, s);
	default:
		printf("exp: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *image_map_complex_result(fcomplex(*func)(fcomplex), sexpr *sp1) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	sexpr *sp2 = make_complex_image(rows, cols);
	fcomplex *cdata2 = complex_image$data(sp2);
	for (i = 0; i < n; i++)
		cdata2[i] = func(Complex(data1[i], 0.0));
	return sp2;
}

sexpr *square_root(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		if (s->u.x >= 0) {
			return num2exp(sqrt(s->u.x));
		} else {
			return complex2exp(Complex(0.0, sqrt(fabs(s->u.x))));
		}
	case COMPLEX:
		return complex2exp(Csqrt(s->u.z));
	case IMAGE:
		if (image_min(s)->u.x >= 0) {
			return image_map(sqrt, s);
		} else {
			return image_map_complex_result(Csqrt, s);
		}
	case COMPLEX_IMAGE:
		return complex_image_map(Csqrt, s);
	default:
		printf("sqrt: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *scheme_abs(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(fabs(s->u.x));
	case COMPLEX:
		return num2exp(Cabs(s->u.z));
	case IMAGE:
		return image_map(fabs, s);
	case COMPLEX_IMAGE:
		return complex_image_map_real_result(Cabs, s);
	default:
		printf("abs: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *scheme_floor(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(floor(s->u.x));
	case IMAGE:
		return image_map(floor, s);
	default:
		printf("floor: Argument is not a real number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *scheme_ceil(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(ceil(s->u.x));
	case IMAGE:
		return image_map(ceil, s);
	default:
		printf("ceil: Argument is not a real number or image.\n");
		longjmp(esc, 1);
	}
}

double random_func(double x) {
	return floor(x * random() / RAND_MAX);
}

sexpr *scheme_random(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp((int) random_func(s->u.x));
	case IMAGE:
		return image_map(random_func, s);
	default:
		printf("random: Argument is not a real number or image.\n");
		longjmp(esc, 1);
	}
}

double angle_func(double x) {
	return (x < 0) ? PI : 0.0;
}

float complex_angle_func(fcomplex z) {
	return atan2(z.i, z.r);
}

sexpr *real_part(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return s;
	case COMPLEX:
		return num2exp(s->u.z.r);
	case IMAGE:
		return s;
	case COMPLEX_IMAGE:
		return complex_image_real(s);
	default:
		printf("real-part: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

double zero_func(double x) {
	return 0.0;
}

sexpr *imag_part(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return number_zero;
	case COMPLEX:
		return num2exp(s->u.z.i);
	case IMAGE:
		return image_map(zero_func, s);
	case COMPLEX_IMAGE:
		return complex_image_imag(s);
	default:
		printf("imag-part: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *angle(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return num2exp(angle_func(s->u.x));
	case COMPLEX:
		return num2exp(complex_angle_func(s->u.z));
	case IMAGE:
		return image_map(angle_func, s);
	case COMPLEX_IMAGE:
		return complex_image_map_real_result(complex_angle_func, s);
	default:
		printf("angle: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *conjugate(sexpr *s) {
	switch (s->type) {
	case NUMBER:
		return s;
	case COMPLEX:
		return complex2exp(Conjg(s->u.z));
	case IMAGE:
		return s;
	case COMPLEX_IMAGE:
		return complex_image_map(Conjg, s);
	default:
		printf("conjugate: Argument is not a number or image.\n");
		longjmp(esc, 1);
	}
}

sexpr *expt(sexpr *s1, sexpr *s2) {
	switch (s1->type) {
	case NUMBER:
		switch (s2->type) {
		case NUMBER:
			return num2exp(pow(s1->u.x, s2->u.x));
		default:
			return exponential(times(s2, logarithm(s1)));
		}
	default:
		return exponential(times(s2, logarithm(s1)));
	}
}

sexpr *eq_pred(sexpr *s1, sexpr *s2) {
	return torf(eq(s1,s2));
}

sexpr *real_equals_image(sexpr *sp1, sexpr *sp2) {
	int rows = image$rows(sp2);
	int cols = image$cols(sp2);
	int i, n = rows * cols;
	float x = (float) sp1->u.x;
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_image(rows, cols);
	float *data3 = image$data(sp3);
	for (i = 0; i < n; i++)
		data3[i] = (data2[i] == x) ? 1 : 0;
	return sp3;
}

sexpr *image_equals_image(sexpr *sp1, sexpr *sp2) {
	int rows = image$rows(sp2);
	int cols = image$cols(sp2);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_image(rows, cols);
	float *data3 = image$data(sp3);
	check_image_sizes("=", rows, cols, sp2);
	for (i = 0; i < n; i++)
		data3[i] = (data1[i] == data2[i]) ? 1 : 0;
	return sp3;
}

sexpr *zero_imag_part(sexpr *sp) {
	return equals(imag_part(sp), number_zero);
}

sexpr *equals(sexpr *sp1, sexpr *sp2) {
	switch (sp1->type) {
	case NUMBER:
		switch (sp2->type) {
		case NUMBER:
			return torf(sp1->u.x == sp2->u.x)
			;
		case COMPLEX:
			return torf((sp1->u.x == sp2->u.z.r) && (sp2->u.z.i == 0.0))
			;
		case IMAGE:
			return real_equals_image(sp1, sp2);
		case COMPLEX_IMAGE:
			return times(real_equals_image(sp1, real_part(sp2)),
					zero_imag_part(sp2));
		default:
			printf("=: Incompatible arguments.\n");
			longjmp(esc, 1);
		}
	case COMPLEX:
		switch (sp2->type) {
		case NUMBER:
			return torf((sp1->u.z.r == sp2->u.x) && (sp1->u.z.i == 0.0))
			;
		case COMPLEX:
			return torf((sp1->u.z.r == sp2->u.z.r) && (sp1->u.z.i == sp2->u.z.i))
			;
		case IMAGE:
			if (sp1->u.z.i == 0.0) {
				return equals(real_part(sp1), sp2);
			} else {
				return times(sp2, number_zero);
			}
		case COMPLEX_IMAGE:
			return times(equals(real_part(sp1), real_part(sp2)), equals(
					imag_part(sp1), imag_part(sp2)));
		default:
			printf("=: Incompatible arguments.\n");
			longjmp(esc, 1);
		}
	case IMAGE:
		switch (sp2->type) {
		case NUMBER:
		case COMPLEX:
			return equals(sp2, sp1);
		case IMAGE:
			return image_equals_image(sp1, sp2);
		case COMPLEX_IMAGE:
			return times(image_equals_image(sp1, real_part(sp2)),
					zero_imag_part(sp2));
		default:
			printf("=: Incompatible arguments.\n");
			longjmp(esc, 1);
		}
	case COMPLEX_IMAGE:
		switch (sp2->type) {
		case NUMBER:
		case COMPLEX:
		case IMAGE:
			return equals(sp2, sp1);
		case COMPLEX_IMAGE:
			return times(equals(real_part(sp1), real_part(sp2)), equals(
					imag_part(sp1), imag_part(sp2)));
		default:
			printf("=: Incompatible arguments.\n");
			longjmp(esc, 1);
		}
	default:
		print_value_escape("=: Argument is not a number or image: ", sp1);
	}
	return NULL;
}

sexpr *real_image_helper(double( func)(double, double), sexpr *sp1, sexpr *sp2) {
	int rows = image$rows(sp2);
	int cols = image$rows(sp2);
	int i, n = rows * cols;
	double x = sp1->u.x;
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_image(rows, cols);
	float *data3 = image$data(sp3);
	for (i = 0; i < n; i++)
		data3[i] = (float) func(x, (double) data2[i]);
	return sp3;
}

sexpr *image_real_helper(double( func)(double, double), sexpr *sp1, sexpr *sp2) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	double x = sp2->u.x;
	sexpr *sp3 = make_image(rows, cols);
	float *data3 = image$data(sp3);
	for (i = 0; i < n; i++)
		data3[i] = (float) func((double) data1[i], x);
	return sp3;
}

sexpr *image_image_helper(double( func)(double, double), sexpr *sp1, sexpr *sp2) {
	int rows = image$rows(sp1);
	int cols = image$cols(sp1);
	int i, n = rows * cols;
	float *data1 = image$data(sp1);
	float *data2 = image$data(sp2);
	sexpr *sp3 = make_image(rows, cols);
	float *data3 = image$data(sp3);
	check_image_sizes("real-helper", rows, cols, sp2);
	for (i = 0; i < n; i++)
		data3[i] = (float) func((double) data1[i], (double) data2[i]);
	return sp3;
}

sexpr *real_helper(double( func)(double, double), sexpr *sp1, sexpr *sp2) {
	switch (sp1->type) {
	case NUMBER:
		switch (sp2->type) {
		case NUMBER:
			return torf(func(sp1->u.x,sp2->u.x))
			;
		case IMAGE:
			return real_image_helper(func, sp1, sp2);
		default:
			printf("real-helper: Incompatible argument types.\n");
			longjmp(esc, 1);
		}
	case IMAGE:
		switch (sp2->type) {
		case NUMBER:
			return image_real_helper(func, sp1, sp2);
		case IMAGE:
			return image_image_helper(func, sp1, sp2);
		default:
			printf("real-helper: Incompatible argument types.\n");
			longjmp(esc, 1);
		}
	default:
		printf("real-helper: Argument is not a real number or image.\n");
		longjmp(esc, 1);
	}
}

double lt_func(double x, double y) {
	return (x < y) ? 1 : 0;
}

double gt_func(double x, double y) {
	return (x > y) ? 1 : 0;
}

double leq_func(double x, double y) {
	return (x <= y) ? 1 : 0;
}

double geq_func(double x, double y) {
	return (x >= y) ? 1 : 0;
}

double min_func(double x, double y) {
	return (x <= y) ? x : y;
}

double max_func(double x, double y) {
	return (x >= y) ? x : y;
}

sexpr *lt(sexpr *sp1, sexpr *sp2) {
	return real_helper(lt_func, sp1, sp2);
}

sexpr *gt(sexpr *sp1, sexpr *sp2) {
	return real_helper(gt_func, sp1, sp2);
}

sexpr *leq(sexpr *sp1, sexpr *sp2) {
	return real_helper(leq_func, sp1, sp2);
}

sexpr *geq(sexpr *sp1, sexpr *sp2) {
	return real_helper(geq_func, sp1, sp2);
}

sexpr *scheme_min(sexpr *sp1, sexpr *sp2) {
	if (!number(sp1) || !number(sp2))
		return real_helper(min_func, sp1, sp2);
	return num2exp((sp1->u.x >= sp2->u.x) ? sp2->u.x : sp1->u.x);
}

sexpr *scheme_max(sexpr *sp1, sexpr *sp2) {
	if (!number(sp1) || !number(sp2))
		return real_helper(max_func, sp1, sp2);
	return num2exp((sp1->u.x >= sp2->u.x) ? sp1->u.x : sp2->u.x);
}

sexpr *positive_pred(sexpr *sp) {
	return gt(sp, number_zero);
}

sexpr *negative_pred(sexpr *sp) {
	return lt(sp, number_zero);
}

sexpr *zero_pred(sexpr *sp) {
	return equals(sp, number_zero);
}

/*
 http://www.astro.princeton.edu/~esirko/idl_html_help/A17.html
 atan(Zy, Zx) = -i alog((Zx + iZy)/sqrt(Zx^2 + Zy^2))
 */

fcomplex complex_complex_atan2(fcomplex z1, fcomplex z2) {
	return Cmul(minus_root_minus_one, Cexp(Cdiv(Cadd(z2, Cmul(root_minus_one,
			z1)), Csqrt(Cadd(Cmul(z1, z1), Cmul(z2, z2))))));
}

fcomplex complex_real_atan2(fcomplex z, float x) {
	double u = z.r / x;
	double v = z.i / x;
	return Cdiv(Csub(Clog(Complex(1 - v, u)), Clog(Complex(1 + v, -u))),
			two_root_minus_one);
}

fcomplex real_complex_atan2(float x, fcomplex z) {
	double m = z.r * z.r + z.i * z.i;
	double u = x * z.r / m;
	double v = x * z.i / m;
	return Cdiv(Csub(Clog(Complex(1 + v, u)), Clog(Complex(1 - v, -u))),
			two_root_minus_one);
}

sexpr *not_likely_to_be_called(sexpr *sp1, sexpr *sp2) {
	int rows = complex_image$rows(sp1);
	int cols = complex_image$cols(sp1);
	int i, n = rows * cols;
	fcomplex *cdata1 = complex_image$data(sp1);
	fcomplex *cdata2 = complex_image$data(sp2);
	sexpr *sp3 = make_complex_image(rows, cols);
	fcomplex *cdata3 = complex_image$data(sp3);
	check_image_sizes("atan", rows, cols, sp2);
	for (i = 0; i < n; i++)
		cdata3[i] = complex_complex_atan2(cdata1[i], cdata2[i]);
	return sp3;
}

sexpr *scalar2image(sexpr *sp1, sexpr *sp2) {
	sexpr *z = complex2exp(zero);
	return plus(times(sp2, z), plus(sp1, z));
}

sexpr *real2complex(sexpr *sp) {
	return make_complex(sp, number_zero);
}

sexpr *arctangent(sexpr *sp1, sexpr *sp2) {
	switch (sp1->type) {
	case NUMBER:
		switch (sp2->type) {
		case NUMBER:
			return num2exp(atan2(sp1->u.x, sp2->u.x));
		case COMPLEX:
			if (Cmag(sp2->u.z) == 0.0) {
				printf("atan: Second argument cannot have zero magnitude.\n");
				longjmp(esc, 1);
			}
			return complex2exp(real_complex_atan2(sp1->u.x, sp2->u.z));
		case IMAGE:
			return real_image_helper(atan2, sp1, sp2);
		case COMPLEX_IMAGE:
			return not_likely_to_be_called(scalar2image(sp1, sp2), sp2);
		default:
			printf("atan: Illegal or unsupported argument.\n");
			longjmp(esc, 1);
		}
	case COMPLEX:
		switch (sp2->type) {
		case NUMBER:
			if (sp2->u.x == 0.0) {
				printf("atan: Second argument cannot be zero.\n");
				longjmp(esc, 1);
			}
			return complex2exp(complex_real_atan2(sp1->u.z, sp2->u.x));
		case COMPLEX:
			return complex2exp(complex_complex_atan2(sp1->u.z, sp2->u.z));
		case IMAGE:
			return not_likely_to_be_called(scalar2image(sp1, sp2),
					real2complex(sp2));
		case COMPLEX_IMAGE:
			return not_likely_to_be_called(scalar2image(sp1, sp2), sp2);
		default:
			printf("atan: Illegal or unsupported argument type.\n");
			longjmp(esc, 1);
		}
	case IMAGE:
		switch (sp2->type) {
		case NUMBER:
			return image_real_helper(atan2, sp1, sp2);
		case COMPLEX:
			return not_likely_to_be_called(real2complex(sp1), scalar2image(sp2,
					sp1));
		case IMAGE:
			return image_image_helper(atan2, sp1, sp2);
		case COMPLEX_IMAGE:
			return not_likely_to_be_called(real2complex(sp1), sp2);
		default:
			printf("atan: Illegal or unsupported argument type.\n");
			longjmp(esc, 1);
		}
	case COMPLEX_IMAGE:
		switch (sp2->type) {
		case NUMBER:
			return not_likely_to_be_called(sp1, scalar2image(sp2, sp1));
		case COMPLEX:
			return not_likely_to_be_called(sp1, scalar2image(sp2, sp1));
		case IMAGE:
			return not_likely_to_be_called(sp1, real2complex(sp2));
		case COMPLEX_IMAGE:
			return not_likely_to_be_called(sp1, sp2);
		default:
			printf("atan: Illegal or unsupported argument type.\n");
			longjmp(esc, 1);
		}
	default:
		printf("atan: Illegal or unsupported argument type.\n");
		longjmp(esc, 1);
	}
}

sexpr *gilgalad() {
	printf("Gil-galad was an Elven king.\n");
	printf("Of him the harpers sadly sing:\n");
	printf("the last whose realm was fair and free\n");
	printf("between the Mountains and the Sea.\n\n");
	printf("His sword was long, his lance was keen,\n");
	printf("his shining helm afar was seen;\n");
	printf("the countless stars of heaven's field\n");
	printf("were mirrored in his silver shield.\n\n");
	printf("But long ago he rode away,\n");
	printf("and where he dwelleth none can say;\n");
	printf("for into darkness fell his star\n");
	printf("in Mordor where the shadows are.\n");
	return undefined;
}

sexpr *char2token(char c) {
	sexpr *sp = malloc(sizeof(sexpr));
	sp->type = TOKEN;
	sp->u.c = c;
	return sp;
}

sexpr *char2exp(char c) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = CHARACTER;
	sp->u.c = c;
	return sp;
}

sexpr *num2exp(double x) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = NUMBER;
	sp->u.x = x;
	return sp;
}

sexpr *complex2exp(fcomplex z) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = COMPLEX;
	sp->u.z = z;
	return sp;
}

sexpr *num2str(sexpr *sp) {
	char *c;
	if (!number(sp)) {
		print_value_escape("number->string: Argument is not a real number: ",
				sp);
	}
	c = (char *) malloc(STRLEN);
	sprintf(c, "%g", sp->u.x);
	return str2exp(c);
}

sexpr *str2num(sexpr *sp) {
	float x;
	if (!string(sp)) {
		print_value_escape("string->number: Argument is not a string: ", sp);
	}
	sscanf(sp->u.text, "%g", &x);
	return num2exp(x);
}

sexpr *symbol2exp(sexpr *sp) {
	if (!symbol(sp)) {
		print_value_escape("symbol->string: Argument is not a symbol: ", sp);
	}
	return str2exp(symbol$name(sp));
}

sexpr *str2exp(char *text) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(sexpr);
	sp->type = STRING;
	sp->u.text = malloc((strlen(text) + 1) * sizeof(char));
	strcpy(sp->u.text, text);
	return sp;
}

sexpr *exp2symbol(sexpr *sp) {

	int m, n = strlen(sp->u.text);

	if (!string(sp)) {
		print_value_escape("string->symbol: Argument not a string: ", sp);
	}

	m = 0;
	if (islegal1st(sp->u.text[0]))
		while (islegal2nd(sp->u.text[m++]))
			;

	if (m < n) {
		char *name = malloc(STRLEN * sizeof(char));
		sprintf(name, "|%s|", sp->u.text);
		return str2symbol(name);
	}

	return str2symbol(sp->u.text);
}

int length(sexpr *ls) {
	int x = 0;

	while (pair(ls)) {
		x++;
		ls = pair$cdr(ls);
	}

	if (null(ls))
		return x;
	if (symbol(ls))
		return -(x + 1);

	return -1000;
}

sexpr *make_extensible_vector(int len, int allocated) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(vector);
	sp->type = VECTOR;
	vector$length(sp) = len;
	vector$allocated(sp) = allocated;
	vector$data(sp) = (sexpr **) smalloc;
	smalloc += allocated * sizeof(sexpr *);

	return sp;
}

sexpr *make_vector(int len) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(vector);
	sp->type = VECTOR;
	vector$length(sp) = len;
	vector$allocated(sp) = len;
	vector$data(sp) = (sexpr **) smalloc;
	smalloc += len * sizeof(sexpr *);
	return sp;
}

sexpr *scheme_make_vector(sexpr *size, sexpr *init) {
	int i, len;
	sexpr *sp;

	if (!number(size)) {
		printf("make-vector: Illegal argument.\n");
		longjmp(esc, 1);
	}

	len = (int) size->u.x;

	if (len < 0) {
		printf("make-vector: Length must be positive: %d.", len);
		longjmp(esc, 1);
	}

	sp = (sexpr *) smalloc;
	smalloc += sizeof(vector);
	sp->type = VECTOR;
	vector$length(sp) = len;
	vector$allocated(sp) = len;
	vector$data(sp) = (sexpr **) smalloc;
	smalloc += len * sizeof(sexpr *);
	for (i = 0; i < len; i++)
		vector$data(sp)[i] = init;
	return sp;
}

sexpr *vector_set(sexpr *sp, sexpr *i, sexpr *value) {

	if (!vector(sp) || !number(i)) {
		printf("vector-set!: Illegal argument.\n");
		longjmp(esc, 1);
	}

	if (i->u.x < 0 || i->u.x >= vector$length(sp)) {
		printf("vector-set!: Index out of range: %g\n", i->u.x);
		longjmp(esc, 1);
	}

	vector$data(sp)[(int) floor((float) i->u.x)] = value;
	return undefined;
}

sexpr *vector_ref(sexpr *sp, sexpr *i) {
	if (!vector(sp) || !number(i)) {
		printf("vector-ref: Illegal argument.\n");
		longjmp(esc, 1);
	}

	if (i->u.x < 0 || i->u.x >= vector$length(sp)) {
		printf("vector-ref: Index out of range: %g\n", i->u.x);
		longjmp(esc, 1);
	}
	return vector$data(sp)[(int) floor((float) i->u.x)];
}

sexpr *scheme_vector$length(sexpr *sp) {
	if (!vector(sp)) {
		print_value_escape("vector-length: Argument is not a vector: ", sp);
	}
	return num2exp(vector$length(sp));
}

sexpr *string_eq(sexpr *sp1, sexpr *sp2) {
	if (!string(sp1)) {
		print_value_escape("string=?: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string=?: Argument is not a string: ", sp2);
	}
	return torf(!strcmp(sp1->u.text,sp2->u.text));
}

sexpr *string_lt(sexpr *sp1, sexpr *sp2) {
	if (!string(sp1)) {
		print_value_escape("string<?: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string<?: Argument is not a string: ", sp2);
	}
	return torf(strcmp(sp1->u.text,sp2->u.text) < 0);
}

sexpr *string_gt(sexpr *sp1, sexpr *sp2) {
	if (!string(sp1)) {
		print_value_escape("string>?: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string>?: Argument is not a string: ", sp2);
	}
	return torf(strcmp(sp1->u.text,sp2->u.text)> 0);
}

sexpr *string_leq(sexpr *sp1, sexpr *sp2) {
	if (!string(sp1)) {
		print_value_escape("string<=?: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string<=?: Argument is not a string: ", sp2);
	}
	return torf(strcmp(sp1->u.text,sp2->u.text) <= 0);
}

sexpr *string_geq(sexpr *sp1, sexpr *sp2) {
	if (!string(sp1)) {
		print_value_escape("string>=?: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string>=?: Argument is not a string: ", sp2);
	}
	return torf(strcmp(sp1->u.text,sp2->u.text) >= 0);
}

sexpr *char_eq(sexpr *sp1, sexpr *sp2) {
	if (!character(sp1)) {
		print_value_escape("char=?: Argument is not a character: ", sp1);
	}

	if (!character(sp2)) {
		print_value_escape("char=?: Argument is not a character: ", sp2);
	}
	return torf(sp1->u.c == sp2->u.c);
}

sexpr *char_lt(sexpr *sp1, sexpr *sp2) {
	if (!character(sp1)) {
		print_value_escape("char<?: Argument is not a character: ", sp1);
	}

	if (!character(sp2)) {
		print_value_escape("char<?: Argument is not a character: ", sp2);
	}
	return torf(sp1->u.c < sp2->u.c);
}

sexpr *char_gt(sexpr *sp1, sexpr *sp2) {
	if (!character(sp1)) {
		print_value_escape("char>?: Argument is not a character: ", sp1);
	}

	if (!character(sp2)) {
		print_value_escape("char>?: Argument is not a character: ", sp2);
	}
	return torf(sp1->u.c> sp2->u.c);
}

sexpr *char_leq(sexpr *sp1, sexpr *sp2) {
	if (!character(sp1)) {
		print_value_escape("char<=?: Argument is not a character: ", sp1);
	}

	if (!character(sp2)) {
		print_value_escape("char<=?: Argument is not a character: ", sp2);
	}
	return torf(sp1->u.c <= sp2->u.c);
}

sexpr *char_geq(sexpr *sp1, sexpr *sp2) {
	if (!character(sp1)) {
		print_value_escape("char>=?: Argument is not a character: ", sp1);
	}
	if (!character(sp2)) {
		print_value_escape("char>=?: Argument is not a character: ", sp2);
	}
	return torf(sp1->u.c >= sp2->u.c);
}

sexpr *string_append(sexpr *sp1, sexpr *sp2) {
	char *c;

	if (!string(sp1)) {
		print_value_escape("string-append: Argument is not a string: ", sp1);
	}

	if (!string(sp2)) {
		print_value_escape("string-append: Argument is not a string: ", sp2);
	}
	c = (char *) malloc(strlen(sp1->u.text) + strlen(sp2->u.text) + 1);
	strcpy(c, sp1->u.text);
	return str2exp(strcat(c, sp2->u.text));
}

sexpr *string_ref(sexpr *text, sexpr *i) {

	if (!string(text)) {
		print_value_escape("string-ref: Argument is not a string: ", text);
	}

	if (!number(i)) {
		print_value_escape("string-ref: Argument is not a number: ", i);
	}

	if ((i->u.x < 0) || (i->u.x > strlen(text->u.text))) {
		printf("string-ref: Index out of range: %g", i->u.x);
	}
	return char2exp(text->u.text[(int) floor((float) i->u.x)]);
}

sexpr *substring(sexpr *text1, sexpr *i, sexpr *j) {

	int len, start, end, pos;
	char *text2;

	if (!string(text1)) {
		print_value_escape("substring: Argument is not a string: ", text1);
	}
	if (!number(i) || !number(j)) {
		printf("substring: Index is not a number.\n");
		longjmp(esc, 1);
	}

	len = strlen(text1->u.text);
	start = (int) floor((float) i->u.x);
	end = (int) floor((float) j->u.x);
	if ((start < 0) || (start > end) || (start > len)) {
		printf("substring: Start index out of range: %d", start);
		longjmp(esc, 1);
	}

	if ((end < 0) || (end > len)) {
		printf("substring: End index out of range: %d", end);
		longjmp(esc, 1);
	}

	text2 = (char *) malloc(sizeof(char) * (end - start) + 1);
	for (pos = 0; pos < (end - start); pos++)
		text2[pos] = text1->u.text[start + pos];
	text2[end - start] = '\0';
	return str2exp(text2);
}

sexpr *string_length(sexpr *text) {
	if (!string(text)) {
		print_value_escape("string-length: Argument is not a string: ", text);
	}
	return num2exp(strlen(text->u.text));
}

sexpr *char2str(sexpr *sp) {
	char *text;

	if (!character(sp)) {
		print_value_escape("character->string: Argument is not a character: ",
				sp);
	}

	text = (char *) malloc(2);
	text[0] = sp->u.c;
	text[1] = '\0';
	return str2exp(text);
}

sexpr *list2(sexpr *s1, sexpr *s2) {
	return cons(s1, cons(s2, nil));
}

sexpr *ploop0(sexpr *ls, FILE *file, int stop) {
	sexpr *foo;
	if (null(ls))
		return ls;
	foo = ploop1(ls, nil, 0, file, stop);
	return cons(car(foo), ploop0(cdr(foo), file, stop));
}

sexpr *ploop1(sexpr *ls, sexpr *acc, int d, FILE *file, int stop) {
	sexpr *foo, *bar;
	char c;

	while (d > 0 || null(acc)) {

		if (null(ls))
			ls = lex(file, stop);

		bar = pair$car(ls);

		if (eq(bar, token_dot) || symbol(bar) || number(bar) || character(bar) || string(bar) || boolean(bar) || komplex(bar)) {
			ls = pair$cdr(ls);
			acc = cons(bar, acc);
			continue;
		}

		c = bar->u.c;

		switch (c) {
		case '(':
			ls = pair$cdr(ls);
			acc = cons(bar, acc);
			d++;
			continue;
		case ')':
			ls = pair$cdr(ls);
			acc = ploop2(acc, nil, file, stop);
			d--;
			continue;
		}

		foo = ploop1(pair$cdr(ls), nil, 0, file, stop);
		ls = pair$cdr(foo);

		switch (c) {
		case '\'':
			acc = cons(list2(symbol_quote, pair$car(pair$car(foo))), acc);
			break;
		case '`':
			acc = cons(list2(symbol_quasiquote, pair$car(pair$car(foo))), acc);
			break;
		case ',':
			acc = cons(list2(symbol_unquote, pair$car(pair$car(foo))), acc);
			break;
		case '@':
			acc = cons(list2(symbol_unquote_splicing, pair$car(pair$car(foo))), acc);
			break;
		case '#':
			acc = cons(scheme_list2vector(pair$car(pair$car(foo))), acc);
			break;
		default:
			printf("Parsing error.\n");
			longjmp(esc, 1);
		}
	}

	return cons(acc, ls);
}

sexpr *ploop2(sexpr *ls, sexpr *acc, FILE *file, int stop) {
	sexpr *bar;

	if (null(ls)) {
		printf("Parsing error.\n");
		longjmp(esc, 1);
	}

	while (!eq(bar = pair$car(ls), token_left)) {
		if (eq(bar, token_dot)) {
			if (null(pair$cdr(acc))) {
				ls = pair$cdr(ls);
				acc = pair$car(acc);
			} else {
				printf("Parsing error.\n");
				longjmp(esc, 1);
			}
		} else {
			ls = pair$cdr(ls);
			acc = cons(bar, acc);
		}
	}

	return cons(acc, cdr(ls));
}

sexpr *parse(FILE *file, int stop) {
	return map(car, ploop0(lex(file, stop), file, stop));
}

sexpr *scheme_exit() {
	exit(0);
}

sexpr *gensym() {
	char *name = malloc(sizeof(char) * 10);
	sexpr *sp;
	sprintf(name, "g%d", gensyms);
	sp = str2symbol(name);
	free(name);
	gensyms++;
	return sp;
}

sexpr *null_pred(sexpr *sp) {
	return torf(null(sp));
}

sexpr *eof_object_pred(sexpr *sp) {
	return torf(eof_object(sp));
}

sexpr *pair_pred(sexpr *sp) {
	return torf(pair(sp));
}

sexpr *number_pred(sexpr *sp) {
	return torf(number(sp));
}

sexpr *character_pred(sexpr *sp) {
	return torf(character(sp));
}

sexpr *string_pred(sexpr *sp) {
	return torf(string(sp));
}

sexpr *symbol_pred(sexpr *sp) {
	return torf(symbol(sp));
}

sexpr *procedure_pred(sexpr *sp) {
	return torf(virgin(sp) || closure(sp) || primitive(sp));
}

sexpr *primitive_pred(sexpr *sp) {
	return torf(primitive(sp));
}

sexpr *boolean_pred(sexpr *sp) {
	return torf(boolean(sp));
}

sexpr *vector_pred(sexpr *sp) {
	return torf(vector(sp));
}

sexpr *input_port_pred(sexpr *sp) {
	return torf(input_port(sp));
}

sexpr *output_port_pred(sexpr *sp) {
	return torf(output_port(sp));
}

sexpr *void_pred(sexpr *sp) {
	return torf(undefined(sp));
}

sexpr *graphic_pred(sexpr *sp) {
	return torf(graphic(sp));
}

sexpr *image_pred(sexpr *sp) {
	return torf(image(sp));
}

sexpr *complex_image_pred(sexpr *sp) {
	return torf(complex_image(sp));
}

sexpr *color_image_pred(sexpr *sp) {
	return torf(color_image(sp));
}

sexpr *line_pred(sexpr *sp) {
	return torf(line(sp));
}

sexpr *sketch_pred(sexpr *sp) {
	return torf(sketch(sp));
}

sexpr *undefined_pred(sexpr *sp) {
	return torf(undefined(sp));
}

sexpr *complex_pred(sexpr *sp) {
	return torf(komplex(sp));
}

sexpr *real_pred(sexpr *sp) {
	return torf(number(sp) || (komplex(sp) && (sp->u.z.i == 0)));
}

int list(sexpr *sp) {
	if (null(sp))
		return 1;
	else if (pair(sp))
		return list(cdr(sp));
	else
		return 0;
}

sexpr *display(sexpr *sp, sexpr *port) {
	if (undefined(sp))
		return sp;
	if (!output_port(port)) {
		print_value_escape("display: Argument is not an output-port:", port);
	}
	display_or_print(sp, DISPLAY, TOPLEVEL, port->u.file);
	return undefined;
}

sexpr *show_link_graph(sexpr *sp) {
	serialize_sexpr(sp, NULL, 1);

	/*#ifdef GL
	 if (toplevel_flag) {
	 switch (pid = fork()) {
	 case -1:
	 printf("fork failed");
	 break;
	 case 0:
	 glutInit(&gargc,gargv);
	 glutInitWindowSize(c,r);
	 glutInitWindowPosition(0,0);
	 glutCreateWindow(text);
	 sketch_sp = sp;
	 glutDisplayFunc(display_sketch);
	 glutMainLoop();
	 exit(0);
	 }
	 }
	 #endif
	 */
	return undefined;
	/*
	 pid_t pid;

	 switch (pid = fork()) {
	 case -1:
	 printf("fork failed");
	 break;
	 case 0:
	 glutInit(&gargc,gargv);
	 glutInitWindowSize(sketch$cols(sp),sketch$rows(sp));
	 glutInitWindowPosition(0,0);
	 glutCreateWindow("link graph");
	 sketch_sp = sp;
	 glutDisplayFunc(display_link_graph);
	 glutMainLoop();
	 exit(0);
	 break;
	 }
	 return undefined;

	 }
	 */
}
sexpr *print(sexpr *sp) {
	if (undefined(sp))
		return sp;
	print2stdout(sp);
	printf("\n");
	return undefined;
}

void pair_print(sexpr *sp, int print_flag, FILE *file) {
	if (null(sp))
		fprintf(file, "()");
	else if (!pair(sp))
		display_or_print(sp, print_flag, EMBEDDED, file);
	else {
		fprintf(file, "(");
		pair_print(pair$car(sp), print_flag, file);
		pair_cdr_print(pair$cdr(sp), print_flag, file);
		fprintf(file, ")");
	}
}

void pair_cdr_print(sexpr *sp, int print_flag, FILE *file) {
	if (!null(sp)) {
		if (pair(sp)) {
			sexpr *rest = pair$cdr(sp);
			fprintf(file, " ");
			pair_print(pair$car(sp), print_flag, file);
			if (!null(rest)) {
				if (pair(rest))
					pair_cdr_print(rest, print_flag, file);
				else {
					fprintf(file, " . ");
					pair_print(rest, print_flag, file);
				}
			}
		} else {
			fprintf(file, " . ");
			display_or_print(sp, print_flag, EMBEDDED, file);
		}
	}
}

void vector_print(sexpr *sp, int print_flag, FILE *file) {

	int i, l;

	if (sp == global_vals) {
		fprintf(file, "#<vector:global-values>");
	} else {
		l = vector$length(sp);
		fprintf(file, "#(");
		for (i = 0; i < l; i++) {
			display_or_print(vector$data(sp)[i], print_flag, EMBEDDED, file);
			if (i < l - 1)
				fprintf(file, " ");
		}
		fprintf(file, ")");
	}
}

sexpr *newline(sexpr *port) {
	if (!output_port(port)) {
		print_value_escape("newline: Argument is not an output-port: ", port);
	}

	fprintf(port->u.file, "\n");
	return undefined;
}

sexpr *scheme_void() {
	return undefined;
}

sexpr *scheme_error() {
	longjmp(esc, 1);
	return nil;
}

sexpr *scheme_system(sexpr *sp0) {
	if (!string(sp0)) {
		print_value_escape("system: Argument is not a string: ", sp0);
	}
	return (num2exp(system(sp0->u.text)));
}

sexpr *assoc(sexpr *var, sexpr *frame) {
	sexpr **data = vector$data(frame);
	int k, len = vector$length(frame);
	for (k = 0; k < len; k++)
		if (var == data[k])
			return num2exp(k);
	return undefined;
}

sexpr *add_new_global_def(sexpr *var, sexpr *val) {
	int len = vector$length(global_vars);
	if (len >= vector$allocated(global_vars)) {
		printf("Global environment allocated exceeded.\n");
		longjmp(esc, 1);
	}
	vector$data(global_vars)[len] = var;
	vector$data(global_vals)[len] = val;
	vector$length(global_vars) = len + 1;
	vector$length(global_vals) = len + 1;
	return num2exp(len);
}

sexpr *add_def_vars(sexpr *var, sexpr *env) {
	sexpr *frame = pair$car(env);
	sexpr **data = vector$data(frame);
	int k, len = vector$length(frame);

	for (k = 0; k < len; k++)
		if (var == data[k])
			return num2exp(k);

	if (frame != global_vars) {
		sexpr **extended_data = (sexpr **) smalloc;
		smalloc += (len + 1) * sizeof(sexpr *);
		vector$length(frame) = len + 1;
		vector$allocated(frame) = len + 1;
		for (k = 0; k < len; k++)
			extended_data[k] = data[k];
		extended_data[len] = var;
		vector$data(frame) = extended_data;
		return num2exp(len);
	}
	return add_new_global_def(var, undefined);
}

void add_def_vals(int offset, sexpr *val, sexpr *env) {
	sexpr *frame = pair$car(env);
	sexpr **data = vector$data(frame);
	int k, len = vector$length(frame);

	if (offset >= len) {
		sexpr **extended_data = (sexpr **) smalloc;
		smalloc += (offset + 1) * sizeof(sexpr *);
		vector$length(frame) = offset + 1;
		vector$allocated(frame) = offset + 1;
		for (k = 0; k < len; k++)
			extended_data[k] = data[k];
		extended_data[offset] = val;
		vector$data(frame) = extended_data;
		return;
	}

	data[offset] = val;
}

void add_macro(sexpr *sp, sexpr *expander) {
	sexpr *code = closure$compiled(expander);
	sexpr **data = vector$data(code);
	int k, len = vector$length(code);
	sexpr **extended_data = (sexpr **) smalloc;
	if (!symbol(sp) || !virgin(expander))
		printf("Error in creating macro.\n");
	smalloc += (len + 2) * sizeof(sexpr *);
	vector$length(code) = len + 2;
	vector$allocated(code) = len + 2;
	extended_data[0] = bytecode_frame;
	extended_data[1] = make_vector(1);
	vector$data(extended_data[1])[0] = bytecode_halt;
	for (k = 0; k < len; k++)
		extended_data[2 + k] = data[k];
	vector$data(code) = extended_data;
	expander->type = CLOSURE;
	closure$lookup(expander) = list2vector(closure$env(expander));
	symbol$macro(sp) = expander;
	macros = cons(sp, macros);
}

sexpr *list2frame(int n, sexpr *vals) {
	int i;
	sexpr *frame = make_vector(abs(n));
	sexpr **data = vector$data(frame);

	/* (lambda args ... ) => -1 */
	if (n == -1) {
		data[0] = vals;
		return frame;
	}

	/* (lambda (x y . z) ... ) => -3 */
	if (n < 0) {
		n = fabs(n);
		for (i = 0; i < n - 1; i++) {
			data[i] = pair$car(vals);
			vals = pair$cdr(vals);
		}
		data[n - 1] = vals;
		return frame;
	}

	/* (lambda (x y) ... ) => 2 */
	for (i = 0; i < n; i++) {
		data[i] = pair$car(vals);
		vals = pair$cdr(vals);
	}

	return frame;
}

sexpr *frame4apply(int n, int r0, int r, sexpr **args) {
	int i, m = r - r0;
	sexpr *ls, *frame = make_vector(abs(n));
	sexpr **data = vector$data(frame);

	/* (lambda (x y . z) ... ) => -3 */
	if (n < 0) {
		n = abs(n);
		/* extra args become named args */
		if (m <= n) {
			for (i = 0; i < m - 1; i++)
				data[i] = args[r - i - 1];
			ls = args[r - m];
			for (i = m - 1; i < n - 1; i++) {
				data[i] = pair$car(ls);
				ls = pair$cdr(ls);
			}
			data[n - 1] = ls;
		} else {
			/* named args become extra args */
			for (i = 0; i < n - 1; i++)
				data[i] = args[r - i - 1];
			ls = args[r - m];
			for (i = m - 1; i > n - 1; i--)
				ls = cons(args[r - i], ls);
			data[n - 1] = ls;
		}
		return frame;
	}

	if (m > n) {
		printf("apply: Wrong number of arguments for user-defined function.\n");
		longjmp(esc, 1);
	}

	/* (lambda (x y) ... ) => 2 */

	/* extra args become named args */
	for (i = 0; i < m - 1; i++)
		data[i] = args[r - i - 1];
	ls = args[r - m];
	for (i = m - 1; i < n; i++) {
		data[i] = pair$car(ls);
		ls = pair$cdr(ls);
	}
	return frame;
}

sexpr *frame4funcall(int n, int r0, int r, sexpr **args) {
	int i;
	sexpr *ls, *frame = make_vector(abs(n));
	sexpr **data = vector$data(frame);

	/* (lambda args ... ) => -1 */
	if (n == -1) {
		ls = nil;
		for (i = r0; i < r; i++)
			ls = cons(args[i], ls);
		data[0] = ls;
		return frame;
	}

	/* (lambda (x y . z) ... ) => -3 */
	if (n < 0) {
		n = abs(n);
		if (r - r0 < n - 1) {
			printf(
					"funcall: Wrong number of arguments for user-defined function.\n");
			longjmp(esc, 1);
		}
		for (i = 0; i < n - 1; i++)
			data[i] = args[r - i - 1];
		ls = nil;
		for (i = r0; i < r - n + 1; i++)
			ls = cons(args[i], ls);
		data[n - 1] = ls;
		return frame;
	}

	if (r - r0 != n) {
		printf(
				"funcall: Wrong number of arguments for user-defined function.\n");
		longjmp(esc, 1);
	}

	/* (lambda (x y) ... ) => 2 */
	for (i = 0; i < n; i++)
		data[i] = args[r - i - 1];
	return frame;
}

sexpr *reduce_pred(sexpr *pred, sexpr *ls) {
	sexpr *rest;
	sexpr *(*func)() = primitive$func(pred);
	if (null(ls))
		longjmp(esc, 1);
	rest = cdr(ls);
	if (null(rest))
		return true;
	if (!isfalse(func(car(ls), car(rest))))
		return reduce_pred(pred, rest);
	return false;
}

sexpr *reduce(sexpr *proc, sexpr *id, sexpr *ls) {
	sexpr *rest;
	sexpr *(*func)() = primitive$func(proc);
	if (null(ls))
		return id;
	rest = cdr(ls);
	if (null(rest))
		return func(car(ls), id);
	return func(car(ls), reduce(proc, id, rest));
}

sexpr *map(sexpr *(*proc)(sexpr *), sexpr *ls) {
	if (null(ls))
		return ls;
	return cons(proc(car(ls)), map(proc, cdr(ls)));
}

sexpr *scheme_map(sexpr *proc, sexpr *ls) {
	return map(primitive$func(proc), ls);
}

sexpr *expand_let(sexpr *sp) {
	sexpr *vars, *vals, *body;
	sexpr *first, *second;
	first = cdr(sp);
	second = car(first);
	vars = map(car, second);
	vals = map(cadr, second);
	body = cdr(first);
	return cons(cons(symbol_lambda, cons(vars, body)), vals);
}

sexpr *append(sexpr *ls1, sexpr *ls2) {
	if (null(ls1))
		return ls2;
	return cons(car(ls1), append(cdr(ls1), ls2));
}

sexpr *reverse(sexpr *ls) {
	if (!pair(ls) && !null(ls)) {
		print_value_escape("reverse: Argument is not a list: ", ls);
	}
	if (null(ls))
		return ls;
	return append(reverse(cdr(ls)), cons(car(ls), nil));
}

sexpr *expand_let_star(sexpr *sp) {
	sexpr *vars, *vals, *body;
	sexpr *rest, *bindings, *first_binding, *rest_bindings;

	rest = cdr(sp);
	bindings = car(rest);
	first_binding = car(bindings);
	rest_bindings = cdr(bindings);

	if (null(rest_bindings)) {
		vars = cons(car(first_binding), nil);
		vals = cdr(first_binding);
		body = cdr(rest);
		return cons(cons(symbol_lambda, cons(vars, body)), vals);
	} else {
		vars = cons(car(first_binding), nil);
		vals = cons(cadr(first_binding), nil);
		body = cons(symbol_let_star, cons(rest_bindings, cdr(rest)));
		return cons(cons(symbol_lambda, cons(vars, cons(body, nil))), vals);
	}
}

sexpr *make_set_list(sexpr *vars, sexpr *vals) {
	if (null(vars))
		return nil;
	return cons(cons(symbol_set_bang, cons(car(vars), cons(car(vals), nil))),
			make_set_list(cdr(vars), cdr(vals)));
}

sexpr *do_nothing(sexpr *sp) {
	return nil;
}

sexpr *expand_letrec(sexpr *sp) {
	sexpr *vars, *vals, *body, *setbangs, *dummy;
	sexpr *first, *second;
	first = cdr(sp);
	second = car(first);
	vars = map(car, second);
	vals = map(cadr, second);
	dummy = map(do_nothing, second);
	body = cdr(first);
	setbangs = make_set_list(vars, vals);
	return cons(cons(symbol_lambda, cons(vars, append(setbangs, body))), dummy);
}

sexpr *not(sexpr *sp) {
	return torf(isfalse(sp));
}

sexpr *make_continuation(int s, int r) {
	sexpr *sp = (sexpr *) smalloc;
	smalloc += sizeof(continuation);
	sp->type = CONTINUATION;
	continuation$s(sp) = s;
	continuation$args(sp) = (sexpr **) smalloc;
	smalloc += r * sizeof(sexpr *);
	continuation$cv_stack(sp) = (sexpr **) smalloc;
	smalloc += s * sizeof(sexpr *);
	continuation$pc_stack(sp) = (int *) smalloc;
	smalloc += s * sizeof(sexpr **);
	continuation$lu_stack(sp) = (sexpr **) smalloc;
	smalloc += s * sizeof(sexpr *);
	continuation$e_stack(sp) = (sexpr **) smalloc;
	smalloc += s * sizeof(sexpr *);
	continuation$r_stack(sp) = (int *) smalloc;
	smalloc += s * sizeof(int);
	return sp;
}

sexpr *virtual_machine(sexpr *a, sexpr *cv, sexpr *e, sexpr *lu) {

	int r = 0, s = 0;

	sexpr **global_data = vector$data(global_vals);

	sexpr *args[STACKSIZE];
	sexpr *lu_stack[STACKSIZE];
	sexpr *cv_stack[STACKSIZE];
	sexpr **pc_stack[STACKSIZE];
	sexpr *e_stack[STACKSIZE];
	int r_stack[STACKSIZE];

	long unsigned pc0;

	int i, j, s0;
	sexpr *sp, **data, *(*func)(), **pc = vector$data(cv);

	virtual_machine:

	switch ((*pc)->type) {
	case HALT:
		return a;
	case REFER:
		++pc;
		j = (int) (*pc)->u.x;
		++pc;
		i = (int) (*pc)->u.x;
		a = vector$data(vector$data(lu)[j-1])[i];
		++pc;
		goto virtual_machine;
	case LOCAL_REFER:
		++pc;
		i = (int) (*pc)->u.x;
		a = vector$data(pair$car(e))[i];
		++pc;
		goto virtual_machine;
	case GLOBAL_REFER:
		++pc;
		i = (int) (*pc)->u.x;
		a = global_data[i];
		++pc;
		goto virtual_machine;
	case CONSTANT:
		++pc;
		a = *pc;
		++pc;
		goto virtual_machine;
	case ARGUMENT:
		args[r++] = a;
		++pc;
		goto virtual_machine;
	case TEST:
		if (boolean(a) && (!a->u.i)) {
			++pc;
			++pc;
		} else {
			++pc;
			cv = *pc;
			pc = vector$data(cv);
		}
		goto virtual_machine;
	case APPLY:
		switch (a->type) {
		case PRIMITIVE:
			func = primitive$func(a);
			i = primitive$arity(a);
			data = vector$data(frame4apply(i,r_stack[s-1],r,args));
			switch (i) {
			case 0:
				a = func();
				break;
			case 1:
				a = func(data[0]);
				break;
			case 2:
				a = func(data[0], data[1]);
				break;
			case 3:
				a = func(data[0], data[1], data[2]);
				break;
			case 4:
				a = func(data[0], data[1], data[2], data[3]);
				break;
			case 5:
				a = func(data[0], data[1], data[2], data[3], data[4]);
				break;
			case 6:
				a = func(data[0], data[1], data[2], data[3], data[4], data[5]);
				break;
			case 7:
				a = func(data[0], data[1], data[2], data[3], data[4], data[5],
						data[6]);
				break;
			case 8:
				a = func(data[0], data[1], data[2], data[3], data[4], data[5],
						data[6], data[7]);
				break;
			case 9:
				a = func(data[0], data[1], data[2], data[3], data[4], data[5],
						data[6], data[7], data[8]);
			}
			s--;
			cv = cv_stack[s];
			pc = pc_stack[s];
			lu = lu_stack[s];
			e = e_stack[s];
			r = r_stack[s];
			goto virtual_machine;
		case VIRGIN:
			cv = closure$compiled(a);
			pc = vector$data(cv);
			lu = list2vector(closure$env(a));
			closure$lookup(a) = lu;
			a->type = CLOSURE;
			e = cons(frame4apply(closure$arity(a), r_stack[s - 1], r, args), closure$env(a));
			r = r_stack[s - 1];
			goto virtual_machine;
		case CLOSURE:
			cv = closure$compiled(a);
			pc = vector$data(cv);
			lu = closure$lookup(a);
			e = cons(frame4apply(closure$arity(a), r_stack[s - 1], r, args), closure$env(a));
			r = r_stack[s - 1];
			goto virtual_machine;
		default:
			print_value_escape("apply: Attempt to apply non-function: ", a);
		}
	case FUNCALL:
		switch (a->type) {
		case PRIMITIVE:
			func = primitive$func(a);
			i = primitive$arity(a);
			if (i != r - r_stack[s - 1]) {
				print_value_escape(
						"apply: Wrong number of arguments for primitive function: ",
						a);
			}
			switch (i) {
			case 0:
				a = func();
				break;
			case 1:
				a = func(args[r - 1]);
				break;
			case 2:
				a = func(args[r - 1], args[r - 2]);
				break;
			case 3:
				a = func(args[r - 1], args[r - 2], args[r - 3]);
				break;
			case 4:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4]);
				break;
			case 5:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4],
						args[r - 5]);
				break;
			case 6:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4],
						args[r - 5], args[r - 6]);
				break;
			case 7:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4],
						args[r - 5], args[r - 6], args[r - 7]);
				break;
			case 8:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4],
						args[r - 5], args[r - 6], args[r - 7], args[r - 8]);
				break;
			case 9:
				a = func(args[r - 1], args[r - 2], args[r - 3], args[r - 4],
						args[r - 5], args[r - 6], args[r - 7], args[r - 8],
						args[r - 9]);
			}
			s--;
			cv = cv_stack[s];
			pc = pc_stack[s];
			lu = lu_stack[s];
			e = e_stack[s];
			r = r_stack[s];
			goto virtual_machine;
		case VIRGIN:
			cv = closure$compiled(a);
			pc = vector$data(cv);
			lu = list2vector(closure$env(a));
			closure$lookup(a) = lu;
			a->type = CLOSURE;
			e = cons(frame4funcall(closure$arity(a), r_stack[s - 1], r, args), closure$env(a));
			r = r_stack[s - 1];
			goto virtual_machine;
		case CLOSURE:
			cv = closure$compiled(a);
			pc = vector$data(cv);
			lu = closure$lookup(a);
			e = cons(frame4funcall(closure$arity(a), r_stack[s - 1], r, args), closure$env(a));
			r = r_stack[s - 1];
			goto virtual_machine;
		default:
			print_value_escape("apply: Attempt to apply non-function: ", a);
		}
	case FRAME:
		++pc;
		cv = *pc;
		cv_stack[s] = cv;
		pc_stack[s] = vector$data(cv);
		lu_stack[s] = lu;
		e_stack[s] = e;
		r_stack[s] = r;
		s++;
		++pc;
		goto virtual_machine;
	case RETURN:
		s--;
		cv = cv_stack[s];
		pc = pc_stack[s];
		lu = lu_stack[s];
		e = e_stack[s];
		r = r_stack[s];
		goto virtual_machine;
	case CLOSE:
		a = (sexpr *) smalloc;
		smalloc += sizeof(closure);
		a->type = VIRGIN;
		++pc;
		closure$arity(a) = (int) (*pc)->u.x;
		++pc;
		closure$lookup(a) = nil;
		closure$compiled(a) = *pc;
		closure$env(a) = e;
		++pc;
		goto virtual_machine;
	case ASSIGN:
		++pc;
		j = (int) (*pc)->u.x;
		++pc;
		i = (int) (*pc)->u.x;
		vector$data(vector$data(lu)[j-1])[i] = a;
		a = undefined;
		++pc;
		goto virtual_machine;
	case LOCAL_ASSIGN:
		++pc;
		i = (int) (*pc)->u.x;
		vector$data(pair$car(e))[i] = a;
		a = undefined;
		++pc;
		goto virtual_machine;
	case GLOBAL_ASSIGN:
		++pc;
		i = (int) (*pc)->u.x;
		global_data[i] = a;
		a = undefined;
		++pc;
		goto virtual_machine;
	case CONTI:
		a = (sexpr *) smalloc;
		smalloc += sizeof(closure);
		a->type = CLOSURE;
		closure$arity(a) = 1;
		closure$compiled(a) = make_vector(2);
		vector$data(closure$compiled(a))[0] = bytecode_nuate;
		sp = make_continuation(s, r);
		vector$data(closure$compiled(a))[1] = sp;
		closure$env(a) = global_env_vals;
		closure$lookup(a) = nil;
		for (i = 0; i < r; i++)
			continuation$args(sp)[i] = args[i];
		for (i = 0; i < s; i++) {
			data = vector$data(cv_stack[i]);
			continuation$pc_stack(sp)[i] = (long unsigned) (pc_stack[i] - data);
			continuation$cv_stack(sp)[i] = cv_stack[i];
			continuation$lu_stack(sp)[i] = lu_stack[i];
			continuation$e_stack(sp)[i] = e_stack[i];
			continuation$r_stack(sp)[i] = r_stack[i];
		}
		++pc;
		goto virtual_machine;
	case NUATE:
		++pc;
		sp = *pc;
		s = continuation$s(sp);
		for (i = 0; i < s; i++) {
			cv_stack[i] = continuation$cv_stack(sp)[i];
			pc0 = (long unsigned) vector$data(cv_stack[i]);
			pc_stack[i] = (sexpr **) (pc0 + continuation$pc_stack(sp)[i]);
			lu_stack[i] = continuation$lu_stack(sp)[i];
			e_stack[i] = continuation$e_stack(sp)[i];
			r_stack[i] = continuation$r_stack(sp)[i];
		}
		s--;
		a = vector$data(pair$car(e))[0];
		cv = cv_stack[s];
		pc = pc_stack[s];
		lu = lu_stack[s];
		e = e_stack[s];
		r = r_stack[s];
		for (i = 0; i < r; i++)
			args[i] = continuation$args(sp)[i];
		goto virtual_machine;
	case THROW:
		a = (sexpr *) smalloc;
		smalloc += sizeof(closure);
		a->type = CLOSURE;
		closure$arity(a) = 1;
		closure$compiled(a) = make_vector(2);
		vector$data(closure$compiled(a))[0] = bytecode_catch;
		vector$data(closure$compiled(a))[1] = num2exp(s);
		closure$env(a) = global_env_vals;
		closure$lookup(a) = nil;
		++pc;
		goto virtual_machine;
	case CATCH:
		++pc;
		s0 = (int) (*pc)->u.x;
		if (s0 > s) {
			printf(
					"Attempt to return to escape continuation which no longer exists.\n");
			longjmp(esc, 1);
		}
		s = s0 - 1;
		a = vector$data(pair$car(e))[0];
		cv = cv_stack[s];
		pc = pc_stack[s];
		lu = lu_stack[s];
		e = e_stack[s];
		r = r_stack[s];
		goto virtual_machine;
	case DEFINE:
		++pc;
		add_def_vals((int) (*pc)->u.x, a, e);
		a = undefined;
		++pc;
		goto virtual_machine;
	default:
		print_value_escape("Unrecognized virtual machine instruction: ", *pc);
	}
	return NULL;
}

sexpr *eval(sexpr *x) {
	return virtual_machine(nil, cps2vector(car(compile(expand(x), cons(
			bytecode_halt, nil), global_env_vars))), global_env_vals, nil);
}

sexpr *scheme_compile(sexpr *x) {
	return cps2vector(car(compile(expand(x), cons(bytecode_halt, nil),
			global_env_vars)));
}

sexpr *expand_quasiquote_list(sexpr *sp) {

	if (!pair(sp))
		return cons(symbol_quote, cons(cons(sp, nil), nil));
	else {
		sexpr *first = car(sp);
		if (symbol(first)) {
			if (first == symbol_unquote)
				return cons(primitive_cons, cons(cadr(sp), cons(nil, nil)));
			else if (first == symbol_unquote_splicing)
				return cadr(sp);
			else if (first == symbol_quasiquote)
				return expand_quasiquote_list(expand_quasiquote(cadr(sp)));
		}
		return cons(primitive_cons, cons(cons(primitive_append, cons(
				expand_quasiquote_list(car(sp)), cons(
						expand_quasiquote(cdr(sp)), nil))), cons(nil, nil)));
	}
}

sexpr *expand_quasiquote(sexpr *sp) {

	if (!pair(sp))
		return cons(symbol_quote, cons(sp, nil));
	else {
		sexpr *first = car(sp);
		if (symbol(first)) {
			if (first == symbol_unquote)
				return cadr(sp);
			else if (first == symbol_unquote_splicing) {
				printf("expand: unquote-splicing not inside list.\n");
				longjmp(esc, 1);
			} else if (first == symbol_quasiquote)
				return expand_quasiquote(expand_quasiquote(cadr(sp)));
		}
		return cons(primitive_append, cons(expand_quasiquote_list(car(sp)),
				cons(expand_quasiquote(cdr(sp)), nil)));
	}
}

sexpr *expand(sexpr *sp) {

	sexpr *first;

	if (self_evaluating(sp)|| symbol(sp))
	return sp;

	first = car(sp);

	if (symbol(first)) {

		if (first == symbol_quote)
		return sp;

		if (first == symbol_quasiquote)
		return expand(expand_quasiquote(cadr(sp)));

		if (first == symbol_let)
		return expand(expand_let(sp));

		if (first == symbol_let_star)
		return expand(expand_let_star(sp));

		if (first == symbol_letrec)
		return expand(expand_letrec(sp));

		if (first == symbol_lambda)
		return cons(symbol_lambda, cons(cadr(sp), map(expand, cddr(sp))));

		if (!null(symbol$macro(first))) {
			sexpr *expander = symbol$macro(first);
			return expand(virtual_machine(nil, closure$compiled(expander),
							cons(list2frame(closure$arity(expander), cdr(sp)),
									closure$env(expander)), closure$lookup(expander)));
		}
	}
	return cons(expand(car(sp)), map(expand, cdr(sp)));
}

sexpr *gtype(sexpr *g) {
	if (!graphic(g)) {
		print_value_escape("graphic-type: Argument is not a graphic object: ",
				g);
	}
	if (pair(graphic$sexpr(g)))
		return car(graphic$sexpr(g));
	else
		return symbol_nil;
}

sexpr *gargs(sexpr *g) {
	if (!graphic(g)) {
		print_value_escape("graphic-args: Argument is not a graphic object: ",
				g);
	}
	return cdr(graphic$sexpr(g));
}

sexpr *gnull(sexpr *g) {
	if (!graphic(g)) {
		print_value_escape("graphic-null?: Argument is not a graphic object: ",
				g);
	}
	return torf(null(graphic$sexpr(g)));
}

sexpr *gcar(sexpr *g1) {
	sexpr *g2;
	if (!graphic(g1)) {
		print_value_escape("gcar: Argument is not a graphic object: ", g1);
	}
	g2 = gtype(g1);
	if (eq(g2, symbol_adjoin) || eq(g2, symbol_adorn)) {
		g2 = (sexpr *) smalloc;
		smalloc += sizeof(graphic);
		g2->type = GRAPHIC;
		graphic$sexpr(g2) = cadr(graphic$sexpr(g1));
		return g2;
	}
	print_value_escape("gcar: Argument is not an adjoin or adorn: ", g2);
	return NULL;
}

sexpr *gcdr(sexpr *g1) {
	sexpr *g2, *rest;

	if (!graphic(g1)) {
		print_value_escape("gcdr: Argument is not a graphic object: ", g1);
	}
	g2 = gtype(g1);
	if (eq(g2, symbol_adjoin) || eq(g2, symbol_adorn)) {
		rest = cddr(graphic$sexpr(g1));
		g2 = (sexpr *) smalloc;
		smalloc += sizeof(graphic);
		g2->type = GRAPHIC;
		if (null(rest))
		graphic$sexpr(g2) = nil;
			else
			graphic$sexpr(g2) = cons(car(graphic$sexpr(g1)), rest);
			return g2;
		}
		print_value_escape("gcdr: Argument is not an adjoin or adorn: ", g2);
		return NULL;
	}

sexpr *adjoin(sexpr *g1, sexpr *g2) {
	sexpr *g3;

	if (!graphic(g1)) {
		print_value_escape("adjoin: Argument is not a graphic object: ", g1);
	}

	if (!graphic(g2)) {
		print_value_escape("adjoin: Argument is not a graphic object: ", g2);
	}

	if (null(graphic$sexpr(g1)))
		return g2;
	if (null(graphic$sexpr(g2)))
		return g1;
	g3 = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g3->type = GRAPHIC;
	graphic$sexpr(g3) = cons(symbol_adjoin, cons(graphic$sexpr(g1), cons(graphic$sexpr(g2), nil)));
	return g3;
}

sexpr *adorn(sexpr *g1, sexpr *g2) {
	sexpr *g3;

	if (!graphic(g1)) {
		print_value_escape("adorn: Argument is not a graphic object: ", g1);
	}

	if (!graphic(g2)) {
		print_value_escape("adorn: Argument is not a graphic object: ", g2);
	}
	if (null(graphic$sexpr(g1)))
		return g2;
	if (null(graphic$sexpr(g2)))
		return g1;

	g3 = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g3->type = GRAPHIC;
	graphic$sexpr(g3) = cons(symbol_adorn, cons(graphic$sexpr(g1), cons(graphic$sexpr(g2), nil)));
	return g3;
}

sexpr *make_plumber(double x, double y, double heading) {
	sexpr *p;
	p = malloc(sizeof(plumber));
	p->type = PLUMBER;
	plumber$x(p) = x;
	plumber$y(p) = y;
	plumber$heading(p) = heading;
	return p;
}

sexpr *straight(sexpr *len) {
	sexpr *g;

	if (!number(len)) {
		print_value_escape("straight: Length argument is not a number: ", len);
	}
	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_straight, cons(len, cons(graphic_default_color,
			cons(num2exp(1.0), nil))));
	return g;
}

sexpr *fancy_straight(sexpr *len, sexpr *color, sexpr *width) {
	sexpr *g;

	if (!number(len)) {
		print_value_escape("straight: Length argument is not a number: ", len);
	}

	if (!pair(color)) {
		print_value_escape("straight: Color argument is not a pair: ", color);
	}

	if (!number(width)) {
		print_value_escape("straight: Width argument is not a number: ", width);
	}
	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g)
			= cons(symbol_straight, cons(len, cons(color, cons(width, nil))));
	return g;
}

sexpr *spot(sexpr *width) {
	sexpr *g;

	if (!number(width)) {
		print_value_escape("spot: Width argument is not a number: ", width);
	}

	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_spot,
			cons(width, cons(graphic_default_color, nil)));
	return g;
}

sexpr *fancy_spot(sexpr *width, sexpr *color) {
	sexpr *g;

	if (!number(width)) {
		print_value_escape("spot: Width argument is not a number: ", width);
	}

	if (!pair(color)) {
		print_value_escape("spot: Color argument is not a pair: ", color);
	}

	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_spot, cons(width, cons(color, nil)));
	return g;
}

sexpr *transparent(sexpr *len) {
	sexpr *g;

	if (!number(len)) {
		print_value_escape("transparent: Length argument is not a number: ",
				len);
	}
	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_transparent, cons(len, nil));
	return g;
}

sexpr *bend(sexpr *angle) {
	sexpr *g;

	if (!number(angle)) {
		print_value_escape("bend: Argument is not a number: ", angle);
	}

	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_bend, cons(angle, nil));
	return g;
}

sexpr *text(sexpr *text) {
	sexpr *g;

	if (!string(text)) {
		print_value_escape("text: Argument is not a string: ", text);
	}

	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_text, cons(text, cons(graphic_default_color, cons(
			num2exp(1.0), nil))));
	return g;
}

sexpr *fancy_text(sexpr *text, sexpr *color, sexpr *size) {
	sexpr *g;

	if (!string(text)) {
		print_value_escape("text: Argument is not a string: ", text);
	}

	if (!pair(color)) {
		print_value_escape("text: Color argument is not a pair: ", color);
	}

	if (!number(size)) {
		print_value_escape("text: Size argument is not a number: ", size);
	}

	g = (sexpr *) smalloc;
	smalloc += sizeof(graphic);
	g->type = GRAPHIC;
	graphic$sexpr(g) = cons(symbol_text, cons(text, cons(color, cons(size, nil))));
	return g;
}

sexpr *gcolor(sexpr *g, sexpr *color) {
	sexpr *first;

	if (!graphic(g)) {
		print_value_escape("gcolor: Argument is not a graphic object: ", g);
	}

	if (!pair(color)) {
		print_value_escape("gcolor: Color argument is not a pair: ", color);
	}

	if (null(graphic$sexpr(g)))
		return g;

	first = car(graphic$sexpr(g));
	if (eq(first, symbol_adjoin))
		return adjoin(gcolor(gcar(g), color), gcolor(gcdr(g), color));
	else if (eq(first, symbol_adorn))
		return adorn(gcolor(gcar(g), color), gcolor(gcdr(g), color));
	else if (eq(first, symbol_straight))
		return fancy_straight(cadr(graphic$sexpr(g)), color, cadr(cddr(graphic$sexpr(g))));
	else if (eq(first, symbol_spot))
		return fancy_spot(cadr(graphic$sexpr(g)), color);
	else if (eq(first, symbol_text))
		return fancy_text(cadr(graphic$sexpr(g)), color, cadr(cddr(graphic$sexpr(g))));
	else
		return g;
}

sexpr *gscale(sexpr *g, sexpr *lscale, sexpr *wscale) {
	sexpr *first;

	if (!graphic(g)) {
		print_value_escape("gscale: Argument is not a graphic object: ", g);
	}

	if (!number(lscale)) {
		print_value_escape("gscale: Length-scale argument is not a number: ",
				lscale);
	}

	if (!number(wscale)) {
		print_value_escape("gscale: Width-scale argument is not a number: ",
				wscale);
	}

	if (null(graphic$sexpr(g)))
		return g;

	first = car(graphic$sexpr(g));
	if (eq(first, symbol_adjoin))
		return adjoin(gscale(gcar(g), lscale, wscale), gscale(gcdr(g), lscale,
				wscale));
	else if (eq(first, symbol_adorn))
		return adorn(gscale(gcar(g), lscale, wscale), gscale(gcdr(g), lscale,
				wscale));
	else if (eq(first, symbol_straight))
		return fancy_straight(times(cadr(graphic$sexpr(g)), lscale), car(cddr(graphic$sexpr(g))), times(cadr(cddr(graphic$sexpr(g))), wscale));
	else if (eq(first, symbol_transparent))
		return transparent(times(cadr(graphic$sexpr(g)), lscale));
	else if (eq(first, symbol_text))
		return fancy_text(cadr(graphic$sexpr(g)), car(cddr(graphic$sexpr(g))), times(cadr(cddr(graphic$sexpr(g))), lscale));
	else
		return g;
}

sexpr *gmirror(sexpr *g) {
	sexpr *first;

	if (!graphic(g)) {
		print_value_escape("gmirror: Argument is not a graphic object: ", g);
	}

	if (null(graphic$sexpr(g)))
		return g;

	first = car(graphic$sexpr(g));
	if (eq(first, symbol_adjoin))
		return adjoin(gmirror(gcar(g)), gmirror(gcdr(g)));
	else if (eq(first, symbol_adorn))
		return adorn(gmirror(gcar(g)), gmirror(gcdr(g)));
	else if (eq(first, symbol_bend))
		return bend(minus(number_zero, cadr(graphic$sexpr(g))));
	else
		return g;
}

sexpr *greverse(sexpr *g) {
	sexpr *first;

	if (!graphic(g)) {
		print_value_escape("greverse: Argument is not a graphic object: ", g);
	}

	if (null(graphic$sexpr(g)))
		return g;

	first = car(graphic$sexpr(g));
	if (eq(first, symbol_adjoin))
		return adjoin(greverse(gcdr(g)), greverse(gcar(g)));
	else if (eq(first, symbol_adorn))
		return adorn(greverse(gcar(g)), greverse(gcdr(g)));
	else
		return g;
}

sexpr *make_primitive(sexpr *(*func)(), int arity, char *name) {
	sexpr *sp = (sexpr *) malloc(sizeof(primitive));
	sp->type = PRIMITIVE;
	primitive$arity(sp) = arity;
	primitive$func(sp) = func;
	primitive$name(sp) = name;
	return sp;
}

sexpr *cps2vector_helper(sexpr *code) {
	if (null(code))
		return nil;
	else {
		sexpr *first = car(code);
		if (first == bytecode_constant || first == bytecode_close)
			return cons(first, cons(cadr(code), cps2vector_helper(cddr(code))));
		else
			return cons(cps2vector(first), cps2vector_helper(cdr(code)));
	}
}

sexpr *cps2vector(sexpr *code) {
	if (pair(code))
		return list2vector(cps2vector_helper(code));
	else
		return code;
}

sexpr *compile_symbol(sexpr *sp, sexpr *next, sexpr *env) {
	sexpr *frame, *rest_env, *result;
	int j = 0;
	rest_env = env;
	frame = car(rest_env);
	while (frame != global_vars) {
		result = assoc(sp, frame);
		if (!undefined(result)) {
			if (j == 0) {
				return cons(append(list2(bytecode_local_refer, result), next),
						env);
			} else {
				return cons(append(cons(bytecode_refer, list2(num2exp(j),
						result)), next), env);
			}
		}
		rest_env = cdr(rest_env);
		frame = car(rest_env);
		j++;
	}
	result = assoc(sp, global_vars);

	if (undefined(result))
		result = add_new_global_def(sp, undefined);
	return cons(append(list2(bytecode_global_refer, result), next), env);
}

sexpr *compile_set_bang(sexpr *sp, sexpr *value, sexpr *next, sexpr *env) {
	sexpr *frame, *rest_env, *result;
	int j = 0;
	rest_env = env;
	frame = car(rest_env);
	while (frame != global_vars) {
		result = assoc(sp, frame);
		if (!undefined(result)) {
			if (j == 0) {
				return compile(value, append(list2(bytecode_local_assign,
						result), next), env);
			} else {
				return compile(value, append(cons(bytecode_assign, list2(
						num2exp(j), result)), next), env);
			}
		}
		rest_env = cdr(rest_env);
		frame = car(rest_env);
		j++;
	}
	result = assoc(sp, global_vars);

	if (undefined(result))
		result = add_new_global_def(sp, undefined);
	return compile(value, append(list2(bytecode_global_assign, result), next),
			env);
}

sexpr *compile_body(sexpr *body, sexpr *next, sexpr *env) {
	if (!null(body)) {
		sexpr *extended_env = cdr(compile(car(body), next, env));
		return car(compile(car(body), compile_body(cdr(body), next,
				extended_env), env));
	} else
		return next;
}

sexpr *compile(sexpr *x, sexpr *next, sexpr *env) {

	if (symbol(x))
		return compile_symbol(x, next, env);

	if (self_evaluating(x))
		return cons(append(list2(bytecode_constant, x), next), env);

	if (pair(x)) {

		sexpr *first = car(x);
		sexpr *args, *c;

		if (first == symbol_quote)
			return cons(append(list2(bytecode_constant, cadr(x)), next), env);

		if (first == symbol_lambda) {
			sexpr *vars = cadr(x);
			int len = length(vars);
			c = compile_body(cddr(x), cons(bytecode_return, nil), cons(
					list2frame(len, vars), env));
			return cons(append(cons(bytecode_close, list2(num2exp(len), c)),
					next), env);
		}

		if (first == symbol_begin) {
			if (null(cdr(x)))
				return cons(compile(undefined, next, env), env);
			else
				return cons(compile_body(cdr(x), next, env), env);
		}

		if (first == symbol_if) {
			sexpr *test = cdr(x);
			sexpr *consequence = cdr(test);
			sexpr *alternative = cdr(consequence);
			if (!null(alternative)) {
				alternative = car(compile(cadr(consequence), next, env));
				consequence = car(compile(car(consequence), next, env));
			} else {
				consequence = car(compile(car(consequence), next, env));
				alternative = car(compile(undefined, next, env));
			}
			test = car(test);
			return compile(test, append(list2(bytecode_test, consequence),
					alternative), env);
		}

		if (first == symbol_define) {
			sexpr *symbol = cdr(x);
			sexpr *value = cadr(symbol);
			return compile(value, append(list2(bytecode_define, add_def_vars(
					car(symbol), env)), next), env);
		}

		if (first == symbol_set_bang) {
			sexpr *symbol = cdr(x);
			sexpr *value = cadr(symbol);
			return compile_set_bang(car(symbol), value, next, env);
		}

		if (first == symbol_callcc) {
			sexpr *esc = cadr(x);
			c = cons(bytecode_conti, append(cons(bytecode_argument, nil), car(
					compile(esc, cons(bytecode_funcall, nil), env))));
			if (car(next) != bytecode_return)
				return cons(append(list2(bytecode_frame, next), c), env);
			else
				return cons(c, env);
		}

		if (first == symbol_callec) {
			sexpr *esc = cadr(x);
			c = cons(bytecode_throw, append(cons(bytecode_argument, nil), car(
					compile(esc, cons(bytecode_funcall, nil), env))));
			if (car(next) != bytecode_return)
				return cons(append(list2(bytecode_frame, next), c), env);
			else
				return cons(c, env);
		}

		if (first == symbol_define_macro) {
			sexpr *var = cdr(x);
			add_macro(car(var), eval(cadr(var)));
			return compile(undefined, next, env);
		}

		if (first == symbol_apply) {
			args = cddr(x);
			c = car(compile(cadr(x), cons(bytecode_apply, nil), env));
			while (!null(args)) {
				c = car(compile(car(args), append(cons(bytecode_argument, nil),
						c), env));
				args = cdr(args);
			}

			if (car(next) != bytecode_return)
				return cons(append(list2(bytecode_frame, next), c), env);
			else
				return cons(c, env);
		}

		args = cdr(x);
		c = car(compile(car(x), cons(bytecode_funcall, nil), env));
		while (!null(args)) {
			c = car(compile(car(args), append(cons(bytecode_argument, nil), c),
					env));
			args = cdr(args);
		}

		if (car(next) != bytecode_return)
			return cons(append(list2(bytecode_frame, next), c), env);
		else
			return cons(c, env);

		return cons(append(list2(bytecode_frame, next), c), env);
	}

	printf("Compilation error.\n");
	longjmp(esc, 1);
}

sexpr *print_global_env() {
	int i, len = vector$length(global_vars);
	for (i = 0; i < len; i++) {
		printf("%d) ", i);
		print(vector$data(global_vars)[i]);
		printf(" ");
		print(vector$data(global_vals)[i]);
		printf("\n");
	}
	return undefined;
}

sexpr *print_closure(sexpr *sp) {
	if (!virgin(sp) && !closure(sp)) {
		printf("print-closure: Argument must be a closure.\n");
		longjmp(esc, 1);
	}
	printf("arity = %d\n", closure$arity(sp));
	printf("lookup = ");
	print(closure$lookup(sp));
	printf("compiled = ");
	print(closure$compiled(sp));
	printf("env = ");
	print(closure$env(sp));
	return undefined;
}

sexpr *print_macro(sexpr *sp) {
	if (!symbol(sp)) {
		printf("print-macro: Argument must be a symbol.\n");
		longjmp(esc, 1);
	}
	print_closure(symbol$macro(sp));
	return undefined;
}

#define install_constant(name,constant) add_new_global_def(str2symbol(name),constant)

#define install_function(name,function,arity) add_new_global_def(str2symbol(name),make_primitive(function,arity,name))

sexpr *make_global_env() {

	/* Install Scheme primitives */
	install_function("car",car,1);
	install_function("cdr",cdr,1);
	add_new_global_def(str2symbol("cons"), primitive_cons);
	install_function("eq?",eq_pred,2);
	install_function("positive?",positive_pred,1);
	install_function("negative?",negative_pred,1);
	install_function("zero?",zero_pred,1);
	install_function("scheme:=",equals,2);
	install_function("scheme:<",lt,2);
	install_function("scheme:>",gt,2);
	install_function("scheme:<=",leq,2);
	install_function("scheme:>=",geq,2);
	install_function("sin",sine,1);
	install_function("cos",cosine,1);
	install_function("tan",tangent,1);
	install_function("asin",arcsine,1);
	install_function("acos",arccosine,1);
	install_function("scheme:atan",arctangent,2);
	install_function("log",logarithm,1);
	install_function("exp",exponential,1);
	install_function("expt",expt,2);
	install_function("sqrt",square_root,1);
	install_function("abs",scheme_abs,1);
	install_function("floor",scheme_floor,1);
	install_function("ceil",scheme_ceil,1);
	install_function("real-part",real_part,1);
	install_function("imag-part",imag_part,1);
	install_function("angle",angle,1);
	install_function("magnitude",scheme_abs,1);
	install_function("conjugate",conjugate,1);
	install_function("polar->complex",polar2complex,2);
	install_function("complex",make_complex,2);
	install_function("conjugate",conjugate,1);
	install_function("scheme:min",scheme_min,2);
	install_function("scheme:max",scheme_max,2);
	install_function("random",scheme_random,1);
	install_function("scheme:reduce",reduce,3);
	install_function("scheme:reduce-pred",reduce_pred,2);
	install_function("scheme:map",scheme_map,2);
	add_new_global_def(str2symbol("scheme:append"), primitive_append);
	install_function("reverse",reverse,1);
	install_function("null?",null_pred,1);
	install_function("symbol?",symbol_pred,1);
	install_function("char?",character_pred,1);
	install_function("string?",string_pred,1);
	install_function("number?",number_pred,1);
	install_function("boolean?",boolean_pred,1);
	install_function("procedure?",procedure_pred,1);
	install_function("primitive?",primitive_pred,1);
	install_function("pair?",pair_pred,1);
	install_function("vector?",vector_pred,1);
	install_function("complex?",complex_pred,1);
	install_function("real?",real_pred,1);
	install_function("not",not,1);
	install_function("delete!",delete,2);
	install_function("set-car!",setcar,2);
	install_function("set-cdr!",setcdr,2);
	install_function("load",load,1);
	install_function("exit",scheme_exit,0);
	install_function("scheme:read",scheme_read,1);
	install_function("scheme:display",display,2);
	install_function("scheme:write",scheme_write,2);
	install_function("scheme:newline",newline,1);
	install_function("scheme:error",scheme_error,0);
	install_function("void",scheme_void,0);
	install_function("void?",void_pred,1);
	install_function("gensym",gensym,0);
	install_function("character->string",char2str,1);
	install_function("number->string",num2str,1);
	install_function("string->number",str2num,1);
	install_function("symbol->string",symbol2exp,1);
	install_function("string->symbol",exp2symbol,1);
	install_function("scheme:string-append",string_append,2);
	install_function("string-ref",string_ref,2);
	install_function("substring",substring,3);
	install_function("string-length",string_length,1);
	install_function("scheme:string=?",string_eq,2);
	install_function("scheme:string<?",string_lt,2);
	install_function("scheme:string>?",string_gt,2);
	install_function("scheme:string<=?",string_leq,2);
	install_function("scheme:string>=?",string_geq,2);
	install_function("scheme:char=?",char_eq,2);
	install_function("scheme:char<?",char_lt,2);
	install_function("scheme:char>?",char_lt,2);
	install_function("scheme:char<=?",char_leq,2);
	install_function("scheme:char>=?",char_geq,2);
	install_function("list->vector",scheme_list2vector,1);
	install_function("vector->list",vector2list,1);
	install_function("scheme:make-vector",scheme_make_vector,2);
	install_function("vector-set!",vector_set,3);
	install_function("vector-ref",vector_ref,2);
	install_function("vector-length",scheme_vector$length,1);
	install_function("open-input-file",open_input_file,1);
	install_function("open-output-file",open_output_file,1);
	install_function("close-input-port",close_input_port,1);
	install_function("close-output-port",close_output_port,1);
	install_function("input-port?",input_port_pred,1);
	install_function("output-port?",output_port_pred,1);
	install_function("eof-object?",eof_object_pred,1);
	install_function("scheme:read-char",read_char,1);
	install_function("scheme:write-char",write_char,2);
	install_function("scheme:peek-char",peek_char,1);
	install_function("scheme:+",plus,2);
	install_function("scheme:-",minus,2);
	install_function("scheme:*",times,2);
	install_function("scheme:/",divide,2);
	install_function("fscanf",scheme_fscanf,2);
	install_function("scheme:fprintf",scheme_fprintf,3);
	install_function("undefined?",undefined_pred,1);
	install_function("expand",expand,1);
	install_function("eval",eval,1);
	install_function("compile",scheme_compile,1);
	install_function("print-closure",print_closure,1);
	install_function("print-macro",print_macro,1);
	install_function("system",scheme_system,1);
	install_function("gil-galad",gilgalad,0);

	/* Install Plumbing graphics constants */
	install_constant("scheme:graphic-nil",graphic_nil);
	install_constant("scheme:graphic-white",color(255.0,255.0,255.0));
	install_constant("scheme:graphic-black",color(0.0,0.0,0.0));
	install_constant("scheme:graphic-red",color(255.0,0.0,0.0));
	install_constant("scheme:graphic-green",color(0.0,255.0,0.0));
	install_constant("scheme:graphic-blue",color(0.0,0.0,255.0));
	install_constant("scheme:graphic-orange",color(255.0,64.0,0.0));

	/* Install Scheme constants */
	install_constant("scheme:current-input-port",current_input_port);
	install_constant("scheme:current-output-port",current_output_port);

	/* Install Boldt line grouping primitives */
	install_function("scheme:zero-crossings",zerocrossings,4);
	install_function("scheme:filter-on-contrast",filter_on_contrast,3);
	install_function("scheme:filter-on-length",filter_on_length,3);
	install_function("filter-on-angle",filter_on_angle,3);
	install_function("po-link!",po_link,2);
	install_function("po-replace!",po_replace,2);
	install_function("scheme:po-merge!",po_merge,2);
	install_function("svd",test_svd,1);
	install_function("make-sketch",make_sketch,3);
	install_function("sketch?",sketch_pred,1);
	install_function("sketch-lines",sketch_lines,1);
	install_function("sketch-rows",sketch_rows,1);
	install_function("sketch-cols",sketch_cols,1);
	install_function("sketch-count",sketch_count,1);
	install_function("sketch-union",sketch_union,2);
	install_function("scheme:make-line",make_line,5);
	install_function("line?",line_pred,1);
	install_function("line-x0",line_x0,1);
	install_function("line-y0",line_y0,1);
	install_function("line-x1",line_x1,1);
	install_function("line-y1",line_y1,1);
	install_function("line-contrast",line_contrast,1);
	install_function("line-theta",line_theta,1);
	install_function("line-length",line_length,1);
	install_function("line-links",line_links,1);
	install_function("set-line-contrast!",set_line_contrast,2);
	install_function("sketch->postscript",sketch2postscript,2);

#ifdef GL
	install_function("show-link-graph",show_link_graph,1);
#endif

	/* Install Plumbing graphics primitives */
	install_function("scheme:fancy-straight",fancy_straight,3);
	install_function("scheme:spot",spot,1);
	install_function("scheme:fancy-spot",fancy_spot,2);
	install_function("transparent",transparent,1);
	install_function("graphic?",graphic_pred,1);
	install_function("graphic-type",gtype,1);
	install_function("graphic-args",gargs,1);
	install_function("graphic-null?",gnull,1);
	install_function("scheme:text",text,1);
	install_function("scheme:fancy-text",fancy_text,3);
	install_function("gcar",gcar,1);
	install_function("gcdr",gcdr,1);
	install_function("gmirror",gmirror,1);
	install_function("greverse",greverse,1);
	install_function("gcolor",gcolor,2);
	install_function("scheme:gscale",gscale,3);
	install_function("set-graphic-color!",set_graphic_default_color,1);
	install_function("scheme:adjoin",adjoin,2);
	install_function("scheme:adorn",adorn,2);
	install_function("bend",bend,1);
	install_function("scheme:straight",straight,1);
	install_function("graphic->postscript",graphic2postscript,2);

	/* Install image processing primitives */
	install_function("set-image-display-scale!",set_image_display_scale,1);
	install_function("scheme:image-map",scheme_image_map,2);
	install_function("read-image",readpgm,1);
	install_function("scheme:write-image",writepgm,2);
	install_function("scheme:image-crop",image_crop,5);
	install_function("scheme:image-pad",image_pad,3);
	install_function("scheme:make-image",scheme_make_image,4);
	install_function("make-hot-image",make_hot_image,1);
	install_function("read-color-image",readppm,1);
	install_function("scheme:write-color-image",writeppm,2);
	install_function("rgb->color-image",rgb2color,3);
	install_function("color-image-red",color_image_red,1);
	install_function("color-image-green",color_image_green,1);
	install_function("color-image-blue",color_image_blue,1);
	install_function("rgb->hsi",rgb2hsi,3);
	install_function("hsi->rgb",hsi2rgb,3);
	install_function("color-image?",color_image_pred,1);
	install_function("complex-image?",complex_image_pred,1);
	install_function("image->complex-image",image2complex,1);
	install_function("complex-image->color-image",complex2color,1);
	install_function("image?",image_pred,1);
	install_function("image-rows",image_rows,1);
	install_function("image-cols",image_cols,1);
	install_function("image-ref",image_ref,3);
	install_function("image-set!",image_set,4);
	install_function("color-image-set!",color_image_set,6);
	install_function("image-transpose",image_transpose,1);
	install_function("scheme:top-to-bottom",scheme_top2bottom,2);
	install_function("scheme:left-to-right",scheme_left2right,2);
	install_function("upsample-rows",upsample_rows,1);
	install_function("upsample-cols",upsample_cols,1);
	install_function("downsample-rows",downsample_rows,1);
	install_function("downsample-cols",downsample_cols,1);
	install_function("convolve",convolve,2);
	install_function("array->image",array2image,1);
	install_function("image->array",image2array,1);
	install_function("shrink",shrink,2);
	install_function("image-normalize",image_normalize,1);
	install_function("color-image-normalize",color_image_normalize,1);
	install_function("scheme:fft",fft,2);
	install_function("scheme:image-fft",image_fft,2);
	install_function("scheme:make-covariance-matrix",make_covariance_matrix,1);
	install_function("matrix-product",matrix_product,2);
	install_function("image-sum",image_sum,1);
	install_function("image-max",image_max,1);
	install_function("image-min",image_min,1);
	install_function("label",label,1);
	install_function("distance-transform",distance_transform,1);
	install_function("scheme:outline",outline,3);
	install_function("perimeters",perimeters,1);
	install_function("areas",areas,1);
	install_function("centers-of-mass",centers_of_mass,1);
	install_function("bounding-boxes",bounding_boxes,1);
	install_function("image->list",image2list,1);
	install_function("scheme:register-images",register_images,6);

	return undefined;
}

unsigned hash(char *name) {

	unsigned hashval;

	for (hashval = 0; *name != '\0'; name++)
		hashval = *name + 31 * hashval;

	return hashval % HASHSIZE;

}

node *symbol_table_lookup(char *name) {

	node *np;

	for (np = hashtab[hash(name)]; np != NULL; np = np->next)
		if (!strcmp(name, symbol$name(np->sp)))
			return np;

	return NULL;
}

sexpr *str2symbol(char *name) {

	node *np;
	unsigned hashval;

	np = symbol_table_lookup(name);

	if (np == NULL) {
		sexpr *sp;
		np = (node *) malloc(sizeof(node));
		sp = (sexpr *) malloc(sizeof(symbol));
		np->sp = sp;
		sp->type = SYMBOL;
		hashval = hash(name);
		symbol$name(sp) = malloc((strlen(name) + 1) * sizeof(char));
		strcpy(symbol$name(sp), name);
		symbol$macro(sp) = nil;
		np->next = hashtab[hashval];
		hashtab[hashval] = np;
	}
	return np->sp;
}

void push(sexpr **spp) {
	*head = spp;
	head--;
}

void copy_continuation(sexpr *sp0, sexpr *sp1) {
	int i, r, s;

	sexpr **args0 = continuation$args(sp0);
	sexpr **cv_stack0 = continuation$cv_stack(sp0);
	int *pc_stack0 = continuation$pc_stack(sp0);
	sexpr **lu_stack0 = continuation$lu_stack(sp0);
	sexpr **e_stack0 = continuation$e_stack(sp0);
	int *r_stack0 = continuation$r_stack(sp0);

	sexpr **args1 = continuation$args(sp1);
	sexpr **cv_stack1 = continuation$cv_stack(sp1);
	int *pc_stack1 = continuation$pc_stack(sp1);
	sexpr **lu_stack1 = continuation$lu_stack(sp1);
	sexpr **e_stack1 = continuation$e_stack(sp1);
	int *r_stack1 = continuation$r_stack(sp1);

	s = continuation$s(sp1);
	for (i = 0; i < s; i++) {
		cv_stack1[i] = cv_stack0[i];
		push(&cv_stack1[i]);
		pc_stack1[i] = pc_stack0[i];
		lu_stack1[i] = lu_stack0[i];
		push(&lu_stack1[i]);
		e_stack1[i] = e_stack0[i];
		push(&e_stack1[i]);
		r_stack1[i] = r_stack0[i];
	}

	r = r_stack1[s - 1];
	for (i = 0; i < r; i++) {
		args1[i] = args0[i];
		push(&args1[i]);
	}
}

void copy(long unsigned start, long unsigned end) {
	int i, len;
	sexpr *sp0, *sp1;

	while (head < tail) {

		sp0 = **(head + 1);

		if ((long unsigned) sp0 < start || (long unsigned) sp0 >= end) {
			head++;
			continue;
		}

		if (copied(sp0)) {
			**(head + 1) = new_address(sp0);
			head++;
			continue;
		}

		switch (sp0->type) {
		case PAIR:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(pair);
			*((pair *) sp1) = *((pair *) sp0);
			break;
		case VIRGIN:
		case CLOSURE:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(closure);
			*((closure *) sp1) = *((closure *) sp0);
			break;
		case VECTOR:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(vector);
			*((vector *) sp1) = *((vector *) sp0);
			vector$data(sp1) = (sexpr **) smalloc;
			smalloc += vector$allocated(sp1) * sizeof(sexpr *);
			break;
		case GRAPHIC:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(graphic);
			*((graphic *) sp1) = *((graphic *) sp0);
			break;
		case PLUMBER:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(plumber);
			*((plumber *) sp1) = *((plumber *) sp0);
			break;
		case LINE:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(line);
			*((line *) sp1) = *((line *) sp0);
			break;
		case SKETCH:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(sketch);
			*((sketch *) sp1) = *((sketch *) sp0);
			sketch$grid(sp1) = (sexpr **) smalloc;
			smalloc += sketch$rows(sp1) * sketch$cols(sp1) * sizeof(sexpr *);
			break;
		case IMAGE:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(image);
			*((image *) sp1) = *((image *) sp0);
			image$data(sp1) = (float *) smalloc;
			smalloc += image$rows(sp1) * image$cols(sp1) * sizeof(float);
			break;
		case COLOR_IMAGE:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(image);
			*((image *) sp1) = *((image *) sp0);
			image$data(sp1) = (float *) smalloc;
			smalloc += 3 * image$rows(sp1) * image$cols(sp1) * sizeof(float);
			break;
		case COMPLEX_IMAGE:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(complex_image);
			*((complex_image *) sp1) = *((complex_image *) sp0);
			complex_image$data(sp1) = (fcomplex *) smalloc;
			smalloc += complex_image$rows(sp1) * complex_image$cols(sp1) * sizeof(fcomplex);
			break;
		case CONTINUATION:
			i = continuation$s(sp0);
			sp1 = make_continuation(i, continuation$r_stack(sp0)[i - 1]);
			break;
		default:
			sp1 = (sexpr *) smalloc;
			smalloc += sizeof(sexpr);
			*sp1 = *sp0;
		}

		sp0->type = COPIED;
		new_address(sp0) = sp1;
		**(head + 1) = sp1;
		head++;

		switch (sp1->type) {
		case PAIR:
			push(&pair$car(sp1));
			push(&pair$cdr(sp1));
			break;
		case VIRGIN:
			push(&closure$compiled(sp1));
			push(&closure$env(sp1));
			break;
		case CLOSURE:
			push(&closure$lookup(sp1));
			push(&closure$compiled(sp1));
			push(&closure$env(sp1));
			break;
		case VECTOR:
			len = vector$length(sp1);
			for (i = 0; i < len; i++) {
				vector$data(sp1)[i] = vector$data(sp0)[i];
				push(&vector$data(sp1)[i]);
			}
			break;
		case GRAPHIC:
			push(&graphic$sexpr(sp1));
			break;
		case LINE:
			push(&line$links(sp1)[0]);
			push(&line$links(sp1)[1]);
			break;
		case SKETCH:
			len = sketch$bixel_rows(sp1) * sketch$bixel_cols(sp1);
			for (i = 0; i < len; i++) {
				sketch$grid(sp1)[i] = sketch$grid(sp0)[i];
				push(&sketch$grid(sp1)[i]);
			}
			push(&sketch$ls(sp1));
			break;
		case IMAGE:
			len = image$rows(sp1) * image$cols(sp1);
			for (i = 0; i < len; i++)
				image$data(sp1)[i] = image$data(sp0)[i];
			break;
		case COLOR_IMAGE:
			len = 3 * image$rows(sp1) * image$cols(sp1);
			for (i = 0; i < len; i++)
				image$data(sp1)[i] = image$data(sp0)[i];
			break;
		case COMPLEX_IMAGE:
			len = complex_image$rows(sp1) * complex_image$cols(sp1);
			for (i = 0; i < len; i++)
				complex_image$data(sp1)[i] = complex_image$data(sp0)[i];
			break;
		case CONTINUATION:
			copy_continuation(sp0, sp1);
			break;
		}
	}
}

void update_macros() {
	sexpr *sp = macros;
	while (!null(sp)) {
		push(&symbol$macro(pair$car(sp)));
		sp = pair$cdr(sp);
	}
}

void garbage_collect() {

	long unsigned start, end, before, after;

	end = smalloc;

	before = (long unsigned) smalloc - (long unsigned) (gc ? smalloc1
			: smalloc0);

	/* switch copy spaces */
	if (gc) {
		start = (long unsigned) smalloc1;
		smalloc = (long unsigned) smalloc0;
		gc = 0;
		tail = (sexpr ***) &tail0;
	} else {
		start = (long unsigned) smalloc0;
		smalloc = (long unsigned) smalloc1;
		gc = 1;
		tail = (sexpr ***) &tail1;
	}

	head = tail;

	/* initiate copying from all roots */
	push(&macros);
	update_macros();
	push(&global_env_vals);
	push(&global_env_vars);
	push(&global_vals);
	push(&global_vars);
	push(&graphic_default_color);
	copy(start, end);

	after = (long unsigned) smalloc
			- (long unsigned) (gc ? smalloc1 : smalloc0);

	//printf("before: %ld after: %ld allocated %ld%% reclaimed: %ld%%\n", before, after, (long unsigned) 100*after / (long unsigned) SEXPRS, (long unsigned) 100*(before-after) / (long unsigned) SEXPRS);


}

void print_table() {

	int i;
	node *np;

	printf("\n");

	for (i = 0; i < HASHSIZE; i++) {
		np = hashtab[i];
		while (np != NULL) {
			print(np->sp);
			np = np->next;
		}
	}
}
