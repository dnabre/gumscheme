/* unmscheme
 *
 *	Copyright (C) 2004-2009		Lance R. Williams
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

sexpr *print_closure(sexpr *);
sexpr *zero_imag_part(sexpr *);
sexpr *equals(sexpr *, sexpr *);
sexpr *make_image(int, int);
sexpr *make_color_image(int, int);
sexpr *complex2color(sexpr *);
sexpr *eval(sexpr *);
sexpr *cps2vector(sexpr *);
sexpr *virtual_machine(sexpr *, sexpr *, sexpr *, sexpr *);
sexpr *compile_body(sexpr *, sexpr *, sexpr *);
sexpr *expand_quasiquote(sexpr *);
sexpr *expand_quasiquote_list(sexpr *);
sexpr *compile(sexpr *, sexpr *, sexpr *);
sexpr *append(sexpr *, sexpr *);
sexpr *make_extensible_vector(int, int);
sexpr *make_vector(int);
sexpr *po_link(sexpr *, sexpr *);
sexpr *vector_ref(sexpr *, sexpr *);
sexpr *cons(sexpr *, sexpr *);
sexpr *car(sexpr *);
sexpr *cdr(sexpr *);
sexpr *caar(sexpr *);
sexpr *cadr(sexpr *);
sexpr *cdar(sexpr *);
sexpr *cddr(sexpr *);
sexpr *caddr(sexpr *);
sexpr *cadddr(sexpr *);
sexpr *lex(FILE *, char);
sexpr *lex_error(FILE *, char);
sexpr *read_newline(FILE *, int);
sexpr *read_space(FILE *, int);
sexpr *backslash(FILE *, int);
sexpr *ploop1(sexpr *, sexpr *, int, FILE *, int);
sexpr *ploop2(sexpr *, sexpr *, FILE *, int);
sexpr *list2(sexpr *, sexpr *);
sexpr *parse(FILE *, int);
sexpr *expand(sexpr *);
sexpr *quasi_expand(sexpr *);
void glut_line();
void display_graphic();
void display_image();
sexpr *image_normalize(sexpr *);
sexpr *color_image_normalize(sexpr *);
sexpr *make_hot_image(sexpr *);
float rhot(float, float, float);
float ghot(float, float, float);
float bhot(float, float, float);
sexpr *draw_graphic_postscript(sexpr *, sexpr *, FILE *);
sexpr *draw_straight_postscript(sexpr *, sexpr *, FILE *);
sexpr *draw_spot_postscript(sexpr *, sexpr *, FILE *);
sexpr *draw_transparent(sexpr *, sexpr *);
sexpr *draw_adjoin_postscript(sexpr*, sexpr *, FILE *);
sexpr *draw_adorn_postscript(sexpr*, sexpr *, FILE *);
sexpr *draw_text_postscript(sexpr*, sexpr *, FILE *);
sexpr *draw_plumber_postscript(sexpr*, sexpr *, FILE *);
sexpr *draw_graphic(sexpr *, sexpr *);
sexpr *draw_straight(sexpr *, sexpr *);
sexpr *draw_spot(sexpr *, sexpr *);
sexpr *draw_transparent(sexpr *, sexpr *);
sexpr *draw_adjoin(sexpr*, sexpr *);
sexpr *draw_adorn(sexpr*, sexpr *);
sexpr *draw_bend(sexpr*, sexpr *);
sexpr *draw_text(sexpr*, sexpr *);
sexpr *draw_plumber(sexpr*, sexpr *);
sexpr *adjoin(sexpr *, sexpr *);
sexpr *adorn(sexpr *, sexpr *);
sexpr *make_plumber(double, double, double);
sexpr *straight(sexpr *);
sexpr *fancy_straight(sexpr *, sexpr *, sexpr *);
sexpr *spot(sexpr *);
sexpr *fancy_spot(sexpr *, sexpr *);
sexpr *transparent(sexpr *);
sexpr *bend(sexpr *);
sexpr *text(sexpr *);
sexpr *gcar(sexpr *);
sexpr *gcdr(sexpr *);
sexpr *gtype(sexpr *);
sexpr *gargs(sexpr *);
void garbage_collect();
void eat_line(FILE *, char);
sexpr *polar2complex(sexpr *, sexpr *);
sexpr *eat_complex(int, FILE *, char);
int eatit(int(*func)(char), char *, FILE *, char);
sexpr *load(sexpr *);
sexpr *str2symbol(char *);
sexpr *str2exp(char *);
sexpr *num2exp(double);
sexpr *complex2exp( fcomplex);
sexpr *char2token(char);
sexpr *char2exp(char);
sexpr *int2boolean(int);
sexpr *list2vector(sexpr *);
int null(sexpr *);
int isfalse(sexpr *);
int notdefined(sexpr *);
int list(sexpr *);
int eq(sexpr *, sexpr *);
sexpr *null_pred(sexpr *);
sexpr *pair_pred(sexpr *);
sexpr *number_pred(sexpr *);
sexpr *not(sexpr *);
sexpr *symbol_pred(sexpr *);
sexpr *procedure_pred(sexpr *);
sexpr *boolean_pred(sexpr *);
sexpr *string_pred(sexpr *);
sexpr *eq_pred(sexpr *, sexpr *);
sexpr *plus(sexpr *, sexpr *);
sexpr *minus(sexpr *, sexpr *);
sexpr *times(sexpr *, sexpr *);
sexpr *divide(sexpr *, sexpr *);
sexpr *positive(sexpr *);
sexpr *display(sexpr *, sexpr *);
sexpr *print(sexpr *);
sexpr *scheme_compile(sexpr *);
sexpr *display_or_print(sexpr *, int, int, FILE *);
void pair_print(sexpr *, int, FILE *);
void pair_cdr_print(sexpr *, int, FILE *);
void vector_print(sexpr *, int, FILE *);
int isletter(char);
int isspecialer(char);
int islegalst(char);
int islegal2nd(char);
int isdigit_or_pt(char);
int notquote(char);
sexpr *make_closure(sexpr *, sexpr *, sexpr *);
sexpr *assoc(sexpr *, sexpr *);
void add_macro(sexpr *, sexpr *);
sexpr *list2frame(int, sexpr *);
int length(sexpr *);
sexpr *map(sexpr *(*proc)(sexpr *), sexpr *);
sexpr *let2lambda(sexpr *);
sexpr *flatten(sexpr *);
sexpr *make_primitive(sexpr *(*func)(), int, char *);
sexpr *make_global_env();
void define_scheme_constants();
unsigned hash(char *);
node *symbol_table_lookup(char *);
double deg2rad(double);
double angle_difference(double, double);
void print_table();
void copy(long unsigned, long unsigned);
