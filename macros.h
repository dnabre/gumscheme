/* unmscheme: macros for manipulating sexprs
 *
 *	Copyright (C) 2006			Lance R. Williams
 *				     			University of New Mexico
 * 	All Rights Reserved.
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
#ifndef MACROS_H_
#define MACROS_H_

#define torf(x) ((x) ? true : false);

#define new_address(x) ((pair *) x)->car

#define pair$car(x) ((pair *) x)->car
#define pair$cdr(x) ((pair *) x)->cdr

#define closure$lookup(x) ((closure *) x)->lookup
#define closure$compiled(x) ((closure *) x)->compiled
#define closure$env(x) ((closure *) x)->env
#define closure$arity(x) ((closure *) x)->arity

#define symbol$macro(x) ((symbol *) x)->macro
#define symbol$name(x) ((symbol *) x)->name

#define primitive$arity(x) ((primitive *) x)->arity
#define primitive$name(x) ((primitive *) x)->name
#define primitive$func(x) ((primitive *) x)->func

#define vector$length(x) ((vector *) x)->length
#define vector$allocated(x) ((vector *) x)->allocated
#define vector$data(x) ((vector *) x)->data

#define continuation$s(x) ((continuation *) x)->s
#define continuation$args(x) ((continuation *) x)->args
#define continuation$cv_stack(x) ((continuation *) x)->cv_stack
#define continuation$pc_stack(x) ((continuation *) x)->pc_stack
#define continuation$lu_stack(x) ((continuation *) x)->lu_stack
#define continuation$e_stack(x) ((continuation *) x)->e_stack
#define continuation$r_stack(x) ((continuation *) x)->r_stack

#define graphic$sexpr(x) ((graphic *) x)->sexpr

#define plumber$x(z) ((plumber *) z)->x
#define plumber$y(z) ((plumber *) z)->y
#define plumber$heading(z) ((plumber *) z)->heading

#define image$rows(x) ((image *) x)->rows
#define image$cols(x) ((image *) x)->cols
#define image$data(x) ((image *) x)->data

#define complex_image$rows(x) ((complex_image *) x)->rows
#define complex_image$cols(x) ((complex_image *) x)->cols
#define complex_image$data(x) ((complex_image *) x)->data

#define line$x(z) ((line *) z)->x
#define line$y(z) ((line *) z)->y
#define line$theta(z) ((line *) z)->theta
#define line$contrast(z) ((line *) z)->contrast
#define line$length(z) ((line *) z)->length
#define line$coverage(z) ((line *) z)->coverage
#define line$links(z) ((line *) z)->links
#define line$replaced(z) ((line *) z)->replaced
#define line$merged(z) ((line *) z)->merged
#define line$age(z) ((line *) z)->age

#define sketch$rows(x) ((sketch *) x)->rows
#define sketch$cols(x) ((sketch *) x)->cols
#define sketch$bixel_rows(x) ((sketch *) x)->bixel_rows
#define sketch$bixel_cols(x) ((sketch *) x)->bixel_cols
#define sketch$bixel_dx(x) ((sketch *) x)->bixel_dx
#define sketch$bixel_dy(x) ((sketch *) x)->bixel_dy
#define sketch$grid(x) ((sketch *) x)->grid
#define sketch$count(x) ((sketch *) x)->count
#define sketch$ls(x) ((sketch *) x)->ls

#define null(sp) ((sp)->type == NIL)
#define pair(sp) (((pair *) sp)->type == PAIR)
#define number(sp) ((sp)->type == NUMBER)
#define token(sp) ((sp)->type == TOKEN)
#define character(sp) ((sp)->type == CHARACTER)
#define string(sp) ((sp)->type == STRING)
#define symbol(sp) (((symbol *) sp)->type == SYMBOL)
#define virgin(sp) (((closure *) sp)->type == VIRGIN)
#define closure(sp) (((closure *) sp)->type == CLOSURE)
#define primitive(sp) (((primitive *) sp)->type == PRIMITIVE)
#define boolean(sp) ((sp)->type == BOOLEAN)
#define vector(sp) (((vector *) sp)->type == VECTOR)
#define graphic(sp) (((graphic *) sp)->type == GRAPHIC)
#define plumber(sp) (((plumber *) sp)->type == PLUMBER)
#define image(sp) (((image *) sp)->type == IMAGE)
#define color_image(sp) (((image *) sp)->type == COLOR_IMAGE)
#define complex_image(sp) (((complex_image *) sp)->type == COMPLEX_IMAGE)
#define line(sp) (((line *) sp)->type == LINE)
#define sketch(sp) (((sketch *) sp)->type == SKETCH)
#define input_port(sp) ((sp)->type == INPUT_PORT)
#define output_port(sp) ((sp)->type == OUTPUT_PORT)
#define eof_object(sp) ((sp)->type == EOF_OBJECT)
#define komplex(sp) ((sp)->type == COMPLEX)
#define undefined(sp) ((sp)->type == UNDEFINED)
#define copied(sp) ((sp)->type == COPIED)
#define self_evaluating(sp) ((sp)->type < PAIR)
#define bytecode(sp) ((((symbol *) sp)->type >= HALT) && (((symbol *) sp)->type <= NUATE))

#ifndef WIN32
#define max(a,b) ((a) > (b) ? (a) : (b));
#define min(a,b) ((a) < (b) ? (a) : (b));
#endif

#define mod(x,n) ((x) >= (n) ? (x)-(n) : ((x) < 0 ? (n)+(x) : (x)))
#define opposite(x) ((x == 1) ? 0 : 1)
#define even(x) ((x) % 2 ? 0 : 1)
#define odd(x) ((x) % 2 ? 1 : 0)
#define print2stdout(x) display_or_print((x),PRINT,TOPLEVEL,stdout);

#endif /* MACROS_H_ */
