/* unmscheme: type defintions
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
 *
 */

#ifndef SEXPR_TYPES_H_
#define SEXPR_TYPES_H_

typedef struct sexpr {
	int type;
	union {
		int i;
		double x;
		char c;
		char *text;
		fcomplex z;
		FILE *file;
		struct sexpr *sexpr;
	} u;
} sexpr;

typedef struct node {
	sexpr *sp;
	struct node *next;
} node;

typedef struct pair {
	int type;
	sexpr *car;
	sexpr *cdr;
} pair;

typedef struct closure {
	int type;
	sexpr *lookup;
	sexpr *compiled;
	sexpr *env;
	int arity;
} closure;

typedef struct symbol {
	int type;
	sexpr *macro;
	char *name;
} symbol;

typedef struct primitive {
	int type;
	int arity;
	char *name;
	sexpr *(*func)();
} primitive;

typedef struct vector {
	int type;
	int length;
	int allocated;
	sexpr **data;
} vector;

typedef struct continuation {
	int type;
	int s;
	sexpr **args;
	sexpr **cv_stack;
	int *pc_stack;
	sexpr **lu_stack;
	sexpr **e_stack;
	int *r_stack;
} continuation;

typedef struct graphic {
	int type;
	sexpr *sexpr;
} graphic;

typedef struct plumber {
	int type;
	double x;
	double y;
	double heading;
} plumber;

typedef struct image {
	int type;
	int rows;
	int cols;
	float *data;
} image;

typedef struct complex_image {
	int type;
	int rows;
	int cols;
	fcomplex *data;
} complex_image;

/* See http://www.cs.unm.edu/~williams/boldt-ref.html */
typedef struct line {
	int type;
	float x[2];
	float y[2];
	float theta;
	float contrast;
	float length;
	float coverage;
	// List of lines, may circular.
	// Always go from x to y
	sexpr *links[2];
	int replaced;
	int merged;
	int age;
} line;

typedef struct sketch {
	int type;
	int rows;
	int cols;
	int bixel_rows;
	int bixel_cols;
	float bixel_dx;
	float bixel_dy;
	sexpr **grid;
	int count;
	sexpr *ls;
} sketch;

#endif /* SEXPR_TYPES_H_ */
