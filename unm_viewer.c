/* unmscheme: opengl viewer for graphical objects.
 *
 *	Copyright (C) 2009			Lance R. Williams
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


#ifdef WIN32
#include <windows.h>
#undef RGB
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "complex.c"
#include "svd.c"
#include "four1.c"

#include "sexpr_types.h"
#include "unmscheme.h"
#include "macros.h"
#include "defines.h"

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif



static sexpr *graphic_sp;
static image img;
static sexpr *sketch_sp;

static sexpr *symbol_nil;
static sexpr *symbol_straight;
static sexpr *symbol_spot;
static sexpr *symbol_transparent;
static sexpr *symbol_bend;
static sexpr *symbol_adjoin;
static sexpr *symbol_adorn;
static sexpr *symbol_text;
static sexpr *nil;

static double image_display_scale = 1.0;

sexpr *cons(sexpr *car, sexpr *cdr) {
	sexpr *sp = (sexpr *) malloc(sizeof(pair));
	sp->type = PAIR;
	pair$car(sp) = car;
	pair$cdr(sp) = cdr;
	return sp;
}

sexpr *car(sexpr *sp) {
	if (pair(sp))
		return pair$car(sp);
	else
		exit(-1);
}

sexpr *cdr(sexpr *sp) {
	if (pair(sp))
		return pair$cdr(sp);
	else
		exit(-1);
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
			return !strcmp(symbol$name(s1), symbol$name(s2));
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

sexpr *append(sexpr *ls1, sexpr *ls2) {
	if (null(ls1))
		return ls2;
	return cons(car(ls1), append(cdr(ls1), ls2));
}

sexpr *reverse(sexpr *ls) {
	if (!pair(ls) && !null(ls)) {
		fprintf(stderr, "reverse: Argument is not a list: \n");
		exit(-1);
	}
	if (null(ls))
		return ls;
	return append(reverse(cdr(ls)), cons(car(ls), nil));
}

static sexpr *true;
static sexpr *false;

sexpr *gnull(sexpr *g) {
	if (!graphic(g)) {
		exit(-1);
	}
	return torf(null(graphic$sexpr(g)));
}
sexpr *gtype(sexpr *g) {
	if (!graphic(g)) {
		exit(-1);
	}
	if (pair(graphic$sexpr(g)))
		return car(graphic$sexpr(g));
	else
		return symbol_nil;
}

sexpr *gcar(sexpr *g1) {
	sexpr *g2;
	if (!graphic(g1)) {
		exit(-1);
	}
	g2 = gtype(g1);
	if (eq(g2, symbol_adjoin) || eq(g2, symbol_adorn)) {
		g2 = (sexpr *) malloc(sizeof(graphic));
		g2->type = GRAPHIC;
		graphic$sexpr(g2) = cadr(graphic$sexpr(g1));
		return g2;
	}
	exit(-1);
	return NULL;
}

sexpr *gcdr(sexpr *g1) {
	sexpr *g2, *rest;

	if (!graphic(g1)) {
		exit(-1);
	}
	g2 = gtype(g1);
	if (eq(g2, symbol_adjoin) || eq(g2, symbol_adorn)) {
		rest = cddr(graphic$sexpr(g1));
		g2 = (sexpr *) malloc(sizeof(graphic));
		g2->type = GRAPHIC;
		if (null(rest))
			graphic$sexpr(g2) = nil;
		else
			graphic$sexpr(g2) = cons(car(graphic$sexpr(g1)), rest);
		return g2;
	}
	exit(-1);
	return NULL;
}

sexpr *str2symbol(char *name) {
	sexpr *sp;
	sp = (sexpr *) malloc(sizeof(symbol));
	sp->type = SYMBOL;

	symbol$name(sp) = malloc((strlen(name) + 1) * sizeof(char));
	strcpy(symbol$name(sp), name);
	symbol$macro(sp) = nil;
	return sp;
}

double deg2rad(double deg) {
	return (remainder(deg, 360) / 180.0) * PI;
}

double angle_difference(double rad1, double rad2) {
	return remainder(rad1 - rad2, TWOPI);
}

void print_line(line* l) {
	fprintf(stderr, "line: x(%f,%f) y(%f,%f) %f %f %f %f %p %p %i %i %i\n",
			l->x[0], l->x[1], l->y[0], l->y[1], l->theta, l->contrast,
			l->length, l->coverage, l->links[0], l->links[1], l->replaced,
			l->merged, l->age);

}

sexpr* read_sexpr() {
	sexpr* sp;
	sp = malloc(sizeof(sexpr));
	fread(sp, sizeof(sexpr), 1, stdin);
	if (null(sp))
		fprintf(stderr, "\tread :nil\n");
	else
		fprintf(stderr, "\tread :cons(x,x)\n");

	return sp;
}

line* read_line();

sexpr* read_line_list(int count, sexpr* acc) {
	sexpr* l;

	if (count == 0)
		return acc;
	else {
		l = (sexpr*) read_line();
		return read_line_list(count - 1, cons(l, acc));
	}
}

line* read_line() {
	line* l;

	l = malloc(sizeof(line));
	fread(l, sizeof(line), 1, stdin);
	return l;
}

sexpr* empty_sexpr(int type) {
	sexpr* sp;
	sp = malloc(sizeof(sexpr));
	sp->type = type;
	return sp;
}

sexpr* read_generic_sexpr() {
	int type;
	double x;
	char* text;
	sexpr* sp;
	sexpr* sp1;
	sexpr* sp2;

	fread(&type, sizeof(int), 1, stdin);

	switch (type) {
	case NIL:

		return nil;
		break;
	case NUMBER:
		fread(&x, sizeof(double), 1, stdin);

		return num2exp(x);
		break;
	case CHARACTER:
		sp = malloc(sizeof(sexpr));
		fread(&(sp->u.c), sizeof(char), 1, stdin);

		return sp;
		break;
	case STRING:
		fread(&type, sizeof(int), 1, stdin);
		text = malloc(sizeof(char) * type);
		fread(text, sizeof(char) * type, 1, stdin);
		sp = malloc(sizeof(sexpr));
		sp->type = STRING;
		sp->u.text = text;

		return sp;
		break;
	case BOOLEAN:
		fread(&type, sizeof(int), 1, stdin);
		sp = empty_sexpr(BOOLEAN);
		sp->u.i = type;

		return sp;
		break;
	case PAIR:

		sp1 = read_generic_sexpr();
		sp2 = read_generic_sexpr();
		return cons(sp1, sp2);
		break;
	case SYMBOL:
		sp = (sexpr*) malloc(sizeof(symbol));
		((symbol*) sp)->macro = nil;
		fread(&type, sizeof(int), 1, stdin);
		text = malloc(sizeof(char) * type);
		fread(text, sizeof(char) * type, 1, stdin);
		((symbol*) sp)->name = text;

		((symbol*) sp)->type = SYMBOL;
		return sp;
		break;
	case GRAPHIC:
		sp = (sexpr*) malloc(sizeof(graphic));
		sp->type = GRAPHIC;
		((graphic*) sp)->sexpr = read_generic_sexpr();

		return sp;
		break;


	default:
		fprintf(stderr, "bad sexpr type in marshalling: %i\n", type);
		exit(-1);

	}

}

void display_image() {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glPixelZoom(image_display_scale, -image_display_scale);
	glRasterPos2i(-1, 1);
	glDrawPixels(img.cols, img.rows, GL_LUMINANCE, GL_FLOAT, img.data);
	glFlush();

}

void display_color_image() {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glPixelZoom(image_display_scale, -image_display_scale);
	glRasterPos2i(-1, 1);
	glDrawPixels(img.cols, img.rows, GL_RGB, GL_FLOAT, img.data);
	glFlush();

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

sexpr *draw_graphic(sexpr *p, sexpr *sp) {
	sexpr *first, *g = graphic$sexpr(sp);

	if (null(g))
		return p;

	first = car(g);
	if (eq(first, symbol_straight))
		return draw_straight(p, sp);
	else if (eq(first, symbol_spot))
		return draw_spot(p, sp);
	else if (eq(first, symbol_transparent))
		return draw_transparent(p, sp);
	else if (eq(first, symbol_adjoin))
		return draw_adjoin(p, sp);
	else if (eq(first, symbol_adorn)) {
		draw_adorn(p, sp);
		return p;
	} else if (eq(first, symbol_bend))
		return draw_bend(p, sp);
	else if (eq(first, symbol_text))
		return draw_text(p, sp);
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
		exit(-1);
	}

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("draw-text: Illegal color argument.\n");
		exit(-1);
	}

	if (!number(size)) {
		printf("draw-text: Illegal size argument.\n");
		exit(-1);
	}

	if (size->u.x < 2.0) {
		glColor3f(r->u.x / 128, g->u.x / 128, b->u.x / 128);
		glRasterPos2f(plumber$x(p), plumber$y(p));
		for (c = text->u.text; *c; c++)
			glutBitmapCharacter(GLUT_BITMAP_8_BY_13, *c);
	} else {
		s = size->u.x * 0.0003;
		glPushMatrix();
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glEnable(GL_BLEND);
		glEnable(GL_LINE_SMOOTH);
		glLineWidth(1.0);
		glTranslatef(plumber$x(p), plumber$y(p), 0.0);
		glScalef(s, s, s);
		glColor3f(r->u.x / 128, g->u.x / 128, b->u.x / 128);
		for (c = text->u.text; *c; c++)
			glutStrokeCharacter(GLUT_STROKE_ROMAN, *c);
		glPopMatrix();
	}
	return p;
}

sexpr *draw_plumber(sexpr *p, sexpr *color) {

	sexpr *r, *g, *b;

	double x, y, c, s;

	c = cos(plumber$heading(p) - PI / 2.0);
	s = sin(plumber$heading(p) - PI / 2.0);
	x = plumber$x(p);
	y = plumber$y(p);

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("draw-plumber: Illegal color argument.\n");
		exit(-1);
	}

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glEnable(GL_LINE_SMOOTH);
	glLineWidth(1.0);
	glBegin(GL_LINE_LOOP);
	glColor3f(r->u.x / 128, g->u.x / 128, b->u.x / 128);
	glVertex2f(x - (C30 * c - S30 * s) / 60.0, y + (-C30 * s - S30 * c) / 60.0);
	glVertex2f(x - s / 60.0, y + c / 60.0);
	glVertex2f(x - (-C30 * c - S30 * s) / 60.0, y + (C30 * s - S30 * c) / 60.0);
	glEnd();

	return p;
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
sexpr *draw_straight(sexpr *p1, sexpr *sp) {

	sexpr *len = cadr(graphic$sexpr(sp));
	sexpr *color = car(cddr(graphic$sexpr(sp)));
	sexpr *width = cadr(cddr(graphic$sexpr(sp)));

	sexpr *p2;
	sexpr *r, *g, *b;
	double x, y, heading;

	if (!number(len)) {
		printf("draw-straight: Illegal length argument.\n");
		exit(-1);
	}

	heading = plumber$heading(p1);
	x = plumber$x(p1) + (len->u.x / 300.0) * cos(heading);
	y = plumber$y(p1) + (len->u.x / 300.0) * sin(heading);
	p2 = make_plumber(x, y, heading);

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("draw-straight: Illegal color argument.\n");
		exit(-1);
	}

	if (!number(width)) {
		printf("draw-straight: Illegal width argument.\n");
		exit(-1);
	}

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glEnable(GL_LINE_SMOOTH);
	glLineWidth(width->u.x);
	glBegin(GL_LINES);
	glColor3f(r->u.x / 128, g->u.x / 128, b->u.x / 128);
	glVertex2f(plumber$x(p1), plumber$y(p1));
	glVertex2f(plumber$x(p2), plumber$y(p2));
	glEnd();
	return p2;
}

sexpr *draw_spot(sexpr *p, sexpr *sp) {

	sexpr *width = cadr(graphic$sexpr(sp));
	sexpr *color = car(cddr(graphic$sexpr(sp)));

	sexpr *r, *g, *b;

	if (!number(width)) {
		printf("draw-spot: Illegal width argument.\n");
		exit(-1);
	}

	r = car(color);
	g = cadr(color);
	b = car(cddr(color));

	if (!number(r) || !number(g) || !number(b)) {
		printf("draw-spot: Illegal color argument.\n");
		exit(-1);
	}

	glPointSize(width->u.x);
	glBegin(GL_POINTS);
	glColor3f(r->u.x / 128, g->u.x / 128, b->u.x / 128);
	glVertex2f(plumber$x(p), plumber$y(p));
	glEnd();
	return p;
}

sexpr *draw_adjoin(sexpr *p, sexpr *sp) {
	sexpr *g = graphic$sexpr(sp);
	if (null(g))
		return p;
	return draw_graphic(draw_graphic(p, gcar(sp)), gcdr(sp));
}

sexpr *draw_adorn(sexpr *p, sexpr *sp) {
	sexpr *g = graphic$sexpr(sp);
	if (null(g))
		return p;
	draw_graphic(p, gcar(sp));
	draw_graphic(p, gcdr(sp));
	return p;
}
static sexpr *graphic_default_color;

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

sexpr *num2exp(double x) {
	sexpr *sp = malloc(sizeof(sexpr));
	sp->type = NUMBER;
	sp->u.x = x;
	return sp;
}

sexpr *color(double r, double g, double b) {
	return cons(num2exp(r), cons(num2exp(g), cons(num2exp(b), nil)));
}

void display_graphic() {
	sexpr *p = make_plumber(0.0, -0.66666666667, HALFPI);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	draw_plumber(p, graphic_default_color);
	p = draw_graphic(p, graphic_sp);
	draw_plumber(p, graphic_default_color);
	glFlush();

}

void draw_line(sexpr *lp, float sx, float sy) {

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glEnable(GL_LINE_SMOOTH);
	glBegin(GL_LINES);
	glVertex2f(line$x(lp)[0] / sx - 1.0, 1.0 - line$y(lp)[0] / sy);
	glVertex2f(line$x(lp)[1] / sx - 1.0, 1.0 - line$y(lp)[1] / sy);
	glEnd();
}

#define RGB 0
#define HOT 1

void draw_lines(sexpr *ls0, float sx, float sy, int color, float r, float g,
		float b) {

	sexpr *first, *ls;
	float contrast, avg, delta, total, stddev;
	int n;

	ls = ls0;
	total = 0.0;
	n = 0;
	while (!null(ls)) {
		total += line$contrast(pair$car(ls));
		n++;
		ls = pair$cdr(ls);
	}

	avg = total / n;

	ls = ls0;
	total = 0.0;
	while (!null(ls)) {
		delta = line$contrast(pair$car(ls)) - avg;
		total += delta * delta;
		ls = pair$cdr(ls);
	}

	stddev = avg + sqrt(total / n);

	ls = ls0;
	while (!null(ls)) {
		first = car(ls);
		if (color == HOT) {
			contrast = line$contrast(first);
			r = rhot(0.0, stddev, contrast);
			g = ghot(0.0, stddev, contrast);
			b = bhot(0.0, stddev, contrast);
		}
		glColor3f(r, g, b);
		draw_line(first, sx, sy);
		ls = cdr(ls);
	}
}

void draw_link(float x1, float y1, float x2, float y2, float sx, float sy) {

	float cos_theta, sin_theta, cos_phi, sin_phi, d, xc, yc, t;

	float r = 0.5F;

	double rad1;
	double rad2;

	d = sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));

	cos_theta = d / (r * 2.0);
	sin_theta = sin(acos(cos_theta));

	cos_phi = (x2 - x1) / d;
	sin_phi = (y2 - y1) / d;

	xc = x1 + r * (cos_theta * cos_phi - sin_theta * sin_phi);
	yc = y1 + r * (sin_theta * cos_phi + cos_theta * sin_phi);

	rad1 = atan2(y1 - yc, x1 - xc);
	rad2 = atan2(y2 - yc, x2 - xc);

	/*glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glEnable(GL_LINE_SMOOTH);*/

	if (angle_difference((double) rad1, (double) rad2) > angle_difference(
			(double) rad2, (double) rad1)) {
		glBegin(GL_LINE_STRIP);
		for (t = rad2; angle_difference((double) t, (double) rad1) > 0.05; t
				= t + 0.05)
			glVertex2f((xc + cos(t) * r) / sx - 1.0, 1.0 - (yc + sin(t) * r)
					/ sy);
		glEnd();
	} else {
		glBegin(GL_LINE_STRIP);
		for (t = rad1; angle_difference((double) t, (double) rad2) > 0.05; t
				= t + 0.05)
			glVertex2f((xc + cos(t) * r) / sx - 1.0, 1.0 - (yc + sin(t) * r)
					/ sy);
		glEnd();
	}
}

void draw_links(sexpr *ls, float sx, float sy) {
	sexpr *ln1, *ln2;
	sexpr *lns;
	int end1, end2;
	float x1, y1, x2, y2;

	glColor3f(1.0, 1.0, 1.0);

	if (!null(ls)) {
		for (end1 = 0; end1 <= 1; end1++) {
			ln1 = pair$car(ls);
			lns = line$links(ln1)[end1];
			while (!null(lns)) {
				ln2 = pair$car(lns);
				end2 = opposite(end1);
				x1 = line$x(ln1)[end1];
				y1 = line$y(ln1)[end1];
				x2 = line$x(ln2)[end2];
				y2 = line$y(ln2)[end2];
				draw_link(x1, y1, x2, y2, sx, sy);
				lns = pair$cdr(lns);
			}
		}
		draw_links(pair$cdr(ls), sx, sy);
	}
}

void display_sketch() {

	sexpr *sp = sketch_sp;
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLineWidth(1.0);
	draw_lines(sketch$ls(sp), sketch$cols(sp) / 2.0, sketch$rows(sp) / 2.0,
			HOT, 0.0, 0.0, 0.0);
	glFlush();

}

void display_link_graph() {
	sexpr *sp = sketch_sp;
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLineWidth(1.0);
	draw_lines(sketch$ls(sp), sketch$cols(sp) / 2.0, sketch$rows(sp) / 2.0,
			RGB, 0.0, 0.0, 1.0);
	draw_links(sketch$ls(sp), sketch$cols(sp) / 2.0, sketch$rows(sp) / 2.0);
	glFlush();

}

int main(int argc, char** argv) {

	int type, r, c;
	char text[STRLEN];
	sexpr sp;
	sketch sk;
	int show_link;
	double color_array[3];
	sexpr* current;
	int len;
	int i;
	fread(&image_display_scale, sizeof(double), 1, stdin);

	nil = malloc(sizeof(sexpr));
	nil->type = NIL;

	true = malloc(sizeof(sexpr));
	true->type = BOOLEAN;
	true->u.i = 1;
	false = malloc(sizeof(sexpr));
	false->type = BOOLEAN;
	false->u.i = 0;

	fread(&sp, sizeof(sexpr), 1, stdin);

	type = sp.type;
	glutInit(&argc, argv);
	//	glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);

	switch (type) {
	case IMAGE:
		fread(&img, sizeof(image), 1, stdin);
		r = img.rows;
		c = img.cols;

		sprintf(text, "#<image: rows = %d cols = %d>", r, c);

		img.data = (float*) malloc(sizeof(float) * img.rows * img.cols);
		fread(img.data, sizeof(float), img.rows * img.cols, stdin);

		glutInitWindowSize(c * image_display_scale, r * image_display_scale);
		glutInitWindowPosition(0, 0);
		glutCreateWindow(text);
		glutDisplayFunc(display_image);

		glutMainLoop();

	case COLOR_IMAGE:
		fread(&img, sizeof(image), 1, stdin);
		r = img.rows;
		c = img.cols;

		sprintf(text, "#<color-image: rows = %d cols = %d>", r, c);

		img.data = (float*) malloc(sizeof(float) * img.rows * img.cols * 3);
		fread(img.data, sizeof(float), img.rows * img.cols * 3, stdin);
		glutInitWindowSize(c * image_display_scale, r * image_display_scale);
		glutInitWindowPosition(0, 0);
		glutCreateWindow(text);
		glutDisplayFunc(display_color_image);

		glutMainLoop();

	case COMPLEX_IMAGE:
		fread(&img, sizeof(image), 1, stdin);
		sprintf(text, "#<complex-image: rows = %d cols = %d>", sk.rows, sk.cols);

		img.data = (float*) malloc(sizeof(float) * img.rows * img.cols * 3);
		fread(img.data, sizeof(float), img.rows * img.cols * 3, stdin);
		glutInitWindowSize(c * image_display_scale, r * image_display_scale);
		glutInitWindowPosition(0, 0);
		glutCreateWindow(text);
		glutDisplayFunc(display_color_image);

		glutMainLoop();

	case SKETCH:
		fread(&show_link, sizeof(int), 1, stdin);
		fread(color_array, sizeof(double), 3, stdin);
		fread(&sk, sizeof(sketch), 1, stdin);
		graphic_default_color = cons(num2exp(color_array[0]), cons(num2exp(
				color_array[1]), cons(num2exp(color_array[2]), nil)));

		r = sk.rows;
		c = sk.cols;
		if (show_link == 0) {
			sprintf(text, "#<sketch: rows = %d cols = %d count = %d>", r, c,
					sketch$count(&sk));

		} else {
			sprintf(text, "%s", "link graph");
		}
		sk.ls = read_line_list(sk.count, nil);
		sk.ls = reverse(sk.ls);

		current = sk.ls;
		for (i = 0; i < sk.count; i++) {
			fread(&len, sizeof(int), 1, stdin);
		//	fprintf(stderr, "Reading %i links\n", len);
			((line*) (car(current)))->links[0] = reverse(read_line_list(len,
					nil));
			fread(&len, sizeof(int), 1, stdin);
		//	fprintf(stderr, "Reading %i links\n", len);
			((line*) (car(current)))->links[1] = reverse(read_line_list(len,
					nil));

			current = cdr(current);
		}

		//fprintf(stderr, "Creating window (r,c)=(%i,%i)\n", r, c);
		sketch_sp = (sexpr*) &sk;
		glutInitWindowSize(c * image_display_scale, r * image_display_scale);
		glutInitWindowPosition(0, 0);
		glutCreateWindow(text);
		if (show_link == 0) {
			glutDisplayFunc(display_sketch);
		} else {
			glutDisplayFunc(display_link_graph);
		}

		glutMainLoop();


	case GRAPHIC:
		fread(color_array, sizeof(double), 3, stdin);
		graphic_default_color = cons(num2exp(color_array[0]), cons(num2exp(
				color_array[1]), cons(num2exp(color_array[2]), nil)));
		symbol_nil = str2symbol("nil");
		symbol_straight = str2symbol("straight");
		symbol_spot = str2symbol("spot");
		symbol_transparent = str2symbol("transparent");
		symbol_bend = str2symbol("bend");
		symbol_adjoin = str2symbol("adjoin");
		symbol_adorn = str2symbol("adorn");
		symbol_text = str2symbol("text");

		graphic_sp = read_generic_sexpr();

		sprintf(text, "#<graphic:%s>", symbol$name(gtype(graphic_sp)));
		glutInitWindowSize(700,700);
		glutCreateWindow(text);
		glutDisplayFunc(display_graphic);
		glutMainLoop();

	}
	fprintf(stderr, "exited glutMainLoop!!!!!\n");
	exit(EXIT_SUCCESS);

	return 0;
}
