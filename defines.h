/* unmscheme: constants and definitions
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

#ifndef DEFINES_H_
#define DEFINES_H_

#define PI     3.141592653589
#define TWOPI  6.283185307179
#define HALFPI 1.570796326794

#define S30 0.5
#define C30 0.86602540378
#define C45 0.70710678118

#define STRLEN 256

#define NIL 0
#define NUMBER 1
#define TOKEN 2
#define CHARACTER 3
#define STRING 4
#define VIRGIN 5
#define CLOSURE 6
#define PRIMITIVE 7
#define BOOLEAN 8
#define VECTOR 9
#define INPUT_PORT 10
#define OUTPUT_PORT 11
#define EOF_OBJECT 12
#define GRAPHIC 13
#define PLUMBER 14
#define IMAGE 15
#define COLOR_IMAGE 16
#define COMPLEX_IMAGE 17
#define LINE 18
#define SKETCH 19
#define CONTINUATION 20
#define COMPLEX 21
#define UNDEFINED 22
#define COPIED 23
#define SYMBOL 100
#define PAIR 200
#define HALT 401
#define LOCAL_REFER 402
#define GLOBAL_REFER 403
#define REFER 404
#define CONSTANT 405
#define CLOSE 406
#define RETURN 407
#define TEST 408
#define LOCAL_ASSIGN 409
#define GLOBAL_ASSIGN 410
#define ASSIGN 411
#define DEFINE 412
#define APPLY 413
#define FUNCALL 414
#define FRAME 415
#define ARGUMENT 416
#define CONTI 417
#define NUATE 418
#define THROW 419
#define CATCH 420

#define HASHSIZE 10001
#define SEXPRS 70000000
#define STACKSIZE 50000

#endif /* DEFINES_H_ */
