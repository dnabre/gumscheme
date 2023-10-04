/* Complex number routines.
;
;	Copyright (C) 2006			Lance R. Williams,
;				     			University of New Mexico
; 	All Rights Reserved.

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation version
; 2.1 of the License.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
; 02110-1301, USA

*/


#include <math.h>

typedef struct FCOMPLEX {float r,i;} fcomplex;

static fcomplex root_minus_one = {0.0, 1.0};
static fcomplex two_root_minus_one = {0.0, 2.0};
static fcomplex one = {1.0, 0.0};
static fcomplex minus_root_minus_one = {0.0, -1.0};
static fcomplex halfpi = {1.570796326794, 0.0};


fcomplex Cadd(fcomplex a, fcomplex b) {
  fcomplex c;
  c.r=a.r+b.r;
  c.i=a.i+b.i;
  return c;
}

fcomplex Csub(fcomplex a, fcomplex b) {
  fcomplex c;
  c.r=a.r-b.r;
  c.i=a.i-b.i;
  return c;
}


fcomplex Cmul(fcomplex a, fcomplex b) {
  fcomplex c;
  c.r=a.r*b.r-a.i*b.i;
  c.i=a.i*b.r+a.r*b.i;
  return c;
}

fcomplex Complex(float re, float im) {
  fcomplex c;
  c.r=re;
  c.i=im;
  return c;
}

fcomplex Conjg(fcomplex z) {
  fcomplex c;
  c.r=z.r;
  c.i = -z.i;
  return c;
}

fcomplex Cdiv(fcomplex a, fcomplex b) {
  fcomplex c;
  float r, den;
  if (fabs(b.r) >= fabs(b.i)) {
    r=b.i/b.r;
    den=b.r+r*b.i;
    c.r=(a.r+r*a.i)/den;
    c.i=(a.i-r*a.r)/den;
  } else {
    r=b.r/b.i;
    den=b.i+r*b.r;
    c.r=(a.r*r+a.i)/den;
    c.i=(a.i*r-a.r)/den;
  }
  return c;
}

float Cmag(fcomplex z) {
  float x, y;
  x=fabs(z.r);
  y=fabs(z.i);
  return sqrt(x*x + y*y);
}


float Cabs(fcomplex z) {
  float x, y, ans, temp;
  x=fabs(z.r);
  y=fabs(z.i);
  if (x == 0.0)
    ans=y;
  else if (y == 0.0)
    ans=x;
  else if (x > y) {
    temp=y/x;
    ans=x*sqrt(1.0+temp*temp);
  } else {
    temp=x/y;
    ans=y*sqrt(1.0+temp*temp);
  }
  return ans;
}

fcomplex Csqrt(fcomplex z) {
  fcomplex c;
  float x, y, w, r;
  if ((z.r == 0.0) && (z.i == 0.0)) {
    c.r=0.0;
    c.i=0.0;
    return c;
  } else {
    x=fabs(z.r);
    y=fabs(z.i);
    if (x >= y) {
      r=y/x;
      w=sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
    } else {
      r=x/y;
      w=sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
    }
    if (z.r >= 0.0) {
      c.r=w;
      c.i=z.i/(2.0*w);
    } else {
      c.i=(z.i >= 0) ? w : -w;
      c.r=z.i/(2.0*c.i);
    }
    return c;
  }
}

fcomplex RCmul(float x, fcomplex a) {
  fcomplex c;
  c.r=x*a.r;
  c.i=x*a.i;
  return c;
}

fcomplex RCadd(float x, fcomplex a) {
  fcomplex c;
  c.r=x + a.r;
  c.i=a.i;
  return c;
}

fcomplex Clog(fcomplex x) {
  return Complex(log(Cabs(x)),atan2(x.i,x.r));
}

fcomplex Cexp(fcomplex x) {
  return RCmul(exp(x.r),Complex(cos(x.i),sin(x.i)));
}

fcomplex Csin(fcomplex z) {
  fcomplex z1 = Complex(cos(z.r),sin(z.r));
  fcomplex z2 = Complex(z1.r,-z1.i);
  return Cdiv(Csub(RCmul(exp(-z.i),z1),RCmul(exp(z.i),z2)),two_root_minus_one);
}

fcomplex Ccos(fcomplex z) {
  fcomplex z1 = Complex(cos(z.r),sin(z.r));
  fcomplex z2 = Complex(z1.r,-z1.i);
  return RCmul(0.5,Cadd(RCmul(exp(-z.i),z1),RCmul(exp(z.i),z2)));
}

fcomplex Casin(fcomplex z) {
  fcomplex z2 = Complex(z.r*z.r - z.i*z.i, 2*z.r*z.i);
  return Cmul(minus_root_minus_one,Clog(Cadd(Cmul(root_minus_one,z),Csqrt(Csub(one,z2)))));
}

fcomplex Cacos(fcomplex z) {
  fcomplex z2 = Complex(z.r*z.r - z.i*z.i, 2*z.r*z.i);
  return Csub(halfpi,Cmul(minus_root_minus_one,Clog(Cadd(Cmul(root_minus_one,z),Csqrt(Csub(one,z2))))));
}



