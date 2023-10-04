; Plumbing Graphics Functions
;
;	Copyright (C) Aug. 16, 2003	Lance R. Williams
;				     			University of New Mexico
; 	All Rights Reserved.
;
; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation version 
; 2.1 of the License.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
; 02110-1301, USA

;; connect
(define adjoin
  (lambda args
    (if (null? args)
	scheme:graphic-nil
	(scheme:adjoin 
	 (car args)
	 (apply adjoin (cdr args))))))

;; attach
(define adorn
  (lambda args
    (if (null? args)
	scheme:graphic-nil
	(scheme:adorn 
	 (car args)
	 (apply adorn (cdr args))))))

(define straight
  (lambda (len . args)
    (if (null? args)
	(scheme:straight len)
	(if (null? (cdr args))
	    (scheme:fancy-straight len (car args) 1.0)
	    (scheme:fancy-straight len (car args) (cadr args))))))

(define spot
  (lambda (width . args)
    (if (null? args)
	(scheme:spot width)
	(scheme:fancy-spot width (car args)))))

(define text
  (lambda (x . args)
    (let* ((convert
	    (lambda (x)
	      (cond ((number? x) (number->string x))
		    ((symbol? x) (symbol->string x))
		    (else x))))
	   (txt (convert x)))
      (if (null? args)
	  (scheme:text txt)
	  (if (null? (cdr args))
	      (scheme:fancy-text txt (car args) 1.0)
	      (scheme:fancy-text txt (car args) (cadr args)))))))

(define gscale
  (lambda (g lscale . args)
    (if (null? args)
	(scheme:gscale g lscale 1.0)
	(scheme:gscale g lscale (car args)))))

(define st straight)
(define sp spot)
(define be bend)
(define adj adjoin)
(define ado adorn)
(define tr transparent)
(define txt text)
(define gc gcolor)
(define gs gscale)
(define gm gmirror)
(define gr greverse)

(define white scheme:graphic-white)
(define black scheme:graphic-black)
(define red scheme:graphic-red)
(define green scheme:graphic-green)
(define blue scheme:graphic-blue)
(define orange scheme:graphic-orange)

;; For backwards compatibility
(define graphic-color gcolor)
(define graphic-scale gscale)
(define graphic-mirror gmirror)
(define graphic-reverse greverse)
