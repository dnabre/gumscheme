; Macros and Functions for basic language features.
;
;	Copyright (C) April 21, 2006 	Lance R. Williams,
;				     				University of New Mexico
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

(define-macro car
  (lambda (x)
    `(,car ,x)))

(define-macro cdr
  (lambda (x)
    `(,cdr ,x)))

(define-macro cons
  (lambda (x y)
    `(,cons ,x ,y)))

(scheme:define caar
  (lambda (x)
    (car (car x))))

(define-macro caar
  (lambda (x)
    `(,car (,car ,x))))

(scheme:define cadr
  (lambda (x)
    (car (cdr x))))

(define-macro cadr
  (lambda (x)
    `(,car (,cdr ,x))))

(scheme:define cdar
  (lambda (x)
    (cdr (car x))))

(define-macro cdar
  (lambda (x)
    `(,cdr (,car ,x))))

(scheme:define cddr
  (lambda (x)
    (cdr (cdr x))))

(define-macro cddr
  (lambda (x)
    `(,cdr (,cdr ,x))))

(scheme:define caddr
  (lambda (x)
    (car (cdr (cdr x)))))

(define-macro caddr
  (lambda (x)
    `(,car (,cdr (,cdr ,x)))))

(scheme:define cadar
  (lambda (x)
    (car (cdr (car x)))))

(define-macro cadar
  (lambda (x)
    `(,car (,cdr (,car ,x)))))

(scheme:define list
  (lambda args args))

(define-macro begin
  (lambda args
    (if (null? args)
	(void)
	(if (null? (cdr args))
	    (car args)
	    `(scheme:begin ,@args)))))

(define-macro cond
  (lambda args
    (if (null? args)
	'(void)
	(if (null? (cdr args))
	    `(if ,(car (car args))
		 (begin ,@(cdr (car args)))
		 ,(void))
	    (if (eq? (car (cadr args)) 'else) 
		`(if ,(car (car args))
		    (begin ,@(cdr (car args)))
		    (begin ,@(cdr (cadr args)))) 
		`(if ,(car (car args))
		     (begin ,@(cdr (car args))) 
		     (cond ,@(cdr args))))))))

(define-macro and
  (lambda args
    (if (null? args)
	#t
	(if (null? (cdr args))
	    (car args)
	    `(if ,(car args)
		 (and ,@(cdr args))
		 #f)))))

(define-macro or
  (lambda args
    (if (null? args)
	#f
	(if (null? (cdr args))
	    (car args)
	    `(if ,(car args)
		 ,(car args)
		 (or ,@(cdr args)))))))

(scheme:define member
  (lambda (item ls)
    (if (null? ls)
	#f
	(if (equal? item (car ls))
	    ls
	    (member item (cdr ls))))))

(scheme:define memq
  (lambda (item ls)
    (if (null? ls)
	#f
	(if (eq? item (car ls))
	    ls
	    (memq item (cdr ls))))))

(define-macro define
  (lambda (template . body)
    (if (symbol? template)
	`(scheme:define ,template ,@body)
	`(scheme:define ,(car template)
	   (lambda ,(cdr template)
	       ,@body)))))

(define-macro case
  (lambda (msg . args)
    (if (null? args)
	'(void)
	(let* ((first (car args))
	       (body (cdr first)))
	  (if (eq? (car first) 'else)
	      (if (null? (cdr body))
		  (car body)
		  `(begin ,@(cdr first)))
	      `(if (member ,msg (quote ,(car first)))
		   ,(if (null? (cdr body))
			(car body)
			`(begin ,@(cdr first)))
		   (case ,msg ,@(cdr args))))))))

(define-macro delay
  (lambda args
    (let ((already-forced (gensym)) (result (gensym)))
      `(let ((,already-forced #f) (,result 0))
	 (lambda ()
	   (if ,already-forced
	       ,result
	       (begin
		 (set! ,already-forced #t)
		 (set! ,result ,(if (null? (cdr args))
				    (car args)
				    `(begin ,@args)))
		 ,result)))))))

(scheme:define force
  (lambda (f) (f)))

(scheme:define +
  (lambda args
    (scheme:reduce scheme:+ 0 args)))

(scheme:define *
  (lambda args
    (scheme:reduce scheme:* 1 args)))

(scheme:define -
  (lambda args
    (if (null? (cdr args))
	(scheme:- 0 (car args))
	(scheme:- 
	 (car args)
	 (scheme:reduce scheme:+ 0 (cdr args))))))

(scheme:define /
  (lambda args
    (if (null? (cdr args))
	(scheme:/ 1 (car args))
	(scheme:/
	 (car args)
	 (scheme:reduce scheme:* 1 (cdr args))))))

(scheme:define =
  (lambda args
    (scheme:reduce-pred scheme:= args)))

(scheme:define <
  (lambda args
    (scheme:reduce-pred scheme:< args)))

(scheme:define >
  (lambda args
    (scheme:reduce-pred scheme:> args)))

(scheme:define <=
  (lambda args
    (scheme:reduce-pred scheme:<= args)))

(scheme:define >=
  (lambda args
    (scheme:reduce-pred scheme:>= args)))

(scheme:define list?
  (lambda (s)
    (or (null? s)
	(and (pair? s) (list? (cdr s))))))

(scheme:define equal?
  (lambda (s1 s2)
    (if (and (pair? s1) (pair? s2))
	(and (equal? (car s1) (car s2))
	     (equal? (cdr s1) (cdr s2)))
	(eq? s1 s2))))

(scheme:define append
  (lambda args
    (scheme:reduce scheme:append () args)))

(scheme:define map
  (lambda (proc . args)
    (if (null? (car args))
	()
	(cons (apply proc (scheme:map car args))
	      (apply map proc (scheme:map cdr args))))))

(scheme:define for-each
  (lambda (proc . args)
    (if (null? (car args))
	(void)
	(begin 
	  (apply
	   proc
	   (scheme:map car args))
	  (apply
	   for-each
	   (cons proc (scheme:map cdr args)))))))

(define-macro do
  (lambda (vars final . body)
    (let ((loop (gensym)))
      `(letrec
	 ((,loop
	   (lambda ,(scheme:map car vars)
	     (if ,(car final)
		 ,(if (null? (cdr final))
		      `(void)
		      (cadr final))
		 (begin
		   ,@body
		   (,loop ,@(map caddr vars)))))))
	 (,loop ,@(map cadr vars))))))

(define-macro defmacro
  (lambda (call-template . body)
    `(define-macro ,(car call-template)
       (lambda ,(cdr call-template)
	 ,@body))))

(scheme:define reverse
  (lambda (ls)
    (letrec
      ((loop
	(lambda (ls acc)
	  (if (null? ls)
	      acc
	      (loop (cdr ls) (cons (car ls) acc))))))
      (loop ls ()))))

(scheme:define assoc
  (lambda (key ls)
    (if (null? ls)
	#f
	(if (equal? key (car (car ls)))
	    (car ls)
	    (assoc key (cdr ls))))))

(scheme:define assq
  (lambda (key ls)
    (if (null? ls)
	#f
	(if (eq? key (car (car ls)))
	    (car ls)
	    (assq key (cdr ls))))))

(define-macro add1
  (lambda (x) `(,scheme:+ ,x 1)))

(scheme:define add1
  (lambda (x) (scheme:+ x 1)))

(define-macro sub1
  (lambda (x) `(,scheme:- ,x 1)))

(scheme:define sub1
  (lambda (x) (scheme:- x 1)))

(scheme:define list-ref
  (lambda (ls n)
    (if (= n 0)
	(car ls)
	(list-ref (cdr ls) (sub1 n)))))

(scheme:define length
  (lambda (ls)		  
    (letrec
      ((loop		  
	(lambda (ls acc)
	  (if (null? ls)
	      acc
	      (loop (cdr ls) (add1 acc))))))
      (loop ls 0))))

(scheme:define iota
  (lambda (x)
    (do ((i x (sub1 i)) (acc () (cons i acc)))
	((= i 0) acc))))

(scheme:define remainder
  (lambda (x y)
    (- x (* (floor (/ x y)) y))))

(scheme:define max
  (lambda args
    (scheme:reduce scheme:max (car args) args)))

(scheme:define min
  (lambda args
    (scheme:reduce scheme:min (car args) args)))

(scheme:define string-append
  (lambda args
    (scheme:reduce scheme:string-append "" args)))

(scheme:define string->list
  (lambda (s)
    (let ((len (string-length s)))
      (letrec
	((loop
	  (lambda (n)
	    (if (= n len)
		()
		(cons (string-ref s n)
		      (loop (add1 n)))))))
	(loop 0)))))

(scheme:define list->string
  (lambda (ls)
    (cond ((null? ls) "")
	  ((null? (cdr ls))
	   (character->string (car ls)))
	  (else
	   (scheme:string-append
	    (character->string (car ls))
	    (list->string (cdr ls)))))))

(scheme:define string
  (lambda args
    (list->string args)))

(scheme:define string=?
  (lambda args
    (scheme:reduce-pred scheme:string=? args)))

(scheme:define string<?
  (lambda args
    (scheme:reduce-pred scheme:string<? args)))

(scheme:define string>?
  (lambda args
    (scheme:reduce-pred scheme:string>? args)))

(scheme:define string<=?
  (lambda args
    (scheme:reduce-pred scheme:string<=? args)))

(scheme:define string>=?
  (lambda args
    (scheme:reduce-pred scheme:string>=? args)))

(scheme:define char=?
  (lambda args
    (scheme:reduce-pred scheme:char=? args)))

(scheme:define char<?
  (lambda args
    (scheme:reduce-pred scheme:char<? args)))

(scheme:define char>?
  (lambda args
    (scheme:reduce-pred scheme:char>? args)))

(scheme:define char<=?
  (lambda args
    (scheme:reduce-pred scheme:char<=? args)))

(scheme:define char>=?
  (lambda args
    (scheme:reduce-pred scheme:char>=? args)))

(scheme:define char-alphabetic?
  (lambda (char)
    (or (and (scheme:char>=? char #\A) (scheme:char<=? char #\Z))
	(and (scheme:char>=? char #\a) (scheme:char<=? char #\z)))))

(scheme:define char-numeric?
  (lambda (char)
    (and (scheme:char>=? char #\0) (scheme:char<=? char #\9))))

(scheme:define inexact->exact (lambda (x) x))
(scheme:define exact->inexact (lambda (x) x))

(scheme:define select
  (lambda (pred ls1 ls2)
    (if (null? ls1)
        ()
        (if (pred (car ls1))
            (cons (car ls2)
                  (select pred (cdr ls1) (cdr ls2)))
            (select pred (cdr ls1) (cdr ls2))))))

(scheme:define filter
  (lambda (pred ls)
    (cond ((null? ls) ())
          ((pred (car ls))
           (cons (car ls) (filter pred (cdr ls))))
          (else
           (filter pred (cdr ls))))))

(scheme:define delete
  (lambda (item ls)
    (filter (lambda (x) (not (eq? x item))) ls)))

(scheme:define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(scheme:define iterate
  (lambda (f n)
    (if (= n 0)
	(lambda (x) x)
	(compose f (iterate f (sub1 n))))))

(scheme:define even?
  (lambda (x)
    (= (remainder x 2) 0)))

(scheme:define odd?
  (lambda (x)
    (not (even? x))))

(scheme:define make-vector
  (lambda (size . args)
    (if (null? args)
	(scheme:make-vector size 0)
	(scheme:make-vector size (car args)))))

(scheme:define vector
  (lambda args
    (list->vector args)))

(scheme:define vector-map
  (lambda (proc . args)
    (if (not (scheme:reduce-pred scheme:= (map vector-length args)))
	(error 'vector-map "Vector arguments must be of equal length.")
	(let* ((m (vector-length (car args)))
	       (v (scheme:make-vector m ())))
	  (do ((i 0 (scheme:+ i 1))) ((scheme:= i m) v)
	    (vector-set! v i (apply proc (map (lambda (u) (vector-ref u i)) args))))))))

(scheme:define make-array
  (lambda (init . dims)
    (let ((n0 (car dims))
	  (n1 (cdr dims)))
      (if (null? n1)
	  (make-vector n0 init)
	  (let ((A (scheme:make-vector n0 ())))
	    (do ((i 0 (add1 i))) ((= i n0) A)
	      (vector-set! A i (apply make-array init n1))))))))

(scheme:define array-ref
  (lambda (A . indices)
    (let ((i0 (car indices))
	  (i1 (cdr indices)))
      (if (null? i1)
	  (vector-ref A i0)
	  (apply array-ref (vector-ref A i0) i1)))))

(scheme:define array-set!
  (lambda (A value . indices)
    (let ((i0 (car indices))
	  (i1 (cdr indices)))
      (if (null? i1)
	  (vector-set! A i0 value)
	  (apply array-set! (vector-ref A i0) value i1)))))

(scheme:define array->list
  (lambda (A)
    (if (vector? A)
	(map array->list (vector->list A))
	A)))

(scheme:define array-dimensions
  (lambda (A)
    (if (vector? A)
	(let ((B (vector-ref A 0)))
	  (cons (vector-length A) (array-dimensions B)))
	())))

(scheme:define array-map
  (lambda (proc . args)
    (let ((A (car args)))
      (if (vector? (vector-ref A 0))
	  (let* ((m (vector-length A))
		 (v (scheme:make-vector m ())))
	    (do ((i 0 (add1 i))) ((= i m) v)
	      (vector-set! v i (apply array-map	proc (map (lambda (u) (vector-ref u i)) args)))))
	  (apply vector-map proc args)))))

(scheme:define make-filter
  (lambda (rows cols . init)
    (if (null? init)
       (scheme:make-image rows cols (lambda (i j) 0) #f)
       (scheme:make-image rows cols (car init) #f))))

(scheme:define make-image
  (lambda (rows cols . init)
    (if (null? init)
       (scheme:make-image rows cols (lambda (i j) 0) #t)
       (scheme:make-image rows cols (car init) #t))))

(scheme:define image-crop
  (lambda (image row0 col0 rows cols)
    (cond ((color-image? image)
	   (apply
	    rgb->color-image
	    (map (lambda (x) (scheme:image-crop x row0 col0 rows cols))
		 (color-image->rgb image))))
	  ((complex-image? image)
	   (complex (scheme:image-crop (real-part image) row0 col0 rows cols)
		    (scheme:image-crop (imag-part image) row0 col0 rows cols)))
	  (else
	   (scheme:image-crop image row0 col0 rows cols)))))

(scheme:define image-pad
  (lambda (image rows cols)
    (cond ((color-image? image)
	   (apply
	    rgb->color-image
	    (map (lambda (x) (scheme:image-pad x rows cols))
		 (color-image->rgb image))))
	  ((complex-image? image)
	   (complex (scheme:image-pad (real-part image) rows cols)
		    (scheme:image-pad (imag-part image) rows cols)))
	  (else
	   (scheme:image-pad image rows cols)))))
  
(scheme:define image-mean
  (lambda (x)
    (/ (image-sum x) (image-rows x) (image-cols x))))

(scheme:define downsample
  (lambda (x)
    (downsample-rows (downsample-cols x))))

(scheme:define upsample
   (lambda (x)
     (upsample-rows (upsample-cols x))))

(scheme:define left-to-right
  (lambda args
    (if (null? (cdr args))
	(car args)
	(scheme:left-to-right
	 (car args)
	 (apply left-to-right (cdr args))))))

(scheme:define top-to-bottom
  (lambda args
    (if (null? (cdr args))
	(car args)
	(scheme:top-to-bottom
	 (car args)
	 (apply top-to-bottom (cdr args))))))

(scheme:define color-image->rgb
  (lambda (x)
    `(,(color-image-red x)
      ,(color-image-green x)
      ,(color-image-blue x))))

(scheme:define color-image->hsi
  (lambda (x)
    (apply rgb->hsi (color-image->rgb x))))

(scheme:define hsi->color-image
  (lambda (h s i)
    (apply rgb->color-image (hsi->rgb h s i))))

(scheme:define complex-image->rectangular
  (lambda (x)
    `(,(real-part x) ,(imag-part x))))

(scheme:define complex-image->polar
  (lambda (x)
    `(,(magnitude x) ,(angle x))))

(scheme:define read
  (lambda port
    (if (pair? port)
	(scheme:read (car port))
	(scheme:read scheme:current-input-port))))

(scheme:define write
  (lambda (x . port)
    (if (pair? port)
	(scheme:write x (car port))
	(scheme:write x scheme:current-output-port))))

(scheme:define display
  (lambda (x . port)
    (if (pair? port)
	(scheme:display x (car port))
	(scheme:display x scheme:current-output-port))))

(scheme:define newline
  (lambda port
    (if (pair? port)
	(scheme:newline (car port))
	(scheme:newline scheme:current-output-port))))

(scheme:define read-char
  (lambda port
    (if (pair? port)
	(scheme:read-char (car port))
	(scheme:read-char scheme:current-input-port))))

(scheme:define write-char
  (lambda (c . port)
    (if (pair? port)
	(scheme:write-char c (car port))
	(scheme:write-char c scheme:current-output-port))))

(scheme:define peek-char
  (lambda port
    (if (pair? port)
	(scheme:peek-char (car port))
	(scheme:peek-char scheme:current-input-port))))

(scheme:define current-input-port
  (lambda ()
    scheme:current-input-port))

(scheme:define current-output-port
  (lambda ()
    scheme:current-output-port))

(scheme:define fprintf
  (lambda (port format . args)
    (scheme:fprintf port format (apply vector args))))

(scheme:define printf
  (lambda (format . args)
    (scheme:fprintf
     scheme:current-output-port
     format
     (apply vector args))))

(scheme:define scanf
  (lambda (format)
    (fscanf scheme:current-input-port format)))

(define-macro name-parts-of
  (lambda (ls names . body)
    `(apply (lambda ,names ,@body) ,ls)))

(scheme:define make-traceable
  (lambda (proc name)
    (letrec
      ((tab
	(lambda (n)
	  (if (scheme:= n 1)
	      ""
	      (scheme:string-append
	       "|  "
	       (tab (scheme:- n 1)))))))
      (let ((c 1))
	(lambda args
	  (display (tab c))
	  (display (cons name args))
	  (newline)
	  (set! c (scheme:+ c 1))
	  (let ((value (apply proc args)))
	    (set! c (scheme:- c 1))
	    (display (tab c))
	    (display value)
	    (newline)
	    value))))))

(define-macro trace
  (lambda (name)
    `(set! ,name (make-traceable ,name (quote ,name)))))

(scheme:define error
  (lambda (x . y)
    (if (null? y)
	(begin
	  (if (symbol? x) (display "error: "))
	  (display x)
	  (newline)
	  (scheme:error))
	(begin
	  (display x)
	  (if (symbol? x) (display ":"))
	  (for-each (lambda (x) (display " ") (display x)) y)
	  (newline)
	  (scheme:error)))))

(define-macro stream-cons
  (lambda (x y)
    `(cons ,x (delay ,y))))

(scheme:define stream-car car)

(scheme:define stream-cdr
  (lambda (x)
    (force (cdr x))))

(scheme:define stream-ref
  (lambda (x n)
    (if (scheme:= n 0)
      (stream-car x)
      (stream-ref (stream-cdr x) (- n 1)))))

(define-macro stream-append
  (letrec
    ((loop
      (lambda (x y)
	(if (null? x)
	    (force y)
	    (stream-cons
	     (stream-car x)
	     (loop (stream-cdr x) y))))))
    (lambda (x y)
      `(,loop ,x (delay ,y)))))

(scheme:define stream-map
  (lambda (proc . args)
    (if (null? (car args))
	(stream)
	(stream-cons
	 (apply proc (map stream-car args))
	 (apply stream-map proc (map stream-cdr args))))))

(scheme:define stream-for-each
  (lambda (proc . args)
    (if (null? (car args))
	(void)
	(begin
	  (apply proc (map stream-car args))
	  (apply stream-for-each proc (map stream-cdr args))))))

(scheme:define stream-filter
  (lambda (pred x)
    (if (null? x)
      (stream)
      (let ((y (stream-car x)))
        (if (pred y)
          (stream-cons y (stream-filter pred (stream-cdr x)))
          (stream-filter pred (stream-cdr x)))))))

(scheme:define stream
  (lambda args
    (list->stream args)))

(scheme:define list->stream
  (lambda (ls)
    (if (null? ls)
	()
	(stream-cons (car ls)
		     (list->stream (cdr ls))))))

(scheme:define stream->list
  (lambda (x)
    (if (null? x)
      ()
      (cons (stream-car x) 
	    (stream->list (stream-cdr x))))))

(scheme:define stream-delete
  (lambda (item x)
    (stream-filter (lambda (y) (not (eq? y item)))
		   x)))

(scheme:define file->stream
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (letrec
        ((build-input-stream
          (lambda ()
            (let ((c (read-char port)))
              (if (eof-object? c)
                  (begin
                    (close-input-port port)
                    ())
                  (stream-cons 
                   c
                   (build-input-stream)))))))
        (build-input-stream)))))

(scheme:define stream->file
  (lambda (filename)
    (lambda (str)
      (let ((port (open-output-file filename)))
        (letrec
          ((write-stream
            (lambda (str)
              (if (not (null? str))
                  (begin
                    (write-char (stream-car str) port)
                    (write-stream (stream-cdr str)))))))
          (write-stream str)
          (close-output-port port))))))

(scheme:define expr->string
  (lambda (s)
    (letrec
      ((cdr->string
	(lambda (ls)
	  (cond ((null? ls) "")
		((pair? ls)
		 (let ((rest (cdr ls)))
		   (string-append
		    " "
		    (expr->string (car ls))
		    (cond ((null? rest) "")
			  ((pair? rest) (cdr->string rest))
			  (else
			   (scheme:string-append
			    " . "
			    (expr->string rest)))))))
		(else
		 (scheme:string-append
		  " . "
		  (expr->string ls)))))))
      (cond ((null? s) "()")
	    ((pair? s)
	     (string-append
	      "("
	      (expr->string (car s))
	      (cdr->string (cdr s))
	      ")"))
	    ((vector? s)
	     (scheme:string-append "#" (expr->string (vector->list s))))
	    ((number? s)
	     (number->string s))
	    ((symbol? s)
	     (symbol->string s))
	    ((char? s)
	     (case s
	       ((#\newline) "#\newline")
	       ((#\space) "#\space")
	       ((#\tab) "#\tab")
	       (else
		(scheme:string-append (string #\# #\\) (string s)))))
	    ((string? s)
	     (string-append (string #\") s (string #\")))
	    ((boolean? s)
	     (if s "#t" "#f"))
	    (else
	     (error 'expr->string "Unreadable type."))))))

(define-macro +
  (lambda args
    (cond ((null? args) 0)
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:+ ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce ,scheme:+ 0 (list ,@args))))))

(define-macro -
  (lambda args
    (cond ((null? args) 0)
	  ((null? (cdr args))
	   `(,scheme:- 0 ,(car args)))
	  ((null? (cddr args))
	   `(,scheme:- ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:- ,(car args) (+ ,@(cdr args)))))))

(define-macro *
  (lambda args
    (cond ((null? args) 1)
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:* ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce ,scheme:* 1 (list ,@args))))))

(define-macro /
  (lambda args
    (cond ((null? args) 1)
	  ((null? (cdr args))
	   `(,scheme:/ 1 ,(car args)))
	  ((null? (cddr args))
	   `(,scheme:/ ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:/ ,(car args) (* ,@(cdr args)))))))

(define-macro max
  (lambda args
    (cond ((null? args)
	   `(error "max: Expects more than zero arguments."))
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:max ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:max ,(car args)
			 (max ,@(cdr args)))))))

(define-macro min
  (lambda args
    (cond ((null? args)
	   `(error "min: Expects more than zero arguments."))
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:min ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:min ,(car args)
			 (min ,@(cdr args)))))))

(define-macro =
  (lambda args
    (cond ((null? args)
	   `(error "=: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:= ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:= (list ,@args))))))

(define-macro <=
  (lambda args
    (cond ((null? args)
	   `(error "<=: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:<= ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:<= (list ,@args))))))

(define-macro >=
  (lambda args
    (cond ((null? args)
	   `(error ">=: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:>= ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:>= (list ,@args))))))

(define-macro <
  (lambda args
    (cond ((null? args)
	   `(error "<: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:< ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:< (list ,@args))))))

(define-macro >
  (lambda args
    (cond ((null? args)
	   `(error ">: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:> ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:> (list ,@args))))))

(define-macro append
  (lambda args
    (cond ((null? args) ())
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:append ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce ,scheme:append () (list ,@args))))))

(define-macro string-append
  (lambda args
    (cond ((null? args) "")
	  ((null? (cdr args)) (car args))
	  ((null? (cddr args))
	   `(,scheme:string-append ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce ,scheme:string-append "" (list ,@args))))))

(define-macro string=?
  (lambda args
    (cond ((null? args)
	   `(error "string=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:string=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:string=? (list ,@args))))))

(define-macro string<=?
  (lambda args
    (cond ((null? args)
	   `(error "string<=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:string<=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:string<=? (list ,@args))))))

(define-macro string>=?
  (lambda args
    (cond ((null? args)
	   `(error "string>=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:string>=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:string>=? (list ,@args))))))

(define-macro string<?
  (lambda args
    (cond ((null? args)
	   `(error "string<?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:string<? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:string<? (list ,@args))))))

(define-macro string>?
  (lambda args
    (cond ((null? args)
	   `(error " string>?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:string>? ,(car args) ,(cadr args)))
	  (else
	   `(and (,scheme:string>? ,(car args) ,(cadr args))
		 (string>? ,@(cdr args)))))))

(define-macro char=?
  (lambda args
    (cond ((null? args)
	   `(error "char=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:char=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:char=? (list ,@args))))))

(define-macro char<=?
  (lambda args
    (cond ((null? args)
	   `(error "char<=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:char<=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:char<=? (list ,@args))))))

(define-macro char>=?
  (lambda args
    (cond ((null? args)
	   `(error "char>=?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:char>=? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:char>=? (list ,@args))))))

(define-macro char<?
  (lambda args
    (cond ((null? args)
	   `(error "char<?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:char<? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:char<? (list ,@args))))))

(define-macro char>?
  (lambda args
    (cond ((null? args)
	   `(error " char>?: Expects more than zero arguments."))
	  ((null? (cdr args)) #t)
	  ((null? (cddr args))
	   `(,scheme:char>? ,(car args) ,(cadr args)))
	  (else
	   `(,scheme:reduce-pred ,scheme:char>? (list ,@args))))))

(scheme:define apply
  (lambda (proc args)
    (apply proc args)))
  
(scheme:define fft
  (lambda (v)
    (cond ((vector? v) (scheme:fft v -1))
	  ((or (image? v) (complex-image? v))
	   (scheme:image-fft v -1))
	  (else
	   (error "ifft: Argument must be vector or image.")))))

(scheme:define ifft
  (lambda (v)
    (cond ((vector? v) (scheme:fft v 1))
	  ((or (image? v) (complex-image? v))
	   (scheme:image-fft v 1))
	  (else
	   (error "fft: Argument must be vector or image.")))))

(scheme:define image-map
  (lambda (proc . args)
    (apply scheme:image-map proc (list (list->vector args)))))

(define-macro atan
  (lambda (y . x)
    (if (null? x)
	`(,scheme:atan ,y 1)
	`(,scheme:atan ,y ,(car x)))))

(scheme:define atan
  (lambda (y . x)
    (if (null? x)
	(scheme:atan y 1)
	(scheme:atan y (car x)))))

(scheme:define make-covariance-matrix
  (lambda args
    (scheme:make-covariance-matrix
     (apply vector args))))

(define (fold proc A . id)
  (define (loop ls acc)
    (if (null? ls)
	acc
	(loop (cdr ls) (proc (car ls) acc))))
  (let ((ls (image->list A)))
    (if (null? id)
	(loop (cdr ls) (car ls))
	(loop ls (car id)))))

(define (convolve-rows x y)
  (convolve x (array->image (vector y))))

(define (convolve-cols x y)
  (convolve x (array->image (array-transpose (vector y)))))

(define array-rows
  (lambda (m)
    (vector-length m)))

(define array-cols
  (lambda (m)
    (vector-length (vector-ref m 0))))

(define array-transpose
  (lambda (A)
    (let* ((m (array-rows A))
	   (n (array-cols A))
	   (B (make-array () n m)))
      (do ((i 0 (+ i 1))) ((= i m) B)
	(do ((j 0 (+ j 1))) ((= j n))
	  (array-set! B (array-ref A i j) j i))))))

(define (erode A . element)
  (if (null? element)
      (= (convolve A #(#(1 1) #(1 1))) 4)
      (let ((c (fold + (array->image (car element)))))
	(= (convolve A (car element)) c))))

(define (dilate A . element)
  (if (null? element)
      (> (convolve A #(#(1 1) #(1 1))) 0)
      (> (convolve A (car element)) 0)))

(define (open A . element)
  (apply dilate (apply erode A element) element))

(define (close A . element)
  (apply erode (apply dilate A element) element))

(define (outline A . labels)
  (if (null? labels)
      (scheme:outline A 1 0)
      (scheme:outline A (car labels) (cadr labels))))

(define (write-image A fname . rest)
  (let ((greylevels (if (null? rest) 256 (car rest))))
    (scheme:write-image (* (- greylevels 1) (image-normalize A)) fname)))

(define (write-color-image A fname . rest)
  (define (scale greylevels)
    (lambda (im) (* (- greylevels 1) (image-normalize im))))
  (let ((greylevels (if (null? rest) 256 (car rest))))
    (scheme:write-color-image
     (apply rgb->color-image (map (scale greylevels) (color-image->rgb A)))
     fname)))

(define (gv ps)
  (cond ((graphic? ps)
	 (graphic->postscript ps "graphic.ps")
	 (system "gv graphic.ps &")
	 (void))
	((sketch? ps)
	 (sketch->postscript ps "sketch.ps")
      	 (system "gv sketch.ps &")
	 (void))
	(else
	 (error "Input must be convertable to postscript."))))

(define (xv im)
  (cond ((image? im)
         (write-image im "unmscheme.pgm")
         (system "xv unmscheme.pgm &")
         (void))
        ((color-image? im)
         (write-color-image im "unmscheme.ppm")
         (system "xv unmscheme.ppm &")
         (void))
	((complex-image? im)
	 (xv (complex-image->color-image im)))
	(else
	 (error "Input must be image."))))

(define (register-images im1 im2 dr dc . rest)
  (if (null? rest)
      (scheme:register-images im1 im2 dr dc 0 0)
      (scheme:register-images im1 im2 dr dc (car rest) (cadr rest))))

(define pi (acos -1))
