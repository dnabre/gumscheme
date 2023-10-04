; boldt edge-detection algorithm
;
;	Copyright (C) 2003-2004		Lance R. Williams,
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



(define (zero-crossings image . method)
  (scheme:zero-crossings
   (convolve image #(#(-0.125 -0.125 -0.125)
		     #(-0.125  1.00  -0.125)
		     #(-0.125 -0.125 -0.125)))
   (convolve image #(#(0.25  0.00 -0.25)
		     #(0.50  0.00 -0.50)
		     #(0.25  0.00 -0.25)))
   (convolve image #(#(0.25  0.50  0.25)
		     #(0.00  0.00  0.00)
		     #(-0.25 -0.50 -0.25)))
   (if (pair? method) (car method) 0)))

(define (make-line x0 y0 x1 y1 . contrast)
  (scheme:make-line x0 y0 x1 y1 (if (null? contrast) 0.0 (car contrast))))

(define (filter-on-contrast sketch lower . upper)
  (scheme:filter-on-contrast
   sketch
   lower
   (if (pair? upper) (car upper) 256.0)))

(define (filter-on-length sketch lower . upper)
  (scheme:filter-on-length
   sketch
   lower
   (if (pair? upper) (car upper) 1000.0)))

(define (link-schedule n)
  (vector-ref
   #(#(#f    1.0   0.5   2.0   0.785398   0.6    2.0)
     #(#f    1.2   0.5   2.0   0.785398   0.5    2.0)
     #(#f    1.4   0.5   2.0   0.785398   0.4    2.0)
     #(#f    1.6   0.5   2.0   0.698132   0.35   2.0)
     #(#f    1.8   0.5   2.0   0.610865   0.30   2.0)
     #(#f    2.0   0.5   2.0   0.523599   0.25   2.0)
     #(#f    3.0   0.5   2.0   0.436332   0.20   2.0)
     #(#f    4.0   0.5   2.0   0.349066   0.15   2.0)
     #(#f    5.0   0.5   2.0   0.261799   0.1    2.0)
     #(#f    6.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f    7.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f    8.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f    9.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f   10.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f   11.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f   12.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f   13.0   0.5   2.0   0.174533   0.05   2.0)
     #(#f   14.0   0.5   2.0   0.174533   0.05   2.0))
   n))

(define (replace-schedule n)
  (vector-ref
   #(#(  1.2   0.005   #t   0.5   0.7   4   #t)
     #(  1.7   0.005   #t   0.5   0.7   4   #t)
     #(  2.5   0.005   #t   0.5   0.7   4   #t)
     #(  4.0   0.005   #t   0.5   0.7   4   #t)
     #(  6.0   0.005   #t   0.5   0.7   4   #t)
     #( 10.0   0.005   #t   0.6   0.8   4   #t)
     #( 16.0   0.005   #t   0.6   0.8   4   #t)
     #( 24.0   0.005   #t   0.6   0.8   4   #t)
     #( 36.0   0.005   #t   0.6   0.8   4   #t)
     #( 54.0   0.005   #t   0.6   0.8   4   #t)
     #( 81.0   0.005   #t   0.6   0.8   4   #t)
     #(120.0   0.005   #t   0.7   0.9   4   #t)
     #(180.0   0.005   #t   0.7   0.9   4   #t)
     #(270.0   0.005   #t   0.7   0.9   4   #t)
     #(270.0   0.005   #t   0.7   0.9   4   #t)
     #(270.0   0.005   #t   0.7   0.9   4   #t)
     #(270.0   0.005   #t   0.7   0.9   4   #t)
     #(270.0   0.005   #t   0.7   0.9   4   #t))
   n))

(define (replace-step! sketch n)
  (po-replace!
   (po-link! sketch (link-schedule n))
   (replace-schedule n)))

(define (link-replace-cycle! zcs m)
  (define (loop acc n)
    (if (scheme:> n m)
	acc
	(let ((sketch (replace-step! (car acc) n)))
	  (loop (cons sketch acc)
		(scheme:+ n 1)))))
  (loop (list zcs) 0))

(define (po-merge! sketches)
  (define (loop acc sketches)
    (if (null? sketches)
	acc
	(loop (scheme:po-merge! acc (car sketches))
	      (cdr sketches))))
  (loop (car sketches)
	(cdr sketches)))

(define (boldt filename . n)
  (po-merge!
   (link-replace-cycle!
    (zero-crossings (read-image filename) 1)
    (if (pair? n) (car n) 17))))

(define (write-sketch sketch filename)
  (let ((output-port (open-output-file filename)))
    (fprintf
     output-port
     "%f %f\n"
     (sketch-rows sketch)
     (sketch-cols sketch))
    (for-each 
     (lambda (line)
       (fprintf
	output-port 
	"%f\t%f\t%f\t%f\t%f\n"
	(line-x0 line)
	(line-y0 line)
	(line-x1 line)
	(line-y1 line)
	(line-contrast line)))
     (sketch-lines sketch))
    (close-output-port output-port)))

(define (read-sketch filename)
  (let ((input-port (open-input-file filename)))
    (define (loop acc)
      (let ((line (fscanf input-port "%f\t%f\t%f\t%f\t%f\n")))
	(if (eof-object? line)
	    acc
	    (loop (cons (apply make-line line) acc)))))
    (apply (lambda (rows cols)
	     (let ((result (make-sketch rows cols (loop ()))))
	       (close-input-port input-port)
	       result))
	   (fscanf input-port "%f %f\n"))))

(define (boldt-script)
  (let* ((input (prompt "Input filename (in quotes)" string?))
	 (output (prompt "Output filename (in quotes)" string?))
	 (output-type (prompt "Enter 0 for .txt or 1 for .ps" number? 1))
	 (lmin (prompt "Minimum length" number? 1.0))
	 (cmin (prompt "Minimum contrast" number? 1.0))
	 (n (prompt "Number of link-replace cycles" number? 17)))
    ((if (zero? output-type) write-sketch sketch->postscript)
     (filter-on-length
      (filter-on-contrast
       (po-merge! (link-replace-cycle! (zero-crossings (read-image input) 1) n))
       cmin)
      lmin)
     output)))

(define (prompt text type . default)
  (if (pair? default)
      (begin
	(display text)
	(display " (default = ")
	(display (car default))
	(display "): ")
	(if (char=? (peek-char) #\newline)
	    (begin
	      (read-char)
	      (car default))
	    (let ((response (read)))
	      (if (type response)
		  response
		  (apply prompt text type default)))))
      (begin
	(display text)
	(display ": ")
	(let ((response (read)))
	  (if (type response)
	      response
	      (prompt type text))))))
