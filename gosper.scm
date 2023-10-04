;	Copyright (C) 2004-2006		Lance R. Williams,
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



(define gosper-step
  (lambda (side n)
    (adjoin
     (bend 15)
     side
     (bend -60)
     (gmirror (greverse side))
     (bend -120)
     (gmirror (greverse side))
     (bend 60)
     side
     (bend 120)
     side
     side
     (bend 60)
     (gmirror (greverse side))
     (bend -75))))

(define draw-gosper
  (lambda (n len)
    (if (= n 0)
        (straight len)
        (gosper-step (draw-gosper (- n 1) len) n))))

(define gosper
  (lambda (n)
    (adjoin
     (transparent 100)
     (bend -90)
     (transparent 100)
     (bend 180)
     (draw-gosper n (/ 20 n)))))
