;;; Follows compute_float and the table of powers of five of
;;; https://github.com/fastfloat/fast_float (commit dbeabee), which is
;;; available under this notice, among others:
;;;
;;; Copyright (c) 2021 The fast_float authors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(define eisel-lemire-limb-mask #x1FFFFF)
(define eisel-lemire-q-min -342)
(define eisel-lemire-table
  (let-syntax
    ([table
      (lambda (x)
        (define (power-of-five q)
          (if (< q 0)
              (let* ([p (expt 5 (- q))]
                     [z (bitwise-length p)])
                (if (>= q -27)
                    (+ (quotient (expt 2 (+ z 127)) p) 1)
                    (let loop ([c (+ (quotient (expt 2 (+ (* 2 z) 128)) p) 1)])
                      (if (>= c (expt 2 128)) (loop (quotient c 2)) c))))
              (let loop ([p (expt 5 q)])
                (cond
                  [(< p (expt 2 127)) (loop (* p 2))]
                  [(>= p (expt 2 128)) (loop (quotient p 2))]
                  [else p]))))
        (syntax-case x ()
          [(_ lo hi)
           (let ([lo (syntax->datum #'lo)] [hi (syntax->datum #'hi)])
             (let ([v (make-fxvector (* 7 (+ 1 (- hi lo))))])
               (do ([q lo (+ q 1)]) ((> q hi))
                 (do ([i 0 (+ i 1)] [t (power-of-five q) (bitwise-arithmetic-shift-right t 21)]) ((= i 7))
                   (fxvector-set! v (+ (* 7 (- q lo)) i) (bitwise-and t #x1FFFFF))))
               #`'#,(datum->syntax #'lo v)))]))])
    (table -342 308)))

;; w must be a positive fixnum and q from -342 to 308. w is multiplied by
;; the 128-bit truncation of 5^q in 21-bit limbs, so that every
;; intermediate is a fixnum; for these ranges the product's high bits round
;; correctly without a fallback (Mushtak and Lemire, "Fast number parsing
;; without fallback", 2023).
(define (eisel-lemire w q)
  (let* ([lz (fx- 64 (fxlength w))]
         [w (fxsll w (fx- lz 4))]
         [base (fx* 7 (fx- q eisel-lemire-q-min))]
         [w0 (fxand w eisel-lemire-limb-mask)]
         [w1 (fxand (fxsrl w 21) eisel-lemire-limb-mask)]
         [w2 (fxsrl w 42)]
         [t0 (fxvector-ref eisel-lemire-table base)]
         [t1 (fxvector-ref eisel-lemire-table (fx+ base 1))]
         [t2 (fxvector-ref eisel-lemire-table (fx+ base 2))]
         [t3 (fxvector-ref eisel-lemire-table (fx+ base 3))]
         [t4 (fxvector-ref eisel-lemire-table (fx+ base 4))]
         [t5 (fxvector-ref eisel-lemire-table (fx+ base 5))]
         [t6 (fxvector-ref eisel-lemire-table (fx+ base 6))]
         [p0 (fx* w0 t0)]
         [p1 (fx+ (fx+ (fx* w0 t1) (fx* w1 t0)) (fxsrl p0 21))]
         [p2 (fx+ (fx+ (fx* w0 t2) (fx* w1 t1)) (fx+ (fx* w2 t0) (fxsrl p1 21)))]
         [p3 (fx+ (fx+ (fx* w0 t3) (fx* w1 t2)) (fx+ (fx* w2 t1) (fxsrl p2 21)))]
         [p4 (fx+ (fx+ (fx* w0 t4) (fx* w1 t3)) (fx+ (fx* w2 t2) (fxsrl p3 21)))]
         [p5 (fx+ (fx+ (fx* w0 t5) (fx* w1 t4)) (fx+ (fx* w2 t3) (fxsrl p4 21)))]
         [p6 (fx+ (fx+ (fx* w0 t6) (fx* w1 t5)) (fx+ (fx* w2 t4) (fxsrl p5 21)))]
         [p7 (fx+ (fx+ (fx* w1 t6) (fx* w2 t5)) (fxsrl p6 21))]
         [p8 (fx+ (fx* w2 t6) (fxsrl p7 21))]
         [l5 (fxand p5 eisel-lemire-limb-mask)]
         [l6 (fxand p6 eisel-lemire-limb-mask)]
         [x (fxior (fxsrl l6 2) (fxsll (fxand p7 eisel-lemire-limb-mask) 19) (fxsll p8 40))]
         [upperbit (fxsrl x 59)]
         [shift (fx+ upperbit 9)]
         [mantissa (fxsrl x (fx- shift 4))]
         [power2 (fx+ (fx+ (fxsra (fx* 217706 q) 16) 63) (fx+ (fx- upperbit lz) 1023))])
    (if (fx<= power2 0)
        (if (fx>= (fx+ (fx- power2) 1) 60)
            0.0
            (let* ([m (fxsrl mantissa (fx+ (fx- power2) 1))]
                   [m (fxsrl (fx+ m (fxand m 1)) 1)])
              (fl* (fixnum->flonum m) (flvector-ref powers-of-two 0))))
        (let* ([mantissa (if (and (fx<= -4 q 23)
                                  (fx= (fxand mantissa 3) 1)
                                  (fx= 0 (fxsrl (fxand p2 eisel-lemire-limb-mask) 19))
                                  (fx= 0 (fxand p3 eisel-lemire-limb-mask))
                                  (fx= 0 (fxand p4 eisel-lemire-limb-mask))
                                  (fx= 0 (fxand l5 #x7FFFF))
                                  (fx= 0 (fxsrl l5 19))
                                  (fx= 0 (fxand l6 3))
                                  (fx= 0 (fxand x (fx- (fxsll 1 (fx- shift 4)) 1))))
                             (fxand mantissa (fxnot 1))
                             mantissa)]
               [mantissa (fxsrl (fx+ mantissa (fxand mantissa 1)) 1)]
               [overflow? (fx>= mantissa (fxsll 2 52))]
               [mantissa (if overflow? (fxsll 1 52) mantissa)]
               [power2 (if overflow? (fx+ power2 1) power2)])
          (if (fx>= power2 2047)
              +inf.0
              (fl* (fixnum->flonum mantissa) (flvector-ref powers-of-two (fx- power2 1))))))))
