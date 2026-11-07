;;; Follows DoubleToDecimal.java and MathUtils.java of
;;; https://github.com/c4f7fcce9cb06515/Schubfach (todec, commit 647fa97),
;;; which carry this notice:
;;;
;;; Copyright 2018-2020 Raffaello Giulietti
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

(define schubfach-k-min -324)
(define schubfach-limb-mask #x1FFFFF)

(define schubfach-g
  (let-syntax
    ([table
      (lambda (x)
        (define (g-of k)
          (let* ([power (expt 10 (- k))]
                 [r (let loop ([r (- (bitwise-length (floor power)) 126)])
                      (let ([b (floor (/ power (expt 2 r)))])
                        (cond
                          [(>= b (expt 2 126)) (loop (+ r 1))]
                          [(< b (expt 2 125)) (loop (- r 1))]
                          [else r])))])
            (+ (floor (/ power (expt 2 r))) 1)))
        (syntax-case x ()
          [(_ k-min k-max)
           (let ([k-min (syntax->datum #'k-min)] [k-max (syntax->datum #'k-max)])
             (let ([v (make-fxvector (* 6 (+ 1 (- k-max k-min))))])
               (do ([k k-min (+ k 1)]) ((> k k-max))
                 (do ([i 0 (+ i 1)] [g (g-of k) (bitwise-arithmetic-shift-right g 21)]) ((= i 6))
                   (fxvector-set! v (+ (* 6 (- k k-min)) i) (bitwise-and g #x1FFFFF))))
               #`'#,(datum->syntax #'k-min v)))]))])
    (table -324 292)))

(define (flog10pow2 q) (fxsra (fx* q 661971961083) 41))
(define (flog10-three-quarters-pow2 q) (fxsra (fx+ (fx* q 661971961083) -274743187321) 41))
(define (flog2pow10 e) (fxsra (fx* e 913124641741) 38))

;; Products with the 126-bit g are taken in 21-bit limbs, so that every
;; intermediate is a fixnum. Sums are written two terms at a time: fx+ with
;; four or more arguments is not inlined and allocates its rest list. As in
;; the original, the low 64 bits of g0 cp are dropped before the sum;
;; computing the product exactly gives wrong digits, because g is rounded
;; up.
(define (schubfach-rop base cp)
  (let* ([c0 (fxand cp schubfach-limb-mask)]
         [c1 (fxand (fxsrl cp 21) schubfach-limb-mask)]
         [c2 (fxsrl cp 42)]
         [g00 (fxvector-ref schubfach-g base)]
         [g01 (fxvector-ref schubfach-g (fx+ base 1))]
         [g02 (fxvector-ref schubfach-g (fx+ base 2))]
         [g10 (fxvector-ref schubfach-g (fx+ base 3))]
         [g11 (fxvector-ref schubfach-g (fx+ base 4))]
         [g12 (fxvector-ref schubfach-g (fx+ base 5))]
         [a0 (fx* g00 c0)]
         [a1 (fx+ (fx* g01 c0) (fx* g00 c1) (fxsrl a0 21))]
         [a2 (fx+ (fx+ (fx* g02 c0) (fx* g01 c1)) (fx+ (fx* g00 c2) (fxsrl a1 21)))]
         [a3 (fx+ (fx* g02 c1) (fx* g01 c2) (fxsrl a2 21))]
         [a4 (fx+ (fx* g02 c2) (fxsrl a3 21))]
         [s0 (fx+ (fx* g10 c0) (fxand a3 (fx- schubfach-limb-mask 1)))]
         [s1 (fx+ (fx+ (fx* g11 c0) (fx* g10 c1)) (fx+ (fxand a4 schubfach-limb-mask) (fxsrl s0 21)))]
         [s2 (fx+ (fx+ (fx* g12 c0) (fx* g11 c1))
                 (fx+ (fx* g10 c2) (fx+ (fxsrl a4 21) (fxsrl s1 21))))]
         [s3 (fx+ (fx* g12 c1) (fx* g11 c2) (fxsrl s2 21))]
         [s4 (fx+ (fx* g12 c2) (fxsrl s3 21))]
         [high (fxior (fxsrl (fxand s3 schubfach-limb-mask) 1) (fxsll s4 20))])
    (if (and (fx= 0 (fxand s0 (fx- schubfach-limb-mask 1)))
             (fx= 0 (fxand s1 schubfach-limb-mask))
             (fx= 0 (fxand s2 schubfach-limb-mask))
             (fx= 0 (fxand s3 1)))
        high
        (fxior high 1))))

;; Giulietti's Schubfach algorithm ("The Schubfach way to render doubles",
;; 2020): Chez's number->string finds the same digits with bignum
;; arithmetic, several times slower. The original tries one digit fewer
;; only from three digits up, because it prints at least two; trying it
;; from two digits up makes subnormals such as 1e-322 come out shortest.
(define (schubfach q c)
  (let* ([out (fxand c 1)]
         [cb (fxsll c 2)]
         [cbr (fx+ cb 2)]
         [regular? (or (not (fx= c #x10000000000000)) (fx= q -1074))]
         [cbl (if regular? (fx- cb 2) (fx- cb 1))]
         [k (if regular? (flog10pow2 q) (flog10-three-quarters-pow2 q))]
         [h (fx+ q (flog2pow10 (fx- k)) 2)]
         [base (fx* 6 (fx- k schubfach-k-min))]
         [vb (schubfach-rop base (fxsll cb h))]
         [vbl (schubfach-rop base (fxsll cbl h))]
         [vbr (schubfach-rop base (fxsll cbr h))]
         [s (fxsra vb 2)])
    (let ([sp10 (fx* 10 (fxquotient s 10))])
      (let ([upin (fx<= (fx+ vbl out) (fxsll sp10 2))]
            [wpin (fx<= (fx+ (fxsll (fx+ sp10 10) 2) out) vbr)])
        (if (and (fx>= s 10) (not (eq? upin wpin)))
            (values (if upin sp10 (fx+ sp10 10)) k)
            (let* ([t (fx+ s 1)]
                   [uin (fx<= (fx+ vbl out) (fxsll s 2))]
                   [win (fx<= (fx+ (fxsll t 2) out) vbr)])
              (if (not (eq? uin win))
                  (values (if uin s t) k)
                  (let ([cmp (fx- vb (fxsll (fx+ s t) 1))])
                    (values
                      (if (or (fx< cmp 0) (and (fx= cmp 0) (fx= 0 (fxand s 1)))) s t)
                      k)))))))))

(define (flonum->decimal x scratch)
  (bytevector-ieee-double-set! scratch 0 x (endianness big))
  (let* ([high (bytevector-u32-ref scratch 0 (endianness big))]
         [low (bytevector-u32-ref scratch 4 (endianness big))]
         [biased-exponent (fxsrl high 20)]
         [t (fxior (fxsll (fxand high #xFFFFF) 32) low)])
    (if (fx> biased-exponent 0)
        (let ([mq (fx- 1075 biased-exponent)] [c (fxior #x10000000000000 t)])
          (if (and (fx< 0 mq 53) (fx= c (fxsll (fxsrl c mq) mq)))
              (values (fxsrl c mq) 0)
              (schubfach (fx- mq) c)))
        ;; The algorithm needs more precision than the two smallest
        ;; subnormals give it.
        (case t
          [(1) (values 5 -324)]
          [(2) (values 1 -323)]
          [else (schubfach -1074 t)]))))
