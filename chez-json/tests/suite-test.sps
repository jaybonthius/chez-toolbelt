(import (chezscheme) (json) (check))

(define suite "tests/JSONTestSuite/test_parsing")

(define i-accepted
  '("i_number_double_huge_neg_exp.json"
    "i_number_real_underflow.json"
    "i_number_too_big_neg_int.json"
    "i_number_too_big_pos_int.json"
    "i_number_very_big_negative_int.json"
    "i_structure_500_nested_arrays.json"
    "i_structure_UTF-8_BOM_empty_object.json"))

(define i-refused
  '("i_number_huge_exp.json"
    "i_number_neg_int_huge_exp.json"
    "i_number_pos_double_huge_exp.json"
    "i_number_real_neg_overflow.json"
    "i_number_real_pos_overflow.json"
    "i_object_key_lone_2nd_surrogate.json"
    "i_string_1st_surrogate_but_2nd_missing.json"
    "i_string_1st_valid_surrogate_2nd_invalid.json"
    "i_string_incomplete_surrogate_and_escape_valid.json"
    "i_string_incomplete_surrogate_pair.json"
    "i_string_incomplete_surrogates_escape_valid.json"
    "i_string_invalid_lonely_surrogate.json"
    "i_string_invalid_surrogate.json"
    "i_string_invalid_utf-8.json"
    "i_string_inverted_surrogates_U+1D11E.json"
    "i_string_iso_latin_1.json"
    "i_string_lone_second_surrogate.json"
    "i_string_lone_utf8_continuation_byte.json"
    "i_string_not_in_unicode_range.json"
    "i_string_overlong_sequence_2_bytes.json"
    "i_string_overlong_sequence_6_bytes.json"
    "i_string_overlong_sequence_6_bytes_null.json"
    "i_string_truncated-utf-8.json"
    "i_string_utf16BE_no_BOM.json"
    "i_string_utf16LE_no_BOM.json"
    "i_string_UTF-16LE_with_BOM.json"
    "i_string_UTF-8_invalid_sequence.json"
    "i_string_UTF8_surrogate_U+D800.json"))

(define (file-bytes name)
  (let ([bv (call-with-port (open-file-input-port (string-append suite "/" name))
              get-bytevector-all)])
    (if (eof-object? bv) (bytevector) bv)))

(define (file-text bv)
  (guard (c [(i/o-decoding-error? c) #f])
    (bytevector->string bv
      (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))))

(define (whitespace-only? bv)
  (for-all (lambda (b) (memv b '(32 9 10 13))) (bytevector->u8-list bv)))

(define (bom-then-whitespace? bv)
  (let ([units (bytevector->u8-list bv)])
    (and (>= (length units) 3)
         (equal? '(#xEF #xBB #xBF) (list-head units 3))
         (whitespace-only? (u8-list->bytevector (list-tail units 3))))))

(define (port-values port)
  (let loop ([acc '()])
    (let ([v (json-read port)])
      (if (eof-object? v) (reverse acc) (loop (cons v acc))))))

(define (refused? thunk)
  (guard (c [(json-error? c)
             (let ([p (json-error-position c)])
               (and (fixnum? p) (fx>= p 0)))])
    (thunk)
    #f))

(define (port-refuses? bv)
  (let ([port (open-bytevector-input-port bv)])
    (guard (c [(json-error? c) #t])
      (let ([v (json-read port)])
        (if (eof-object? v)
            (or (whitespace-only? bv) (bom-then-whitespace? bv))
            (let ([rest (get-bytevector-all port)])
              (and (bytevector? rest) (not (whitespace-only? rest)))))))))

(define names (sort string<? (directory-list suite)))
(define (named prefix) (filter (lambda (n) (string=? (substring n 0 2) prefix)) names))

(define counts (make-eq-hashtable))
(define (count! key) (hashtable-update! counts key (lambda (n) (+ n 1)) 0))

(test "the suite has its 95 y_, 188 n_ and 35 i_ files"
  (check-equal? 95 (length (named "y_")))
  (check-equal? 188 (length (named "n_")))
  (check-equal? 35 (length (named "i_")))
  (check-equal? (sort string<? (append i-accepted i-refused)) (named "i_")))

(define duplicate-key-files
  '("y_object_duplicated_key.json" "y_object_duplicated_key_and_value.json"))

(define (read-everywhere name)
  (let* ([bv (file-bytes name)] [text (file-text bv)] [v (bytevector->json bv)])
    (check text)
    (check-equal? v (string->json text))
    (check-equal? (list v) (port-values (open-bytevector-input-port bv)))
    (check-equal? (list v) (port-values (open-string-input-port text)))
    (check-equal? v (string->json (json->string v)))
    (check-equal? v (bytevector->json (json->bytevector v)))
    (equal? v (bytevector->json (json->bytevector v)))))

(test "y_ files are accepted and written back, except those with duplicate keys"
  (for-each
    (lambda (name)
      (if (member name duplicate-key-files)
          (check (refused? (lambda () (bytevector->json (file-bytes name)))))
          (begin
            (count! 'y-accepted)
            (when (read-everywhere name) (count! 'y-written-back)))))
    (named "y_")))

(test "y_ files are all accepted and written back when duplicate keys are allowed"
  (parameterize ([json-allow-duplicate-keys? #t])
    (for-each
      (lambda (name)
        (count! 'y-accepted-duplicates)
        (when (read-everywhere name) (count! 'y-written-back-duplicates)))
      (named "y_"))
    (check-equal? '#((("a" . "b") ("a" . "c")))
      (vector (bytevector->json (file-bytes "y_object_duplicated_key.json"))))))

(test "n_ files are refused"
  (for-each
    (lambda (name)
      (let ([bv (file-bytes name)])
        (check (refused? (lambda () (bytevector->json bv))))
        (when (refused? (lambda () (bytevector->json bv))) (count! 'n-refused))
        (let ([text (file-text bv)])
          (when text
            (check (refused? (lambda () (string->json text))))))
        ;; A port may hold several values, so a port read refuses the file
        ;; or stops before its end; it reads the end only of input that is
        ;; blank after a byte order mark.
        (check (port-refuses? bv))))
    (named "n_")))

(test "i_ files have their listed outcomes"
  (for-each
    (lambda (name)
      (let ([v (bytevector->json (file-bytes name))])
        (count! 'i-accepted)
        (check-equal? v (bytevector->json (json->bytevector v)))))
    i-accepted)
  (for-each
    (lambda (name)
      (let ([bv (file-bytes name)])
        (check (refused? (lambda () (bytevector->json bv))))
        (let ([text (file-text bv)])
          (when text (check (refused? (lambda () (string->json text))))))))
    i-refused))

(printf "y_ accepted ~a of 95, written back equal ~a; with duplicate keys allowed ~a and ~a; n_ refused ~a of 188; i_ accepted ~a of 35\n"
  (hashtable-ref counts 'y-accepted 0) (hashtable-ref counts 'y-written-back 0)
  (hashtable-ref counts 'y-accepted-duplicates 0) (hashtable-ref counts 'y-written-back-duplicates 0)
  (hashtable-ref counts 'n-refused 0) (hashtable-ref counts 'i-accepted 0))

(test-exit)
