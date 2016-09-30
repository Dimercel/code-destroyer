(require 'ert)
(require 'code-destroyer)


(ert-deftest cdg-to-length ()
  (should
   (equal (cdg-to-length "test string" 4 ? )
          "test"))
  (should
   (equal (cdg-to-length "" 4 ?x)
          "xxxx"))
  (should
   (equal (cdg-to-length "test string" 0 ?x)
          "")))

(ert-deftest cdg-make-char-buffer ()
  (should
   (equal (cdg-make-char-buffer 3 2 ?x)
          (list "xxxxxx" 3 2)))
  (should
   (equal (cdg-make-char-buffer 0 2 ?x)
          (list "" 0 0)))
  (should
   (equal (cdg-make-char-buffer 2 0 ?x)
          (list "" 0 0))))

(ert-deftest cdg-make-char-buffer-by-string ()
  (should
   (equal (cdg-make-char-buffer-by-string "xxxxxx" 2 ?x)
          (list "xxxxxx" 3 2)))
  (should
   (equal (cdg-make-char-buffer-by-string "" 4 ?x)
          (list "" 0 0)))
  (should
   (equal (cdg-make-char-buffer-by-string "xxxx" 2 ?x)
          (cdg-make-char-buffer 2 2 ?x)))
  (should
   (equal (cdg-make-char-buffer-by-string "xxx" 2 ?x)
          (cdg-make-char-buffer 2 2 ?x))))

(ert-deftest cdg-resize-char-buffer ()
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  6
                                  6
                                  ?x)
          (cdg-make-char-buffer 6 6 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  0
                                  ?x)
          (cdg-make-char-buffer 0 0 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  2
                                  ?x)
          (cdg-make-char-buffer 0 2 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  0
                                  ?x)
          (cdg-make-char-buffer 0 0 ?x))))

(ert-deftest cdg-char-buf-row-count ()
  (should
   (equal (cdg-char-buf-row-count (cdg-make-char-buffer 10 8 ?x))
          10)))

(ert-deftest cdg-char-buf-col-count ()
  (should
    (equal (cdg-char-buf-col-count (cdg-make-char-buffer 10 8 ?x))
          8)))

(ert-deftest cdg-char-buf-size ()
  (let ((test-buffer (cdg-make-char-buffer 10 8 ?x)))
    (should
     (equal (* (cdg-char-buf-row-count test-buffer)
               (cdg-char-buf-col-count test-buffer))
            (cdg-char-buf-size test-buffer)))))

(ert-deftest cdg-mirror-vector ()
  (should
   (equal (cdg-mirror-vector [0.5 -0.5] 'horizontal)
          [0.5 0.5]))
  (should
   (equal (cdg-mirror-vector [0.5 -0.5] 'vertical)
          [-0.5 -0.5]))
  (should
   (not
    (equal (cdg-mirror-vector [0.5 -0.5] 'vertical)
           (cdg-mirror-vector [0.5 -0.5] 'horizontal))))
  (should
   (equal (cdg-mirror-vector
           (cdg-mirror-vector [0.5 -0.5] 'vertical)
           'vertical)
          [0.5 -0.5]))
  (should
   (equal (cdg-mirror-vector
           (cdg-mirror-vector [0.5 -0.5] 'horizontal)
           'horizontal)
          [0.5 -0.5])))
