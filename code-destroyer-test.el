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
   (equal (cdg-char-buf-body (cdg-make-char-buffer 3 2 ?x))
          "xxxxxx"))
  (should
   (equal (cdg-char-buf-body (cdg-make-char-buffer 0 2 ?x))
          ""))
  (should
   (equal (cdg-char-buf-body (cdg-make-char-buffer 2 0 ?x))
          "")))

(ert-deftest cdg-make-char-buffer-by-string ()
  (should
   (equal (cdg-make-char-buffer-by-string "xxxxxx" 2 ?x)
          (cdg-make-char-buffer 3 2 ?x)))
  (should
   (equal (cdg-make-char-buffer-by-string "" 4 ?x)
          (cdg-make-char-buffer 0 4 ?x)))
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
   (not (equal (cdg-mirror-vector [0.5 -0.5] 'vertical)
               (cdg-mirror-vector [0.5 -0.5] 'horizontal))))
  (should
   (equal (cdg-mirror-vector
           (cdg-mirror-vector [0.5 -0.5] 'horizontal)
           'horizontal)
          [0.5 -0.5])))

(ert-deftest cdg-erase-char-buffer ()
  (should
   (equal (cdg-erase-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                 ?t)
          (cdg-make-char-buffer 4 4 ?t)))
  (should
   (equal (cdg-erase-char-buffer (cdg-make-char-buffer 0 4 ?x)
                                 ?t)
          (cdg-make-char-buffer 0 4 ?x))))

(ert-deftest cdg-2d-to-1d-inx ()
  (let ((test-buffer (cdg-make-char-buffer 2 4 ?x)))
    (should
     (equal (cdg-2d-to-1d-inx test-buffer -1 3)
            nil))
    (should
     (equal (cdg-2d-to-1d-inx test-buffer 1 -1)
            nil))
    (should
     (equal (cdg-2d-to-1d-inx test-buffer 1 2)
            6))))

(ert-deftest cdg-invert-vector ()
  (let ((test-vec [2.0 -1.7]))
    (should
     (equal (cdg-mirror-vector
             (cdg-mirror-vector test-vec 'vertical)
             'horizontal)
            (cdg-invert-vector test-vec)))))

(ert-deftest cdg-platform-move ()
  (should
   (equal (cdg-platform-pos (cdg-platform-move (cdg-make-platform 10.0 5 0.1 ?x) 5.0))
          15.0))
  (should
   (equal (cdg-platform-pos (cdg-platform-move (cdg-make-platform 10.0 5 0.1 ?x) -5.0))
          5.0))
  (should
   (equal (cdg-platform-pos (cdg-platform-move (cdg-make-platform 10.0 5 0.1 ?x) -15.0))
          0)))

(ert-deftest cdg-platform-move-to ()
  (should
   (equal (cdg-platform-pos (cdg-platform-move-to (cdg-make-platform 10.0 5 0.1 ?x) 5.0))
          5.0))
  (should
   (equal (cdg-platform-pos (cdg-platform-move-to (cdg-make-platform 10.0 5 0.1 ?x) -5.0))
          10.0)))
