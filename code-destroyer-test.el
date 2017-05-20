(require 'ert)
(require 'code-destroyer-utils)
(require 'code-destroyer-geom)
(require 'code-destroyer-game)
(require 'code-destroyer)



;;; Тесты для вспомогательных функций



(ert-deftest cdg-space-p ()
  (should (cdg-space-p +cdg-space-sym+))
  (should-not (cdg-space-p (+ +cdg-space-sym+ 1)))
  (should-not (cdg-space-p nil)))

(ert-deftest cdg-to-length ()
  (should
   (equal (cdg-to-length "test string" 4 ? )
          "test"))
  (should
   (equal (cdg-to-length "" 4 ?x)
          "xxxx"))
  (should
   (equal (cdg-to-length "test string" 0 ?x)
          ""))
  (should-error (cdg-to-length "" -1 ?x)
                :type 'args-out-of-range))

(ert-deftest cdg-same-sign-p ()
  (should (cdg-same-sign-p 1 2 3 4 5))
  (should (cdg-same-sign-p -1 -2 -3 -4 -5))
  (should-not (cdg-same-sign-p -1 2 -3)))

(ert-deftest cdg-2d->1d ()
  ;; Массив не содержит отрицательных индексов,
  ;; поэтому нужно вернуть nil
  (should
   (equal (cdg-2d->1d 2 2 0) nil))
  (should
   (equal (cdg-2d->1d 1 2 4) 6)))


(ert-deftest cdg-make-char-buffer ()
  (should
   (equal (cdg-make-char-buffer 3 0 ?x)
          nil))
  (should
   (equal (cdg-make-char-buffer 0 3 ?x)
          nil))
  (should
   (equal (cdg-make-char-buffer 0 0 ?x)
          nil)))

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

(ert-deftest cdg-char-buffer-rows ()
  (should
   (equal (cdg-char-buffer-rows (cdg-make-char-buffer 10 8 ?x))
          10)))

(ert-deftest cdg-char-buffer-cols ()
  (should
    (equal (cdg-char-buffer-cols (cdg-make-char-buffer 10 8 ?x))
          8)))

(ert-deftest cdg-char-buffer-size ()
  (let ((test-buffer (cdg-make-char-buffer 10 8 ?x)))
    (should
     (equal (* (cdg-char-buffer-rows test-buffer)
               (cdg-char-buffer-cols test-buffer))
            (cdg-char-buffer-size test-buffer)))))

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

(ert-deftest cdg-invert-vector ()
  (let ((test-vec [2.0 -1.7]))
    (should
     (equal (cdg-mirror-vector
             (cdg-mirror-vector test-vec 'vertical)
             'horizontal)
            (cdg-invert-vector test-vec)))))

(ert-deftest cdg-platform-move ()
  (let ((test-platform (cdg-make-platform 10.0 5.0 0.1 ?x)))
    (cdg-platform-move test-platform 5.0)
    (should
     (equal (cdg-platform-pos test-platform)
            15.0))
    (cdg-platform-move test-platform -5.0)
    (should
     (equal (cdg-platform-pos test-platform)
            10.0))))

(ert-deftest cdg-platform-move-to ()
  (let ((test-platform (cdg-make-platform 10.0 5.0 0.1 ?x)))
    (cdg-platform-move-to test-platform 5.0)
    (should
     (equal (cdg-platform-pos test-platform)
            5.0))
    (cdg-platform-move-to test-platform -5.0)
    (should
     (equal (cdg-platform-pos test-platform)
            -5.0))))
