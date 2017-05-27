(require 'ert)
(require 'code-destroyer-utils)
(require 'code-destroyer-geom)
(require 'code-destroyer-game)
(require 'code-destroyer)


;; Вспомогательные функции

(defun cdg-vector-len (vec)
  (cdg-point-dist (cdg-make-point 0 0)
                  (cdg-make-point-by-vec vec)))



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
  (should
   (equal (cdg-2d->1d 2 2 0) nil))
  (should
   (equal (cdg-2d->1d 1 2 4) 6)))



;;; Тесты геометрических функций



(ert-deftest cdg-make-point ()
  (should
   (and (floatp (cdg-point-x (cdg-make-point 1 1)))
        (floatp (cdg-point-y (cdg-make-point 1 1)))))
  (should
   (equal (cdg-make-point 1 1)
          (cdg-make-point-by-vec [1 1])))
  (should
   (and (= 2.0 (cdg-point-x (cdg-make-point 2 4)))
        (= 4.0 (cdg-point-y (cdg-make-point 2 4))))))


(ert-deftest cdg-make-point-by-vec ()
  (should
   (and (= -2.0 (cdg-point-x (cdg-make-point-by-vec [-2 -4])))
        (= -4.0 (cdg-point-y (cdg-make-point-by-vec [-2 -4])))))
  (should-error (cdg-make-point-by-vec [1])))

(ert-deftest cdg-point-xy ()
  (should
   (and (= 4.0 (cdg-point-x (cdg-make-point 4.0 2.0)))
        (= 2.0 (cdg-point-y (cdg-make-point 4.0 2.0))))))

(ert-deftest cdg-point-inc ()
  (should
   (equal (cdg-point-inc (cdg-make-point 1 1) 5)
          (cdg-make-point 6 6)))
  (should
   (equal (cdg-point-inc (cdg-make-point 42 7) 7 42)
          (cdg-make-point 49 49)))
  (should
   (equal (cdg-point-inc (cdg-make-point 1 1) -5)
          (cdg-make-point -4 -4)))
  (should
   (equal (cdg-point-inc (cdg-make-point 42 7) -42 -7)
          (cdg-make-point 0 0))))

(ert-deftest cdg-dist-between-points ()
  (let ((p1 (cdg-make-point 1 2.2))
        (p2 (cdg-make-point 3.3 -4)))
    (should
     (< (cdg-point-dist p1 p2)
        (cdg-point-dist-square p1 p2)))
    (should
     (and (= 0 (cdg-point-dist p1 p1))
          (= 0 (cdg-point-dist-square p2 p2))))))

(ert-deftest cdg-closest-point ()
  (should-not (cdg-closest-point (cdg-make-point 0 0) '()))
  (should
   (equal (cdg-closest-point (cdg-make-point 0 0)
                             (list (cdg-make-point 0 0)
                                   (cdg-make-point 1 1)
                                   (cdg-make-point -1 -1)))
          (cdg-make-point 0 0)))
  (should
   (equal (cdg-closest-point (cdg-make-point 0 0)
                             (list (cdg-make-point -0.5 -0.5)
                                   (cdg-make-point -1 -1)
                                   (cdg-make-point 1 1)))
          (cdg-make-point -0.5 -0.5))))

(ert-deftest cdg-point-hline-vline ()
  (let ((point (cdg-make-point 0 0)))
    (should (cdg-left-vline-p 1 point))
    (should (cdg-right-vline-p -1 point))
    (should (cdg-above-hline-p -1 point))
    (should (cdg-under-hline-p 1 point))

    (should-not (cdg-left-vline-p -1 point))
    (should-not (cdg-right-vline-p 1 point))
    (should-not (cdg-above-hline-p 1 point))
    (should-not (cdg-under-hline-p -1 point))

    ;; Если точка принадлежит прямой, то она
    ;; не левее, не выше, не ниже и не выше ее
    (should-not (cdg-left-vline-p 0 point))
    (should-not (cdg-right-vline-p 0 point))
    (should-not (cdg-above-hline-p 0 point))
    (should-not (cdg-under-hline-p 0 point))))

(ert-deftest cdg-normalize-vec ()
  ;; Вектор и его орт должны быть одинаковых знаков
  (should
   (apply #'cdg-same-sign-p
          (append
           (vconcat [1.0 2.0]
                    (cdg-normalize-vec [1.0 2.0]))
           nil)))
  (should
   (apply #'cdg-same-sign-p
          (append
           (vconcat [-1.0 -2.0]
                    (cdg-normalize-vec [-1.0 -2.0]))
           nil)))
  (should
   (< (cdg-vector-len (cdg-normalize-vec [5 5]))
      (cdg-vector-len [5 5]))))

(ert-deftest cdg-mirror-vector ()
  (should
   (equal (cdg-mirror-vector [-1 -1] 'horizontal)
          [-1 1]))
  (should
   (equal (cdg-mirror-vector [-1 -1] 'vertical)
          [1 -1]))
  (should
   (equal (cdg-mirror-vector [0 0] 'vertical)
          [0 0]))
  (should-not
    (equal (cdg-mirror-vector [0.5 -0.5] 'vertical)
           (cdg-mirror-vector [0.5 -0.5] 'horizontal)))

  (should
   (equal (cdg-mirror-vector
           (cdg-mirror-vector [0.5 -0.5] 'horizontal)
           'horizontal)
          [0.5 -0.5]))
  (should
   (equal (cdg-mirror-vector
           (cdg-mirror-vector [0.5 -0.5] 'vertical)
           'vertical)
          [0.5 -0.5])))

(ert-deftest cdg-invert-vector ()
  (should
   (equal [2.0 -1.7]
          (cdg-invert-vector
           (cdg-invert-vector [2.0 -1.7]))))
  (should
   (equal [0 0]
          (cdg-invert-vector [0 0]))))

(ert-deftest cdg-make-rect ()
  (let ((p1 (cdg-make-point -7 17.0))
        (p2 (cdg-make-point 5.5 -1.1)))
  (should
   (equal (cdg-make-rect p1 p2)
          (cdg-make-rect-by-2-points p2 p1)))))

(ert-deftest cdg-rect-accessors ()
  (let ((test-rect (cdg-make-rect (cdg-make-point -1 1)
                                  (cdg-make-point 2 -2))))
    (should
     (equal (cdg-rect-left-top test-rect)
            (cdg-make-point -1 1)))
    (should
     (equal (cdg-rect-right-bottom test-rect)
            (cdg-make-point 2 -2)))
    (should
     (equal (cdg-rect-min-x test-rect) -1.0))
    (should
     (equal (cdg-rect-max-x test-rect) 2.0))
    (should
     (equal (cdg-rect-min-y test-rect) -2.0))
    (should
     (equal (cdg-rect-max-y test-rect) 1.0))
    (should
     (equal (cdg-rect-width test-rect) 3.0))
    (should
     (equal (cdg-rect-height test-rect) 3.0))))

(ert-deftest cdg-rect-point-test ()
  (let ((test-rect (cdg-make-rect (cdg-make-point 0 1)
                                  (cdg-make-point 1 0))))
    ;; Точка внутри
    (should
     (cdg-rect-point-test test-rect (cdg-make-point 0.5 0.5)))
    ;; Точка снаружи
    (should-not
     (cdg-rect-point-test test-rect (cdg-make-point -0.5 -0.5)))
    ;; Точка принадлежит стороне прямоугольника
    (should
     (cdg-rect-point-test test-rect (cdg-make-point 0.5 1)))))

(ert-deftest cdg-contain-rect-test ()
  (let ((test-rect (cdg-make-rect (cdg-make-point 0 1)
                                  (cdg-make-point 1 0))))
    (should
     (cdg-contain-rect-test
      test-rect
      (cdg-make-rect (cdg-make-point 0.1 0.9)
                     (cdg-make-point 0.9 0.1))))
    (should-not
     (cdg-contain-rect-test
      test-rect
      (cdg-make-rect (cdg-make-point -1 10)
                     (cdg-make-point 10 -10))))
    (should
     (cdg-contain-rect-test test-rect test-rect))))

(ert-deftest cdg-vline-ray-inter-exist-p ()
  (should
   (cdg-vline-ray-inter-exist-p 0 [1 1] [-1 1]))
  (should-not
   (cdg-vline-ray-inter-exist-p 0 [1 1] [1 1]))
  (should
   (cdg-vline-ray-inter-exist-p 0 [0 1] [1 1]))
  ;; Случай параллельных прямых
  (should-not
   (cdg-vline-ray-inter-exist-p 0 [1 0] [0 1])))

(ert-deftest cdg-hline-ray-inter-exist-p ()
  (should
   (cdg-hline-ray-inter-exist-p 0 [-5 -5] [1 1]))
  (should-not
   (cdg-hline-ray-inter-exist-p 0 [-5 -5] [1 -1]))
  (should
   (cdg-hline-ray-inter-exist-p 0 [0 0] [1 1]))
  ;; Случай параллельных прямых
  (should-not
   (cdg-hline-ray-inter-exist-p 0 [0 1] [1 0])))

(ert-deftest cdg-hline-ray-intersection ()
  ;; нет пересечения
  (should-not
   (cdg-hline-ray-intersection 0 [1 1] [1 1]))
  ;; прямая выше
  (should
   (equal (cdg-hline-ray-intersection 1
                                      [0 0]
                                      (cdg-normalize-vec [1 1]))
          (cdg-make-point 1 1)))
  ;; прямая ниже
  (should
   (equal (cdg-hline-ray-intersection -1
                                      [5 0]
                                      (cdg-normalize-vec [-1 -1]))
          (cdg-make-point 4 -1)))
  ;; прямая и луч параллельны
  (should-not
   (cdg-hline-ray-intersection 0
                               [0 1]
                               (cdg-normalize-vec [1 0]))))

(ert-deftest cdg-vline-ray-intersection ()
  ;; нет пересечения
  (should-not
   (cdg-vline-ray-intersection 0 [1 1] [1 1]))
  ;; прямая левее
  (should
   (equal (cdg-vline-ray-intersection 0
                                      [7 1]
                                      (cdg-normalize-vec [-1 0]))
          (cdg-make-point 0 1)))
  ;; прямая правее
  (should
   (equal (cdg-vline-ray-intersection 0
                                      [-4 0]
                                      (cdg-normalize-vec [1 0]))
          (cdg-make-point 0 0)))
  ;; прямая и луч параллельны
  (should-not
   (cdg-vline-ray-intersection 0
                               [1 0]
                               (cdg-normalize-vec [0 1]))))

(ert-deftest cdg-rect-ray-intersection ()
  (let ((test-rect (cdg-make-rect (cdg-make-point -1 1)
                                  (cdg-make-point 1 -1))))
    ;; Луч внутри прямоугольника
    (should
     (equal (cdg-rect-ray-intersection test-rect
                                       [0 0]
                                       (cdg-normalize-vec [-1 1]))
            (cdg-make-point -1 1)))
    ;; Луч снаружи
    (should
     (equal (cdg-rect-ray-intersection test-rect
                                       [2 -1]
                                       (cdg-normalize-vec [-1 1]))
            (cdg-make-point 1 0)))
    ;; пересечение отсутствует
    (should-not
     (cdg-rect-ray-intersection test-rect
                                [10 0]
                                (cdg-normalize-vec [1 0])))))

(ert-deftest cdg-rect-point-side ()
  (let ((test-rect (cdg-make-rect (cdg-make-point -1 1)
                                  (cdg-make-point 1 -1))))
    (should
     (equal (cdg-rect-point-side test-rect (cdg-make-point 1 0))
            'vertical))
    (should
     (equal (cdg-rect-point-side test-rect (cdg-make-point 0 1))
            'horizontal))
    ;; нет пересечения
    (should-not
     (cdg-rect-point-side test-rect (cdg-make-point 0 0)))))



;;; Тесты для игровых объектов



(ert-deftest cdg-make-char-buffer ()
  (should
   (equal (cdg-make-char-buffer 3 0 ?x) nil))
  (should
   (equal (cdg-make-char-buffer 0 3 ?x) nil))
  (should
   (equal (cdg-make-char-buffer 0 0 ?x) nil)))

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
