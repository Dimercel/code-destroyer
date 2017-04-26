;;; Геометрические функции в Декартовой системе координат


(defun cdg-make-point (x y)
  "Создает точку в Декартовой системе координат"
  (vector x y))

(defun cdg-make-point-by-vec (vec)
  "Создает точку по 2-х мерному вектору"
  (cdg-make-point (aref vec 0) (aref vec 1)))

(defun cdg-point-x (point)
   (aref point 0))

(defun cdg-point-y (point)
  (aref point 1))

(defun cdg-point-inc (point inc &optional inc2)
  "Дает точке point приращение inc(скаляр)"
  (if (null inc2)
      (cdg-point-inc point inc inc)
      (cdg-make-point (+ (cdg-point-x point) inc)
                      (+ (cdg-point-y point) inc2))))

(defun cdg-point-dist-square (point1 point2)
  "Вернет квадрат расстояния между точками"
  (+ (expt (- (cdg-point-x point2) (cdg-point-x point1)) 2)
     (expt (- (cdg-point-y point2) (cdg-point-y point1)) 2)))

(defun cdg-point-dist (point1 point2)
  "Высчитывет расстояние между точками"
  (sqrt (cdg-point-dist-square point1 point2)))

(defun cdg-closest-point (point other)
  "Возвращает ближайшую к point точку из списка other"
  (when (not (eq nil other))
    (aref (reduce (lambda (acc x)
                    (let ((dist (cdg-point-dist-square point x)))
                      (if (< dist (aref acc 1))
                          (vector x dist)
                        acc)))
                  other
                  :initial-value (vector
                                  (first other)
                                  (cdg-point-dist-square point (first other))))
          0)))

(defun cdg-left-vline (line-x point)
  (< (cdg-point-x point) line-x))

(defun cdg-right-vline (line-x point)
  (> (cdg-point-x point) line-x))

(defun cdg-above-hline (line-y point)
  (> (cdg-point-y point) line-y))

(defun cdg-under-hline (line-y point)
  (< (cdg-point-y point) line-y))

;; Векторы и работа с ними

(defun cdg-normalize-vec (vec)
  "Нормализует 2-х мерный вектор"
  (let* ((x (aref vec 0))
         (y (aref vec 1))
         (inv-len (/ 1.0 (sqrt (+ (* x x) (* y y))))))
    (vector (* x inv-len) (* y inv-len))))

(defun cdg-mirror-vector (vector direction)
  "Отражает вектор относительно указанного направления"
  (let ((x (elt vector 0))
        (y (elt vector 1)))
    (case direction
      ('horizontal (vector x (- y)))
      ('vertical   (vector (- x) y)))))

(defun cdg-invert-vector (vector)
  "Инвертирует направление вектора"
  (cdg-mirror-vector
   (cdg-mirror-vector vector 'vertical)
   'horizontal))

;; Прямоугольник параллельный оси Ox

(defun cdg-make-rect (left-top right-bottom)
  "Строит прямоугольник по двум переданным точкам"
  (list left-top right-bottom))

(defun cdg-make-rect-by-2-points (point1 point2)
  "Строит прямоугольник по двум, произвольным точкам"
  (let ((min-x (min (cdg-point-x point1) (cdg-point-x point2)))
        (max-x (max (cdg-point-x point1) (cdg-point-x point2)))
        (min-y (min (cdg-point-y point1) (cdg-point-y point2)))
        (max-y (max (cdg-point-y point1) (cdg-point-y point2))))
    (cdg-make-rect (cdg-make-point min-x max-y)
                   (cdg-make-point max-x min-y))))

(defun cdg-rect-left-top (rect)
  (first rect))

(defun cdg-rect-right-bottom (rect)
  (second rect))

(defun cdg-rect-min-x (rect)
  (cdg-point-x (first rect)))

(defun cdg-rect-max-x (rect)
  (cdg-point-x (second rect)))

(defun cdg-rect-min-y (rect)
  (cdg-point-y (second rect)))

(defun cdg-rect-max-y (rect)
  (cdg-point-y (first rect)))

(defun cdg-rect-width (rect)
  (abs (- (cdg-rect-max-x rect)
          (cdg-rect-min-x rect))))

(defun cdg-rect-height (rect)
  (abs (- (cdg-rect-max-y rect)
          (cdg-rect-min-y rect))))

;; Функции, тестирующие разного рода пересечения

(defun cdg-rect-point-test (rect point)
  "Принадлежит ли точка прямоугольнику?"
  (let ((x (cdg-point-x point))
        (y (cdg-point-y point)))
    (and (<= x (cdg-rect-max-x rect))
         (>= x (cdg-rect-min-x rect))
         (<= y (cdg-rect-max-y rect))
         (>= y (cdg-rect-min-y rect)))))

(defun cdg-contain-rect-test (rect1 rect2)
  "Содержит ли в себе целиком rect1 прямоугольник rect2"
  (and (cdg-rect-point-test rect1
                            (cdg-rect-left-top rect2))
       (cdg-rect-point-test rect1
                            (cdg-rect-right-bottom rect2))))

(defun cdg-vline-ray-inter-existp (line-x ray-start ray-direction)
  "Существует ли точка пересичения вертикальной прямой и
   другой прямой, заданной вектором"
  (if (= line-x (cdg-point-x ray-start))
      t
    (or
     (and (cdg-left-vline line-x ray-start)
          (> (aref ray-direction 0) 0))
     (and (cdg-right-vline line-x ray-start)
          (< (aref ray-direction 0) 0)))))

(defun cdg-hline-ray-inter-existp (line-y ray-start ray-direction)
  "Существует ли точка пересичения горизонтальной прямой и
   другой прямой, заданной вектором"
  (if (= line-y (cdg-point-y ray-start))
      t
    (or
     (and (cdg-above-hline line-y ray-start)
          (< (aref ray-direction 1) 0))
     (and (cdg-under-hline line-y ray-start)
          (> (aref ray-direction 1) 0)))))

(defun cdg-hline-ray-intersection (line-y ray-start ray-direction)
  "Вычисляет точку пересечения горизонтальной прямой и луча. Вектор
   ray-direction должен быть нормированным"
  (if (cdg-hline-ray-inter-existp line-y ray-start ray-direction)
      (let ((line-dist (abs (- line-y (cdg-point-y ray-start)))))
        (vector (+ (cdg-point-x ray-start)
                   (* (abs (/ line-dist (aref ray-direction 1)))
                      (aref ray-direction 0)))
                line-y))
    nil))

(defun cdg-vline-ray-intersection (line-x ray-start ray-direction)
  "Вычисляет точку пересечения вертикальной прямой и луча. Вектор
   ray-direction должен быть нормированным"
  (if (cdg-vline-ray-inter-existp line-x ray-start ray-direction)
      (let ((line-dist (abs (- line-x (cdg-point-x ray-start)))))
        (vector line-x
                (+ (cdg-point-y ray-start)
                   (* (abs (/ line-dist (aref ray-direction 0)))
                      (aref ray-direction 1)))))
    nil))

(defun cdg-rect-ray-intersection (rect ray-start ray-direction)
  "Находит точку пересечения прямоугольника параллельного Ox и луча, если
   она существует. Вектор должен быть нормированным. Возвращается самая
   близкая к началу луча точка пересечения."
  (cdg-closest-point
   ray-start
   (remove-if-not
    (lambda (x) (cdg-rect-point-test rect x))
    (remove-if 'null
               (list (cdg-hline-ray-intersection (cdg-rect-min-y rect)
                                                 ray-start
                                                 ray-direction)
                     (cdg-hline-ray-intersection (cdg-rect-max-y rect)
                                                 ray-start
                                                 ray-direction)
                     (cdg-vline-ray-intersection (cdg-rect-min-x rect)
                                                 ray-start
                                                 ray-direction)
                     (cdg-vline-ray-intersection (cdg-rect-max-x rect)
                                                 ray-start
                                                 ray-direction))))))
