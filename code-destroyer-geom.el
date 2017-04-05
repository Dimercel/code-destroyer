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

(defun cdg-hline-ray-intersection (line-y ray-start ray-direction)
  "Вычисляет точку пересечения горизонтальной прямой и луча. Вектор
   ray-direction должен быть нормированным"
  (if (and (cdg-same-signp line-y (aref ray-direction 1))
           (> (abs line-y) (abs (cdg-point-y ray-start))))
      (let ((line-dist (abs (- line-y (cdg-point-y ray-start)))))
        (vector (+ (cdg-point-x ray-start)
                   (* (abs (/ line-dist (aref ray-direction 1)))
                      (aref ray-direction 0)))
                line-y))
    nil))

(defun cdg-vline-ray-intersection (line-x ray-start ray-direction)
  "Вычисляет точку пересечения вертикальной прямой и луча. Вектор
   ray-direction должен быть нормированным"
  (if (and (cdg-same-signp line-x (aref ray-direction 0))
           (> (abs line-x) (abs (cdg-point-x ray-start))))
      (let ((line-dist (abs (- line-x (cdg-point-x ray-start)))))
        (vector line-x
                (+ (cdg-point-y ray-start)
                   (* (abs (/ line-dist (aref ray-direction 0)))
                      (aref ray-direction 1)))))
    nil))
