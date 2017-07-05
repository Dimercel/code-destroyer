;;; Игровые объекты


(defun cdg-make-ball (coord direction char)
  "Игровой мяч уничтожающий блоки и
   отражаемый платформой"
  (vector coord (cdg-normalize-vec direction) char))

(defun cdg-ball-pos (ball)
  "Собственно координаты центра мяча
   в Декартовой системе координат"
  (aref ball 0))

(defun cdg-ball-direct (ball)
  "Нормализованный вектор-направление
   движения мяча"
  (aref ball 1))

(defun cdg-ball-char (ball)
  "Каким символом в буфере отображается мяч?"
  (aref ball 2))

(defun cdg-ball-move-to (ball coord)
  "Перемещает мяч в точку coord"
  (aset ball 0 coord))

(defun cdg-ball-move (ball step)
  "Перемещает мяч в новую точку в
   соответствии с его направлением"
  (let ((pos (cdg-ball-pos ball))
        (dir (cdg-ball-direct ball)))
    (cdg-ball-move-to ball
                      (cdg-point-inc pos
                                     (* (aref dir 0) step)
                                     (* (aref dir 1) step)))))

(defun cdg-ball-change-direct (ball direct)
  "Меняет направление движения мяча"
  (aset ball 1 (cdg-normalize-vec direct)))

;; Создает игровой бокс. Он представляет собой препятствие на пути игрового
;; мяча. Геометрически, box описывется квадратом с координатами левого верхнего
;; угла coord. Размер задается в условных игровых единицах
(defun cdg-make-box (coord size char)
  (vector
   (cdg-make-rect coord
                  (cdg-point-inc coord
                                 (* size +cdg-game-unit+)
                                 (* (* size +cdg-game-unit+) -1)))
   char))

(defun cdg-box-pos (box)
  (cdg-rect-left-top (aref box 0)))

(defun cdg-box-rect (box)
  (aref box 0))

(defun cdg-box-size (box)
  (truncate (/ (cdg-rect-width (cdg-box-rect box)) +cdg-game-unit+)))

(defun cdg-box-char (box)
  (aref box 1))

(defun cdg-find-boxes-by-rect (boxes rects)
  "Отыскивает в списке игровых боксов такие,
   что их огранич. прямоугольник является
   элементом RECTS"
  (remove-if-not
   (lambda (x)
     (some (lambda (y)
             (equal (cdg-box-rect x) y))
           rects))
   boxes))

;; Описывает игровую платформу. Она отбивает мяч. В начале игры мяч находится на
;; платформе
(defun cdg-make-platform (center-pos size speed char)
  (vector center-pos size speed char))

(defun cdg-platform-pos (platform)
  (aref platform 0))

(defun cdg-platform-size (platform)
  (aref platform 1))

(defun cdg-platform-speed (platform)
  (aref platform 2))

(defun cdg-platform-char (platform)
  (aref platform 3))

(defun cdg-platform-start-pos (platform)
  "Вернет позицию начала платформы"
  (let ((half-size (/ (cdg-platform-size platform) 2.0)))
    (- (cdg-platform-pos platform) half-size)))

(defun cdg-platform-end-pos (platform)
  "Позиция конца платформы"
  (let ((half-size (/ (cdg-platform-size platform) 2.0)))
    (+ (cdg-platform-pos platform) half-size)))

(defun cdg-platform-move (platform step)
  "Сдвигает платформу на шаг, величиной step. Если step - положителен,
   то сдвиг вправо, иначе влево."
  (let ((new-value (+ (cdg-platform-pos platform) step)))
    (cdg-platform-move-to platform new-value)))

(defun cdg-platform-move-to (platform value)
  "Передвигает платформу так, чтобы позиция
   центра совпадала со значением value"
  (aset platform 0 value))

(defun cdg-platform-mirror-vec (platform cross-point)
  "Вернет направляющий вектор мяча, при условии что
   он столкнулся с платформой в точке CROSS-POINT"
  (let* ((plt-pos (cdg-platform-pos platform))
         (half-size (/ (cdg-platform-size platform) 2.0))
         (center-dist (abs (- plt-pos (cdg-point-x cross-point))))
         (angle (* (/ pi 2.0) (/ center-dist half-size))))
    (if (cdg-left-vline-p plt-pos cross-point)
        (vector (- (cos angle)) (sin angle))
      (vector (cos angle) (sin angle)))))

(defun cdg-return-ball-to-platform (ball platform zone)
  "Возвращает мяч на подвижную платформу. Используется
   для начала игры"
  (setq *cdg-ball-on-platform* t)
  (cdg-make-ball (cdg-make-point (cdg-platform-pos platform)
                                 (+ (* (1+ (cdg-zone-platform-start zone :row))
                                       +cdg-game-unit+)
                                    +cdg-gap+))
                 (cdg-ball-change-direct ball *cdg-ball-init-dir*)
                 (cdg-ball-char ball)))

;; Описывает игровую зону. Представляет собой прямоугольную область в которой
;; происходит игра. Мяч не может покинуть пределов игровой зоны, как и любой
;; другой игровой объект
(defun cdg-make-zone (row-count col-count)
  "Определяет ограниченную зону, в которой происходит игра.
   Не один игровой объект находящийся за ее пределами не отображается."
  (cdg-make-rect (cdg-make-point 0 (* row-count +cdg-game-unit+))
                 (cdg-make-point (* col-count +cdg-game-unit+) 0)))

(defun cdg-make-zone-by-window (window)
  "В этом случае границы зоны вычисляются
   на основе переданного emacs-буфера"
  (with-selected-window window
    (if (< (cdg-max-len-str-in-win) (window-body-width))
        (cdg-make-zone (window-body-height)
                       (cdg-max-len-str-in-win))
        (cdg-make-zone (window-body-height)
                       (window-body-width)))))

(defun cdg-zone-rect (zone)
  "Возвращает ограничивающий прямоугольник зоны"
  zone)

(defun cdg-zone-rows (zone)
  "Количество строк в игровой зоне. Выражается в игровых единицах"
  (/ (truncate (cdg-rect-height (cdg-zone-rect zone)) +cdg-game-unit+)))

(defun cdg-zone-cols (zone)
  "Количество столбцов в игровой зоне. Выражается в игровых единицах"
  (/ (truncate (cdg-rect-width (cdg-zone-rect zone)) +cdg-game-unit+)))

(defun cdg-zone-status-rows ()
  "Количество строк в игровой зоне, отведенных под строку статуса"
  1)

(defun cdg-zone-space-rows ()
  "Количество пустых строк в зоне разделяющих игровую платформу и область боксов"
  +cdg-min-platform-space+)

(defun cdg-zone-platform-rows ()
  "Количество строк, отведенных под область перемещения платформы"
  1)

(defun cdg-zone-box-rows (zone)
  "Количество строк, отведенных под область, где могут размещаться игровые боксы."
  (- (cdg-zone-rows zone)
     (+ (cdg-zone-status-rows)
        (cdg-zone-space-rows)
        (cdg-zone-platform-rows))))

(defun cdg-zone-status-start (zone unit-type)
  "Вернет индекс строки начала статусной зоны"
  (let ((result-in-rows (- (cdg-zone-rows zone)
                           (cdg-zone-status-rows))))
    (if (eq unit-type :row)
        result-in-rows
        (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-box-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-status-start zone :row)
                           (cdg-zone-box-rows zone))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-space-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-box-start zone :row)
                           (cdg-zone-space-rows))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-platform-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-space-start zone :row)
                           (cdg-zone-platform-rows))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-box-rect (zone)
  (cdg-make-rect
   (cdg-make-point 0 (cdg-zone-status-start zone :descart))
   (cdg-make-point (cdg-rect-width (cdg-zone-rect zone))
                   (cdg-zone-box-start zone :descart))))

(defun cdg-zone-point-coord (zone point)
  "Возвращает координаты квадрата, в котором содержится точка.
   Координаты указаны в количестве строк и столбцов."
  (if (cdg-rect-point-test (cdg-zone-rect zone) point)
      (cdg-make-point (truncate (/ (cdg-point-x point) +cdg-game-unit+))
                      (truncate (/ (cdg-point-y point) +cdg-game-unit+)))
    nil))

(defun cdg-zone-point-rect (zone point)
  "По указанной точке, вернет прямоугольник которому,
   эта точка принадлежит. С длиной стороны +cdg-game-unit+"
  (let ((box-pos (cdg-zone-point-coord zone point)))
    (if (null box-pos)
        nil
      (cdg-make-rect
       (cdg-make-point (* (cdg-point-x box-pos) +cdg-game-unit+)
                       (* (1+ (cdg-point-y box-pos)) +cdg-game-unit+))
       (cdg-make-point (* (1+ (cdg-point-x box-pos)) +cdg-game-unit+)
                       (* (cdg-point-y box-pos) +cdg-game-unit+))))))

(defun cdg-zone-neighbors-rect (zone point)
  "Вернет список прямоугольников, соседствующих с тем,
  что принадлежит точке point"
  (let ((x (cdg-point-x point))
        (y (cdg-point-y point)))
    (delete-if
     #'null
     (list (cdg-zone-point-rect zone (cdg-make-point x (+ y +cdg-game-unit+)))
           (cdg-zone-point-rect zone (cdg-make-point (+ x +cdg-game-unit+) y))
           (cdg-zone-point-rect zone (cdg-make-point x (- y +cdg-game-unit+)))
           (cdg-zone-point-rect zone (cdg-make-point (- x +cdg-game-unit+) y))))))

(defun cdg-squares-on-line (zone point1 point2)
  "Возвращает список квадратов, которые пересекает отрезок, заданный двумя
  точками. Так как внутри игровой зоны расчеты ведутся в игровых единицах, то
  размеры квадратов равны 1 единице"
  (if (and (cdg-zone-point-test zone point1)
           (cdg-zone-point-test zone point2))
    (let* ((main-rect (cdg-make-rect-by-2-points point1 point2))
            (lt (cdg-rect-left-top main-rect))
            (rb (cdg-rect-right-bottom main-rect))
            (lt-square (cdg-zone-point-coord zone lt))
            (rb-square (cdg-zone-point-coord zone rb))
            (hdist (- (cdg-point-x rb-square) (cdg-point-x lt-square)))
            (vdist (- (cdg-point-y lt-square) (cdg-point-y rb-square)))
            (result '()))
      (dotimes (r (1+ hdist))
        (dotimes (c (1+ vdist))
          (let* ((left (* (+ (cdg-point-x lt-square) c) +cdg-game-unit+))
                  (top  (* (1+ (- (cdg-point-y lt-square) r)) +cdg-game-unit+))
                  (right (+ left +cdg-game-unit+))
                  (bottom (- top +cdg-game-unit+)))
            (setq result
                  (cons (cdg-make-rect (cdg-make-point left top)
                                        (cdg-make-point right bottom))
                        result)))))
      result)
    nil))

(defun cdg-limiting-rects (zone)
  "Вернет список из трех прямоугольников,ограничивающих игровую
  зону. Они представляют собой границы, от которых должен
  отскакивать мяч, если на его пути не оказалось боксов"
  (let ((zwidth (cdg-rect-width (cdg-zone-rect zone)))
        (zheight (cdg-rect-height (cdg-zone-rect zone))))
    (list
     (cdg-make-rect (cdg-make-point -1 zheight) ; прямоугольник слева
                    (cdg-make-point 0 0))
     (cdg-make-rect (cdg-make-point 0 zheight) ; прямоугольник сверху
                    (cdg-make-point zwidth
                                    (cdg-zone-status-start zone :descart)))
     (cdg-make-rect (cdg-make-point zwidth zheight) ; прямоугольник справа
                    (cdg-make-point (1+ zwidth) 0)))))


;;; Проверка на столкновения


(defun cdg-ball-boxes-test (zone boxes ball dist)
  "Вернет ближайший игровой бокс с которым
  столкнется мяч, пройдя расстояние DIST"
  (let* ((ball-x (cdg-point-x (cdg-ball-pos ball)))
         (ball-y (cdg-point-y (cdg-ball-pos ball)))
         (dir-x  (aref (cdg-ball-direct ball) 0))
         (dir-y  (aref (cdg-ball-direct ball) 1))
         (test-rects (cdg-squares-on-line
                      zone
                      (cdg-ball-pos ball)
                      (cdg-make-point (+ ball-x (* dir-x dist))
                                      (+ ball-y (* dir-y dist)))))
         (result '()))
    ;; ищем все боксы, пересекающиеся с путем мяча
    (setq result (cdg-find-boxes-by-rect boxes test-rects))
    ;; удаляем боксы без пересечения
    (setq result
      (delete-if-not (lambda (x)
                        (cdg-rect-ray-intersection (cdg-box-rect x)
                                                  (cdg-ball-pos ball)
                                                  (cdg-ball-direct ball)))
                      result))
    ;; выбираем ближайший к мячу игровой бокс
    (let ((near (cdg-closest-point (cdg-ball-pos ball)
                                   (map 'list #'cdg-box-pos result))))
      (first (delete-if-not (lambda (x) (equal near (cdg-box-pos x)))
                            result)))))

(defun cdg-zone-point-test (zone point)
  "Если точка находится внутри зоны вернет истину,
   иначе nil"
  (cdg-rect-point-test (cdg-zone-rect zone)
                       point))
