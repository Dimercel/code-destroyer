;;; Игровые объекты


(defun cdg-make-ball (coord direction)
  "Игровой мяч уничтожающий блоки и
   отражаемый платформой"
  (vector coord (cdg-normalize-vec direction)))

(defun cdg-ball-pos (ball)
  "Собственно координаты центра мяча
   в Декартовой системе координат"
  (aref ball 0))

(defun cdg-ball-direct (ball)
  "Нормализованный вектор-направление
   движения мяча"
  (aref ball 1))

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
;; угла coord.
(defun cdg-make-box (coord size char)
  (vector (cdg-make-rect coord
                         (cdg-point-inc coord size (* size -1)))
          char))

(defun cdg-box-pos (box)
  (cdg-rect-left-top (aref box 0)))

(defun cdg-box-rect (box)
  (aref box 0))

(defun cdg-box-size (box)
  (cdg-rect-width (cdg-box-rect box)))

(defun cdg-box-char (box)
  (aref box 1))

;; Описывает игровую платформу. Она отбивает мяч. В начале игры мяч находится на
;; платформе
(defun cdg-make-platform (center-pos size speed symbol)
  (vector center-pos size speed symbol))

(defun cdg-platform-pos (platform)
  (aref platform 0))

(defun cdg-platform-size (platform)
  (aref platform 1))

(defun cdg-platform-speed (platform)
  (aref platform 2))

(defun cdg-platform-symbol (platform)
  (aref platform 3))

(defun cdg-platform-move (platform step)
  "Сдвигает платформу на шаг, величиной step. Если step - положителен,
   то сдвиг вправо, иначе влево."
  (let ((new-value (+ (cdg-platform-pos platform) step)))
    (cdg-platform-move-to platform new-value)))

(defun cdg-platform-move-to (platform value)
  "Передвигает платформу так, чтобы позиция
   центра совпадала со значением value"
  (aset platform 0 value))

(defun cdg-return-ball-to-platform (ball platform zone)
  "Возвращает мяч на подвижную платформу. Используется
   для начала игры"
  (setq *cdg-ball-on-platform* t)
  (cdg-make-ball (cdg-make-point (- (cdg-zone-rows zone) 2)
                                 (cdg-platform-pos platform))
                 (cdg-ball-direct ball)))

;; Описывает игровую зону. Представляет собой прямоугольную область в которой
;; происходит игра. Мяч не может покинуть пределов игровой зоны, как и любой
;; другой игровой объект
(defun cdg-make-zone (row-count col-count)
  "Определяет ограниченную зону, в которой происходит игра.
   Не один игровой объект находящийся за ее пределами не отображается."
  (cdg-make-rect (cdg-make-point 0 (* row-count +cdg-game-unit+))
                 (cdg-make-point (* col-count +cdg-game-unit+) 0)))

(defun cdg-zone-rect (zone)
  "Возвращает ограничивающий прямоуглольник зоны"
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
  (let ((result-in-rows (- (cdg-zone-rows zone) 1)))
    (if (eq unit-type :row)
        result-in-rows
        (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-box-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-status-start zone :row)
                           (cdg-zone-status-rows))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-space-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-box-start zone :row)
                           (cdg-zone-box-rows zone))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-platform-start (zone unit-type)
  (let ((result-in-rows (- (cdg-zone-space-start zone :row)
                           (cdg-zone-space-rows))))
    (if (eq unit-type :row)
        result-in-rows
      (* result-in-rows +cdg-game-unit+))))

(defun cdg-zone-point-coord (zone point)
  "Возвращает координаты квадрата, в котором содержится точка."
  (if (cdg-rect-point-test (cdg-zone-rect zone) point)
      (cdg-make-point (truncate (/ (cdg-point-x point) +cdg-game-unit+))
                      (truncate (/ (cdg-point-y point) +cdg-game-unit+)))
    nil))
