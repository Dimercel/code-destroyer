(define-derived-mode code-destroyer-mode special-mode "code-destroyer-mode"
  (define-key code-destroyer-mode-map (kbd "<left>")  'cdg-left)
  (define-key code-destroyer-mode-map (kbd "<right>") 'cdg-right)
  (define-key code-destroyer-mode-map (kbd "<space>") 'cdg-action))

(defgroup cdg nil
  ""
  :group 'games
    :prefix "cdg-")

;;;###autoload
(defun code-destroyer-game ()
  "Start playing Code Destroyer."
  (interactive)

  (setq *cdg-code-buffer* (current-buffer))
  (setq *cdg-game-buffer* (get-buffer-create "cdg"))

  (switch-to-buffer *cdg-game-buffer*)
  (buffer-disable-undo *cdg-game-buffer*)
  (use-local-map code-destroyer-mode-map)
  (code-destroyer-mode)

  (cdg-init)
  (switch-to-buffer *cdg-game-buffer*)
  (setq *cdg-game-timer* (run-with-timer 0.5 0.5 'cdg-main-loop)))


(require 'cl-lib)


;;; Раздел объявления констант и переменных


(defvar *cdg-game-timer* nil
  "Основной таймер игры. Запускает main-loop через
   фиксированный интервал времени")

(defvar *cdg-code-buffer* nil
  "Буфер, в которой расположен текст играющий роль игрвого поля")

(defvar *cdg-game-buffer* nil
  "Буфер, в котором происходит сама игра")

(defvar *cdg-draw-buffer* nil
  "Символьный буфер, в котором идет отрисовка всех
   элементов игры.")

(defvar *cdg-game-board* nil
  "Игровое поле. Представляет из себя текст с пробелами")

(defvar *cdg-ball* nil
  "Игровой мяч. Хранит размеры мяча, текущее положение и
   вектор направления")

(defvar *cdg-ball-on-platform* t
  "Если мяч еще не в движении и лежит на платформе, то
   переменная истинна, иначе ложна")

(defvar *cdg-platform* nil
  "Игровая платформа которая отбивает мяч. Пользователь
   управляет платформой, передвигая ее влево или вправо")

(defvar *cdg-score* 0
  "Количество игровых очков игрока")

(defconst +cdg-min-platform-space+ 7
  "Определяет минимальное начальное расстояние между
   игровым полем и платформой игрока. Измереятся в игровых
   единицах масштаба")

(defconst +cdg-space-sym+ ? )

(defconst +cdg-game-unit+ 10.0
  "Игровая единица масштаба. Игровое поле целиком состоит из квадратов
  одинаковых размеров, данная константа представляет размер стороны такого
  элементарного квадрата.")

(defconst *cdg-debug* nil
  "Переменная отвечает за режим вывода отладочной ин-ии")


;;; Вспомогательные функции, используемые повсеместно


(defun cdg-to-length (str new-length fill-char)
  "Приводит строку к указанной длине, добавляя
   символы-заполнители, либо удаляя не нужные
   символы с конца."
  (let ((old-length (length str)))
    (if (< new-length old-length)
        (substring str 0 new-length)
      (concat str
              (make-string (- new-length old-length)
                           fill-char)))))

(defun cdg-same-signp (&rest numbers)
  "Все указанные числа одного знака?"
  (or (every (lambda (x) (>= x 0)) numbers)
      (every (lambda (x) (<= x 0)) numbers)))

(defun cdg-2d->1d (row col col-count)
  "Конвертирует индекс двумерного массива в одномерный"
  (if (< col col-count)
      (+ (* row col-count) col)
    nil))


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

(defun cdg-zone (row-count col-count)
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

(defun cdg-zone-point-coord (zone point)
  "Возвращает координаты квадрата, в котором содержится точка."
  (if (cdg-rect-point-test (cdg-zone-rect zone) point)
      (cdg-make-point (truncate (/ (cdg-point-x point) +cdg-game-unit+))
                      (truncate (/ (cdg-point-y point) +cdg-game-unit+)))
    nil))

;;; Код ниже описывает символьный буфер. Такой буфер состоит из одиночных
;;; символов и является двумерным.


(defun cdg-make-char-buffer (rows cols fill-char)
  "Представляет прямоугольный массив из текстовых символов. Хранится одной большой строкой.
   ROWS - кол-во строк, COLS - кол-во столбцов, FILL-CHAR - символ-заполнитель, которым
   будет инициализорован буфер."
  (cdg-make-char-buffer-by-string (make-string (* rows cols) fill-char)
                                  cols
                                  fill-char))

(defun cdg-make-char-buffer-by-string (str cols fill-char)
  (if (/= cols 0)
      (let ((suffix-len (mod (length str) cols)))
        (cond
         ((or (= (length str) 0) (= cols 0)) nil)
         ((/= suffix-len 0) (vector
                             (cdg-to-length str
                                            (+ (length str) (- cols suffix-len))
                                            fill-char)
                             cols))
         (t (vector str cols))))
    nil))

(defun cdg-char-buffer-size (buffer)
  "Общее количество символов в буфере"
  (length (aref buffer 0)))

(defun cdg-char-buffer-cols (buffer)
  "Количество столбцов символьного буфера"
  (aref buffer 1))

(defun cdg-char-buffer-rows (buffer)
  "Количество строк в символьном буфере"
  (truncate (/ (cdg-char-buffer-size buffer)
               (cdg-char-buffer-cols buffer))))

(defun cdg-get-char (buffer row col)
  "Вернет символ находящийся в позиции row, col.
   Выход за пределы буфера не проверяется"
  (let ((str (aref buffer 0)))
    (aref str (cdg-2d->1d row col (cdg-char-buffer-cols buffer)))))

(defun cdg-set-char (buffer row col new-value)
  (aset (aref buffer 0)
        (cdg-2d->1d row col (cdg-char-buffer-cols buffer))
        new-value))

(defun cdg-get-char-safe (buffer row col &optional bad-value)
  "Безопасная версия cdg-get-char возвращающая BAD-VALUE в случае
   отсутсвия указанного индекса"
  (let ((size (cdg-char-buffer-size buffer))
        (char-inx (cdg-2d->1d row col (cdg-char-buffer-cols buffer))))
    (if (and (< char-inx size) (> row 0) (> col 0))
        (cdg-get-char buffer row col)
      bad-value)))

(defun cdg-set-char-safe (buffer row col new-value)
  "Безопасная версия cdg-set-char возвращающая NIL в случае
   отсутсвия указанного индекса. Не присваивает значения в
   случае не корректного индекса"
  (let ((size (cdg-char-buffer-size buffer))
        (char-inx (cdg-2d->1d row col (cdg-char-buffer-cols buffer))))
    (if (and (< char-inx size) (> row 0) (> col 0))
        (cdg-set-char buffer row col new-value)
      nil)))

(defun cdg-get-char-row (buffer row)
  "Вернет строку из символьного буфера в позиции row"
  (let ((col-count (cdg-char-buffer-cols buffer)))
    (substring (aref buffer 0)
               (cdg-2d->1d row 0 col-count)
               (1+ (cdg-2d->1d row (1- col-count) col-count)))))

(defun cdg-output-char-buffer (buffer)
  "Выводит содержимое символьного буфера в
   emacs-буфер, в текущую позицию курсора."
  (let ((inhibit-read-only t))
    (dotimes (r (cdg-char-buffer-rows buffer))
      (insert
       (format "%s\n"
               (cdg-get-char-row buffer r))))))


;;; Взаимодействие логики игры с Emacs


(defun cdg-left ()
  "Двигает платформу влево"
  (interactive)
  (setq *cdg-platform*
        (cdg-platform-move *cdg-platform*
                           (- (cdg-platform-speed *cdg-platform*))))
  (when *cdg-ball-on-platform*
    (setq *cdg-ball*
          (cdg-return-ball-to-platform *cdg-ball*
                                 *cdg-platform*
                                 *cdg-draw-buffer*)))
  (cdg-draw-game))

(defun cdg-right ()
  "Двигает платформу вправо"
  (interactive)
  (setq *cdg-platform*
        (cdg-platform-move *cdg-platform*
                           (+ (cdg-platform-speed *cdg-platform*))))
  (when *cdg-ball-on-platform*
    (cdg-return-ball-to-platform *cdg-ball*
                                 *cdg-platform*
                                 *cdg-draw-buffer*))
  (cdg-draw-game))

(defun cdg-action ()
  "Отпускает мяч с платформы"
  (interactive)
  (setq *cdg-ball-on-platform* nil))

(defun cdg-init ()
  "Собственно инициализация игры."
  (let ((inhibit-read-only t))
    (setq-local truncate-lines t)
    (setq *cdg-game-board*
          (cdg-build-game-board *cdg-code-buffer*))
    (setq *cdg-draw-buffer*
          (cdg-make-char-buffer (window-body-height)
                                (window-body-width)
                                +cdg-space-sym+))
    (setq *cdg-platform*
          (cdg-make-platform
           (/ (cdg-char-buf-col-count *cdg-game-board*) 2.0)
           5.0
           1.0
           ?-))
    (setq *cdg-ball*
          (cdg-return-ball-to-platform (cdg-make-ball [0 0] [-0.5 -0.5])
                                       *cdg-platform*
                                       *cdg-draw-buffer*))

    (cdg-debug (format "Создано игровое поле %d x %d"
                       (cdg-char-buf-row-count *cdg-game-board*)
                       (cdg-char-buf-col-count *cdg-game-board*)))
    (cdg-debug (format "Создан буфер отрисовки %d x %d"
                       (cdg-char-buf-row-count *cdg-draw-buffer*)
                       (cdg-char-buf-col-count *cdg-draw-buffer*)))
    (cdg-debug (format "Начальное положение платформы: %d"
                       (cdg-platform-pos *cdg-platform*)))
    (cdg-debug (format "Начальное положение мяча: %s"
                       (cdg-ball-pos *cdg-ball*)))))

(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))

(defun cdg-build-game-board (buffer)
  "Создает игровое поле на основе текста буфера."
  (switch-to-buffer buffer)
  (let ((begin-line nil)
        (win-width (window-body-width))
        (buf-text-with-limit ""))
    (beginning-of-buffer)
    (while (not (eobp))
      (beginning-of-line)
      (setq begin-line (point))
      (end-of-line)
      (setq buf-text-with-limit
            (concat buf-text-with-limit
                    (cdg-to-length
                     (buffer-substring-no-properties begin-line (point))
                     win-width
                     +cdg-space-sym+)))
      (forward-line 1))
    (cdg-resize-char-buffer
     (cdg-make-char-buffer-by-string buf-text-with-limit
                                     win-width
                                     +cdg-space-sym+)
     (- (window-body-height) +cdg-min-platform-space+)
     win-width
     +cdg-space-sym+)))

;; Функции отрисовки игровых объектов

(defun cdg-draw-platform (platform char-buffer)
  "Отрисовка игровой платформы, находящейся на
   самой нижней строке."
  (let* ((half-size (/ (cdg-platform-size platform) 2))
         (last-row  (1- (cdg-char-buf-row-count char-buffer)))
         (start-pos (truncate (- (cdg-platform-pos platform)
                                 half-size))))
    (dotimes (i (ceiling (cdg-platform-size platform)))
      (cdg-set-char-safe char-buffer
                         last-row
                         (+ start-pos i)
                         (cdg-platform-symbol platform)))))

(defun cdg-draw-ball (ball char-buffer)
  "Отрисова игрового мяча. Представляется одним
   единственным символом в указанной позиции"
  (let ((ball-pos (cdg-ball-pos ball)))
    (cdg-set-char-safe char-buffer
                       (truncate (elt ball-pos 0))
                       (truncate (elt ball-pos 1))
                       ?o)))

(defun cdg-draw-game-board (board char-buffer start-row)
  (dotimes (r (cdg-char-buf-row-count board))
    (dotimes (c (cdg-char-buf-col-count board))
      (cdg-set-char-safe char-buffer
                         (+ r start-row)
                         c
                         (cdg-get-char board r c )))))

(defun cdg-draw-game ()
  "Собственно отрисовка всех элементов игры"
  (let ((inhibit-read-only t))
    (switch-to-buffer *cdg-game-buffer*)
    (erase-buffer)
    (setq *cdg-draw-buffer*
          (cdg-erase-char-buffer *cdg-draw-buffer*
                                 +cdg-space-sym+))

    (cdg-draw-game-board *cdg-game-board*
                         *cdg-draw-buffer*
                         1)
    (cdg-draw-platform *cdg-platform* *cdg-draw-buffer*)
    (cdg-draw-ball *cdg-ball* *cdg-draw-buffer*)

    (cdg-output-char-buffer *cdg-draw-buffer*)
    (beginning-of-buffer)))

(defun cdg-main-loop ()
  "Главный цикл игры"
  (when (eq (current-buffer) *cdg-game-buffer*)
    (unless *cdg-ball-on-platform*
      (setq *cdg-ball* (cdg-ball-move *cdg-ball* 0.7)))
    (cdg-draw-game)))


(provide 'code-destroyer-game)
