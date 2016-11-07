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

  (switch-to-buffer "cdg")
  (use-local-map code-destroyer-mode-map)
  (setq *cdg-game-buffer* (current-buffer))
  (buffer-disable-undo *cdg-game-buffer*)

  (code-destroyer-mode)
  (cdg-init)
  (setq *cdg-game-timer* (run-with-timer 0.5 0.5 'cdg-main-loop)))


(require 'cl-lib)


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

(defconst *cdg-min-platform-space* 7
  "Определяет минимальное начальное расстояние между
   игровым полем и платформой игрока")

(defconst *cdg-space-sym* ? )

(defconst *cdg-debug* nil
  "Переменная отвечает за режим вывода отладочной ин-ии")


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

;; Функции широкого назначения
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

(defun cdg-normalize-2d-vec (vec)
  "Нормализует 2-х мерный вектор"
  (let* ((x (elt vec 0))
         (y (elt vec 1))
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

(defun cdg-box-ray-intersection (box ray)
  "Возвращает t если прямоугольник и луч пересекаются
   и nil в обратном случае"
  (let* ((inv-vec (cdg-invert-vector (second ray)))
         (tx1 (* (elt inv-vec 0)
                 (- (cdg-rect-min-x box)
                    (elt (first ray) 0))))
         (tx2 (* (elt inv-vec 0)
                 (- (cdg-rect-max-x box)
                    (elt (first ray) 0))))
         (tmin (min tx1 tx2))
         (tmax (max tx1 tx2))
         (ty1 (* (elt inv-vec 1)
                 (- (cdg-rect-min-y box)
                    (elt (first ray) 1))))
         (ty2 (* (elt inv-vec 1)
                 (- (cdg-rect-max-y box)
                    (elt (first ray) 1)))))
    (setq tmin (max tmin (min ty1 ty2)))
    (setq tmax (min tmax (max ty1 ty2)))
    (>= tmax tmin)))


(defun cdg-make-rect (left-top right-bottom)
  (list left-top right-bottom))

(defun cdg-rect-left-top (rect)
  (first rect))

(defun cdg-rect-right-bottom (rect)
  (second rect))

(defun cdg-rect-min-x (rect)
  (elt (first rect) 0))

(defun cdg-rect-max-x (rect)
  (elt (second rect) 0))

(defun cdg-rect-min-y (rect)
  (elt (first rect) 1))

(defun cdg-rect-max-y (rect)
  (elt (second rect) 1))


(defun cdg-init ()
  "Собственно инициализация игры."
  (let ((inhibit-read-only t))
    (setq-local truncate-lines t)
    (setq *cdg-game-board*
          (cdg-build-game-board *cdg-code-buffer*))
    (setq *cdg-draw-buffer*
          (cdg-make-char-buffer (window-body-height)
                                (window-body-width)
                                *cdg-space-sym*))
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

(defun cdg-make-char-buffer (rows cols fill-char)
  "Представляет прямоугольный массив из текстовых символов. Хранится одной большой строкой.
   ROWS - кол-во строк, COLS - кол-во столбцов, FILL-CHAR - символ-заполнитель, которым
   будет инициализорован буфер."
  (cond
   ((or (eql rows 0) (eql cols 0))
    (list "" 0 0))
   (t (list (make-string (* rows cols) fill-char)
            rows
            cols))))

(defun cdg-make-char-buffer-by-string (str col-count fill-char)
  (cond
   ((or (= (length str) 0) (= col-count 0))
    (list "" 0 0))
   (t
    (let ((row-count (/ (length str) col-count)))
      (when (not (equalp 0 (mod (length str) col-count)))
        (setq row-count (1+ row-count))
        (setq str (cdg-to-length str
                                 (* row-count col-count)
                                 fill-char)))
      (list str row-count col-count)))))


(defun cdg-resize-char-buffer (buffer row-count col-count fill-char)
  "Изменяет размер символьного буфера. Если размер становится больше,
   то недостающие позиции заполняются символом-заполнителем"
  (let ((old-row-count (cdg-char-buf-row-count buffer))
        (old-col-count (cdg-char-buf-col-count buffer))
        (result-body   ""))
    ;; сначала приводим все строки к единой длине
    (dotimes (r old-row-count)
      (setq result-body
            (concat result-body
                    (cdg-to-length (cdg-get-char-row buffer r)
                                   col-count
                                   fill-char))))
    (cdg-make-char-buffer-by-string (cdg-to-length result-body
                                                   (* row-count col-count)
                                                   fill-char)
                                    col-count
                                    fill-char)))

(defun cdg-char-buf-row-count (buffer)
  (second buffer))

(defun cdg-char-buf-col-count (buffer)
  (third buffer))

(defun cdg-char-buf-size (buffer)
  (* (cdg-char-buf-row-count buffer)
     (cdg-char-buf-col-count buffer)))

(defun cdg-char-buf-body (buffer)
  (first buffer))

(defun cdg-erase-char-buffer (buffer fill-char)
  ""
  (cdg-make-char-buffer (cdg-char-buf-row-count buffer)
                        (cdg-char-buf-col-count buffer)
                        fill-char))

(defun cdg-2d-to-1d-inx (char-buffer row col)
  "Перевод координат из прямоугольной 2-х мерной,
   в одномерный индекс. Допускаются только
   положительные значения."
  (if (and (>= row 0) (>= col 0))
      (+ col
         (* row (cdg-char-buf-col-count char-buffer)))
    nil))

(defun cdg-get-char (char-buffer row col)
  (elt (cdg-char-buf-body char-buffer)
       (cdg-2d-to-1d-inx char-buffer row col)))

(defun cdg-set-char (char-buffer row col new-value)
  (aset (cdg-char-buf-body char-buffer)
        (cdg-2d-to-1d-inx char-buffer row col)
        new-value))

(defun cdg-get-char-safe (char-buffer row col &optional bad-value)
  "Безопасная версия cdg-get-char возвращающая BAD-VALUE в случае
   отсутсвия указанного индекса"
  (let ((size (cdg-char-buf-size char-buffer))
        (1d-inx (cdg-2d-to-1d-inx char-buffer row col)))
    (if (and (not (null 1d-inx))
             (< 1d-inx size))
      (elt (cdg-char-buf-body char-buffer) 1d-inx)
      bad-value)))

(defun cdg-set-char-safe (char-buffer row col new-value)
  "Безопасная версия cdg-set-char возвращающая NIL в случае
   отсутсвия указанного индекса. Не присваивает значения в
   случае не корректного индекса"
  (let ((size (cdg-char-buf-size char-buffer))
        (1d-inx (cdg-2d-to-1d-inx char-buffer row col)))
    (if (and (not (null 1d-inx))
             (< 1d-inx size))
        (aset (cdg-char-buf-body char-buffer) 1d-inx new-value)
      nil)))

(defun cdg-get-char-row (char-buffer row)
  (let* ((start (cdg-2d-to-1d-inx char-buffer row 0))
         (end   (+ start (cdg-char-buf-col-count char-buffer))))
    (if (< row (cdg-char-buf-row-count char-buffer))
        (substring (cdg-char-buf-body char-buffer) start end)
      nil)))

;; TODO: do correct work
(defun cdg-set-char-row (char-buffer row-inx row-data)
  (let* ((row-begin (cdg-2d-to-1d-inx char-buffer row-inx 0))
         (col-count (cdg-char-buf-col-count char-buffer))
         (row-end   (+ row-begin col-count))
         (row-count (cdg-char-buf-row-count char-buffer))
         (body      (cdg-char-buf-body char-buffer)))
    (when (< row-inx row-count)
        (setq char-buffer
              (list (concat (substring body 0 row-begin)
                            (cdg-to-length row-data col-count *cdg-space-sym*)
                            (substring body row-end))
                    row-count
                    col-count)))))

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
                     *cdg-space-sym*)))
      (forward-line 1))
    (cdg-resize-char-buffer
     (cdg-make-char-buffer-by-string buf-text-with-limit
                                     win-width
                                     *cdg-space-sym*)
     (- (window-body-height) *cdg-min-platform-space*)
     win-width
     *cdg-space-sym*)))

(defun cdg-draw-game-board (board char-buffer start-row)
  (dotimes (r (cdg-char-buf-row-count board))
    (dotimes (c (cdg-char-buf-col-count board))
      (cdg-set-char-safe char-buffer
                         (+ r start-row)
                         c
                         (cdg-get-char board r c )))))

(defun cdg-output-char-buffer (char-buffer)
  "Выводит содержимое символьного буфера в
   emacs-буфер, в текущую позицию курсора."
  (let ((inhibit-read-only t))
    (dotimes (r (cdg-char-buf-row-count char-buffer))
      (insert
       (format "%s\n"
               (cdg-get-char-row char-buffer r))))))

(defun cdg-make-ball (coord direction)
  (list coord (cdg-normalize-2d-vec direction)))

(defun cdg-ball-pos (ball)
  "Собственно координаты центра мяча в
   двухмерной системе координат"
  (elt ball 0))

(defun cdg-ball-direct (ball)
  "Нормализованный вектор-направление
   движения мяча"
  (elt ball 1))

(defun cdg-ball-move (ball step)
  "Перемещает мяч в новую точку в
   соответствии с его направлением"
  (let ((pos (cdg-ball-pos ball))
        (vec (cdg-ball-direct ball)))
    (cdg-make-ball (vector
                    (+ (elt pos 0) (* (elt vec 0) step))
                    (+ (elt pos 1) (* (elt vec 1) step)))
                   vec)))

(defun cdg-return-ball-to-platform (ball platform board)
  "Возвращает мяч на подвижную платформу. Используется
   для начала игры"
  (setq *cdg-ball-on-platform* t)
  (cdg-make-ball (vector (- (cdg-char-buf-row-count board) 2)
                         (cdg-platform-pos platform))
                 (cdg-ball-direct ball)))

(defun cdg-draw-ball (ball char-buffer)
  (let ((ball-pos (cdg-ball-pos ball)))
    (cdg-set-char-safe char-buffer
                       (truncate (elt ball-pos 0))
                       (truncate (elt ball-pos 1))
                       ?o)))

(defun cdg-make-platform (center-pos size speed symbol)
  (list center-pos size speed symbol))

(defun cdg-platform-pos (platform)
  (first platform))

(defun cdg-platform-size (platform)
  (second platform))

(defun cdg-platform-speed (platform)
  (third platform))

(defun cdg-platform-symbol (platform)
  (fourth platform))

(defun cdg-platform-move (platform value)
  (let ((new-value (+ (cdg-platform-pos platform) value)))
    (when (< new-value 0)
      (setq new-value 0))
    (cdg-make-platform new-value
                       (cdg-platform-size platform)
                       (cdg-platform-speed platform)
                       (cdg-platform-symbol platform))))

(defun cdg-platform-move-to (platform value)
  (if (< value 0)
      platform
    (cdg-make-platform value
                       (cdg-platform-size platform)
                       (cdg-platform-speed platform)
                       (cdg-platform-symbol platform))))

(defun cdg-draw-platform (platform char-buffer)
  ""
  (let* ((half-size (/ (cdg-platform-size platform) 2))
         (last-row  (1- (cdg-char-buf-row-count char-buffer)))
         (start-pos (truncate (- (cdg-platform-pos platform)
                                 half-size))))
    (dotimes (i (ceiling (cdg-platform-size platform)))
      (cdg-set-char-safe char-buffer
                         last-row
                         (+ start-pos i)
                         (cdg-platform-symbol platform)))))

(defun cdg-draw-game ()
  "Собственно отрисовка всех элементов игры"
  (let ((inhibit-read-only t))
    (switch-to-buffer *cdg-game-buffer*)
    (erase-buffer)
    (setq *cdg-draw-buffer*
          (cdg-erase-char-buffer *cdg-draw-buffer*
                                 *cdg-space-sym*))

    (cdg-draw-game-board *cdg-game-board*
                         *cdg-draw-buffer*
                         1)
    (cdg-draw-platform *cdg-platform* *cdg-draw-buffer*)
    (cdg-draw-ball *cdg-ball* *cdg-draw-buffer*)

    (cdg-output-char-buffer *cdg-draw-buffer*)
    (beginning-of-buffer)))

(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))

(defun cdg-main-loop ()
  "Главный цикл игры"
  (when (eq (current-buffer) *cdg-game-buffer*)
    (unless *cdg-ball-on-platform*
      (setq *cdg-ball* (cdg-ball-move *cdg-ball* 0.7)))
    (cdg-draw-game)))


(provide 'code-destroyer-game)
