(define-derived-mode code-destroyer-mode special-mode "code-destroyer-mode"
  (define-key code-destroyer-mode-map (kbd "<up>")    'cdg-up)
  (define-key code-destroyer-mode-map (kbd "<down>")  'cdg-down)
  (define-key code-destroyer-mode-map (kbd "<left>")  'cdg-left)
  (define-key code-destroyer-mode-map (kbd "<right>") 'cdg-right))

(defgroup cdg nil
  ""
  :group 'games
    :prefix "cdg-")

;;;###autoload
(defun code-destroyer-game ()
  "Start playing Code Destroyer."
  (interactive)

  (setq *cdg-code-buffer* (current-buffer))
  (setq *cdg-board-cols* (window-body-width))

  (switch-to-buffer "cdg")
  (setq *cdg-game-buffer* (current-buffer))
  (buffer-disable-undo "cdg")

  (code-destroyer-mode)
  (cdg-init))


(require 'cl-lib)


(defvar *cdg-code-buffer* nil
  "Буфер, в которой расположен текст играющий роль игрвого поля")

(defvar *cdg-game-buffer* nil
  "Буфер, в котором происходит сама игра")

(defvar *cdg-game-board* nil
  "Игровое поле. Представляет из себя текст с пробелами")

(defvar *cdg-board-rows* nil
  "Количество строк на игровом поле")

(defvar *cdg-board-cols* nil
  "Количество столбцов на игровом поле")

(defvar *cdg-ball* nil
  "Игровой мяч. Хранит размеры мяча, текущее положение и
   вектор направления")

(defvar *cdg-platform* nil
  "Игровая платформа которая отбивает мяч. Пользователь
   управляет платформой, передвигая ее влево или вправо")

(defvar *cdg-score* 0
  "Количество игровых очков игрока")

(defconst *cdg-min-platform-space* 7
  "Определяет минимальное начальное расстояние между
   игровым полем и платформой игрока")

(defconst *cdg-space-sym* ? )

(defconst *cdg-debug* t
  "Переменная отвечает за режим вывода отладочной ин-ии")


(defun cdg-up ()
  ""
  (interactive))

(defun cdg-down ()
  ""
  (interactive))

(defun cdg-left ()
  ""
  (interactive))

(defun cdg-right ()
  ""
  (interactive))

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


(defun cdg-init ()
  "Собственно инициализация игры."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local truncate-lines t)
    (cdg-copy-view-part-buffer *cdg-code-buffer*
                               *cdg-game-buffer*
                               *cdg-min-space-platform*
    (cdg-build-game-board *cdg-game-buffer*)
    (setq *cdg-board-rows*
          (/ (length *cdg-game-board*) *cdg-board-cols*))
    (erase-buffer)
    (cdg-draw-game-board *cdg-game-buffer* *cdg-game-board*))))

(defun cdg-make-char-buffer (rows cols fill-char)
  "Представляет прямоугольный массив из текстовых символов. Хранится одной большой строкой.
   ROWS - кол-во строк, COLS - кол-во столбцов, FILL-CHAR - символ-заполнитель, которым
   будет инициализорован буфер."
  (list (make-string (* rows cols) fill-char)
        rows
        cols))

(defun cdg-make-char-buffer-by-string (str col-count fill-char)
  (let ((row-count (/ (length str) col-count)))
    (when (not (equalp 0 (mod (length str) col-count)))
      (setq row-count (1+ row-count))
      (setq str (cdg-to-length str
                               (* row-count col-count)
                               fill-char)))
    (list str row-count col-count)))


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

(defun cdg-set-char-row (char-buffer row-inx row-data)
  (let* ((row-begin (cdg-2d-to-1d-inx char-buffer row-inx 0))
         (row-end   (+ row-begin (cdg-char-buf-col-count char-buffer)))
         (col-count (cdg-char-buf-col-count char-buffer))
         (body      (cdg-char-buf-body char-buffer)))
    (when (< row-inx col-count)
        (setq char-buffer
              (list (concat (substring body 0 row-begin)
                            (cdg-to-length row-data col-count *cdg-space-sym*)
                            (substring body row-end))
              (cdg-char-buf-row-count char-buffer)
              col-count)))))

(defun cdg-copy-view-part-buffer (source receiver &optional margin)
  "Копирует часть текста из буфера source в буфер receiver.
   Копируется только видимый на текущий момент в окне текст.
   margin - кол-во нижних строк, которые не будут скопированы"
  (let ((start nil)
        (finish nil))
    (switch-to-buffer source)

    (move-to-window-line 0)
    (beginning-of-line)
    (setq start (point))

    (if (null margin)
      (move-to-window-line (- 1))
      (move-to-window-line (- margin)))
    (end-of-line)
    (setq finish (point))

    (switch-to-buffer receiver)
    (erase-buffer)
    (insert-buffer-substring-no-properties source start finish)))

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
     (cdg-make-char-buffer-by-string
      buf-text-with-limit
      win-width
      *cdg-space-sym*)
     (- (window-body-height) *cdg-min-space-platform)
     win-width)))

(defun cdg-draw-game-board (board char-buffer start-row)
  (dotimes (r (cdg-char-buf-row-count board))
    (cdg-set-char-row char-buffer
                      (+ start-row r)
                      (cdg-get-char-row board r))))

  ;; (let ((inhibit-read-only t))
  ;;   (dotimes (row *cdg-board-rows*)
  ;;     (let ((line-pos (* row *cdg-board-cols*)))
  ;;     (insert
  ;;      (format "%s\n"
  ;;              (substring board
  ;;                         line-pos
  ;;                         (+ line-pos *cdg-board-cols*))))))))

(defun cdg-make-ball (radius coord direction)
  (list radius coord (cdg-normalize-2d-vec direction)))

(defun cdg-ball-radius (ball)
  "Радиус меча. Определяет размер,
   а соответственно и площадь поражения"
  (elt ball 0))

(defun cdg-ball-pos (ball)
  "Собственно координаты центра мяча в
   двухмерной системе координат"
  (elt ball 1))

(defun cdg-ball-direct (ball)
  "Нормализованный вектор-направление
   движения мяча"
  (elt ball 2))

(defun cdg-normalize-2d-vec (vec)
  "Нормализует 2-х мерный вектор"
  (let* ((x (elt vec 0))
         (y (elt vec 1))
         (vec-len (sqrt (+ (x*x) (y*y)))))
    [(* x vec-len) (* y vec-len)]))

(defun cdg-make-platform (center-pos size speed)
  (list center-pos size speed))

(defun cdg-platform-pos (platform)
  (first platform))

(defun cdg-platform-size (platform)
  (second platform))

(defun cdg-platform-speed (platform)
  (third platform))

(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))


(provide 'code-destroyer-game)
