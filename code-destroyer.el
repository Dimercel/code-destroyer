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

(defvar *cdg-score* 0
  "Количество игровых очков игрока")

(defconst *cdg-space-margin* 7
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

(defun cdg-init ()
  "Собственно инициализация игры."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local truncate-lines t)
    (cdg-copy-view-part-buffer *cdg-code-buffer*
                               *cdg-game-buffer*
                               *cdg-space-margin*)
    (cdg-build-game-board *cdg-game-buffer*)
    (setq *cdg-board-rows*
          (/ (length *cdg-game-board*) *cdg-board-cols*))
    (erase-buffer)
    (cdg-draw-game-board *cdg-game-buffer* *cdg-game-board*)))

(defun cdg-make-char-buffer (rows cols fill-char)
  "Представляет прямоугольный массив из текстовых символов. Хранится одной большой строкой.
   ROWS - кол-во строк, COLS - кол-во столбцов, FILL-CHAR - символ-заполнитель, которым
   будет инициализорован буфер."
  (list (make-string (* rows cols) fill-char)
        rows
        cols))

(defun cdg-buf-row-count (char-buffer)
  (second char-buffer))

(defun cdg-buf-col-count (char-buffer)
  (third char-buffer))

(defun cdg-char-buf-size (char-buffer)
  (* (cdg-buf-row-count char-buffer)
     (cdg-buf-col-count char-buffer)))

(defun cdg-2d-to-1d-inx (char-buffer row col)
  "Перевод координат из прямоугольной 2-х мерной,
   в одномерный индекс. Допускаются только
   положительные значения."
  (if (and (>= row 0) (>= col 0))
      (+ col
         (* row (cdg-buf-col-count char-buffer)))
    nil))

(defun cdg-get-char (char-buffer row col)
  (elt (first char-buffer)
       (cdg-2d-to-1d-inx char-buffer row col)))

(defun cdg-set-char (char-buffer row col new-value)
  (aset (first char-buffer)
        (cdg-2d-to-1d-inx char-buffer row col)
        new-value))

(defun cdg-get-char-safe (char-buffer row col &optional bad-value)
  "Безопасная версия cdg-get-char возвращающая BAD-VALUE в случае
   отсутсвия указанного индекса"
  (let ((size (cdg-char-buf-size char-buffer))
        (1d-inx (cdg-2d-to-1d-inx char-buffer row col)))
    (if (and (not (null 1d-inx))
             (< 1d-inx size))
      (elt (first char-buffer) 1d-inx)
      bad-value)))

(defun cdg-set-char-safe (char-buffer row col new-value)
  "Безопасная версия cdg-set-char возвращающая NIL в случае
   отсутсвия указанного индекса. Не присваивает значения в
   случае не корректного индекса"
  (let ((size (cdg-char-buf-size char-buffer))
        (1d-inx (cdg-2d-to-1d-inx char-buffer row col)))
    (if (and (not (null 1d-inx))
             (< 1d-inx size))
        (aset (first char-buffer) 1d-inx new-value)
      nil)))


(defun cdg-get-char-row (char-buffer row)
  (let* ((start (cdg-2d-to-1d-inx char-buffer row 0))
         (end   (+ start (cdg-buf-col-count char-buffer))))
    (if (< row (cdg-buf-row-count char-buffer))
        (substring (first char-buffer) start end)
      nil)))

(defun cdg-get-cell (row col)
  "Get the value in (ROW, COL)."
  (elt *cdg-game-board*
       (+ (* row *cdg-board-cols*)
          col)))

(defun cdg-set-cell (row column val)
  "Set the value in (ROW, COLUMN) to VAL."
  (aset *cdg-game-board*
        (+ (* row *cdg-board-cols*)
           column)
        val))

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
  (let ((begin-line nil))
    (beginning-of-buffer)
    (setq *cdg-game-board* "")
    (while (not (eobp))
      (beginning-of-line)
      (setq begin-line (point))
      (end-of-line)
      (setq *cdg-game-board*
            (concat *cdg-game-board*
                    (cdg-build-game-board-row
                     (buffer-substring-no-properties begin-line (point))
                     *cdg-board-cols*)))
      (forward-line 1))))

(defun cdg-build-game-board-row (code-str row-width)
  "Строит одну строку игрового поля на основе текстовой
   строки. Приводит строку к единой длине row-width"
  (let* ((trim-str (string-trim-right code-str))
         (str-len (length trim-str)))
    (if (< str-len row-width)
        ;; Дополняем строку пробелами до длины row-width
        (concat trim-str
                (make-string (- row-width str-len) *cdg-space-sym*))
        (substring trim-str 0 row-width))))

(defun cdg-draw-game-board (buffer board)
  (let ((inhibit-read-only t))
    (dotimes (row *cdg-board-rows*)
      (let ((line-pos (* row *cdg-board-cols*)))
      (insert
       (format "%s\n"
               (substring board
                          line-pos
                          (+ line-pos *cdg-board-cols*))))))))

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

(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))


(provide 'code-destroyer-game)
