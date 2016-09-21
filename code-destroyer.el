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

(defvar *cdg-score* 0
  "Количество игровых очков игрока")

(defconst *cdg-space-margin* 7
  "Определяет минимальное начальное расстояние между
   игровым полем и платформой игрока")

(defconst *cdg-space-sym* ? )

(defvar *cdg-debug* t
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

(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))


(provide 'code-destroyer-game)
