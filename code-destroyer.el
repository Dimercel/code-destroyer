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
  (switch-to-buffer "cdg")
  (buffer-disable-undo "cdg")

  (setq *cdg-game-buffer* (current-buffer))
  (setq *cdg-board-cols* (window-total-width))
  (setq *cdg-board-rows* (- (window-total-height) *cdg-space-margin*))

  (code-destroyer-mode)
  (cdg-init))


(require 'cl-lib)


(defvar *cdg-code-buffer* nil
  "")

(defvar *cdg-game-buffer* nil
  "")

(defvar *cdg-game-board* nil
  "")

(defvar *cdg-board-rows* nil
  "")

(defvar *cdg-board-cols* nil
  "")

(defvar *cdg-score* 0)

(defvar *cdg-space-margin* 7
  "")

(defvar *cdg-debug* t
  "")


(defun cdg-up ()
  ""
  (print "xolcman"))
(defun cdg-down ()
  ""
  (insert "Down"))
(defun cdg-left ()
  ""
  nil)
(defun cdg-right ()
  ""
  nil)

(defun cdg-init ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local truncate-lines t)
    (cdg-copy-view-part-buffer *cdg-code-buffer*
                               *cdg-game-buffer*
                               *cdg-space-margin*)
    (cdg-build-game-board *cdg-game-buffer*)))

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
  (switch-to-buffer buffer)
  (let ((begin-line nil))
    (beginning-of-buffer)
    (while (not (eobp))
      (beginning-of-line)
      (setq begin-line (point))
      (end-of-line)
      (cdg-debug (buffer-substring-no-properties begin-line (point)))
      (forward-line 1))))



(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))


(provide 'code-destroyer-game)
