(require 'cl-lib)
(require 'code-destroyer-utils)
(require 'code-destroyer-geom)
(require 'code-destroyer-game)


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

(defun cdg-make-boxes-by-buf-text (buffer start-y box-size)
  "Строит игровые боксы на основе текста буфера. Каждый символ
   текста рассматривается как игровой бокс"
  (switch-to-buffer buffer)
  (let ((begin-line nil)
        (pos-y start-pos)
        (boxes '()))
    (beginning-of-buffer)
    (while (not (eobp))
      (beginning-of-line)
      (setq begin-line (point))
      (end-of-line)
      (let ((text-line (buffer-substring-no-properties begin-line (point))))
        (dotimes (i (length text-line))
          (when (not (eq (aref text-line i) +cdg-space-sym+))
            (setq boxes
                  (cons (cdg-make-box (vector (* i box-size) pos-y)
                                      box-size
                                      (aref text-line i))
                        boxes)))))
      (setq pos-y (+ pos-y +cdg-game-unit+))
      (forward-line 1))
    boxes))

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
