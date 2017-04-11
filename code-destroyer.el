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
  "Буфер, в которой расположен текст играющий роль игрового поля")

(defvar *cdg-game-buffer* nil
  "Буфер, в котором происходит сама игра")

(defvar *cdg-draw-buffer* nil
  "Символьный буфер, в котором идет отрисовка всех
   элементов игры.")

(defvar *cdg-game-zone* nil
  "Игровая зона. Внутри нее создаются все игровые объекты")

(defvar *cdg-ball* nil
  "Игровой мяч. Хранит размеры мяча, текущее положение и
   вектор направления")

(defvar *cdg-ball-on-platform* t
  "Если мяч еще не в движении и лежит на платформе, то
   переменная истинна, иначе ложна")

(defvar *cdg-boxes* nil
  "Список игровых боксов, которые должен поразить мяч,
   чтобы выиграть игру")

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
    (setq *cdg-game-zone* (cdg-make-zone-by-window (selected-window)))
    (setq *cdg-boxes*
          (remove-if-not (lambda (x)
                           (cdg-contain-rect-test (cdg-zone-box-rect *cdg-game-zone*)
                                                  (cdg-box-rect x)))
                         (cdg-make-boxes-by-buf-text
                          (current-buffer)
                          (cdg-zone-box-start *cdg-game-zone* :descart)
                          1)))
    (setq *cdg-draw-buffer*
          (cdg-make-char-buffer (cdg-zone-rows *cdg-game-zone*)
                                (cdg-zone-cols *cdg-game-zone*)
                                +cdg-space-sym+))
    (setq *cdg-platform*
          (cdg-make-platform
           (/ (cdg-rect-width (cdg-zone-rect *cdg-game-zone*)) 2.0)
           (* 5.0 +cdg-game-unit+)
           1.0
           ?-))
    (setq *cdg-ball*
          (cdg-return-ball-to-platform (cdg-make-ball (cdg-make-point 0 0) [1 1] ?o)
                                       *cdg-platform*
                                       *cdg-game-zone*))

    (cdg-debug (format "Создана игровaя зона %d x %d"
                       (cdg-zone-rows *cdg-game-zone*)
                       (cdg-zone-cols *cdg-game-zone*)))
    (cdg-debug (format "Создан буфер отрисовки %d x %d"
                       (cdg-char-buffer-rows *cdg-draw-buffer*)
                       (cdg-char-buffer-cols *cdg-draw-buffer*)))
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
  "Строит игровые боксы на основе текста буфера. Каждый символ текста
   рассматривается как игровой бокс. Размер бокса указывается в игровых единицах"
  (let ((begin-line nil)
        (box-size-dec (* box-size +cdg-game-unit+))
        (boxes '())
        (y-pos start-y)) ; Текущая позиция бокса по вертикали
    (with-buffer buffer
      (move-to-window-line 0)
      (setq *inx* 0)
      ; Проход по всем строкам окна
      (while (and (not (eobp)) (< *inx* (window-body-height)))
        (beginning-of-line)
        (setq begin-line (point))
        (end-of-line)
        (let ((text-line (buffer-substring-no-properties begin-line (point))))
          (dotimes (i (length text-line))
            (when (not (eq (aref text-line i) +cdg-space-sym+))
              (setq boxes
                    (cons (cdg-make-box (cdg-make-point (* i box-size-dec) y-pos)
                                        box-size
                                        (aref text-line i))
                          boxes)))))
        (setq y-pos (- y-pos box-size-dec))
        (forward-line 1)
        (setq *inx* (1+ *inx*))))
      boxes))

;; Функции отрисовки игровых объектов

(defun cdg-draw-platform (platform char-buffer zone)
  "Отрисовка игровой платформы, находящейся на
   самой нижней строке."
  (let* ((half-size (/ (cdg-platform-size platform) 2.0))
         (platform-row  (cdg-zone-platform-start zone :row))
         (start-pos (truncate (- (cdg-platform-pos platform)
                                 half-size))))
    (dotimes (i (ceiling (cdg-platform-size platform)))
      (cdg-set-char-safe char-buffer
                         (- (cdg-char-buffer-rows char-buffer)
                            (1+ platform-row))
                         (cdg-point-y
                          (cdg-zone-point-coord zone
                                                (cdg-make-point 0 (+ start-pos i))))
                         (cdg-platform-char platform)))))

(defun cdg-draw-ball (ball char-buffer zone)
  "Отрисова игрового мяча. Представляется одним
   единственным символом в указанной позиции"
  (let ((ball-pos (cdg-zone-point-coord zone
                                        (cdg-ball-pos ball))))
    (cdg-set-char-safe char-buffer
                       (- (cdg-char-buffer-rows char-buffer)
                          (1+ (cdg-point-y ball-pos)))
                       (cdg-point-x ball-pos)
                       (cdg-ball-char ball))))

(defun cdg-draw-boxes (boxes char-buffer zone)
  (dolist (box boxes)
    (let ((coord (cdg-zone-point-coord zone
                                       (cdg-box-pos box))))
      (cdg-set-char-safe char-buffer
                         (- (cdg-char-buffer-rows char-buffer)
                            (1+ (cdg-point-y coord)))
                         (cdg-point-x coord)
                         (cdg-box-char box)))))

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
