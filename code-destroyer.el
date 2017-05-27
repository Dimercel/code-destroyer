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
  (setq *cdg-game-timer* (run-with-timer 0 0.5 'cdg-main-loop)))


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

(defconst +cdg-gap+ (/ +cdg-game-unit+ 100.0)
  "Величина зазора. Малая относительно величины игровой единицы.")

(defconst +cdg-ball-step+ (/ +cdg-game-unit+ 2.0)
  "Длина пути, который проходит мяч за один шаг")

(defconst *cdg-debug* nil
  "Константа отвечает за режим вывода отладочной ин-ии")


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
    (if (and (< char-inx size) (>= row 0) (>= col 0))
        (cdg-get-char buffer row col)
      bad-value)))

(defun cdg-set-char-safe (buffer row col new-value)
  "Безопасная версия cdg-set-char возвращающая NIL в случае
   отсутсвия указанного индекса. Не присваивает значения в
   случае не корректного индекса"
  (let ((size (cdg-char-buffer-size buffer))
        (char-inx (cdg-2d->1d row col (cdg-char-buffer-cols buffer))))
    (if (and (< char-inx size) (>= row 0) (>= col 0))
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
  (cdg-platform-move *cdg-platform*
                     (- (cdg-platform-speed *cdg-platform*)))
  (when *cdg-ball-on-platform*
    (setq *cdg-ball*
          (cdg-return-ball-to-platform *cdg-ball*
                                       *cdg-platform*
                                       *cdg-game-zone*))))

(defun cdg-right ()
  "Двигает платформу вправо"
  (interactive)
  (cdg-platform-move *cdg-platform*
                           (cdg-platform-speed *cdg-platform*))
  (when *cdg-ball-on-platform*
    (setq *cdg-ball*
          (cdg-return-ball-to-platform *cdg-ball*
                                       *cdg-platform*
                                       *cdg-game-zone*))))

(defun cdg-action ()
  "Отпускает мяч с платформы"
  (interactive)
  (setq *cdg-ball-on-platform* nil))

(defun cdg-init ()
  "Собственно инициализация игры."
  (let ((inhibit-read-only t))
    (setq-local truncate-lines t)
    (with-buffer *cdg-code-buffer*
                 (setq *cdg-game-zone* (cdg-make-zone-by-window (selected-window))))
    (setq *cdg-draw-buffer*
          (cdg-make-char-buffer (cdg-zone-rows *cdg-game-zone*)
                                (cdg-zone-cols *cdg-game-zone*)
                                +cdg-space-sym+))
    (with-buffer *cdg-code-buffer*
      (setq *cdg-boxes*
            (remove-if-not (lambda (x)
                            (cdg-contain-rect-test (cdg-zone-box-rect *cdg-game-zone*)
                                                   (cdg-box-rect x)))
                          (cdg-make-boxes-by-buf-text
                            (current-buffer)
                            1
                            *cdg-game-zone*))))
    (setq *cdg-platform*
          (cdg-make-platform
           (/ (cdg-rect-width (cdg-zone-rect *cdg-game-zone*)) 2.0)
           (* 5.0 +cdg-game-unit+)
           +cdg-game-unit+
           ?-))
    (setq *cdg-ball*
          (cdg-return-ball-to-platform (cdg-make-ball (cdg-make-point 0 0) [-0.8 1] ?o)
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

(defun cdg-make-boxes-by-buf-text (buffer box-size zone)
  "Строит игровые боксы на основе текста буфера. Каждый символ текста
   рассматривается как игровой бокс. Размер бокса указывается в игровых единицах"
  (let ((y-pos 0) ; Текущая позиция бокса по вертикали
        (box-size-dec (* box-size +cdg-game-unit+))
        (boxes '())
        (zone-height (cdg-rect-height (cdg-zone-rect zone))))
    (with-buffer buffer
      (with-win-text text-line
        (dotimes (char-inx (length text-line))
          (let ((cur-char (aref text-line char-inx)))
            (when (not (cdg-space-p cur-char))
              (setq boxes
                    (cons (cdg-make-box (cdg-make-point (* char-inx box-size-dec)
                                                        (- zone-height y-pos))
                                        box-size
                                        cur-char)
                          boxes)))))
        (setq y-pos (+ y-pos box-size-dec))))
      boxes))



;;; Функции проверки коллизий игровых объектов



(defun cdg-ball-platform-collision ()
  (let* ((platform-rect
          (cdg-make-rect
           (cdg-make-point (cdg-platform-start-pos *cdg-platform*)
                           (cdg-zone-space-start *cdg-game-zone*
                                                 :descart))
           (cdg-make-point (cdg-platform-end-pos *cdg-platform*)
                           0)))
         (cross-point (cdg-rect-ray-intersection
                       platform-rect
                       (cdg-ball-pos *cdg-ball*)
                       (cdg-ball-direct *cdg-ball*))))
    (when (and cross-point
               (< (cdg-point-dist (cdg-ball-pos *cdg-ball*)
                                  cross-point)
                  +cdg-ball-step+))
      (cdg-ball-move-to *cdg-ball* cross-point)
      (cdg-ball-change-direct
       *cdg-ball*
       (cdg-mirror-vector (cdg-ball-direct *cdg-ball*)
                          'horizontal))
      (cdg-ball-move *cdg-ball* +cdg-gap+)
      t)))

(defun cdg-ball-boxes-collision ()
  "Определяет поведение игры при соприкосновении
  мяча с игровым боксом"
  (let ((crash-box (cdg-ball-boxes-test *cdg-game-zone*
                                        *cdg-boxes*
                                        *cdg-ball*
                                        +cdg-ball-step+)))
    (when crash-box
      (let ((cross-point (cdg-rect-ray-intersection
                          (cdg-box-rect crash-box)
                          (cdg-ball-pos *cdg-ball*)
                          (cdg-ball-direct *cdg-ball*)))
            (ball-dir (cdg-ball-direct *cdg-ball*)))
        (setq *cdg-boxes*
              (delete-if (lambda (x)
                           (equal (cdg-box-pos crash-box)
                                  (cdg-box-pos x)))
                         *cdg-boxes*))
        ;; Перемещаем игровой бокс на точку его пересечения
        ;; с боксом
        (cdg-ball-move-to *cdg-ball* cross-point)
        ;; Произошло столкновение, вектор направления игрового
        ;; мяча должен измениться
        (let ((cross-side (cdg-rect-point-side
                           (cdg-box-rect crash-box)
                           cross-point)))
          (when cross-side
            (cdg-ball-change-direct *cdg-ball*
                                    (cdg-mirror-vector ball-dir
                                                       cross-side))
            ;; передвигаем мяч вперед, чтобы он не находился
            ;; на границе пересечения
            (cdg-ball-move *cdg-ball* +cdg-gap+)))))))


(defun cdg-ball-limits-collision ()
  "Не позволяет мячу улететь за пределы игровой зоны.
  При соприкосновении мяча и границы зоны, мяч должен
  вести себя как и при столкновении с боксом."
  (let ((crash-item nil))
    (labels ((calc-cross-point (rect)
                               (vector rect
                                       (cdg-rect-ray-intersection
                                        rect
                                        (cdg-ball-pos *cdg-ball*)
                                        (cdg-ball-direct *cdg-ball*))))
             (filter-by-dist (x)
                             (or (null (aref x 1))
                                 (> (cdg-point-dist (cdg-ball-pos *cdg-ball*)
                                                    (aref x 1))
                                    +cdg-ball-step+))))
      (setq crash-item
            (first
             (remove-if #'filter-by-dist
                        (map 'list
                             #'calc-cross-point
                             (cdg-limiting-rects *cdg-game-zone*)))))
      (if crash-item
          (progn
            (cdg-debug (format "Пересечение с границей окна: %s" (aref crash-item 1)))
            (cdg-ball-move-to *cdg-ball* (aref crash-item 1))
            (cdg-ball-change-direct
             *cdg-ball*
             (cdg-mirror-vector (cdg-ball-direct *cdg-ball*)
                                (cdg-rect-point-side (aref crash-item 0)
                                                     (aref crash-item 1))))
            (cdg-ball-move *cdg-ball* +cdg-gap+)
            t)
        nil))))


(defun cdg-collision ()
  (or
   (cdg-ball-boxes-collision)
   (cdg-ball-limits-collision)
   (cdg-ball-platform-collision)))

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
                         (truncate
                          (cdg-point-y
                           (cdg-zone-point-coord
                            zone
                            (cdg-make-point 0 (+ start-pos i)))))
                         (cdg-platform-char platform)))))

(defun cdg-draw-ball (ball char-buffer zone)
  "Отрисовка игрового мяча. Представляется одним
   единственным символом в указанной позиции"
  (let ((ball-pos (cdg-zone-point-coord zone
                                        (cdg-ball-pos ball))))
    (when ball-pos
      (cdg-set-char-safe char-buffer
                         (- (cdg-char-buffer-rows char-buffer)
                            (truncate (1+ (cdg-point-y ball-pos))))
                         (truncate (cdg-point-x ball-pos))
                         (cdg-ball-char ball)))))

(defun cdg-draw-boxes (boxes char-buffer zone)
  (dolist (box boxes)
    (let ((coord (cdg-zone-point-coord zone
                                       (cdg-box-pos box))))
      (cdg-set-char-safe char-buffer
                         (- (cdg-char-buffer-rows char-buffer)
                            (truncate (1+ (cdg-point-y coord))))
                         (truncate (cdg-point-x coord))
                         (cdg-box-char box)))))

(defun cdg-draw-game ()
  "Собственно отрисовка всех элементов игры"
  (let ((inhibit-read-only t))
    (switch-to-buffer *cdg-game-buffer*)
    (erase-buffer)
    (setq *cdg-draw-buffer*
          (cdg-make-char-buffer (cdg-zone-rows *cdg-game-zone*)
                                (cdg-zone-cols *cdg-game-zone*)
                                +cdg-space-sym+))

    (cdg-draw-boxes *cdg-boxes* *cdg-draw-buffer* *cdg-game-zone*)
    (cdg-draw-platform *cdg-platform* *cdg-draw-buffer* *cdg-game-zone*)
    (cdg-draw-ball *cdg-ball* *cdg-draw-buffer* *cdg-game-zone*)

    (cdg-output-char-buffer *cdg-draw-buffer*)
    (beginning-of-buffer)))

(defun cdg-main-loop ()
  "Главный цикл игры"
    (when (eq (current-buffer) *cdg-game-buffer*)
      (unless *cdg-ball-on-platform*
        (unless (cdg-collision)
          (cdg-ball-move *cdg-ball* +cdg-ball-step+)))
      (cdg-draw-game)))


(provide 'code-destroyer-game)
