;;; Вспомогательные функции, используемые повсеместно


(defmacro cdg-debug (&rest body)
  "Output debug info, if *cdg-debug* is t"
  `(when *cdg-debug*
     (print (concat ,@body)
            (get-buffer-create "cdg-debug"))))

(defun cdg-space-p (char)
  (eq char +cdg-space-sym+))

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

(defun cdg-same-sign-p (&rest numbers)
  "Все указанные числа одного знака?"
  (or (every (lambda (x) (>= x 0)) numbers)
      (every (lambda (x) (<= x 0)) numbers)))

(defun cdg-2d->1d (row col col-count)
  "Конвертирует индекс двумерного массива в одномерный"
  (if (< col col-count)
      (+ (* row col-count) col)
    nil))

(defmacro cdg-with-buffer (buffer &rest body)
  (let ((old-buffer (gensym))
        (result (gensym)))
    `(let ((,old-buffer (current-buffer))
           (,result nil))
       (switch-to-buffer ,buffer)
       (setq ,result (progn ,@body))
       (switch-to-buffer ,old-buffer)
       ,result)))

(defmacro cdg-with-win-text (line-sym &rest body)
  "Проходит по всему тексту, видимому в окне."
  (let ((begin-pos (gensym))
        (line-inx (gensym)))
    `(let ((,line-sym "")
           (,begin-pos nil)
           (,line-inx 0))
       (move-to-window-line 0)
       (while (and (not (eobp))
                   (< ,line-inx ,(window-body-height)))
         (beginning-of-line)
         (setq ,begin-pos (point))
         (end-of-line)
         (setq ,line-sym (buffer-substring-no-properties ,begin-pos (point)))
         ,@body
         (forward-line 1)
         (incf ,line-inx)))))

(defun cdg-max-len-str-in-win ()
  "Отыскивает самую длинную строку текста в активном окне и
  возвращает ее длину. Учитывается только текст умещающийся в
  окне, остальная часть буфера не учитывается"
  (let ((result 0))
    (cdg-with-win-text line-text
      (when (> (length line-text) result)
        (setq result (length line-text))))
    result))
