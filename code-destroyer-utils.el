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

(defmacro with-buffer (buffer &rest body)
  (let ((old-buffer (gensym))
        (result (gensym)))
    `(let ((,old-buffer (current-buffer))
           (,result nil))
       (switch-to-buffer ,buffer)
       (setq ,result (progn ,@body))
       (switch-to-buffer ,old-buffer)
       ,result)))

(defun cdg-max-len-str-in-win ()
  "Отыскивает самую длинную строку текста в активном окне и
  возвращает ее длину. Учитывается только текст умещающийся в
  окне, остальная часть буфера не учитывается"
  (let ((begin-pos nil)
        (line-inx 0)
        (result 0))
    (move-to-window-line 0)
    (while (and (not (eobp))
                (< line-inx (window-body-height)))
      (beginning-of-line)
      (setq begin-pos (point))
      (end-of-line)
      (let ((line-text (buffer-substring-no-properties begin-pos (point))))
        (when (> (length line-text) result)
          (setq result (length line-text))))
      (forward-line 1)
      (setq line-inx (1+ line-inx)))
    result))
