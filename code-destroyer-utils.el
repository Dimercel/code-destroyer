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
