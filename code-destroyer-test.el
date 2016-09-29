(require 'ert)
(require 'code-destroyer)


(ert-deftest cdg-to-length ()
  (should
   (equal (cdg-to-length "test string" 4 ? )
          "test"))
  (should
   (equal (cdg-to-length "" 4 ?x)
          "xxxx"))
  (should
   (equal (cdg-to-length "test string" 0 ?x)
          "")))

(ert-deftest cdg-make-char-buffer ()
  (should
   (equal (cdg-make-char-buffer 3 2 ?x)
          (list "xxxxxx" 3 2)))
  (should
   (equal (cdg-make-char-buffer 0 2 ?x)
          (list "" 0 0)))
  (should
   (equal (cdg-make-char-buffer 2 0 ?x)
          (list "" 0 0))))

(ert-deftest cdg-make-char-buffer-by-string ()
  (should
   (equal (cdg-make-char-buffer-by-string "xxxxxx" 2 ?x)
          (list "xxxxxx" 3 2)))
  (should
   (equal (cdg-make-char-buffer-by-string "" 4 ?x)
          (list "" 0 0)))
  (should
   (equal (cdg-make-char-buffer-by-string "xxxx" 2 ?x)
          (cdg-make-char-buffer 2 2 ?x)))
  (should
   (equal (cdg-make-char-buffer-by-string "xxx" 2 ?x)
          (cdg-make-char-buffer 2 2 ?x))))

(ert-deftest cdg-resize-char-buffer ()
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  6
                                  6
                                  ?x)
          (cdg-make-char-buffer 6 6 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  0
                                  ?x)
          (cdg-make-char-buffer 0 0 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  2
                                  ?x)
          (cdg-make-char-buffer 0 2 ?x)))
  (should
   (equal (cdg-resize-char-buffer (cdg-make-char-buffer 4 4 ?x)
                                  0
                                  0
                                  ?x)
          (cdg-make-char-buffer 0 0 ?x))))
