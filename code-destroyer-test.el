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


