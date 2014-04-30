(require 'ert)
(require 'cl-lib)
(require 'ac-mozc)

(defun ac-mozc-test/last-prefix (str &optional init)
  (with-temp-buffer
    (if init
        (funcall init))
    (insert str)
    (let ((prefix (ac-mozc-prefix)))
      (if prefix
          (buffer-substring-no-properties prefix (point-max))))))

(defun ac-mozc-test/prefix-in-mode (mode)
  (mapc (lambda (pair)
          (should (string= (ac-mozc-test/last-prefix (car pair) mode)
                           (cdr pair))))
        '(("emacs" . "emacs")
          ("emacs lisp" . "lisp")
          ("emacs " . nil)
          ("one two three" . "three")
          ("mo-doresu" . "mo-doresu")
          ("so-suko-do" . "so-suko-do")
          ("変換kouho" . "kouho")
          ("(def" . "def")
          ("[def" . "def")
          ("{def" . "def")
          ("\"def" . "def")
          ("'def" . "def")
          ("#def" . "def")
          ("です." . ".")
          ("です," . ",")
          ("はい!" . "!")
          ("はい?" . "?")
          ("はい!?" . "!?")
          ("はい!!" . "!!")
          ("はい!!!" . "!!!")
          ("はい..." . "...")
          (" ." . ".")
          (" ," . ","))))

(ert-deftest ac-mozc-test/prefix ()
  (cl-loop for mode in '(fundamental-mode text-mode perl-mode ruby-mode)
           do
           (ac-mozc-test/prefix-in-mode mode)))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
