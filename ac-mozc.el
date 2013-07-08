;; -*- coding: utf-8 -*-
;;; ac-mozc.el --- An auto-complete source for Mozc

(eval-when-compile (require 'cl))
(require 'mozc)
(require 'auto-complete)

(defvar ac-mozc-preedit nil)
(defvar ac-mozc-candidates nil)
(defvar ac-mozc-ac-point nil)

(defvar ac-source-mozc
  '((match . ac-mozc-match)
    (prefix . ac-mozc-prefix)
    (symbol . "M")
    (action . ac-mozc-action)))

(defun ac-mozc-prefix ()
  (save-excursion
    (if (memq (char-before) '(?, ?. ?? ?!))
        ;; Include punctuation
        (backward-char))
    (let ((prefix (ac-prefix-symbol)))
      (if prefix
          (let ((point (re-search-backward "[^\x0-\x255]" nil t)))
            (if (and point
                     (<= prefix point))
                (1+ point)
              prefix))))))

(defadvice ac-cleanup (before ac-mozc-before-cleanup-advice activate)
  (setq ac-mozc-ac-point ac-point))

(defun ac-mozc-action ()
  (save-excursion
    (goto-char ac-mozc-ac-point)
    (let ((str (buffer-substring-no-properties
                (max (point-at-bol) (- (point) 2))
                (point))))
      (if (string-match "\\w " str)
          (backward-delete-char 1))))
  (setq ac-mozc-ac-point nil))

(defun ac-mozc-match (ac-prefix candidates)
  (ac-mozc-send-word ac-prefix))

(defun ac-mozc-send-word (word)
  (mozc-clean-up-session)
  ;; Send word
  (mapc 'ac-mozc-handle-event (string-to-vector word))
  (let ((size (cdr (assq 'size ac-mozc-candidates))))
    (if (> size 1)
        ;; Get more candidates
        (loop repeat 4 do (ac-mozc-handle-event 'tab))))
  (if (not (ac-mozc-kana-p (ac-mozc-pick-preedit ac-mozc-preedit)))
      nil
    (let ((candidates (ac-mozc-pick-candidates ac-mozc-candidates)))
      ;; Cancel selection (C-g)
      (ac-mozc-handle-event ?\^g)
      ;; Henkan (SPC SPC)
      (loop repeat 2 do (ac-mozc-handle-event ?\s))
      (append candidates (ac-mozc-pick-candidates ac-mozc-candidates)))))

(defun ac-mozc-handle-event (event)
  (let ((output (mozc-session-sendkey
                 (mozc-key-event-to-key-and-modifiers event))))
    (cond
     ;; error
     ((null output)
      (mozc-clean-up-session)
      (mozc-abort)
      (error "Mozc session failed."))
     ;; consumed
     ((mozc-protobuf-get output 'consumed)
      (let ((preedit (mozc-protobuf-get output 'preedit))
            (candidates (mozc-protobuf-get output 'candidates)))
        (setq ac-mozc-preedit preedit
              ac-mozc-candidates candidates)
        t))
     ;; not consumed
     (t
      (setq ac-mozc-preedit nil
            ac-mozc-candidates nil)
      nil))))

(defun ac-mozc-pick-preedit (preedit)
  (cdr (assq 'value (cadr (assq 'segment preedit)))))

(defun ac-mozc-pick-candidates (candidates)
  (mapcar
   (lambda (c) (cdr (assoc 'value c)))
   (cdr (assoc 'candidate candidates))))

(defun ac-mozc-kana-p (str)
  (string-match "^[^ａ-ｚ]+[ｂｃｄｆｇｈｊｋｌｍｎｐｑｒｓｔｖｗｘｙｚ]?[ｙｈ]?$" str))

(defun ac-mozc-complete ()
  "Start mozc completion at current point."
  (interactive)
  (auto-complete '(ac-source-mozc)))

(provide 'ac-mozc)
;;; ac-mozc.el ends here
