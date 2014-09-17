;;; ac-mozc.el --- auto-complete sources for Japanese input using Mozc

;; Copyright (C) 2014 igjit

;; Author: igjit <igjit1@gmail.com>
;; URL: https://github.com/igjit/ac-mozc
;; Version: 0.0.3
;; Package-Requires: ((cl-lib "0.5") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'mozc)
(require 'auto-complete)

(defgroup ac-mozc nil
  "Auto-complete sources for Japanese input using Mozc."
  :group 'auto-complete
  :prefix "ac-mozc-")

(defcustom ac-mozc-remove-space t
  "Non-nil if a space between two alphanumeric strings should be removed.
When a translated Japanese word is selected and it follows an
alphanumeric string and a space, the space in between is removed. To
stop this behavior, set this variable to nil."
  :type 'boolean
  :group 'ac-mozc)

(defvar ac-mozc-preedit nil)
(defvar ac-mozc-candidates nil)
(defvar ac-mozc-ac-point nil)
(defvar ac-mozc-sending nil)

(defun ac-mozc-prefix ()
  (save-excursion
    (if (re-search-backward "\\(?:^\\|[^a-zA-Z-,.!?]\\)\\([a-zA-Z-,.!?]+\\)\\="
                            (line-beginning-position) t)
        (match-beginning 1))))

(defadvice ac-cleanup (before ac-mozc-before-cleanup-advice activate)
  (setq ac-mozc-ac-point ac-point))

(defun ac-mozc-action ()
  (if ac-mozc-remove-space
      (save-excursion
        (goto-char ac-mozc-ac-point)
        (let ((str (buffer-substring-no-properties
                    (max (point-at-bol) (- (point) 2))
                    (point))))
          (if (string-match "\\w " str)
              (backward-delete-char 1)))))
  (setq ac-mozc-ac-point nil))

(defun ac-mozc-match (ac-prefix candidates)
  (if ac-mozc-sending
      nil
    (setq ac-mozc-sending t)
    (unwind-protect (ac-mozc-send-word ac-prefix)
      (setq ac-mozc-sending nil))))

(defun ac-mozc-send-word (word)
  (mozc-clean-up-session)
  ;; Send word
  (mapc 'ac-mozc-handle-event (string-to-vector word))
  (if (not (ac-mozc-kana-p (ac-mozc-pick-preedit ac-mozc-preedit)))
      nil
    (let ((candidates (ac-mozc-pick-candidates ac-mozc-candidates)))
      ;; Convert (SPC)
      (ac-mozc-handle-event ?\s)
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
            (candidates (ac-mozc-all-candidate-words-to-candidates
                         (mozc-protobuf-get output 'all-candidate-words))))
        (setq ac-mozc-preedit preedit
              ac-mozc-candidates candidates)
        t))
     ;; not consumed
     (t
      (setq ac-mozc-preedit nil
            ac-mozc-candidates nil)
      nil))))

(defun ac-mozc-all-candidate-words-to-candidates (all-candidate-words)
  (list (cons 'candidate (cdr (assq 'candidates all-candidate-words)))))

(defun ac-mozc-pick-preedit (preedit)
  (cdr (assq 'key (cadr (assq 'segment preedit)))))

(defun ac-mozc-pick-candidates (candidates)
  (mapcar
   (lambda (c) (cdr (assoc 'value c)))
   (cdr (assoc 'candidate candidates))))

(defun ac-mozc-kana-p (str)
  (string-match "^[^ａ-ｚ]+[ｂｃｄｆｇｈｊｋｌｍｎｐｑｒｓｔｖｗｘｙｚ]?[ｙｈ]?$" str))

(ac-define-source mozc
  '((match . ac-mozc-match)
    (prefix . ac-mozc-prefix)
    (symbol . "M")
    (action . ac-mozc-action)))

(defun ac-mozc-remove-non-ascii-character (words)
  (cl-mapcan (lambda (x) (split-string x "\\Ca+" t)) words))

(defun ac-mozc-partial-match (string collection)
  (let ((regex (concat "\\<" string)))
    (cl-remove-if-not (lambda (x) (string-match-p regex x)) collection)))

(defun ac-mozc-word-candidates-ascii-only (&optional buffer-pred)
  (let ((ac-match-function 'ac-mozc-partial-match))
    (ac-mozc-remove-non-ascii-character
     (ac-word-candidates buffer-pred))))

(ac-define-source ascii-words-in-same-mode-buffers
  '((prefix . ac-mozc-prefix)
    (init . ac-update-word-index)
    (candidates . (ac-mozc-word-candidates-ascii-only
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))))))

(provide 'ac-mozc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ac-mozc.el ends here
