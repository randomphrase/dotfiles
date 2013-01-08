;;
;; Editing/movement commands - bound to keys a bit later on
;;

;; from http://www.emacswiki.org/cgi-bin/wiki/BackToIndentationOrBeginning
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;;
;; Functions to delete words without adding to the kill ring (bound to
;; M-backspace further down)
;; from http://www.emacswiki.org/cgi-bin/wiki/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; Recognise these are movement functions so that shift-<key> works properly
(dolist (cmd
 '(back-to-indentation-or-beginning delete-word backward-delete-word))
  (put cmd 'CUA 'move)
)
