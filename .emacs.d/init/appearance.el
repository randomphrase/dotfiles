(setq is-mac (equal system-type 'darwin))

;; Turn off this stuff - we don't need it. Except for mac menu bars - they don't cost any screen
;; real-estate, might as well leave them enabled...
(if (and (not is-mac) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Frame title format swiped from here: http://emacs-fu.blogspot.com/2011/01/setting-frame-title.html
(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
                                                        (abbreviate-file-name (buffer-file-name))
                                                      "%b")) " [%*]"))

;; Add watchwords when coding
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'coding-hook 'add-watchwords)

;; Make zooming affect frame instead of buffers
;; (require 'zoom-frm)

(provide 'init/appearance)
