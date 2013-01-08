(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system

  ;; Set the frame format to show the visited file, often handy
  (setq frame-title-format '("" "Emacs - %f [%b]"))
  )

;; Make zooming affect frame instead of buffers
;; (require 'zoom-frm)

(provide 'init/appearance)
