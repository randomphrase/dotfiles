;; smartparens mode initialization
(require 'smartparens-config)

;; Use paredit bindings minus C-right/C-left
(sp-use-paredit-bindings)
(setq sp-override-key-bindings '(("C-<right>" . nil) ("C-<left>" . nil)))
(sp--update-override-key-bindings)

;; TODO: needed?
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(show-smartparens-global-mode +1)

;; Move to keybindings?
(define-key prog-mode-map (kbd "M-(") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
(define-key prog-mode-map (kbd "M-[") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
(define-key prog-mode-map (kbd "M-\"") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))

(provide 'init/smartparens)
