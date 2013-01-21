;; WindMove mode - use mod-arrow keys to move focus to the frame in that direction
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


(global-set-key [f5] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f6] 'kmacro-end-or-call-macro)

(global-set-key [f4] 'next-error)
(global-set-key [f7] 'compile)
(global-set-key [(shift f7)] 'recompile)
(global-set-key [(shift f4)] 'previous-error)

(global-set-key [f9]    'gdb-toggle-breakpoint)
(global-set-key [f10]   'gud-next)
(global-set-key [f11]   'gud-step)
(global-set-key [(shift f11)]   'gud-finish)

(global-set-key [(home)] 'back-to-indentation-or-beginning)
(global-set-key [(end)] 'move-end-of-line)
(global-set-key [(meta backspace)] 'backward-delete-word)

;; The goto-last-change command is handy
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key [(control \')] 'goto-last-change)

(global-set-key [(control tab)] 'next-multiframe-window)

;; I *hate* getting overwrite mode by accident...
(global-unset-key [(insert)])

;; PuTTY sends this instead of end:
(global-set-key [select] 'end-of-line)

(autoload 'copy-from-above-command "misc")
(global-set-key [(meta p)] 'copy-from-above-command)

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")


;; On mac, use C-s-up/down to toggle fullscreen, similar bindings to ubuntu defaults
(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key [(control super up)] 'ns-toggle-fullscreen)
  (global-set-key [(control super down)] 'ns-toggle-fullscreen)
  )

;; smerge has horrible key bindings by default, add some nicer ones
(defun my-smerge-hook ()
  (define-key smerge-mode-map (kbd "C-M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "C-M-a") 'smerge-keep-all)
  (define-key smerge-mode-map (kbd "C-M-m") 'smerge-keep-mine)
  (define-key smerge-mode-map (kbd "C-M-o") 'smerge-keep-other))
(add-hook 'smerge-mode-hook 'my-smerge-hook)

(when (require-soft 'iedit)
  (define-key global-map (kbd "C-;") 'iedit-mode)
  (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
)

;; Use option as meta
;(setq mac-option-modifier 'meta)
;(setq mac-pass-option-to-system nil)

(provide 'init/key-bindings)
