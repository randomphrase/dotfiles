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

(global-set-key [(control tab)] 'other-window)

;; I *hate* getting overwrite mode by accident...
(global-unset-key [(insert)])

;; Use option as meta
;(setq mac-option-modifier 'meta)
;(setq mac-pass-option-to-system nil)

(provide 'init/key-bindings)
