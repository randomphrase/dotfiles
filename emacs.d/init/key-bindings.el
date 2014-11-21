;; TODO: Get more good ones here: https://github.com/bbatsov/prelude/blob/master/core/prelude-global-keybindings.el

;; WindMove mode - use mod-arrow keys to move focus to the frame in that direction
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))
  
(define-key ctl-x-4-map [t]     'transpose-windows)
(define-key ctl-x-4-map [left]  'transpose-windows-left)
(define-key ctl-x-4-map [right] 'transpose-windows-right)

(require 'subword)
(define-key subword-mode-map [(control left)] 'subword-backward)
(define-key subword-mode-map [(control right)] 'subword-forward)

(global-set-key [f5] 'next-error)
(global-set-key [(shift f5)] 'previous-error)

(global-set-key [f7] 'compile)
(global-set-key [(shift f7)] 'recompile)

;; TODO: replace?
;; (define-key c-mode-base-map [(f10)] 'gud-next)
;; (define-key c-mode-base-map [(f11)] 'gud-step)
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

;; Don't need to be prompted for a buffer to kill...
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; I *hate* getting overwrite mode by accident...
(global-unset-key [(insert)])

;; PuTTY sends this instead of end:
;; (not needed?)
;;(global-set-key [select] 'end-of-line)

(autoload 'copy-from-above-command "misc")
(global-set-key [(meta p)] 'copy-from-above-command)

;; Magit
(autoload 'magit-status "magit")
(global-set-key (kbd "C-x g") 'magit-status)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Proced mode is cool!
(global-set-key (kbd "C-x p") 'proced)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; smerge has horrible key bindings by default, add some nicer ones
(defun my-smerge-hook ()
  (define-key smerge-mode-map (kbd "C-M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "C-M-a") 'smerge-keep-all)
  (define-key smerge-mode-map (kbd "C-M-m") 'smerge-keep-mine)
  (define-key smerge-mode-map (kbd "C-M-o") 'smerge-keep-other))
(add-hook 'smerge-mode-hook 'my-smerge-hook)

(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(define-key global-map (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x F") 'projectile-find-file-dwim)

;; Use option as meta
;(setq mac-option-modifier 'meta)
;(setq mac-pass-option-to-system nil)

(provide 'init/key-bindings)
