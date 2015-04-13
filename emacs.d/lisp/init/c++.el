
;;
;; c/c++ stuff
;;

(defun my-c-initialization-hook ()

  (subword-mode 1)

  ;; Show hard tabs
  ;; TODO: Use whitespace mode from Emacs 24
  ;;(show-ws-highlight-tabs)

  ;; Handy toggling of source/header files
  (define-key c-mode-base-map [(meta o)] 'projectile-find-other-file)
  (define-key c-mode-base-map [(shift meta o)] 'projectile-find-other-file-other-window)

  ;; If we have clang-format, load and bind it to C-|
  (when (fboundp 'clang-format-region)
    (define-key c-mode-base-map [(ctrl |)] 'clang-format-region)
    (define-key c-mode-base-map [(ctrl meta |)] 'clang-format-buffer)
    )
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-common-hook ()

  ;(c-toggle-auto-newline 1)
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'init/c++)
