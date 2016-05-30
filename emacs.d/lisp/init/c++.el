
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
  (setq-local show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Use modern c++ font lock mode
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(provide 'init/c++)
