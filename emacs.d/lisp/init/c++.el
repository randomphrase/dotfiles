
;;
;; c/c++ stuff
;;

;; set up our own c++ extension mappings for ff-other-file support
(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".ipp"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c"))
    ))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(defun my-c-initialization-hook ()

  (subword-mode 1)

  ;; Show hard tabs
  ;; TODO: Use whitespace mode from Emacs 24
  ;;(show-ws-highlight-tabs)

  ;; (setq ff-ignore-include t)
  (define-key c-mode-base-map [(meta o)] 'projectile-find-other-file)

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
(add-hook 'c-mode-common-hook 'google-set-c-style)

(provide 'init/c++)
