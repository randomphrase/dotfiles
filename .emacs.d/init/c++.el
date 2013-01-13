
;;
;; c/c++ stuff
;;
(require-soft 'gtags)

;; Don't seem to need this stuff any more - the standard c++ headers have their own mode detection strings
;; 
;; (defun file-in-directory-list-p (file dirlist)
;;   "Returns true if the file specified is contained within one of
;; the directories in the list. The directories must also exist."
;;   (let ((dirs (mapcar 'expand-file-name dirlist))
;;         (filedir (expand-file-name (file-name-directory file))))
;;     (and
;;      (file-directory-p filedir)
;;      (member-if (lambda (x) ; Check directory prefix matches
;;                   (string-match (substring x 0 (min(length filedir) (length x))) filedir))
;;                 dirs))))

;; (defun buffer-standard-include-p ()
;;   "Returns true if the current buffer is contained within one of
;; the directories in the INCLUDE environment variable."
;;   (and (getenv "INCLUDE")
;;        (file-in-directory-list-p buffer-file-name (split-string (getenv "INCLUDE") path-separator))))

;; (add-to-list 'magic-fallback-mode-alist '(buffer-standard-include-p . c++-mode))

(defun my-c-initialization-hook ()
  ;; Debug keys
  (define-key c-mode-base-map [(f10)] 'gud-next)
  (define-key c-mode-base-map [(f11)] 'gud-step)

  ;; Show hard tabs
  ;; TODO: Use whitespace mode from Emacs 24
  ;;(show-ws-highlight-tabs)

  (add-to-list 'c-default-style (cons 'c++-mode (if (assoc "tibra" c-style-alist) "tibra" "stroustrup")))
)
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-common-hook ()
  (subword-mode 1)
  ;(c-toggle-auto-newline 1)
  (when (fboundp 'gtags-mode)
    (gtags-mode 1))
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Load work style and skeletons
(when (require-soft 'tibra-style)
  (autoload 'tibra-header-file "tibra-skeleton"
    "A skeleton for a Tibra c++ header file" t)
  (autoload 'tibra-source-file "tibra-skeleton"
    "A skeleton for a Tibra c++ source file" t)
  (autoload 'tibra-test-file "tibra-skeleton"
    "A skeleton for a Tibra c++ unit test source file" t)
)


;;
;; Doxymacs for doxygen comments:
;;
(when (require-soft 'doxymacs)
  ;; Use doxymacs in all c-modes:
  (add-hook 'c-mode-common-hook 'doxymacs-mode)
  ;; Use font-lock mode in c and c++ mode:
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
  (setq doxymacs-doxygen-style "C++")
)

(provide 'init/c++)
