
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

;; set up our own c++ extension mappings
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
  (define-key c-mode-base-map [(meta o)] 'ff-get-other-file)

  (add-to-list 'c-default-style (cons 'c++-mode (if (assoc "tibra" c-style-alist) "tibra" "stroustrup")))
)
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-common-hook ()

  ;(c-toggle-auto-newline 1)
  (when (fboundp 'gtags-mode)
    (gtags-mode 1))
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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

;; Add our file extensions to enable project searching using find-file-in-project
(require 'find-file-in-project)
(setq ffip-patterns (append '("*.cpp" "*.hpp" "*.cxx" "*.hxx") ffip-patterns))

(provide 'init/c++)
