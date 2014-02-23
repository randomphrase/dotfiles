;; TODO replace with dash
(require 'cl)

;; Set path to dependencies
(setq extern-lisp-dir
      (expand-file-name "extern" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path extern-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files extern-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; My lisp functions are here:
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-readable-p file)
    (load file)))

;; clang-format is installed in strange locations
(let ((D (car (remove-if-not 'file-directory-p
                             (append (file-expand-wildcards "/opt/local/libexec/llvm-*/libexec/clang-format")
                                     (file-expand-wildcards "/usr/share/emacs/site-lisp/clang-format-*"))))))
  (when D (add-to-list 'load-path D)))


;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; abbrevs are here
(setq abbrev-file-name (expand-file-name "abbrevs" user-emacs-directory))

;; Twiddle exec paths and PATH for similar reasons
(dolist (my-exec-path
         (remove-if-not 'file-directory-p
                        '("/opt/local/bin")))
  (add-to-list 'exec-path my-exec-path)
  (setenv "PATH" (concat (getenv "PATH") ":" my-exec-path))
  )

;; And Info paths
(dolist (my-info-path
         (remove-if-not (lambda (d) (and d (file-directory-p d)))
		    (list (expand-file-name "info" user-emacs-directory)
			  (expand-file-name "cedet/doc/info" extern-lisp-dir)
			  (car (last (file-expand-wildcards "/usr/local/gcc-*/share/info")))
			  )))

  ;; Append it so that the emacs stuff appears first (a bit neater :)
  (add-to-list 'Info-default-directory-list my-info-path)
  )

; Move some stuff out of the home directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
(setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))

;; TODO: w32/cygwin stuff, see http://www.emacswiki.org/emacs/NTEmacsWithCygwin


(provide 'init/paths)
