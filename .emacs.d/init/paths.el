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
  (when (file-regular-p file)
    (load file)))


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
(let ((my-exec-paths '("/opt/local/bin"
                       "c:/cygwin/bin"
                       "c:/cygwin/usr/local/bin")))
  (dolist (my-exec-path 
           (remove-if-not 'file-directory-p
                          (mapcar 'expand-file-name my-exec-paths)))
    (add-to-list 'exec-path my-exec-path)
    (setenv "PATH" (concat (getenv "PATH") ":" my-exec-path))
    ))

;; And Info paths
(let ((my-info-paths (list (expand-file-name "info" user-emacs-directory)
			   (expand-file-name "cedet/doc/info" extern-lisp-dir)
			   "/usr/local/gcc-4.6.0/share/info"
			   "c:/cygwin/usr/share/info")))
  (dolist (my-info-path
           (remove-if-not 'file-directory-p
                          (mapcar 'expand-file-name my-info-paths)))
    ;; Append it so that the emacs stuff appears first (a bit neater :)
    (add-to-list 'Info-default-directory-list my-info-path)))

; Move some stuff out of the home directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))


;; TODO: w32/cygwin stuff, see http://www.emacswiki.org/emacs/NTEmacsWithCygwin


(provide 'init/paths)
