(require 'cl-lib)

;; Set path to dependencies
(defvar extern-lisp-dir
  (expand-file-name "extern" user-emacs-directory))

;; Do package management first
(add-to-list 'load-path (expand-file-name "cask" extern-lisp-dir))
(require 'cask)
(cask-initialize)

;; CEDET needs to be set up early - replaces some built-in libraries
(load-file (expand-file-name "cedet/cedet-devel-load.el" extern-lisp-dir))

;; Add remaining extern directories to the load path
(dolist (E (directory-files extern-lisp-dir t "\\w+"))
  (when (and (file-directory-p E)
             (not (member (file-name-nondirectory E) '("cedet" "cask"))))
    (let ((EL (expand-file-name "lisp" E)))
      (add-to-list 'load-path (if (file-directory-p EL) EL E)))))

;; My lisp functions are here:
(setq defuns-dir (expand-file-name "defuns" user-lisp-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; abbrevs are here
(setq abbrev-file-name (expand-file-name "abbrevs" user-emacs-directory))

;; Twiddle exec paths and PATH for similar reasons
(let* ((trypaths '("/opt/local/bin"))
       (addpaths (cl-remove-if-not #'file-directory-p trypaths)))
  (nconc exec-path addpaths)
  (mapconcat #'concat `(,(getenv "PATH") ,@addpaths) path-separator))

;; And Info paths
(let* ((trypaths `(,(expand-file-name "info" user-emacs-directory)
                   ,(expand-file-name "cedet/doc/info" extern-lisp-dir)
                   ,(car (last (file-expand-wildcards "/usr/local/gcc-*/share/info")))
                   "/opt/local/share/info"))
       (addpaths (cl-remove-if-not (lambda (d) (and d (file-directory-p d))) trypaths)))
  ;; Append my stuff so that the emacs stuff appears first (a bit neater :)
  (nconc Info-default-directory-list addpaths))

; Move some stuff out of the home directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
(setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))

;; TODO: w32/cygwin stuff, see http://www.emacswiki.org/emacs/NTEmacsWithCygwin


(provide 'init/paths)
