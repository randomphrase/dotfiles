;; Alastair's emacs file

;; We use remove-if-not, part of cl
(require 'cl)

;; Twiddle load paths to find my stuff:
(setq my-load-paths '("~/.emacs.d/lisp"
                      "/Library/Application Support/Emacs/site-lisp"))
(dolist (my-path 
         (remove-if-not 'file-directory-p
                        (mapcar 'expand-file-name my-load-paths)))
  (let ((default-directory my-path))
    (progn
      (add-to-list 'load-path my-path)
      ;; Now add the subdirectories
      (dolist (subdir (remove-if-not 'file-directory-p (directory-files my-path)))
        (progn
          (add-to-list 'load-path (expand-file-name subdir))
          ;; Special case to ensure we can load cedet
          (if (and (string= subdir "cedet") (file-directory-p "cedet/common"))
              (add-to-list 'load-path (expand-file-name "cedet/common")))
          ))
      )))

;; Twiddle exec paths for similar reasons
(let ((my-exec-paths '("/opt/local/bin"
                       "c:/cygwin/bin"
                       "c:/cygwin/usr/local/bin")))
  (dolist (my-exec-path 
           (remove-if-not 'file-directory-p
                          (mapcar 'expand-file-name my-exec-paths)))
    (add-to-list 'exec-path my-exec-path)))

;; And Info paths
(let ((my-info-paths '("~/.emacs.d/info"
                       "/usr/share/info"
                       "/opt/local/share/info")))
  (dolist (my-info-path
           (remove-if-not 'file-directory-p
                          (mapcar 'expand-file-name my-info-paths)))
    ;; Append it so that the emacs stuff appears first (a bit neater :)
    (add-to-list 'Info-default-directory-list my-info-path t)))


;; Windows stuff:
(if (eq system-type 'windows-nt)
    (progn 
      (require 'w32shell)
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      ))

;; Start the emacs server mode
(server-start)

;; Visual effects first
;(require 'zenburn)
;(zenburn)
(require 'color-theme)
(color-theme-initialize)
(color-theme-gtk-ide)

;; from http://www.emacswiki.org/cgi-bin/wiki/BackToIndentationOrBeginning
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;;
;; Functions to delete words without adding to the kill ring (bound to
;; M-backspace further down)
;; from http://www.emacswiki.org/cgi-bin/wiki/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn (forward-word (- arg))
                        (point))))

;; Recognise these are movement functions so that shift-<key> works properly
(dolist (cmd
 '(back-to-indentation-or-beginning delete-word backward-delete-word))
  (put cmd 'CUA 'move)
)

;; The goto-last-change command is handy
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)


;;
;; Favourite settings/modes
;;

;; Tab setup
(setq-default tab-width 4)          ; Set tab width to 4
(setq-default indent-tabs-mode nil)	; Use spaces for indenting - NOT TABs

;; Start session mode
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;
;; Major modes / Language setup
;;

;; Markdown mode for text editing
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

;; load makefile-mode for *.mak files
(setq auto-mode-alist
      (append (list (cons "\\.mak\\'" 'makefile-mode))
	      auto-mode-alist))

;; CMake mode for CMake*.txt files
(require 'cmake-mode)
(setq cmake-tab-width 4)
(setq auto-mode-alist
      (append '(("CMake[A-Za-z0-9-]*\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; Apache mode
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))

;; ;;
;; ;; Python mode
;; ;;
;; (setq auto-mode-alist
;;       (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode)
;;             interpreter-mode-alist))

;; (autoload 'python-mode "python-mode" "Python editing mode." t)


;;
;; cedet
;;
(load "cedet")
(semantic-load-enable-gaudy-code-helpers)

(let ((dir "/opt/local/include/"))
  (and (file-directory-p dir)
       (semantic-add-system-include dir)))

(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

;; TODO: don't load semantic for a tramp-sourced file
;; (add-hook 'semantic-init-hooks (lambda ()
;;                                  (imenu-add-to-menubar "Tokens")))

;; (add-hook 'semantic--before-fetch-tags-hook
;;  	  (lambda () (if (string-match "^/opt/local/include/boost-1_35/boost/preprocessor/\\(repetition\\|seq\\)"
;;  	                               (buffer-file-name))
;;                      nil
;;                    t)))

;;
;; c/c++ stuff
;;
(require 'eassist)
;(require 'gtags)

(defun my-c-initialization-hook ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
  (define-key c-mode-base-map (kbd "M-<up>") 'senator-previous-tag)
  (define-key c-mode-base-map (kbd "M-<down>") 'senator-next-tag)
  )
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; Indentation style popular around here
(c-add-style "tibra" 
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist
                (statement-block-intro . +)
                (substatement-open . 0)
                (substatement-label . 0)
                (label . 0)
                (statement-cont . +)
                (innamespace . -)
                )
               (c-hanging-braces-alist
                (brace-list-open)
                (brace-entry-open)
                (statement-cont)
                (block-close . c-snug-do-while)
                (namespace-open)
                )))
(setq c-default-style "tibra")

(defun tibra-header-guard (filename)
  "Calculates the header guard #define for a given file"
  (let ((dir (upcase (file-name-nondirectory (directory-file-name (file-name-directory filename)))))
        (base (upcase (file-name-sans-extension (file-name-nondirectory filename))))
        (ext (upcase (file-name-extension filename))))
    (concat dir "_" base "_" ext)
  ))

(defun tibra-namespaces (filename)
  "Returns the namespace for a given file as a list of strings, eg (\"Tibra\", \"Libname\")"
  (list "Tibra" (upcase-initials (file-name-nondirectory (directory-file-name (file-name-directory filename))))
        ))

(defun tibra-namespace-declare-open (components)
  "Creates a c++ declaration for opening the specified namespaces"
  (mapconcat (lambda (x) (concat "namespace " x " { ")) components ""))

(defun tibra-namespace-declare-close (components)
  "Creates a c++ declaration for closing the specified namespaces"
  (mapconcat (lambda (x) "} ") components ""))

(define-skeleton tibra-header-file
  "A Tibra header file"
  nil
  "#ifndef " (tibra-header-guard buffer-file-name) \n
  "#define " (tibra-header-guard buffer-file-name) \n
  \n
  (tibra-namespace-declare-open (tibra-namespaces buffer-file-name))
  \n
  \n
  _
  \n
  \n
  (tibra-namespace-declare-close (tibra-namespaces buffer-file-name))
  " // " (tibra-namespace-declare-open (tibra-namespaces buffer-file-name))
  )


;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (c-subword-mode 1)
;  (c-toggle-auto-newline 1)
  ;(gtags-mode 1)
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;
;; JDE Mode for Java stuff
;;
;; (setq defer-loading-jde t)
;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;;             (append
;;              '(("\\.java\\'" . jde-mode))
;;              auto-mode-alist)))
;;   (require 'jde))

;; ;; Sets the basic indentation for Java source files
;; ;; to two spaces.
;; (defun my-jde-mode-hook ()
;;   (setq c-basic-offset 2))
;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)


;;
;; XML mode stuff
;;

;; load nxml mode
(load "rng-auto.el")

;; load nxml-script (for php/css/js embedded in XHTML)
;(setq nxml-script-uses-php t)
;(setq nxml-script-uses-css t)
;(setq nxml-script-uses-generic-javascript t)
;(setq nxml-script-key [f1])
;(eval-after-load "nxml-mode" (load "nxml-script"))

(defun nxml-custom-keybindings ()
    (define-key nxml-mode-map [(meta return)] 'nxml-complete))
(add-hook 'nxml-mode-hook 'nxml-custom-keybindings)


;; associate with file extensions:
(add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"   . nxml-mode))
;(add-to-list 'auto-mode-alist '("\\.php[34]?$"   . nxml-mode))

;; update date comment between "<!-- ts start -->" and
;; "<!-- ts end -->" if file is written
(add-hook 'nxml-mode-hook
          '(lambda ()
             (add-hook
              'local-write-file-hooks
              '(lambda ()
                 (save-excursion
                   (beginning-of-buffer)
                   (if (re-search-forward
                        "<!-- ts start -->\\(.*\\)<!-- ts end -->" nil t)
                       (replace-match
                        (format-time-string "%Y-%m-%d")
                        nil nil nil 1)))))))

;; load Relax NG compact mode
(autoload 'rnc-mode "rnc-mode")
(setq auto-mode-alist       
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))


;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))


;;
;; Unicode stuff
;;
(setq unicode-character-list-file (locate-library "unichars"))
(if unicode-character-list-file
    (load "xmlunicode"))


;;
;; Ruby and Rails stuff
;;

;; Firstly ruby mode:
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
;; inf-ruby mode
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             ))

;; You must add path installed yaml-mode.el to load-path.
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)) auto-mode-alist))

;; RoR Emacs Mode
;(require 'rinari)


;; Subversion mode
;;(require 'psvn)

;; Bazaar mode
;(require 'bazaar)
;(add-hook 'find-file-hooks 'bzr-maybe-activate)


;;
;; mailcrypt - used for file mode especially!
;;
;; (require 'mailcrypt)
;; (mc-setversion "gpg")
;; (require 'mc-gpg-file-mode)
(require 'epa-setup)
;;(epa-file-enable)

;;
;; TRAMP
;; Here is my favourite tramp method for windows...
;;
(if (eq system-type 'windows-nt)
    (progn
      (require 'tramp)
      (add-to-list 'tramp-methods
                   '("pscpx"
                     (tramp-login-program "plink")
                     (tramp-login-args
                      (("-load") ("%h") ("-t")
                       ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=$ '")
                       ("/bin/sh")))
                     (tramp-remote-sh "/bin/sh")
                     (tramp-copy-program "pscp")
                     (tramp-copy-args
                      (("-P" "%p") ("-scp") ("-p" "%k")))
                     (tramp-copy-keep-date t)))
      (setq tramp-default-method "pscpx")
      ))

;;
;; My favorite key bindings
;;
(global-set-key [f4] 'next-error)
(global-set-key [f7] 'compile)
(global-set-key [(shift f7)] 'recompile)
(global-set-key [(shift f4)] 'previous-error)

(global-set-key [f9] 'call-last-kbd-macro)

(global-set-key [(home)] 'back-to-indentation-or-beginning)
(global-set-key [(meta backspace)] 'backward-delete-word)

(global-set-key [(control \')] 'goto-last-change)

(global-set-key [(control tab)] 'other-window)

;; Use option as meta
;(setq mac-option-modifier 'meta)       
;(setq mac-pass-option-to-system nil)    

;(global-set-key [(control x) (control r)] 'find-file-read-only)

;;
;; Customisation is done here:
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
