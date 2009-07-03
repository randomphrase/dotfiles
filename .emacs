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
;; (let ((my-info-paths '("~/.emacs.d/info"
;;                        "c:/cygwin/usr/share/info")))
;;   (dolist (my-info-path
;;            (remove-if-not 'file-directory-p
;;                           (mapcar 'expand-file-name my-info-paths)))
;;     ;; Append it so that the emacs stuff appears first (a bit neater :)
;;     (add-to-list 'Info-default-directory-list my-info-path)))

;; Backups go here:
(if (file-directory-p "~/.emacs.d/backups")
    (add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups")))


;; Windows/cygwin stuff:
(when (eq window-system 'w32)
  (require 'w32shell)
  ;; (require 'cygwin-mount)
  ;; (cygwin-mount-activate)
)

;; Start the emacs server mode
(server-start)

;;
;; Visual effects
;;

;(require 'zenburn)
;(zenburn)
(require 'color-theme)
(color-theme-initialize)
;(color-theme-scintilla)
(color-theme-gtk-ide)

;;
;; Editing/movement commands - bound to keys a bit later on
;;

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
  (delete-word (- arg)))

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
(setq-default fill-column 100)    ; Screens are wide enough these days

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; abbrevs are here
(setq abbrev-file-name "~/.emacs.d/abbrevs")

;; Start session mode
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;
;; Major modes / Language setup
;;

;; Markdown mode for text editing
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
;;     (setq auto-mode-alist
;;        (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; Wikipedia mode for wiki editing
;; (autoload 'wikipedia-mode
;;   "wikipedia-mode.el"
;;   "Major mode for editing documents in Wikipedia markup." t)
;; ; Fix up some poor keybinding choices:
;; (add-hook 'wikipedia-mode-hook 
;;           (lambda ()
;;             (define-key wikipedia-mode-map [(control right)] nil)
;;             (define-key wikipedia-mode-map [(control left)] nil)
;;             (define-key wikipedia-mode-map [(meta right)] nil)
;;             (define-key wikipedia-mode-map [(meta left)] nil)
;;             ))
;; (add-to-list 'auto-mode-alist
;;              '("\\(.*\\.wikipedia.org\\)\\|\\(wiki\\.*.\\.\\(org\\|net\\|com\\)\\(\\.au\\)?\\).*\\.txt\\'"
;;                . wikipedia-mode))

;; load makefile-mode for *.mak files and Doxyfile files
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode))

;; CMake mode for CMake*.txt files
(require 'cmake-mode)
(setq cmake-tab-width 4)
(add-to-list 'auto-mode-alist '("CMake[A-Za-z0-9-]*\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; Apache mode
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))

;; Graphviz mode
(load-library "graphviz-dot-mode")

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
;; CEDET Stuff
;;
(load "cedet")
(semantic-load-enable-gaudy-code-helpers)
(require 'semantic-ia)

;; MacPorts installs headers here, make sure semantic knows about them:
(let ((dir "/opt/local/include/"))
  (if (file-directory-p dir)
      (progn
        (semantic-reset-system-include 'c-mode)
        (semantic-add-system-include dir 'c-mode)
        (semantic-reset-system-include 'c++-mode)
        (semantic-add-system-include dir 'c++-mode)
        )))

;; Ensure semantic can get info from gnu global
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; (add-hook 'semantic-init-hooks (lambda ()
;;                                  (imenu-add-to-menubar "Tokens")))

;; Adds semantic analyze display for speedbar
(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

;; don't use semantic on remote files:
(add-to-list 'semantic-inhibit-functions
             (lambda () (and buffer-file-name (file-remote-p buffer-file-name))))

(defun my-cedet-hook ()
  (local-set-key [(control c) (tab)] 'semantic-ia-complete-symbol-menu)
  (local-set-key [(control c) (\?)] 'semantic-ia-complete-symbol)
  ;;
  (local-set-key [(control c) (\>)] 'semantic-complete-analyze-inline)
  (local-set-key [(control c) (=)] 'semantic-decoration-include-visit)

  (local-set-key [(control c) (j)] 'semantic-ia-fast-jump)
  (local-set-key [(control c) (q)] 'semantic-ia-show-doc)
  (local-set-key [(control c) (s)] 'semantic-ia-show-summary)
  (local-set-key [(control c) (p)] 'semantic-analyze-proto-impl-toggle)
  )
;;(add-hook 'semantic-init-hooks 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
;; (add-hook 'erlang-mode-hook 'my-cedet-hook)

;; Don't parse these files...
;; (add-hook 'semantic--before-fetch-tags-hook
;;           (lambda () (if (string-match
;;                           "^c:/program files/boost/boost_\[0-9_\]+/boost/preprocessor/\\(repetition\\|seq\\)/"
;;                           (buffer-file-name))
;;                          nil
;;                        t)))



;;
;; EDE
;;

(global-ede-mode t)

(defun my-locate-pch-header (name dir)
  (cond
   ((string= "globalPch.hpp" name)
    (concat dir "pchNone/globalPch.hpp"))
   ((string-match "^\\([a-zA-Z]+\\)Pch.hpp$" name)
    (concat dir (match-string 1 name) "/pchNone/" name))
   ))

(defun my-locate-project (&optional dir)
  "Return a full file name to the project file stored in dir, or nil"
  (let ((cmakepath (expand-file-name "CMakeLists.txt" dir)))
    (if (and (not (file-remote-p cmakepath))
             (file-exists-p cmakepath))
        cmakepath
      nil)))

(defun my-load-project (dir)
  "Load a project of type `cpp-root' for the directory DIR.
     Return nil if there isn't one."
  (ede-cpp-root-project (file-name-nondirectory dir)
                        :locate-fcn 'my-locate-pch-header
                        :file (expand-file-name "CMakeLists.txt" dir)
                        :include-path '( "/" )
                        :system-include-path '( "c:/Program Files/boost/boost_1_37_0/"
                                                "~/hack/build/boost_1_37_0/" )
;;;                       :spp-table '( ( "_MSC_VER" . "1400" ) )
                        ))

(add-to-list 'ede-project-class-files
     	     (ede-project-autoload "cpp-root"
                                   :name "Repo Projects"
                                   :file 'ede-cpp-root
                                   :proj-file 'my-locate-project
                                   :proj-root 'ede-cpp-root-project-root
                                   :load-type 'my-load-project
                                   :class-sym 'ede-cpp-root)
     	     t)




;;
;; c/c++ stuff
;;
(require 'gtags)

(require 'eassist)
(add-to-list 'eassist-header-switches '("cxx" . ("hxx" "hpp" "h")))
(add-to-list 'eassist-header-switches '("hxx" . ("cxx" "cpp")))
(add-to-list 'eassist-header-switches '("ipp" . ("hxx" "hpp" "h")))
(add-to-list 'eassist-header-switches '("hpp" . ("cxx" "cpp" "ipp")))

(add-to-list 'auto-mode-alist '("\\.[it]pp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[it]xx$" . c++-mode))

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
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
;;;   (define-key c-mode-base-map (kbd "M-<up>") 'senator-previous-tag)
;;;   (define-key c-mode-base-map (kbd "M-<down>") 'senator-next-tag)

  ;; Debug keys
  (define-key c-mode-base-map [(f10)] 'gud-next)
  (define-key c-mode-base-map [(f11)] 'gud-step)

  ;; semantic
  ;;(define-key c-mode-base-map (kbd ".") 'semantic-complete-self-insert)
  ;;(define-key c-mode-base-map (kbd ">") 'semantic-complete-self-insert)
  (define-key c-mode-base-map (kbd "\C-c\C-r") 'semantic-symref)
)
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-common-hook ()
  (c-subword-mode 1)
  ;(c-toggle-auto-newline 1)
  (gtags-mode 1)
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-cedet-hook)


;; Indentation style popular around here
(c-add-style "tibra" 
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist
                (statement-block-intro . +)
                (substatement-open . 0)
                (substatement-label . 0)
                (inher-intro . 0)
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

(defun add-underscores-to-camel-case (str &optional dontSplitInitialCapital)
  "Adds underscores to camelCaseStrings to make them
camel_Case_Strings. Also split an INitialCapital to make
I_Nitial_Captial, unless dontSplitInitialCapital is set"
  (let ((case-fold-search nil))
    (if (or (and (not dontSplitInitialCapital) ;; Apologies for the double-negative
                 (string-match "^\\([A-Z]\\)\\([A-Za-z]\\)" str))
            (string-match "\\([a-z]\\)\\([A-Z]\\)" str))
        (concat (substring str 0 (match-beginning 0))
                (match-string 1 str)
                "_"
                (match-string 2 str)
                (add-underscores-to-camel-case (substring str (match-end 0)) t))
      str)
    ))

(defun tibra-header-guard-define (&optional filename)
  "Calculates the header guard #define for a given file (or the
current buffer file if not specified)"
  (let ((fn (or filename buffer-file-name)))
    (concat (upcase (add-underscores-to-camel-case
                     (file-name-nondirectory (directory-file-name (file-name-directory fn)))))
            "_" (upcase (add-underscores-to-camel-case
                         (file-name-sans-extension (file-name-nondirectory fn))))
            "_" (upcase (or (file-name-extension fn) "")))
    ))

(defun tibra-namespaces (filename)
  "Returns the namespace for a given file as a list of strings,
eg (\"Tibra\", \"Libname\")"
  (list "Tibra" (upcase-initials (file-name-nondirectory (directory-file-name (file-name-directory filename))))
        ))

(defun tibra-namespace-declare-open (&optional components)
  "Creates a c++ declaration for opening the specified
namespaces. If not specified, namespaces default to calling
tibra-namespaces on the current buffer file."
  (mapconcat (lambda (x) (format "namespace %s { " x))
             (or components (tibra-namespaces buffer-file-name))
             ""))

(defun tibra-namespace-declare-close (&optional components)
  "Creates a c++ declaration for closing the specified
namespaces. If not specified, namespaces default to calling
tibra-namespaces on the current buffer file."
  (let ((c (or components (tibra-namespaces buffer-file-name))))
    (concat (mapconcat (lambda (x) "} ") c "")
            " // " (tibra-namespace-declare-open c))
    ))

(defun file-name-and-parent-directory (file)
  "Returns the filename and its immediate parent directory only"
    (concat (file-name-nondirectory (directory-file-name (file-name-directory file)))
            "/" (file-name-nondirectory file)))

(defun tibra-corresponding-file (&optional filename)
  "Returns the corresponding source/header for the specified
file (or the current buffer file), or nil if it can't be
determined."
  (let* ((file (or filename buffer-file-name))
         (ext (assoc-default (file-name-extension file)
                             '(("cpp" . "hpp")
                               ("cxx" . "hxx")
                               ("hpp" . "cpp")
                               ("hxx" . "cxx")
                               ))))
    (if ext (concat (file-name-sans-extension file) "." ext)
      nil)))

(defun tibra-include (&optional filename)
  "Returns an #include declaration for the specified filename, or
an empty string if no filename specified."
  (if filename (concat
                "#include \"" (file-name-and-parent-directory filename) "\"")
    ""))

(define-skeleton tibra-header-file
  "A skeleton for a Tibra c++ header file"
  nil
  "#ifndef " (tibra-header-guard-define) \n
  "#define " (tibra-header-guard-define) \n
  \n
  (tibra-namespace-declare-open)
  \n
  \n
  _
  \n
  \n
  (tibra-namespace-declare-close) \n
  \n
  "#endif // " (tibra-header-guard-define) \n
  )

(define-skeleton tibra-source-file
  "A skeleton for a Tibra c++ source file"
  nil
  "#include \"globalPch.hpp\"" \n
  \n
  (tibra-include (tibra-corresponding-file)) \n
  \n
  (tibra-namespace-declare-open) \n
  \n
  _
  \n
  \n
  (tibra-namespace-declare-close) \n
  )



;;
;; Doxymacs for doxygen comments:
;;
(require 'doxymacs)
; Use doxymacs in all c-modes:
(add-hook 'c-mode-common-hook 'doxymacs-mode)
; Use font-lock mode in c and c++ mode:
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "C++")


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

;; Bazaar mode
;(require 'bazaar)
;(add-hook 'find-file-hooks 'bzr-maybe-activate)
(require 'vc-bzr)

;;
;; mailcrypt - used for file mode especially!
;;
;; (require 'mailcrypt)
;; (mc-setversion "gpg")
;; (require 'mc-gpg-file-mode)
(require 'epa-setup)
(epa-file-enable)

;; TRAMP
(when (eq window-system 'w32)
  ;; Here is my favourite tramp method for windows...
  (require 'tramp)
  (add-to-list 'tramp-methods
               '("pscpx"
                 (tramp-login-program "plink")
                 (tramp-login-args
                  (("-load")
                   ("%h")
                   ("-t")
                   ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=$ '")
                   ("/bin/sh")))
                 (tramp-remote-sh "/bin/sh")
                 (tramp-copy-program "pscp")
                 (tramp-copy-args
                  (("-P" "%p")
                   ("-scp")
                   ("-p" "%k")))
                 (tramp-copy-keep-date t))))
;; bjdev02 has an ancient /bin/sh which means it won't work with tramp. Fortunately the standard
;; bash seems to work, sheesh.
;(add-to-list 'tramp-default-method-alist '("bjdev02" "" "pscp"))

;;
;; My favorite key bindings
;;

(global-set-key [f4] 'next-error)
(global-set-key [f7] 'compile)
(global-set-key [(shift f7)] 'recompile)
(global-set-key [(shift f4)] 'previous-error)

(global-set-key [f9]    'gdb-toggle-breakpoint)
(global-set-key [f10]   'gud-next)
(global-set-key [f11]   'gud-step)
(global-set-key [(shift f11)]   'gud-finish)

(global-set-key [(home)] 'back-to-indentation-or-beginning)
(global-set-key [(meta backspace)] 'backward-delete-word)

(global-set-key [(control \')] 'goto-last-change)

(global-set-key [(control tab)] 'other-window)

;; I *hate* getting overwrite mode by accident...
(global-unset-key [(insert)])

;; Use option as meta
;(setq mac-option-modifier 'meta)
;(setq mac-pass-option-to-system nil)

;;
;; All custom variables live in here
;;
;; Put this last so that we can customise variables defined by modes loaded above
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
