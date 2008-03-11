;; Alastair's emacs file

;; Twiddle load paths to find my stuff:
(let ((my-load-paths '("~/.emacs.d/lisp"
                       "/Library/Application Support/Emacs/site-lisp")))
  (while my-load-paths
    (let* ((my-path (car my-load-paths))
           (default-directory my-path))
      (if (and (file-directory-p my-path) (not (member my-path load-path)))
          (progn (setq load-path (cons my-path load-path))
                 (normal-top-level-add-subdirs-to-load-path)))
      (setq my-load-paths (cdr my-load-paths))
      )))

;; Twiddle exec paths for similar reasons
(let ( (macports-path "/opt/local/bin") )
  (if (and (file-directory-p macports-path) (not (member macports-path exec-path)))
      (setq exec-path (cons macports-path exec-path))
    ))

;; Visual effects first
;(require 'zenburn)
;(zenburn)
(require 'color-theme)
(color-theme-initialize)
(color-theme-scintilla)

;; Tab setup
(setq-default tab-width 4)          ; Set tab width to 4
(setq-default indent-tabs-mode nil)	; Use spaces for indenting - NOT TABs

;; Start the emacs server mode
(server-start)

;; from http://www.emacswiki.org/cgi-bin/wiki/BackToIndentationOrBeginning
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;;
;; Don't let M-backspace add to the kill ring
;;
(defun my-backward-delete-word (arg)
  "DELETE characters backward until encountering the end of a word. With argument, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn (forward-word (- arg))
                        (point))))

;;
;; Load clearcase (TODO: work out how to skip this)
;;
;(load "clearcase")
;(load "vc-clearcase-auto")

;; load makefile-mode for *.mak files
(setq auto-mode-alist
      (append (list (cons "\\.mak\\'" 'makefile-mode))
	      auto-mode-alist))

;;
;; Apache mode
;;
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))

;;
;; Subversion mode
;;
(require 'psvn)

;;
;; PHP mode
;;
;(require 'php-mode)
;(autoload 'php-mode "php-mode" "PHP editing mode" t)
;(add-to-list 'auto-mode-alist '("\\.php3\\'" . php-mode))


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
(semantic-load-enable-code-helpers)


;;
;; mailcrypt - used for file mode especially!
;;
;; (require 'mailcrypt)
;; (mc-setversion "gpg")
;; (require 'mc-gpg-file-mode)
(require 'epa-file)
(epa-file-enable)

;;
;; JDE Mode for Java stuff
;;
(setq defer-loading-jde t)
(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
            (append
             '(("\\.java\\'" . jde-mode))
             auto-mode-alist)))
  (require 'jde))

;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'jde-mode-hook 'my-jde-mode-hook)


;; Start session mode
(require 'session)
(add-hook 'after-init-hook 'session-initialize)


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
;; Markdown mode
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)


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

;;
;; My favorite key bindings
;;
(global-set-key [f4] 'next-error)
(global-set-key [f7] 'compile)
(global-set-key [(shift f7)] 'recompile)
(global-set-key [(shift f4)] 'previous-error)

(global-set-key [f9] 'call-last-kbd-macro)

(global-set-key [(home)] 'back-to-indentation-or-beginning)
;(global-set-key [(end)] 'end-of-visible-line)
;; (global-set-key [(meta backspace)] 'my-backward-delete-word)

;(global-set-key [(meta s)] 'search-forward)

(global-set-key [(control tab)] 'other-window)

;; Use option as meta
;(setq mac-option-modifier 'meta)       
;(setq mac-pass-option-to-system nil)    

;(global-set-key [(control x) (control r)] 'find-file-read-only)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(cua-mode t nil (cua-base))
;;  '(global-font-lock-mode t nil (font-core))
;;  '(initial-frame-alist (quote ((top . 1) (left . 1) (width . 120) (height . 55))))
;;  '(jde-ant-program "/opt/local/bin/ant")
;;  '(jde-build-function (quote (jde-ant-build)))
;;  '(jde-key-bindings (quote (("[? ? ?]" . jde-run-menu-run-applet) ("[f7]" . jde-build) ("[? ? ?]" . jde-compile) ("[? ? ?]" . jde-debug) ("[? ? ?]" . jde-find) ("[? ? ?]" . jde-open-class-at-point) ("[? ? ?]" . jde-bsh-run) ("[? ? ?]" . jde-gen-println) ("[? ? ?]" . jde-help-browse-jdk-doc) ("[? ? ?]" . jde-save-project) ("[? ? ?]" . jde-wiz-update-class-list) ("[f9]" . jde-run) ("[? ? ?]" . speedbar-frame-mode) ("[? ? ?]" . jde-jdb-menu-debug-applet) ("[? ? ?]" . jde-help-symbol) ("[? ? ?]" . jde-show-superclass-source) ("[? ? ?]" . jde-open-class-at-point) ("[? ? ?]" . jde-import-find-and-import) ("[? ? ?e]" . jde-wiz-extend-abstract-class) ("[? ? ?f]" . jde-gen-try-finally-wrapper) ("[? ? ?i]" . jde-wiz-implement-interface) ("[? ? ?j]" . jde-javadoc-autodoc-at-line) ("[? ? ?o]" . jde-wiz-override-method) ("[? ? ?t]" . jde-gen-try-catch-wrapper) ("[? ? ?]" . jde-run-etrace-prev) ("[? ? ?]" . jde-run-etrace-next) ("[(control c) (control v) (control ?.)]" . jde-complete) ("[(control c) (control v) ?.]" . jde-complete-in-line))))
;;  '(jde-read-compile-args t)
;;  '(load-home-init-file t)
;;  '(nxml-slash-auto-complete-flag t)
;;  '(show-paren-mode t nil (paren)))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "courier"))))
;;  '(font-lock-builtin-face ((((class color) (background light)) (:foreground "DarkBlue"))))
;;  '(font-lock-comment-face ((((class color) (background light)) (:foreground "green4" :slant italic))))
;;  '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
;;  '(font-lock-function-name-face ((((class color) (background light)) (:inherit font-lock-variable-name-face :weight bold))))
;;  '(font-lock-keyword-face ((((class color) (background light)) (:inherit font-lock-builtin-face :weight bold))))
;;  '(font-lock-string-face ((((class color) (background light)) (:foreground "dark olive green"))))
;;  '(font-lock-variable-name-face ((((class color) (background light)) (:inherit font-lock-function-name-face)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(mac-option-modifier (quote meta))
 '(nxml-slash-auto-complete-flag t)
 '(show-paren-mode t)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
