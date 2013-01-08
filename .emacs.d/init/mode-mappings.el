;; load makefile-mode for *.mak files and Doxyfile files
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-to-list 'auto-mode-alist '("CMake[A-Za-z0-9-]*\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; associate with file extensions:
(add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$"   . nxml-mode))
;(add-to-list 'auto-mode-alist '("\\.php[34]?$"   . nxml-mode))

;; load Relax NG compact mode
(autoload 'rnc-mode "rnc-mode")
(setq auto-mode-alist       
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))

;; Javascript mode for .js files
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Transparently open .gpg encrypted files
(require 'epa-file)
(epa-file-enable)

(provide 'init/mode-mappings)
