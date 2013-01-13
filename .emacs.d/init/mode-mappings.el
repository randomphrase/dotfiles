;; load makefile-mode for *.mak files and Doxyfile files
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode))

;; C++ Files
(add-to-list 'auto-mode-alist '("\\.[it]pp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[it]xx$" . c++-mode))

;; Org-mode files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; CMake files
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMake[A-Za-z0-9-]*\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; XML files
(add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$"   . nxml-mode))
;(add-to-list 'auto-mode-alist '("\\.php[34]?$"   . nxml-mode))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Relax NG Compact
(autoload 'rnc-mode "rnc-mode")
(setq auto-mode-alist       
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

;; CSS mode
;; (autoload 'css-mode "css-mode")
;; (setq auto-mode-alist       
;;       (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Javascript mode for .js files
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Transparently open .gpg encrypted files
(require 'epa-file)
(epa-file-enable)

(provide 'init/mode-mappings)
