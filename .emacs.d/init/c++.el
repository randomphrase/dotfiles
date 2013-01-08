
;;
;; c/c++ stuff
;;
(require 'gtags)

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

  ;; Show hard tabs
  ;; TODO: Use whitespace mode from Emacs 24
  ;;(show-ws-highlight-tabs)
)
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-mode-common-hook ()
  (subword-mode 1)
  ;(c-toggle-auto-newline 1)
  (gtags-mode 1)
  ;(semantic-tag-folding-mode 1)
  ;(setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun tibra-lineup-init-intro (langelem)
  "Line up member initializer lists so that when you have a hanging
colon, subsequent lines are indented."
  (save-excursion
    (back-to-indentation)
    (if (looking-at ":")
        0 c-basic-offset)))

(defun tibra-lineup-init-cont (langelem)
  "Line up member initializer lists so that when you have a hanging
colon, subsequent lines are indented."
  (save-excursion
    (back-to-indentation)
    (if (looking-at ":")
        (- c-basic-offset) 0)))

;; Indentation style popular around here
(c-add-style "tibra" 
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist
                (statement-block-intro . +)
                (substatement-open . 0)
                (inline-open . 0)
                (substatement-label . 0)
                (inher-intro . 0)
                (member-init-intro . tibra-lineup-init-intro)
                (member-init-cont . tibra-lineup-init-cont)
                (label . 0)
                (statement-cont . +)
                (innamespace . -)
                (case-label . +)
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


;; Handy skeleton for simple unit tests
(define-skeleton boost-unit-test-module
  "A skeleton for Boost UTF modules"
  nil
  "#define BOOST_TEST_MODULE " (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) \n
  "#include <boost/test/unit_test.hpp>" \n
  \n
  "BOOST_AUTO_TEST_CASE(" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ")" \n
  "{" \n
  _ \n
  "}" \n
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

(provide 'init/c++)
