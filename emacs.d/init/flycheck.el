;; 
(require 'flycheck)
(require 'cl-lib)
(require 'ede-compdb)

;; This code based on discussion with flycheck maintainer, see https://github.com/flycheck/flycheck/issues/354

(flycheck-define-checker c/c++-clang-compdb
  "A flycheck checker which compiles the current source using a compilation database."
  :command ("clang" 
            ;; TODO: What if the compiler isn't clang and isn't compatible with these args?
            "-fsyntax-only"
            ;"-fno-color-diagnostics"
            "-fno-diagnostics-show-caret"
            ;"-fno-diagnostics-show-option"
            "-x" (eval (cl-case major-mode
                         (c++-mode "c++")
                         (c-mode "c")))
            (eval (flycheck-compdb-get-arglist))
            source-inplace)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode)
  :next-checkers ((warnings-only . c/c++-cppcheck))
  :predicate (lambda () (flycheck-compdb-exists-p)))

(defun flycheck-compdb-exists-p ()
  "Whether the current buffer has a compiler database or not"
  (and ede-object
       (slot-boundp ede-object :compilation)
       (oref ede-object compilation))
  )

(defun flycheck-compdb-get-arglist ()
  "Get the argument list from the compiler database."
  
  (let* ((comp (oref ede-object compilation))
         (args (split-string (get-command-line comp)))
         ret)

    ;; Remove the compiler
    ;; TODO: what if compiler has multiple args (eg ccache) ?
    (pop args)

    ;; Process args, building up a new list as we go. Each new element is added to the head of the
    ;; list, so we need to reverse it once done
    (while args
      (let ((argi (pop args)) (case-fold-search nil))
        (cond
         ;; substitude /dev/null for the output file
         ((equal argi "-o")
          (setq ret (cons "/dev/null" (cons argi ret)))
          (pop args))

         ;; substitute -S for -c (ie just compile, don't assemble)
         ((equal argi "-c")
          (setq ret (cons "-S" ret)))

         ;; Don't do any makefile generation
         ((member argi '("-M" "-MM" "-MMD" "-MG" "-MP" "-MD")))
         ((member argi '("-MF" "-MT" "-MQ"))
          (pop args))

         ;; Relative include directories need to be resolved to absolute :(
         ((string-match "\\`\\(-[IF]\\)\\(.+\\)\\'" argi)
          (setq ret (cons (concat (match-string 1 argi)
                                  (expand-file-name (match-string 2 argi) (oref comp directory)))
                          ret)))

         ((equal argi "-isystem")
          (setq ret (cons (expand-file-name (pop args) (oref comp directory))
                          (cons argi ret))))

         (t
          (setq ret (cons argi ret)))
         )))

    ;; remove the source file
    ;; FIXME: is it always the last?
    (pop ret)

    (reverse ret)
    ))

(defun flycheck-compdb-get-compiler ()
  "Get the compiler executable from the compiler database."
  (let ((comp (oref ede-object compilation)))
    (parse-command-line-if-needed comp)
    (oref comp compiler)))

(defun flycheck-compdb-setup ()
  "Setup compiler database support for Flycheck."
  (when (flycheck-compdb-exists-p)
    (setq flycheck-c/c++-clang-compdb-executable
          (flycheck-compdb-get-compiler))))

(when (boundp 'ede-compdb-project-rescan-hook)
  (add-hook 'ede-compdb-project-rescan-hook 'flycheck-compdb-setup)
  (add-hook 'ede-minor-mode-hook 'flycheck-compdb-setup)
  (add-to-list 'flycheck-checkers 'c/c++-clang-compdb)
  (setq-default flycheck-disabled-checkers '(c/c++-clang))
  )

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (flycheck-define-checker compdb
;;                          "A checker which invokes the specified 
  
;;   (when (and ede-object (slot-boundp ede-object :compilation))
;;     (let* ((comp (oref ede-object compilation))
;;            (cmd (get-command-line comp)))

;;       (setq flycheck-clang-definitions (oref comp defines))
;;       (setq flycheck-clang-include-path (get-include-path comp))
;;       (setq flycheck-clang-includes (oref comp includes))
;;       (when (string-match "\\_<-std=\((\\W+\\)\\_>" cmd)
;;         (setq flycheck-clang-language-standard 

(provide 'init/flycheck)
