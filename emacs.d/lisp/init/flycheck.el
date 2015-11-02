(require 'flycheck)
(require 'dash)

;; TODO: load lazily...
(require 'ede/compdb)

(defun flycheck-compdb-setup ()
  (when (and ede-object (oref ede-object compilation))
    (let* ((comp (oref ede-object compilation))
           (cmd (get-command-line comp)))
      
      ;; Configure flycheck clang and GCC checker.
      (when (string-match " -std=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-language-standard (match-string 1 cmd))
        (setq-local flycheck-gcc-language-standard (match-string 1 cmd)))
      (when (string-match " -stdlib=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-standard-library (match-string 1 cmd)))
      (when (string-match " -fms-extensions " cmd)
        (setq-local flycheck-clang-ms-extensions t))
      (when (string-match " -fno-exceptions " cmd)
        (setq-local flycheck-clang-no-exceptions t)
        (setq-local flycheck-gcc-no-exceptions t))
      (when (string-match " -fno-rtti " cmd)
        (setq-local flycheck-clang-no-rtti t)
        (setq-local flycheck-gcc-no-rtti t))
      (when (string-match " -fblocks " cmd)
        (setq-local flycheck-clang-blocks t))
      (when (string-match " -fopenmp " cmd)
        (setq-local flycheck-gcc-openmp t))
      (when (string-match " -pedantic " cmd)
        (setq-local flycheck-gcc-pedantic t))
      (when (string-match " -pedantic-errors " cmd)
        (setq-local flycheck-gcc-pedantic-errors t))

      (setq-local flycheck-clang-includes (get-includes comp))
      (setq-local flycheck-gcc-includes flycheck-clang-includes)
      (setq-local flycheck-clang-definitions (get-defines comp))
      (setq-local flycheck-gcc-definitions flycheck-clang-definitions)
      (setq-local flycheck-clang-include-path (get-user-include-path comp t))
      (setq-local flycheck-gcc-include-path flycheck-clang-include-path)
      )))

;; On MacOS we get gcc from macports, which uses a different executable, search for it
(-when-let (gcc-exe (locate-file "gcc-mp-" exec-path '("4.9" "4.8" "4.7") #'file-executable-p))
  (setq-default flycheck-c/c++-gcc-executable gcc-exe))

(add-hook 'ede-compdb-project-rescan-hook #'flycheck-compdb-setup)
(add-hook 'ede-minor-mode-hook #'flycheck-compdb-setup)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init/flycheck)
