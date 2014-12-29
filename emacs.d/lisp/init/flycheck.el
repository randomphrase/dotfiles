(require 'flycheck)

;; TODO: load lazily...
(require 'ede-compdb)

(defun flycheck-compdb-setup ()
  (when (and ede-object (oref ede-object compilation))
    (let* ((comp (oref ede-object compilation))
           (cmd (get-command-line comp)))
      
      ;; Configure flycheck clang checker.
      ;; TODO: configure gcc checker also
      (when (string-match " -std=\\([^ ]+\\)" cmd)
        (setq flycheck-clang-language-standard (match-string 1 cmd)))
      (when (string-match " -stdlib=\\([^ ]+\\)" cmd)
        (setq flycheck-clang-standard-library (match-string 1 cmd)))
      (when (string-match " -fms-extensions " cmd)
        (setq flycheck-clang-ms-extensions t))
      (when (string-match " -fno-exceptions " cmd)
        (setq flycheck-clang-no-exceptions t))
      (when (string-match " -fno-rtti " cmd)
        (setq flycheck-clang-no-rtti t))
      (when (string-match " -fblocks " cmd)
        (setq flycheck-clang-blocks t))
      (setq flycheck-clang-includes (get-includes comp))
      (setq flycheck-clang-definitions (get-defines comp))
      (setq flycheck-clang-include-path (get-include-path comp t))
      )))

(add-hook 'ede-compdb-project-rescan-hook #'flycheck-compdb-setup)
(add-hook 'ede-minor-mode-hook #'flycheck-compdb-setup)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init/flycheck)
