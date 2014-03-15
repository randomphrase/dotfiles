(eval-after-load "org"
  `(progn
    ;; Fontify org-mode code blocks
    (setq org-src-fontify-natively t)

    ;; Can't get out of the shift-select habit
    (setq org-support-shift-select t)

    ;; View everything by default
    (setq org-startup-folded 'showeverything)
    ))

;; Prevent org taking over my meta-arrow keys
(setq org-disputed-keys
      '(([M-right]   . [S-M-right])
        ([M-left]    . [S-M-left])
        ([S-up]	     . [M-p])
        ([S-down]    . [M-n])
        ([S-left]    . [M--])
        ([S-right]   . [M-+])
        ([C-S-right] . [M-S-+])
        ([C-S-left]  . [M-S--])))
(setq org-replace-disputed-keys t)

;; (require 'org-install)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

(provide 'init/org)
