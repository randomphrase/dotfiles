(autoload 'ibuffer "ibuffer" "List buffers." t)

(global-set-key [remap list-buffers] 'ibuffer)

;; use ibuffer-vc to sort buffers by VC status
(eval-after-load 'ibuffer
  '(add-hook 'ibuffer-hook
             (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(provide 'init/ibuffer)

