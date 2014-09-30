(projectile-global-mode)

(add-to-list 'projectile-project-root-files "Jamroot")

(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

(provide 'init/projectile)
