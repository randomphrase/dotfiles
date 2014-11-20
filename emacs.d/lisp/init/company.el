(autoload 'company-mode "company" nil t)

(global-company-mode)

(add-to-list 'company-backends 'company-c-headers)

(provide 'init/company)
