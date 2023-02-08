;;; custom-post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-


;; use C-c y for yasnippet instead (previously bound to my-youdao-dictionary-search-at-point, already accessible from C-c d Y)
(bind-key "C-c y" 'company-yasnippet)

;; unbind these from magit mode, used for window switching
(with-eval-after-load 'magit-status
  (unbind-key "M-1" magit-section-mode-map)
  (unbind-key "M-2" magit-section-mode-map)
  (unbind-key "M-3" magit-section-mode-map)
  (unbind-key "M-4" magit-section-mode-map)
  )
