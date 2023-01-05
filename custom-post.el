;;; custom-post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-

;; company-yasnippet is far too noisy - just stick to company-capf for now
(setcar company-backends 'company-capf)

;; use C-c y for yasnippet instead (previously bound to my-youdao-dictionary-search-at-point, already accessible from C-c d Y)
(bind-key "C-c y" 'company-yasnippet)
