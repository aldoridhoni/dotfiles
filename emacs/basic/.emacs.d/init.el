;;; init.el --- Emacs init.el
;; (package-initialize)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-reveal-mode t)
 '(package-selected-packages
   (quote
    (restart-emacs use-package org-bullets typescript-mode company flycheck material-theme monokai-theme which-key web-mode helm-projectile helm-descbinds)))
 '(visible-bell t))
(custom-set-faces
)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Replace HOME
(setq default-directory user-home-real-dir)
(setenv "HOME" user-home-real-dir)
