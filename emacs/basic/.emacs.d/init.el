;;; init.el --- Emacs init.el
;;; Commentary:

;;; Code:

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-reveal-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (typescript-mode company flycheck material-theme monokai-theme which-key web-mode helm-projectile helm-descbinds blank-mode)))
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-file ((t (:background "gainsboro" :foreground "dim gray"))))
 '(which-key-local-map-description-face ((t (:foreground "dim gray")))))
 '(which-key-command-description-face ((t (:foreground "dim gray"))))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
