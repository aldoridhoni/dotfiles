(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fetch packages-list
(unless package-archive-contents
  (or (file-exists-p package-user-dir)
      (package-refresh-contents)))

;; Install the missing packages
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-reveal-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (monokai-theme which-key web-mode helm-projectile helm-descbinds blank-mode)))
 '(visible-bell t)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-key-command-description-face ((t (:foreground "dim gray"))))
 '(which-key-local-map-description-face ((t (:foreground "dim gray")))))

;; Do something if using X
(when (display-graphic-p)
  ;; Hide tool-bar
  (tool-bar-mode 0)
  ;; Toggle menu-bar
  (menu-bar-mode)
  ;; Set leuven theme
  (load-theme 'leuven)
  (enable-theme 'leuven)
  ;; Maximize initial frame
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
)

;; Helm Package
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)
(setq helm-descbinds-window-style 'split
      helm-ff-file-name-history-use-recentf t
      help-window-select t
      helm-display-header-line nil
      helm-autoresize-mode 1)
(defvar helm-source-header-default-background
  (face-attribute 'helm-source-header :background))
(defvar helm-source-header-default-foreground
  (face-attribute 'helm-source-header :foreground))
(defvar helm-source-header-default-box
  (face-attribute 'helm-source-header :box))
(defvar helm-source-header-default-height
  (face-attribute 'helm-source-header :height))

;; Which-key Package
(require 'which-key)
(which-key-mode t)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 0.4)

;; Keybinds
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

(setq inhibit-splash-screen t)
(setq require-final-newline t)
(setq make-backup-files nil)
(setq initial-scratch-message nil)
(setq delete-trailing-lines nil)
;; Auto refresh
(setq global-auto-revert-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(fset 'yes-or-no-p 'y-or-n-p)
