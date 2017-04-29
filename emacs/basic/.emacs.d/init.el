(require 'package)
(setq package-list '(helm helm-descbinds which-key blank-mode web-mode monokai-theme))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fetch packages-list
(unless package-archive-contents
  (or (file-exists-p package-user-dir)
      (package-refresh-contents)))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(custom-set-variables
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-reveal-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (monokai-theme which-key web-mode helm-projectile helm-descbinds blank-mode)))
 '(visible-bell t))
(custom-set-faces
 )

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
      helm-ff-file-name-history-use-recentf t)

;; Which-key Package
(require 'which-key)
(which-key-mode)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 0.4)

;; Keybinds
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
