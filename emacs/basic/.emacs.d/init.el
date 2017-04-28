(require 'package)
(setq package-list '(helm helm-descbinds which-key blank-mode web-mode))
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
 '(custom-enabled-themes (quote (leuven)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-reveal-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
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

;; Keybind
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Maximize initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
