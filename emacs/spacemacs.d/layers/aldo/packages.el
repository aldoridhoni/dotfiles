;;; packages.el --- aldo layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Aldo Ridhoni <aldoridhoni@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst aldo-packages
  '(
    ox-html5slide
    spaceline
    neotree
    persp-mode
    all-the-icons
    all-the-icons-dired
    spacemacs-theme
    ace-window
    recentf
    ))

(defun aldo/init-ox-html5slide ()
  (use-package ox-html5slide
    :defer t))

(defun aldo/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun aldo/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun aldo/post-init-persp-mode ()
  (spacemacs|define-custom-layout "terminal"
    :binding "t"
    :body
    (aldo/fish-term)
    (persp-add-buffer)
    (split-window-right)
    (aldo/fish-term)
    (persp-add-buffer))

  (spacemacs|define-custom-layout "blog"
    :binding "b"
    :body
    (aldo/fish-term "cd ~/blog\n. venv/bin/activate.fish")
    (persp-add-buffer)
    ;; (set-window-dedicated-p (get-buffer-window) t)
    (split-window-right)
    (dired "~/blog")
    (persp-add-buffer)
    ))

(defun aldo/post-init-neotree ()
  (setq neo-vc-integration '(face char)
        neo-show-updir-line t
        neo-smart-open t
        neo-show-hidden-files t
        neo-banner-message nil
        neo-theme 'arrow)
  (when (member "all-the-icons" (font-family-list))
    (setq neo-theme 'icons))
  (aldo//debug-message "(aldo) --> post-init-neotree"))

(defun aldo/post-init-spaceline ()
  (when (display-graphic-p)
    (progn
      (setq powerline-default-separator 'slant)
      (setq dotspacemacs-mode-line-unicode-symbols t)
      (spaceline-compile)))
  (aldo//debug-message "(aldo) --> post-init-spaceline"))

(defun aldo/post-init-spacemacs-theme ()
  (setq spacemacs-theme-comment-bg nil))

(defun aldo/post-init-ace-window ()
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(defun aldo/post-init-recentf ()
  ;; Recent files
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 200
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude "\\elpa\\'")
  (recentf-cleanup))

(spacemacs|do-after-display-system-init
 (setq dotspacemacs-colorize-cursor-according-to-state nil)
 (when (spacemacs/system-is-mac)
   (setq ns-use-srgb-colorspace nil)
   (setq dotspacemacs-fullscreen-at-startup t)
   (setq dotspacemacs-maximized-at-startup nil)
   (set-frame-parameter nil 'fullscreen 'fullscreen)
   (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
   (load-theme 'leuven t)))

;;; packages.el ends here
