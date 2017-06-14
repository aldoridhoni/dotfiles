;;; packages.el --- aldo layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
  (message "(aldo) --> post-init-neotree"))

(defun aldo/post-init-spaceline ()
  (when (display-graphic-p)
      (progn
        (setq powerline-default-separator 'slant)
        (setq dotspacemacs-mode-line-unicode-symbols t)
        (spaceline-compile)))
  (message "(aldo) --> post-init-spaceline"))

(defun aldo/post-init-spacemacs-theme ()
  (setq spacemacs-theme-comment-bg nil))

(spacemacs|do-after-display-system-init
 (setq dotspacemacs-colorize-cursor-according-to-state nil
       dotspacemacs-startup-banner "~/Pictures/spacemacs-logo.png")
 (message "(aldo) --> packages.el do-after-display-system-init"))

;;; packages.el ends here
