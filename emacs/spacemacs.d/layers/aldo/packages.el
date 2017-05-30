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
    persp-mode
    neotree
    spaceline
    ))

(defun aldo/init-ox-html5slide ()
  (use-package ox-html5slide
    :defer t)
  )

(defun aldo/post-init-persp-mode ()
  (spacemacs|define-custom-layout "terminal"
    :binding "t"
    :body
    (aldo/fish-term)
    (split-window-right)
    (aldo/fish-term)))

(defun aldo/post-init-neotree ()
  (setq neo-vc-integration '(face char)
        neo-show-updir-line t
        neo-smart-open t
        neo-show-hidden-files t
        neo-banner-message nil)
  (when (spacemacs/system-is-mac)
    (setq neo-theme 'arrow))
  (message "(aldo) --> post-init-neotree"))

(defun aldo/post-init-spaceline ()
  (setq powerline-default-separator 'slant)
  (setq dotspacemacs-mode-line-unicode-symbols t)
  (spaceline-compile)
  (message "(aldo) --> post-init-spaceline"))

(spacemacs|do-after-display-system-init
 (setq neo-theme 'icons)
 (setq dotspacemacs-colorize-cursor-according-to-state nil
       dotspacemacs-startup-banner "~/Pictures/spacemacs-logo.png")
 (message "(aldo) --> packages.el do-after-display-system-init"))

;;; packages.el ends here
