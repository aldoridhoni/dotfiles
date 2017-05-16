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
    ))

(defun aldo/init-ox-html5slide ()
  (use-package ox-html5slide
    :defer t)
  )

;;; packages.el ends here
