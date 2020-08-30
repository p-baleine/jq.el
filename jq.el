;;; jq.el --- Emacs Lisp bindings for jq. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Junpei Tajima
;;
;; Author: Junpei Tajima <http://github/p-baleine>
;; Maintainer: Junpei Tajima <p-baleine@gmail.com>
;; Created: August 29, 2020
;; Modified: August 29, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/p-baleine/jq
;; Package-Requires: ((emacs "27.0.60") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs Lisp bindings for jq.
;;
;;; Code:

(require 'generator)
(require 'jq-impl)
(require 'json)

(iter-defun jq (input program)
  (let ((jq (jq-impl-init
             input (string-bytes input)
             program (string-bytes program)))
        value)
    (while (setq value (jq-impl-next jq))
      (iter-yield (json-read-from-string value)))))

(provide 'jq)
;;; jq.el ends here
