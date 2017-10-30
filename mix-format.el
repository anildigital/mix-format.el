;;; mix-format.el --- Emacs plugin to mix format Elixir files

;; Copyright (C) 2017 Anil Wadghule

;; Author: Anil Wadghule <anildigital@gmail.com>
;; URL: https://github.com/anildigital/mix-format

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; The mix-format function formats the elixir files with Elixir's `mix format`
;; command

;; e.g.
;;
;; (require 'mix-format)
;; M-x mix-format
;;

(defcustom mixfmt-elixir "elixir"
  "Path to the Elixir interpreter."
  :type 'string
  :group 'mix-format)

(defcustom mixfmt-mix "/usr/bin/mix"
  "Path to the 'mix' executable."
  :type 'string
  :group 'mix-format)

;;; Code
(defun mix-format-before-save ()
  "Add this to .emacs to run mix format on the current buffer when saving:
\(add-hook 'before-save-hook 'mix-format-before-save).

Note that this will cause ‘elixir-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'elixir-mode) (mix-format)))


(defun mix-format (&optional is-interactive)
  (interactive "p")

  (unwind-protect
      (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
             (p (point))
             (errbuff (get-buffer-create "mix-format-errors"))
             (retcode (call-process-region (point-min) (point-max)
                                           mixfmt-elixir
                                           nil
                                           errbuff
                                           t
                                           mixfmt-mix "format" "-"))
             (output nil))

        (if (zerop retcode)
            (progn
              (with-current-buffer errbuff
                (setq output (buffer-substring-no-properties (point-min) (point-max))))

              (save-excursion
                (erase-buffer)
                (insert-string output))

              (message "mix format applied")
              (goto-char p)
              (kill-buffer "mix-format-errors"))

          (let ((error-buffer (get-buffer "mix-format-errors")))
            (with-current-buffer error-buffer
              (read-only-mode 0)
              (ansi-color-apply-on-region (point-min) (point-max))
              (special-mode))

            (if is-interactive
                (display-buffer error-buffer)
              (message "mix-format failed: see %s" (buffer-name error-buffer)))))
        )))

(provide 'mix-format)

;;; mix-format.el ends here
