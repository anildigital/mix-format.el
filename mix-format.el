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

(defcustom mixfmt-args nil
  "Additional arguments to 'mix format'"
  :type '(repeat string)
  :group 'mix-format)

(defcustom mix-format-hook nil
  "Hook called by `mix-format'."
  :type 'hook
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

  (when (get-buffer "*mix-format-output*")
    (kill-buffer "*mix-format-output*"))

  (unwind-protect
      (let* ((p (point))
             (outbuff (get-buffer-create "*mix-format-output*"))
             (errfile (make-temp-file "mix-format"))
             (our-mixfmt-args (list mixfmt-mix "format"))
             (retcode nil)
             (output nil))

        (run-hooks 'mix-format-hook)

        (when mixfmt-args
          (setq our-mixfmt-args (append our-mixfmt-args mixfmt-args)))
        (setq our-mixfmt-args (append our-mixfmt-args (list "-")))

        (setq retcode (apply #'call-process-region (point-min) (point-max)
                             mixfmt-elixir
                             nil
                             (list outbuff errfile)
                             t
                             our-mixfmt-args))

        (if (zerop retcode)
            (progn
              (with-current-buffer outbuff
                (setq output (buffer-substring-no-properties (point-min) (point-max))))

              (save-excursion
                (erase-buffer)
                (insert-string output))

              (when (get-buffer "*mix-format-errors*")
                (kill-buffer "*mix-format-errors*"))

              (message "mix format applied")
              (goto-char p))

          (progn
            (with-current-buffer (get-buffer-create "*mix-format-errors*")
              (insert-file-contents errfile nil nil nil t)
              (setq buffer-read-only t)
              (ansi-color-apply-on-region (point-min) (point-max))
              (special-mode))

            (if is-interactive
                (display-buffer outbuff)
              (message "mix-format failed: see %s" (buffer-name outbuff)))))

        (delete-file errfile)
        (kill-buffer "*mix-format-output*")
        )))

(provide 'mix-format)

;;; mix-format.el ends here
