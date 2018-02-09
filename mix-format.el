;;; mix-format.el --- Emacs plugin to mix format Elixir files
;; Version: 0.1.0

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

;;;###autoload
(defun mix-format-before-save ()
  "Add this to .emacs to run mix format on the current buffer when saving:
\(add-hook 'before-save-hook 'mix-format-before-save).

Note that this will cause ‘elixir-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'elixir-mode) (mix-format)))


(defun mixfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun mixfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun mixfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.
Shamelessly stolen from go-mode (https://github.com/dominikh/go-mode.el)"

  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in mixfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (mixfmt--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (mixfmt--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in mixfmt--apply-rcs-patch"))))))))
  )

;;;###autoload
(defun mix-format (&optional is-interactive)
  (interactive "p")

  (let ((outbuff (get-buffer-create "*mix-format-output*"))
        (errbuff (get-buffer-create "*mix-format-errors*"))
        (tmpfile (make-temp-file "mix-format" nil ".ex"))
        (our-mixfmt-args (list mixfmt-mix "format"))
        (output nil))

    (unwind-protect
        (save-restriction
          (with-current-buffer outbuff
            (erase-buffer))

          (with-current-buffer errbuff
            (setq buffer-read-only nil)
            (erase-buffer))

          (write-region nil nil tmpfile)

          (run-hooks 'mix-format-hook)

          (when mixfmt-args
            (setq our-mixfmt-args (append our-mixfmt-args mixfmt-args)))
          (setq our-mixfmt-args (append our-mixfmt-args (list tmpfile)))

          (if (zerop (apply #'call-process mixfmt-elixir nil errbuff nil our-mixfmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil outbuff nil "-n" "-" tmpfile))
                    (message "File is already formatted")
                  (progn
                    (mixfmt--apply-rcs-patch outbuff)
                    (message "mix format applied")))
                (kill-buffer errbuff))

            (progn
              (with-current-buffer errbuff
                (setq buffer-read-only t)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode))

              (if is-interactive
                  (display-buffer errbuff)
                (message "mix-format failed: see %s" (buffer-name errbuff)))))

          (delete-file tmpfile)
          (kill-buffer outbuff)))))

(provide 'mix-format)

;;; mix-format.el ends here
