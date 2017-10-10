;;; mix-format.el --- Emacs plugin to mix format Elixir files

;; Copyright (C) 2017 Anil Wadghule

;; Author: Anil Wadghule <anildigital@gmail.com>
;; URL: https://github.com/anildigital/mix-format

;; This file is NOT part of GNU Emacs.

;; MIT License

;; Copyright (c) 2017 Anil Wadghule

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; The mix-format function formats the elixir files with Elixir's `mix format`
;; command

;; e.g.
;;
;; (require 'mix-format)
;; M-x mix-format
;;

;;; Code
(defun mix-format ()
  (interactive)
  (let* (
         (out-file (generate-new-buffer "mix-format")))

    (let* ((command "mix")
           (filename (buffer-file-name))
           (retcode (with-temp-buffer
                      (call-process command
                                    nil
                                    out-file
                                    nil
                                    "format"
                                    "--print"
                                    filename))))
      (when (zerop retcode )
        (with-current-buffer
            (let ((p (point)))
              (erase-buffer)
              (insert-buffer-substring out-file)
              (goto-char p)
              (kill-buffer out-file)
              (message "mix format applied")))))))

(provide 'mix-format)
