;;; elixir-format-test.el --- Basic tests for elixir-format


;;; Code:

(ert-deftest indents-a-buffer ()
  (ert-with-test-buffer (:name "(Expected)indents-a-buffer")
    (insert elixir-format-test-example)
    (elixir-format)
    (should (equal (buffer-string) elixir-format-formatted-test-example))))

(ert-deftest indents-a-buffer-and-undoes-changes ()
  (ert-with-test-buffer ()
    (buffer-enable-undo)
    (setq buffer-undo-list nil)

    (insert elixir-format-test-example)

    (undo-boundary)
    (elixir-format)

    (should (equal (buffer-string) elixir-format-formatted-test-example))
    (undo 0)
    (should (equal (buffer-string) elixir-format-test-example))))

(ert-deftest elixir-format-before-save-formats-buffer ()
  (ert-with-test-buffer ()
    (insert elixir-format-test-example)
    (setq major-mode 'elixir-mode)
    (elixir-format-before-save)
    (should (equal (buffer-string) elixir-format-formatted-test-example))))

(ert-deftest elixir-format-before-save-should-not-format-buffer ()
  (ert-with-test-buffer ()
    (insert elixir-format-test-example)
    (elixir-format-before-save)
    (should (equal (buffer-string) elixir-format-test-example))))

(ert-deftest elixir-format-should-run-hook-before-formatting ()
  (ert-with-test-buffer ()
    (let ((has-been-run nil))
      (insert elixir-format-test-example)
      (add-hook 'elixir-format-hook (lambda () (setq has-been-run t)))
      (elixir-format)
      (should (equal has-been-run t)))))

(ert-deftest elixir-format-should-message-on-error ()
  (ert-with-test-buffer ()
    (insert elixir-format-wrong-test-example)
    (should-error
     (elixir-format))))
