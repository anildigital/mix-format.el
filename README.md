# mix-format
Emacs plugin to format your Elixir code.

### Usage

``` elisp
;; require from Emacs
(require 'mix-format)

;; Use it
M-x mix-format
```

### Add elixir-mode hook to run mix format on file save

``` elisp
;; elixir-mode hook
(add-hook 'after-save-hook
          (lambda () (when (eq major-mode 'elixir-mode) (mix-format))))
```


### Contribute
Feel free to contribute


### Author
(anil@anilwadghule.com) ; anil wadghule
