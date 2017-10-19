# mix-format
Emacs package to format your Elixir code.


![](https://i.imgur.com/OV5YQBx.gif)

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
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'mix-format)))

```


### Contribute
Feel free to contribute


### Author
(anil@anilwadghule.com) ; anil wadghule
