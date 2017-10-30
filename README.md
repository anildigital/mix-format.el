# mix-format
Emacs package to format your Elixir code.


![](https://i.imgur.com/OV5YQBx.gif)


### Setup
Customize the elixir and mix pathes

In Emacs, run following command to customize option
``` elisp
M-x customize-option

Customize-variable: mixfmt-elixir
```
and set your elixir executable path there. After that run:
``` elisp
M-x customize-option

Customize-variable: mixfmt-mix
```
and set your mix executable path there.

Your machine's elixir and mix executable paths can be found with `which` command as shown below

``` shell
$ which mix
/usr/local/bin/mix
```


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
          (lambda () (add-hook 'before-save-hook 'mix-format-before-save)))

```


### Contribute
Feel free to contribute


### Author
(anil@anilwadghule.com) ; anil wadghule
