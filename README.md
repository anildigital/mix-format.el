# elixir-format
Emacs package to format your Elixir code.


![](https://i.imgur.com/OV5YQBx.gif)


### Setup
Customize the elixir and mix pathes

In Emacs, run following command to customize option
``` elisp
M-x customize-option

Customize-variable: elixir-format-elixir-path
```
and set your elixir executable path there. After that run:
``` elisp
M-x customize-option

Customize-variable: elixir-format-mix-path
```
and set your mix executable path there.

Alternavively you can define variables as below

``` elisp
(setq elixir-format-elixir-path "/usr/local/bin/elixir")
(setq elixir-format-mix-path "/usr/local/bin/mix")
```

Your machine's elixir and mix executable paths can be found with `which` command as shown below

``` shell
$ which mix
/usr/local/bin/mix
```


### Usage
``` elisp
;; require from Emacs
(require 'elixir-format)

;; Use it
M-x elixir-format
```

### Add elixir-mode hook to run mix format on file save

``` elisp
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
```

To use a `.formatter.exs` you can either set `elixir-format-arguments` globally to a path like this:

``` elisp
(setq elixir-format-arguments (list "--dot-formatter" "/path/to/.formatter.exs"))
```

or you set `elixir-format-arguments` in a hook like this:

```elisp
(add-hook elixir-format-hook '(lambda ()
                                 (if (projectile-project-p)
                                     (setq elixir-format-arguments (list "--dot-formatter" (concat (projectile-project-root) "/.formatter.exs")))
                                   (setq elixir-format-arguments nil))))
```

In this example we use [Projectile](https://github.com/bbatsov/projectile) to get the project root and set `elixir-format-arguments` accordingly.

### Contribute
Feel free to contribute


### Author
(anil@anilwadghule.com) ; anil wadghule
