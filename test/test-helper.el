(require 'ert-x)
(require 'cl-lib)

(require 'elixir-format)

(cond
 ((file-exists-p "/usr/bin/elixir")
  (setq elixir-format-elixir-path "/usr/bin/elixir")
  (setq elixir-format-mix-path "/usr/bin/mix"))
 ((file-exists-p "/usr/local/bin/elixir")
  (setq elixir-format-elixir-path "/usr/local/bin/elixir")
  (setq elixir-format-mix-path "/usr/local/bin/mix"))
 (t
  (error "We need elixir and mix!")))


(defconst elixir-format-test-example "defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end
end")

(defconst elixir-format-wrong-test-example "defmodule Foo do
use GenServer.Behaviour
def foobar do
if true, do: IO.puts \"yay\"
end")

(defconst elixir-format-formatted-test-example
  "defmodule Foo do
  use GenServer.Behaviour

  def foobar do
    if true, do: IO.puts(\"yay\")
  end
end
")
