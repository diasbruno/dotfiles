#!/usr/bin/env bash

LC_ALL=en_US.UTF-8
LANG=en_US.UTF-8

TERM="screen-256color"

ASDF_PATH="/dias/asdf"

[[ -f $ASDF_PATH/asdf.sh ]] &&
    source $ASDF_PATH/asdf.sh
[[ -f $ASDF_PATH/completions/asdf.bash ]] &&
    source $ASDF_PATH/completions/asdf.bash

open () {
    ($1 2> /dev/null) &
}

alias localtime="TZ=America/Sao_Paulo date"

_pl_version() {
    local VERSION=`asdf current $2 | awk '{ print $1 }'`
    echo "$1 version: $VERSION"
}

versions() {
    _pl_version "PHP" "php"
    _pl_version "Ruby" "ruby"
    _pl_version "Nodejs" "nodejs"
    _pl_version "Rust" "rust"
    _pl_version "Python" "python"
    _pl_version "Erlang" "erlang"
    _pl_version "Elixir" "elixir"
    _pl_version "Ocaml" "ocaml"
    _pl_version "Lisp" "sbcl"
    _pl_version "Haskell" "haskell"
}

versions

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"

# initialize ocaml's package manager.
[[ -r /home/dias/.opam/opam-init/init.sh ]] &&
    source /home/dias/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# add cargo's bin path
test -d $HOME/.cargo/bin &&
    export PATH="$HOME/.cargo/bin:$PATH";
