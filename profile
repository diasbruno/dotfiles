#!/usr/bin/env bash

open () {
    ($1 2> /dev/null) &
}

alias localtime="TZ=America/Sao_Paulo date"
