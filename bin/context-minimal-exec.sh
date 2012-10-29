#!/bin/zsh

TeXSetup=$HOME/programs/context/tex/setuptex
Args=$*

zsh -c "source $TeXSetup; $Args"
