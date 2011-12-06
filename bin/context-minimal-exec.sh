#!/bin/sh

TeXSetup=$HOME/programs/context/tex/setuptex
Args=$*

sh -c "source $TeXSetup; $Args"
