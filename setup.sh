#!/bin/bash

# Emacs
ln -nfs $(pwd)/emacs.d/ $HOME/.emacs.d

# Git setup
ln -nfs $(pwd)/git/.gitconfig $HOME/.gitconfig
