#!/bin/bash

# Emacs
ln -nfs $(pwd)/emacs.d/ $HOME/.emacs.d

# configure path to necesary files
mkdir -p $(pwd)/emacs.d/var/auto-save/

# Git setup
ln -nfs $(pwd)/git/gitconfig $HOME/.gitconfig

# Profile
ln -nfs $(pwd)/profile $HOME/.profile

# Ag
ln -nfs $(pwd)/agignore $HOME/.agignore
