#!/bin/bash

# Emacs
ln -nfs $(pwd)/emacs.d/ $HOME/.emacs.d

# configure path to necesary files
mkdir -p $(pwd)/emacs.d/var/auto-save/

# Bash Setup
ln -nfs $(pwd)/bashrc $HOME/.bashrc

# Git setup
ln -nfs $(pwd)/git/gitconfig $HOME/.gitconfig

# Ag
ln -nfs $(pwd)/agignore $HOME/.agignore

# NPM RC
ln -nfs $(pwd)/npmrc $HOME/.npmrc
