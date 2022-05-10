# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Golang configuration
export GOHOME=$HOME/go
export GOPATH=$GOHOME
export GOMODCACHE=$GOHOME/pkg/mod
export PATH=$GOPATH/bin:$PATH

# NPM configuration: which will help reducing permission issue and avoid using sudo
export PATH=~/.npm-global/bin:$PATH


# YH: google cloud sdk path
# The next line updates PATH for the Google Cloud SDK.
if [ -f '~/Programs/google-cloud-sdk/path.bash.inc' ]; then . '~/Programs/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '~/Programs/google-cloud-sdk/completion.bash.inc' ]; then . '~/Programs/google-cloud-sdk/completion.bash.inc'; fi

# NodeJs configuration
VERSION=v16.14.2
DISTRO=linux-x64
export PATH=/usr/local/lib/nodejs/node-$VERSION-$DISTRO/bin:$PATH

# Rust Cargo Executable
export PATH=/home/yhou/.cargo/bin:$PATH
