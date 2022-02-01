# Section - Aliases
alias ls='ls -lAFh'

# Section Prompt
RPROMPT='%*'

# Functions
function mkcd() {
    mkdir -p "$@" && cd "$_";
}
