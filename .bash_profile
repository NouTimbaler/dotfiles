#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:$HOME/.local/bin/:$HOME/.local/bin/statusbar:$HOME/.cabal/bin"
export TERMINAL="alacritty"
export EDITOR="vim"
export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"

xinput --map-to-output 12 "DP1"

