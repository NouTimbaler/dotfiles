#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:$HOME/.local/bin/:$HOME/.local/bin/statusbar"
export TERMINAL="alacritty"
export EDITOR="nvim"
export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"
export HISTSIZE=10000
