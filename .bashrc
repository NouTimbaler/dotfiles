#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#history
HISTCONTROL=ignoreboth:erasedups    # dont duplicate lines
shopt -s histappend                 # append hist file
shopt -s cdspell                    # correct cd misspells
shopt -s expand_aliases             # expand aliases
HISTSIZE=10000                      # history length
HISTFILESIZE=20000                  # history length

shopt -s checkwinsize              # update column value after each command

#prompt
color_prompt=yes

GREEN="\[$(tput setaf 2)\]"
YELLOW="\[$(tput setaf 3)\]"
BLUE="\[$(tput setaf 4)\]"
WHITE="\[$(tput setaf 7)\]"
RESET="\[$(tput sgr0)\]"
#PS1="[\e[0;32m\u\e[0m@\e[0;33m\h\e[0m \e[0;36m\W\e[0m]\$ "
PS1="[${GREEN}\u${RESET}@${YELLOW}\h ${BLUE}\W${RESET}]\$ "

#colors
alias ls='ls --color=auto'
alias grep='grep --color=auto'

#gcc warning and error colors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

