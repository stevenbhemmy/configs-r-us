# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
# PATH=$PATH:/home/shemmy/bin
# export PATH
# WORKON_HOME=/home/shemmy/.venvs
# export WORKON_HOME
# source /usr/bin/virtualenvwrapper.sh

export EDITOR='vim'
export VISUAL='vim'

# Generic timestamp for backups/migrations/etc
alias upnow="NOW=$(date "+%Y-%m-%d-%H%M%S")"

# Change terminal colors for Solarized colorscheme
# export TERM=screen-256color-bce

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
