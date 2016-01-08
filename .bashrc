# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
PATH=$PATH:/home/shemmy/bin
export PATH
WORKON_HOME=/home/shemmy/.venvs
export WORKON_HOME
source /usr/bin/virtualenvwrapper.sh

# Change terminal colors for Solarized colorscheme
# export TERM=screen-256color-bce

# Enable autocomplete for rhc tool from OpenShift
. /home/shemmy/.openshift/bash_autocomplete
