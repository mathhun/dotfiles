# .zshrc

#----------------------------------------------------------------------
#  Environment variables
#

if [ -e ~/.zprofile ];then
  source ~/.zprofile
fi

export LANG=C
export LANG=en_US.UTF-8
export EDITOR=vi
export PAGER=less
export LESS='-R'
export MANPATH=/usr/local/share/man:/opt/local/share/man:/usr/share/man:$HOME/local/man
export INFOPATH=/usr/info:/usr/local/info
export WORDCHARS='*?_.[]~=&;!#$%^(){}<>' # ^W: backward-kill-word

### History ###
HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
function histall { history -E 1 }

# PATH
export PATH=$HOME/.cask/bin:$PATH

#----------------------------------------------------------------------
# Zsh options
#

setopt share_history
setopt hist_reduce_blanks
setopt nohup no_clobber complete_aliases
setopt auto_pushd path_dirs
setopt list_packed
setopt auto_menu auto_cd auto_name_dirs auto_remove_slash
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt pushd_ignore_dups sun_keyboard_hack
setopt list_types no_beep always_last_prompt extended_glob
setopt sh_word_split auto_param_keys

# completion (menu select)
zstyle ':completion:*:default' menu select=1
# completion
autoload -U compinit; compinit
zstyle ':completion:*' use-cache true
compctl -g '*.scm' + -f gosh
compctl -g '*.rb' ruby
compctl -g '*.hs' runhaskell
compctl -g '*.hs' runghc

# emacs-like keybind
bindkey -e

## Zsh master No.4
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N"   history-beginning-search-backward-end

[ -n "`alias run-help`" ] && unalias run-help
autoload run-help

autoload -U colors
colors
MAILPATH="/var/mail/$USER?${fg[green]}New mail arrived in \$_."
MAILCHECK=10


#----------------------------------------------------------------------
# Pormpt
#

# PROMPT ##########################################
# %/    current directory
# %~    current directory (print home directory as '~')
# %C    base of current directory
# %c    base of current directory (print home directory as '~')
# %h,%! history number
# %n    user name
# %M    domain name and host name
# %M    host name
# %y    year (2 digit)
# %Y    year (4 digit)
# %w    month (word)
# %W    month (number)
# %d    week day
# %T    time (24 HH:MM)
# %t    time (12 HH:MM)
# %P    time (24 HH:MM:SS)
# %p    time (12 HH:MM:SS)
# %B,%b bold font (%B word %b)
# %U,%u under line (%U word %u)
# %{,%} escape sequence (%{ escape %})
###################################################

PROMPT="%B%{[36m%}%n%{[37m%} @ %{[35m%}%M%b%{[0m%} %{[33m%}%B%C %#%b %{[0m%}"
RPROMPT="%{[35m%}%B[%/]%{[0m%}%b"

precmd() {
  _update_rprompt;
}
chpwd() {
  _update_rprompt;
}
_update_rprompt () {
  GIT_CURRENT_BRANCH=$( git branch 2> /dev/null | grep '^\*' | cut -b 3- )

  if [ "`git ls-files 2>/dev/null`" ]; then
    RPROMPT="%B%{[35m%}[%{[33m%}$GIT_CURRENT_BRANCH%{[37m%}:%{[35m%}%/]%{[0m%}%b"
  else
    RPROMPT="%B %{[35m%} [%/] %{[0m%} %b"
  fi
}

fignore=(\~)


#----------------------------------------------------------------------
# Aliases
#

alias a=alias
alias h=history
alias ls='ls -F'
alias ll='ls -l'
alias la='ls -a'
alias l.='ls -d .*'
alias cp='cp -i'
alias mv='mv -i'
alias grep='grep --color'
alias realias='$EDITOR ~/.aliases; source ~/.aliases'

alias -g L='| TERM=vt100 less'
alias -g LL='2>&1 | TERM=vt100 less'
alias -g G='| grep'
alias -g Gi='| grep -i'
alias -g W='| wc'
alias -g A='| awk'
alias -g N="1>/dev/null 2>/dev/null"
alias -g B="1>/dev/null 2>/dev/null &"

alias -s hs=runhaskell
alias -s scm=gosh
alias -s lisp=clisp
alias -s pl=perl
alias -s rb=ruby
alias -s py=python

# alias
if [ -e ~/.aliases ];then
  source ~/.aliases
fi


#----------------------------------------------------------------------
# programming
#

# perl
if [ -e $HOME/perl5/perlbrew/etc/bashrc ]; then
  source $HOME/perl5/perlbrew/etc/bashrc
fi

# python
export PYENV_ROOT=$HOME/.pyenv
export PATH=$PYENV_ROOT/bin:$PATH
eval "$(pyenv init -)"

# ruby
