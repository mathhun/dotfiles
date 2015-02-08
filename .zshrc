# .zshrc

#----------------------------------------------------------------------
#  Environment variables
#

export LANG=C
export LANG=en_US.UTF-8
export EDITOR=vi
export PAGER=less
export LESS='-R'
export MANPATH=/usr/local/share/man:/opt/local/share/man:/usr/share/man:$HOME/local/man
export INFOPATH=/usr/info:/usr/local/info
#export WORDCHARS='*?_.[]~=&;!#$%^(){}<>' # ^W: backward-kill-word

### History ###
HISTFILE=~/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function histall { history -E 1 }

# PATH
export PATH=$HOME/.cask/bin:$PATH

MAILPATH="/var/mail/$USER?${fg[green]}New mail arrived in \$_."
MAILCHECK=10

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

# zsh-completions
# cd ~/.zsh; git clone git://github.com/zsh-users/zsh-completions.git
fpath=($HOME/.zsh/zsh-completions/src(N-/) $fpath)

# completion
autoload -Uz compinit; compinit
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' use-cache true
# case
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
fignore=(\~)

# ^W
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# emacs-like keybind
bindkey -e

# history
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
bindkey "^P" history-beginning-search-backward-end
#bindkey "^N"   history-beginning-search-backward-end
# incremental search
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

[ -n "`alias run-help`" ] && unalias run-help
autoload run-help

autoload -U colors
colors

function zman() {
    PAGER="less -g -s '+/^ {7}"$1"'" man zshall
}

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

#precmd() {
#  _update_rprompt;
#}
#chpwd() {
#  _update_rprompt;
#}
#_update_rprompt () {
#  GIT_CURRENT_BRANCH=$( git branch 2> /dev/null | grep '^\*' | cut -b 3- )
#
#  if [ "`git ls-files 2>/dev/null`" ]; then
#    RPROMPT="%B%{[35m%}[%{[33m%}$GIT_CURRENT_BRANCH%{[37m%}:%{[35m%}%/]%{[0m%}%b"
#  else
#    RPROMPT="%B %{[35m%} [%/] %{[0m%} %b"
#  fi
#}


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
alias -g H='| head'
alias -g T='| tail'
alias -g V='| vim -R -'

alias -s hs=runhaskell
alias -s scm=gosh
alias -s lisp=clisp
alias -s pl=perl
alias -s rb=ruby
alias -s py=python

compctl -g '*.scm' + -f gosh
compctl -g '*.rb' ruby
compctl -g '*.hs' runhaskell
compctl -g '*.hs' runghc

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
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

#----------------------------------------------------------------------
# package
#

# cdr
autoload -Uz add-zsh-hook
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs

# zmv
# eg: zmv -W '*.txt' '*.html'
# man zshcontrib 'OTHER FUNCTIONS'
autoload -Uz zmv
alias zmv='noglob zmv -W'

# vcs_info
# man zshcontrib 'GATHERING INFORMATION FROM VERSION CONTROL SYSTEMS'
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '%F{yellow}%b%f:'
zstyle ':vcs_info:*' actionformats '%F{red}[%b|%a]%f:'
function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="%B%F{magenta}[%f${vcs_info_msg_0_}%F{magenta}%/]%f%b"
}
add-zsh-hook precmd _update_vcs_info_msg

# antigen
# cd ~/.zsh; git clone git@github.com:zsh-users/antigen.git
if [[ -f $HOME/.zsh/antigen/antigen.zsh ]]; then
    source $HOME/.zsh/antigen/antigen.zsh
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen apply
fi

# peco history
function peco-execute-history() {
    local item
    item=$(builtin history -n -r 1 | peco --query="$LBUFFER")

    if [[ -z "$item" ]]; then
        return 1
    fi

    BUFFER="$item"
    CURSOR=$#BUFFER
    zle accept-line
}
zle -N peco-execute-history
bindkey '^x^r' peco-execute-history
# anyframe version
# bindkey '^x^r' anyframe-widget-execute-history

# peco cdr
function peco-cdr() {
    local item
    item=$(cdr -l | sed 's/^[^ ]\{1,\} \{1,\}//' | peco)

    if [[ -z "$item" ]]; then
        return 1
    fi

    BUFFER="cd -- $item"
    CURSOR=$#BUFFER
    zle accept-line
}
zle -N peco-cdr
bindkey '^xb' peco-cdr
# anyframe version
# bindkey '^xb' anyframe-widget-cdr

# anyframe: peco-* definition framework
antigen bundle mollifier/anyframe

# put history
function peco-put-history() {
    builtin history -n -r 1 \
        | anyframe-selector-auto "$LBUFFER" \
        | anyframe-action-put
}
zle -N peco-put-history
bindkey '^x^p' peco-put-history

# kill process
function peco-kill() {
    ps -u $USER -o pid,stat,%cpu,%mem,cputime,command \
        | anyframe-selector-auto \
        | awk '{print $1}' \
        | anyframe-action-execute kill
}
zle -N peco-kill
bindkey '^x^k' peco-kill

# peco tmux
function peco-tmux() {
    local i=$(tmux lsw | awk '/active.$/ {print NR-1}')
    local f='#{window_index}: #{window_name}#{window_flags} #{pane_current_path}'
    tmux lsw -F "$f" \
        | anyframe-selector-auto "" --initial-index $i \
        | cut -d ':' -f 1 \
        | anyframe-action-execute tmux select-window -t
}
zle -N peco-tmux
bindkey '^xw' peco-tmux
