#
#  env
#

export LANG=en_US.UTF-8
export LC_CTYPE=
export LC_ALL=$LANG
export LANGUAGE=

# ^W: backward-kill-word
export WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

# history
HISTFILE=~/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function histall { history -E 1 }

# path
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH

# go
export GOPATH=$HOME/dev
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/opt/go/bin:$PATH

function path() {
    echo $PATH | perl -nle 'print for split ":"'
}

# homebrew
export HOMEBREW_MAKE_JOBS=1

# emacs keybind
bindkey -e

# setopt
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

# history
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
bindkey "^P" history-beginning-search-backward-end
# incremental search
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

# completion
autoload -Uz compinit; compinit
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' use-cache true

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

#
# prompt
#
autoload -Uz colors; colors
PROMPT="%B%{[36m%}%n%{[37m%} @ %{[35m%}%M%b%{[0m%} %{[33m%}%B%C %#%b %{[0m%}"
RPROMPT="%{[35m%}%B[%/]%{[0m%}%b"

#
# alias
#
alias a=alias
alias h=history
alias ls='ls -F'
alias ll='ls -l'
alias la='ls -a'
alias l.='ls -d .*'
alias cp='cp -i'
alias mv='mv -i'

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

if type exa >/dev/null 2>&1; then
  alias ls='exa --time-style=long-iso --git'
fi
alias s=sbt
alias c=cargo
alias stack='stack -j1'

alias vi=nvim
alias vim=nvim
