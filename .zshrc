#####[ OH-MY-ZSH ]##########################################################

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME='xiong-chiamiov-plus'
ZSH_THEME='woof'

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(archlinux battery cabal coffee colored-man git git-extras safe-paste screen zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

#####[ CONFIG SELECTION ]###################################################
#
#   TODO add /usr/games to path?
#   TODO tolerate the case that I set a machine's hostname to "ac.uk" or "com"?

[[ -z "$_HOST" ]] && [[ -z "$_IS_LABS" ]] && [[ -z "$_IS_ARCH" ]] && \
  [[ -z "$_IS_LAPTOP" ]] && [[ -z "$_IS_SUDOER" ]] && [[ -z "$_HAS_YAOURT" ]] && \
  [[ -z "$_HAS_OPTICAL_DRIVE" ]] || echo "Somethings being used?! :("

# Lowercase $HOST, and split by '.' into an array
_HOST=("${(Ls/./)HOST}")

_HAS_OPTICAL_DRIVE=false
_HAS_YAOURT=false
_IS_ARCH=false
_IS_LABS=false
_IS_LAPTOP=false
_IS_SUDOER=false

if [[ "${(j/./)_HOST[-2,-1]}" = 'ac.uk' ]]; then
  echo "You're in labs!"
  _IS_LABS=true
fi

if [[ "${_HOST[-1]}" = 'com' ]]; then
  echo "You're at work!"
fi

case ${_HOST[1]} in
  gladys | winona)
    _IS_LAPTOP=true
    ;| # break but continue scanning
  gladys | vera)
    _HAS_OPTICAL_DRIVE=true
    ;& # fall through
  tombstone | winona)
    _HAS_YAOURT=true
    ;& # fall through
  watchtower)
    _IS_ARCH=true
    ;& # fall through
  buzzard)
    _IS_SUDOER=true
    ;| # break but continue scanning
esac

unset _HOST

#####[ ZSH OPTIONS ]########################################################

#setopt correct
unsetopt correctall

setopt autopushd
setopt autocd
setopt auto_param_slash

HISTFILE=/tmp/.zsh-hist-`whoami`
HISTSIZE=1000
SAVEHIST=1000
setopt append_history
setopt inc_append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_ignore_space
setopt hist_verify
setopt share_history
setopt no_hist_beep

#####[ KEYBINDINGS ]########################################################

#bindkey -e

bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
# for rxvt
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End

bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey ' ' magic-space # history expansion on space

#####[ ENVIRONMENTAL VARIABLES ]############################################

export EDITOR=vim
export VISUAL=$EDITOR

if [[ $_IS_LABS = true ]]; then
  source ~/.profile
  #location of the dropbox lock file, LEAVE UNASSIGNED OTHERWISE
  DLOCKDIR=~/.dropboxLock
fi

#####[ ALIASES ]############################################################

### UNIVERSAL ALIASES ###
alias -g G='| grep --color=auto'
alias -g L='| less'
alias ls='ls -Fhv --color=auto --group-directories-first'
alias l='ls -l'
alias ll='ls -la'
alias lg='ls -gG'
alias la='ls -a'
alias lR='ls -lR'
alias v='vim'
alias vd='vimdiff'
alias df='df -h'
alias mountc='mount | column -t'
#alias -g less='less -R'
alias -g grep='grep --color=auto'
alias hd='hexdump -C'
alias du='du -h'
### DROPBOX ###
alias dls='dropbox filestatus -l'
alias dwstat='watch -n 1 dropbox status'

alias git-repack-aggressive='git repack -Adf --depth=300 --window=300'

alias gfa='git fetch --all'
alias gdc='git diff --cached'
alias gds='git diff --stat'
alias gdcs='git diff --cached --stat'
alias gitkaaa="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/stash'"
alias gitkaa="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/tags refs/stash'"
alias gitka='gitk --all'
alias tigaaa='tig $(git for-each-ref --format="%(refname)" refs/heads refs/stash)'
alias tigaa='tig $(git for-each-ref --format="%(refname)" refs/heads refs/tags refs/stash)'
alias tiga='tig --all'

function bckground {
  nohup $@ </dev/null &>/dev/null &
}

alias cabal-install-xmobar='cabal install xmobar --flags="with_threaded with_utf8 with_xft with_alsa with_iwlib with_mpd"'
alias cabal-install-everything='cabal update && cabal install happy alex && cabal install cabal-install && cabal install c2hs -j && cabal-install-xmobar -j'

function find-loose-git-objects {
  find ./ -wholename "*/.git/*objects/*" -type d \! \( -name info -o -name pack \) "$@"
}

function find-dropbox-conflicts {
  find ~/Dropbox/ \( -name "*'s conflicted copy *" -o -name "*(Case Conflict)*" \) "$@"
}

function fliptable {
  echo "（╯°□°）╯︵ ┻━┻"
}

#TODO update this
function sbt-clean {
  echo "Cleaning ~/.sbt:"
  rm -rfv ~/.sbt/{boot,staging,plugins/{project,target}}

  echo "Cleaning current project:"
  for d in ./{,project/{,project/{,project/}}}target ; do
    [[ -d "$d" ]] && echo "${d}:" && rm -rv "$d"
  done

  return 0
}

if [[ $_IS_LABS = true ]]; then
  ### Labs aliases ###
  alias lp-staple='lp -d ICTMono -o "OutputBin=UStapler HPStaplerOptions=1diagonal"'
  alias quota='quota -sQ'
  alias locked='startx -display :1 -- :1 vt9'
  function dstat {
    if [[ -e "$DLOCKDIR" ]]; then
      echo -n "dropbox running on "
      cat "$DLOCKDIR"
    else
      echo "no dropbox lock detected"
    fi
    dropbox status
  }
  function dstart {
    if [[ -e "$DLOCKDIR" ]]; then
      echo -n "Dropbox is already running on "
      cat "$DLOCKDIR"
      return 1
    else
      echo "Creating $DLOCKDIR..."
      uname -n > "$DLOCKDIR"
      dropbox start
    fi
  }
  function dstop {
    if [[ ! -e "$DLOCKDIR" ]]; then
      echo "Dropbox *shouldn't* be running..."
      # dropbox stop
      return 1
    fi
    if [[ `uname -n` != `cat $DLOCKDIR` ]]; then
      echo -n "dropbox is running on "
      cat $DLOCKDIR
      return 1
    fi
    dropbox stop && rm -v $DLOCKDIR
  }
  function drestart {
    dstop && sleep 1 && dstart
  }
else
  ### ALL NON-LABS ###
  alias dstat='dropbox status'
  function dstart {
    sudo systemctl start dropbox@`whoami`
  }
  function dstop {
    sudo systemctl stop dropbox@`whoami`
  }
  function drestart {
    sudo systemctl restart dropbox@`whoami`
  }
  if [[ $_IS_SUDOER = true ]]; then
    alias s='sudo'
    alias sv='sudo vim'
    alias svd='sudo vimdiff'
    if [[ $_IS_ARCH = true ]]; then
      alias sp='sudo pacman'
      alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
      alias mkinitcpio-all='sudo sh -c "mkinitcpio -p linux & \
                                        mkinitcpio -p linux-ck & \
                                        mkinitcpio -p linux-lts &"'
      if [[ $_HAS_YAOURT = true ]]; then
        alias y='yaourt'
        alias ysc='yaourt -Sc --noconfirm'
      else
        alias p='pacman'
      fi
    fi
  fi
  if [[ $_HAS_OPTICAL_DRIVE = true ]]; then
    alias abcde-mp3-high='abcde -o mp3'
    alias abcde-flac='abcde -o flac'
  fi
  if [[ $_IS_LAPTOP = true ]]; then
    alias hdmi-off='xrandr --output HDMI-0 --off'
    alias hdmi-on='xrandr --output HDMI-0 --mode 1920x1080 --rate 60 \
                          --set underscan off --right-of LVDS'
    alias crt-off='xrandr --output CRT1 --off'
    alias crt-on='xrandr --output CRT1 --mode 1920x1080 --rate 60 \
                          --right-of LVDS'
    alias vga-off='xrandr --output VGA-0 --off'
    alias vga-on='xrandr --output VGA-0 --mode 1920x1080 --rate 60 \
                          --right-of LVDS'
  fi
  case "${(L)HOST}" in
    GLADYS)
      alias hdmi-test='aplay -D plughw:1,3 \
                       /usr/share/sounds/alsa/Front_Center.wav'
      alias mplayer-hdmi='mplayer -ao alsa:device=hw1.3'
      ;;
    WINONA)
      alias hdmi-test='aplay -D plughw:0,1 \
                       /usr/share/sounds/alsa/Front_Center.wav'
      alias mplayer-hdmi='mplayer -ao alsa:device=hw0.1'
      ;;
  esac
fi

#####[ GREETING ]###########################################################

# TODO more readable method for colouring?
tput setaf 2
uname -nrmo
[[ -f /proc/cpuinfo ]] && grep "^model name" /proc/cpuinfo -m1 | tail -c+14
date
if command -v fortune &> /dev/null; then
  if command -v cowsay &> /dev/null; then
    # TODO rewrite this with zsh-isms?
    fortune -as | cowsay -W 74 -f `cowsay -l | tail -n+2 | tr " " "\n" | shuf -n1`
  else
    echo
    fortune -a
    echo
  fi
else
  echo "No fortunes for you!"
fi
tput sgr0

#####[ CLEAN UP ]###########################################################

unset _HAS_OPTICAL_DRIVE
unset _HAS_YAOURT
unset _IS_ARCH
unset _IS_LABS
unset _IS_LAPTOP
unset _IS_SUDOER

[[ -d ~/Desktop ]] && rm -rv ~/Desktop
[[ -d ~/Downloads ]] && rm -rv ~/Downloads
[[ -f ~/Dropbox/desktop.ini ]] && rm -v ~/Dropbox/desktop.ini

############################################################################

true

