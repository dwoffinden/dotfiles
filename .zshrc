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
plugins=(archlinux battery coffee colored-man git git-extras safe-paste screen zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

#####[ CONFIG SELECTION ]###################################################
#
#   {LABS,GLADYS,WINONA,VERA,TOMBSTONE}
#
#   TODO feature-specific flags? e.g. archlinux, ubuntu, laptop, labs?
#   TODO use hostname / domainname
#   TODO add /usr/games to path?

[[ -n "$CONF" ]] && echo "CONF already set to \"$CONF\", WTF?"

CONF=${(U)HOST}
case $CONF in
  GLADYS | WINONA | VERA | TOMBSTONE)
    ;;
  *)
    CONF=LABS
    ;;
esac

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

if [[ "$CONF" = LABS ]]; then
  source ~/.profile
  #location of the dropbox lock file, LEAVE UNASSIGNED OTHERWISE
  export DLOCKDIR=~/.dropboxLock
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

alias timetable='elinks www3.imperial.ac.uk/computing/internal/timetables'

alias screen-off='xset dpms force off'
alias screen-on='xset dpms force on'
alias screensaver-off='xset s off ; xset -dpms'
alias screensaver-on='xset s on ; xset +dpms'

alias git-repack-aggressive='git repack -Adf --depth=300 --window=300'

alias gfa='git fetch --all'
alias gdc='git diff --cached'
alias gds='git diff --stat'
alias gitka="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/tags'"
alias tiga='tig --all'

function bckground {
  nohup $@ </dev/null &>/dev/null &
}

alias cabal-install-xmobar='cabal install xmobar --flags="with_threaded with_utf8 with_xft with_mpd with_alsa"'

function find-loose-git-objects {
  find ./ -wholename "*/.git/*objects/*" -type d \! \( -name info -o -name pack \) "$@"
}

function find-dropbox-conflicts {
  find ~/Dropbox/ -name "*'s conflicted copy *" "$@"
}

function fliptable {
  echo "（╯°□°）╯︵ ┻━┻"
}

function sbt-clean {
  echo "Cleaning ~/.sbt:"
  rm -rfv ~/.sbt/{boot,staging,plugins/{project,target}}

  echo "Cleaning current project:"
  for d in ./{,project/{,project/{,project/}}}target ; do
    [[ -d "$d" ]] && echo "${d}:" && rm -rv "$d"
  done

  return 0
}

case "$CONF" in
  LABS)
    alias quota='quota -sQ'
    alias locked='startx -display :1 -- :1 vt9'
    function dstat {
      if [[ -n "$DLOCKDIR" ]]; then
        if [[ -e "$DLOCKDIR" ]]; then
          echo -n "dropbox running on "
          cat "$DLOCKDIR"
        else
          echo "no dropbox lock detected"
        fi
      fi
      dropbox status
    }
    function dstart {
      if [[ -n "$DLOCKDIR" ]]; then
        if [[ -e "$DLOCKDIR" ]]; then
          echo -n "Dropbox is already running on "
          cat "$DLOCKDIR"
          return 1
        else
          echo "Creating $DLOCKDIR..."
          uname -n > "$DLOCKDIR"
          dropbox start
        fi
      else
        dropbox start
      fi
    }
    function dstop {
      if [[ -z "$DLOCKDIR" ]]; then
        dropbox stop
        return
      fi
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
    alias drestart='dstop && sleep 1 && dstart'
    ;;
  *)
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
    alias writer='libreoffice --writer'
    alias s='sudo'
    alias y='yaourt'
    alias ysc='yaourt -Sc --noconfirm'
    alias sp='sudo pacman'
    alias sv='sudo vim'
    alias svd='sudo vimdiff'
    alias start='sudo systemctl start'
    alias stop='sudo systemctl stop'
    alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
    alias mkinitcpio-all='sudo sh -c "mkinitcpio -p linux & \
                                      mkinitcpio -p linux-ck & \
                                      mkinitcpio -p linux-lts &"'
    alias mount-labs='mount /media/labs'
    alias mount-icfs='mount /media/icfs'
    #//fs-homes.doc.ic.ac.uk /mnt/labs cifs defaults,user,noauto,relatime,domain=WIN,user=daw10 0 0
    #//icfs7.cc.ic.ac.uk     /mnt/icfs cifs defaults,user,noauto,relatime,domain=IC,user=daw10  0 0
    #alias mount-labs='sudo mount -t cifs //fs-homes.doc.ic.ac.uk/daw10 \
    #             /media/labs -o user=WIN/daw10'
    #alias umount-labs='sudo umount /media/labs'
    #alias mount-college='sudo mount -t cifs //icfs7.cc.ic.ac.uk/daw10 \
    #             /media/college -o user=IC/daw10'
    #alias umount-college='sudo umount /media/college'
    alias pwoff='systemctl poweroff'
    alias hbernate='systemctl hibernate'
    alias rboot='systemctl reboot'
    alias abcde-mp3-high='abcde -o mp3:"-V0 -q0 -ms"'
    ;|
  GLADYS | WINONA) # Laptops
    alias hdmi-off='xrandr --output HDMI-0 --off'
    alias hdmi-on='xrandr --output HDMI-0 --mode 1920x1080 --rate 60 \
                          --set underscan off --right-of LVDS'
    alias crt-off='xrandr --output CRT1 --off'
    alias crt-on='xrandr --output CRT1 --mode 1920x1080 --rate 60 \
                          --right-of LVDS'
    alias vga-off='xrandr --output VGA-0 --off'
    alias vga-on='xrandr --output VGA-0 --mode 1920x1080 --rate 60 \
                          --right-of LVDS'
    ;|
    #aplay -l
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
  VERA)
    ;;
esac

#####[ GREETING ]###########################################################

tput setaf 2
uname -nrmo
[[ -f /proc/cpuinfo ]] && grep "^model name" /proc/cpuinfo -m1 | tail -c+14
date
if (command -v fortune &> /dev/null); then
  fortune -as | cowsay -W 74 -f `cowsay -l | tail -n+2 | tr " " "\n" | shuf -n1`
else
  echo "No fortunes for you!"
fi
tput sgr0

#####[ CLEAN UP ]###########################################################

unset CONF

[[ -d ~/Desktop ]] && rm -rv ~/Desktop
[[ -d ~/Downloads ]] && rm -rv ~/Downloads
[[ -f ~/Dropbox/desktop.ini ]] && rm -v ~/Dropbox/desktop.ini

############################################################################

true
