#####[ BASE16-SHELL ]#######################################################

BASE16_SCHEME=solarized
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

#####[ PREZTO ]#############################################################

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#####[ ZSH OPTIONS ]########################################################

#setopt correct
unsetopt correctall

setopt autopushd
setopt autocd
setopt auto_param_slash

HISTFILE=/tmp/$USER/zsh-history
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

# Disable the VCS prompt in the managed mount points
zstyle ':vcs_info:*' disable-patterns "/run/user/*/gvfs/*"

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

#####[ ALIASES ]############################################################

### UNIVERSAL ALIASES ###
ls () {
  command ls -Fhv --color=always --group-directories-first "$@" | less -FRX
}
alias l='ls -l'
alias ll='ls -la'
alias lg='ls -gG'
alias la='ls -a'
alias lR='ls -lR'
alias v='vim'
alias vd='vimdiff'
alias df='df -h'
alias mountc='mount | column -t'
alias grep='grep --color=auto'
alias hd='hexdump -C'
alias du='du -h'

### DROPBOX ###
alias dls='dropbox filestatus -l'
alias dwstat='watch -n 1 dropbox status'

alias git-repack-aggressive='git repack -Adf --depth=300 --window=300'

alias gca='git commit -av'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --stat'
alias gdcs='git diff --cached --stat'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gst='git status'

alias gitkaaa="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/stash'"
alias gitkaa="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/stash refs/tags'"
alias gitka="gitk --argscmd='git for-each-ref --format=\"%(refname)\" refs/heads refs/remotes refs/stash refs/tags'"
alias tigaaa='tig $(git for-each-ref --format="%(refname)" refs/heads refs/stash)'
alias tigaa='tig $(git for-each-ref --format="%(refname)" refs/heads refs/stash refs/tags)'
alias tiga='tig $(git for-each-ref --format="%(refname)" refs/heads refs/remotes refs/stash refs/tags)'

function bckground {
  nohup $@ </dev/null &>/dev/null &
}

alias cabal-install-everything='cabal update && cabal install -j alex happy && cabal install -j gtk2hs-buildtools && cabal install -j taffybar libmpd'

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

### ALL NON-LABS ###
alias dstat='dropbox status'
if [[ $_IS_SUDOER = true ]]; then
  alias s='sudo'
  alias sv='sudo vim'
  alias svd='sudo vimdiff'
  if [[ $_IS_ARCH = true ]]; then
    function dstart {
      sudo systemctl start dropbox@$USER
    }
    function dstop {
      sudo systemctl stop dropbox@$USER
    }
    function drestart {
      sudo systemctl restart dropbox@$USER
    }
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
case ${_HOST[1]} in
  gladys)
    alias hdmi-test='aplay -D plughw:1,3 \
                     /usr/share/sounds/alsa/Front_Center.wav'
    alias mplayer-hdmi='mplayer -ao alsa:device=hw=1.3'
    ;;
  winona)
    alias hdmi-test='aplay -D plughw:0,1 \
                     /usr/share/sounds/alsa/Front_Center.wav'
    alias mplayer-hdmi='mplayer -ao alsa:device=hw=0.1'
    ;;
esac

alias dbus-halt='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop'
alias dbus-reboot='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart'
alias dbus-suspend='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend'
alias dbus-hibernate='dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate'

#####[ GREETING ]###########################################################

echo -n $FG[green]
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
echo -n $FG[normal]

#####[ CLEAN UP ]###########################################################

[[ -d ~/Desktop ]] && rmdir ~/Desktop
[[ -d ~/Downloads ]] && rmdir ~/Downloads
[[ -f ~/Dropbox/desktop.ini ]] && rm -v ~/Dropbox/desktop.ini
[[ -d ~/Templates ]] && rmdir ~/Templates

############################################################################

true

