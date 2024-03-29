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

HISTSIZE=100000
SAVEHIST=100000
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

# silly prezto
unalias ls

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
alias mountc='mount | column -t'
alias hd='hexdump -C'
alias cp="${aliases[cp]:-cp} --reflink=auto"

alias ga='git add'
alias gca='git commit -av'
alias gf='git fetch'
alias gfa='git remote update'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --stat'
alias gdcs='git diff --cached --stat'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias grs='git rebase --skip'
alias gst='git status'

alias gitkaaa="gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported'"
alias gitkaa="gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported --tags'"
alias gitka="gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported --tags --remotes'"
alias tigaaa='tig HEAD $(git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported)'
alias tigaa='tig HEAD $(git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported refs/tags)'
alias tiga='tig HEAD $(git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported refs/tags refs/remotes)'

function bckground {
  nohup $@ </dev/null &>/dev/null &
}

function find-loose-git-objects {
  find ./ -wholename "*/.git/*objects/*" -type d \! \( -name info -o -name pack \) "$@"
}

if [[ $_IS_SUDOER = true ]]; then
  alias s='sudo'
  alias sv='sudo vim'
  alias svd='sudo vimdiff'
  if [[ $_DISTRO = Arch ]]; then
    alias p='pacman'
    alias sp='sudo pacman'
    alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
    alias mkinitcpio-all='sudo sh -c "mkinitcpio -p linux & \
                                      mkinitcpio -p linux-lts &"'
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

if [[ $_DISTRO = Ubuntu ]]; then
  alias dbus-halt='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop'
  alias dbus-reboot='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart'
fi

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
    fortune -s | cowsay -W 74 -f `cowsay -l | tail -n+2 | tr " " "\n" | shuf -n1`
  else
    echo
    fortune
    echo
  fi
else
  echo "No fortunes for you!"
fi
echo -n $FG[normal]

#####[ CLEAN UP ]###########################################################

[[ -d ~/Templates ]] && rmdir ~/Templates

############################################################################

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null

true

