#####[ PREZTO ]#############################################################

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#####[ CONFIG SELECTION ]###################################################

_HAS_OPTICAL_DRIVE=false
_IS_LAPTOP=false
_IS_SUDOER=false

if (( $+commands[lsb_release] )); then
  _DISTRO=`lsb_release -si`
else
  # probably windows
  _DISTRO=Unknown
fi

# Lowercase $HOST, and split by '.' into an array
_HOST=("${(Ls/./)HOST}")

if [[ $_HOST[-1] = com ]]; then
  case $_HOST[-5] in
    liveware-problem | not-invented-here)
      _IS_SUDOER=true
      ;; # break
  esac
  if [[ $_HOST[-4] = roam ]]; then
    _IS_LAPTOP=true
  fi
fi

case $_HOST[1] in
  gladys | winona)
    _IS_LAPTOP=true
    ;| # break but continue scanning
  gladys | contents-may-differ)
    _HAS_OPTICAL_DRIVE=true
    ;& # fall through
  tombstone | winona)
    ;& # fall through
  xenophobe)
    _IS_SUDOER=true
    ;| # break but continue scanning
esac

#####[ ENVIRONMENTAL VARIABLES ]############################################

export EDITOR=vim
export VISUAL=$EDITOR
export PAGER=less

if [[ -z "$LANG" ]]; then
  export LANG='en_GB.UTF-8'
fi

ZSH_COMPDUMP="/tmp/$USER-zcompdump-$ZSH_VERSION"

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  /usr/local/{bin,sbin}
  $HOME/.local/bin
  $HOME/.cargo/bin
  $path
)

if [[ -d /usr/games ]]; then
  path=($path /usr/games)
fi

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

############################################################################

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

