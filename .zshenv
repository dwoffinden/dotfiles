#####[ PREZTO ]#############################################################

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#####[ CONFIG SELECTION ]###################################################

_HAS_OPTICAL_DRIVE=false
_HAS_YAOURT=false
_IS_LAPTOP=false
_IS_SUDOER=false
_IS_WORK=false

if (( $+commands[lsb_release] )); then
  _DISTRO=`lsb_release -si`
else
  # probably windows
  _DISTRO=Unknown
fi

# Lowercase $HOST, and split by '.' into an array
_HOST=("${(Ls/./)HOST}")

if [[ $_HOST[-1] = com ]]; then
  _IS_WORK=true
  if [[ $_HOST[-5] = daw ]]; then
    _IS_SUDOER=true
  fi
  if [[ $_HOST[-4] = roam ]]; then
    _IS_LAPTOP=true
  fi
fi

case $_HOST[1] in
  gladys | winona)
    _IS_LAPTOP=true
    ;| # break but continue scanning
  gladys | vera)
    _HAS_OPTICAL_DRIVE=true
    ;& # fall through
  tombstone | winona)
    _HAS_YAOURT=true
    ;& # fall through
  watchtower | buzzard | edgar)
    _IS_SUDOER=true
    ;| # break but continue scanning
esac

#####[ ENVIRONMENTAL VARIABLES ]############################################

export EDITOR=vim
export VISUAL=$EDITOR

if (( $+commands[nproc] )); then
  export MAKEFLAGS=-j`nproc`
fi

ZSH_COMPDUMP="/tmp/$USER-zcompdump-$ZSH_VERSION"

if [[ -d /usr/games ]]; then
  path=($path /usr/games)
fi

############################################################################

