#####[ PREZTO ]#############################################################

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#####[ PRELIMINARIES ]######################################################

[[ -d /tmp/$USER ]] || mkdir /tmp/$USER && chmod 700 /tmp/$USER

#####[ CONFIG SELECTION ]###################################################
#
#   TODO tolerate the case that I set a machine's hostname to "ac.uk" or "com"?

_HAS_OPTICAL_DRIVE=false
_HAS_YAOURT=false
_IS_ARCH=false
_IS_LAPTOP=false
_IS_SUDOER=false
_IS_WORK=false

# Lowercase $HOST, and split by '.' into an array
_HOST=("${(Ls/./)HOST}")

if [[ "${_HOST[-1]}" = 'com' ]]; then
  echo "You're at work!"
fi

case ${_HOST[1]} in
  gladys | winona | daw-glaptop)
    _IS_LAPTOP=true
    ;| # break but continue scanning
  gladys | vera)
    _HAS_OPTICAL_DRIVE=true
    ;& # fall through
  tombstone | winona)
    _HAS_YAOURT=true
    ;& # fall through
  watchtower | buzzard)
    _IS_SUDOER=true
    ;| # break but continue scanning
  daw | daw-glaptop)
    _IS_WORK=true
    _IS_SUDOER=true
    ;| # break but continue scanning
esac

if [[ `lsb_release -si` = 'Arch' ]]; then
  _IS_ARCH=true
fi

#####[ ENVIRONMENTAL VARIABLES ]############################################

export EDITOR=vim
export VISUAL=$EDITOR

export MAKEFLAGS=-j`nproc`

ZSH_COMPDUMP="/tmp/$USER/zcompdump-$ZSH_VERSION"

if [[ $_IS_WORK = true ]] && [[ -d /usr/games ]]; then
  path=($path /usr/games)
fi

############################################################################

