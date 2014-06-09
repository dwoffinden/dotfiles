#####[ PRELIMINARIES ]######################################################

[[ -d /tmp/$USER ]] || mkdir /tmp/$USER && chmod 700 /tmp/$USER

#####[ CONFIG SELECTION ]###################################################
#
#   TODO add /usr/games to path?
#   TODO tolerate the case that I set a machine's hostname to "ac.uk" or "com"?

_HAS_OPTICAL_DRIVE=false
_HAS_YAOURT=false
_IS_ARCH=false
_IS_LABS=false
_IS_LAPTOP=false
_IS_SUDOER=false

# Lowercase $HOST, and split by '.' into an array
_HOST=("${(Ls/./)HOST}")

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

#####[ ENVIRONMENTAL VARIABLES ]############################################

export EDITOR=vim
export VISUAL=$EDITOR

export MAKEFLAGS=-j`nproc`

if [[ $_IS_LABS = true ]]; then
  source ~/.profile
  #location of the dropbox lock file
  DLOCKDIR=~/.dropboxLock
fi

############################################################################

