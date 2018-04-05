# base16-shell is super complicated and not ported to elvish, but this seems to be the jist:
E:BASE16_SHELL=~/.config/base16-shell/
E:BASE16_THEME=base16-solarized-dark
try {
  bash ~/'.config/base16-shell/scripts/'$E:BASE16_THEME'.sh'
} except e { put $e }

fn ls [@a]{ e:ls -Fhv --color --group-directories-first $@a | less -FRX }
fn la [@a]{ ls --all $@a }

fn gst { git status }
fn ga [@a]{ git add $@a }

fn gc [@a]{ git commit --verbose $@a }
fn gca [@a]{ gc --all $@a }

fn gd [@a]{ git diff $@a }
fn gdc [@a]{ gd --cached $@a }
fn gds [@a]{ gd --stat $@a }

fn gitkaaa {
  gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported'
}
fn gitkaa {
  gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported --tags'
}
fn gitka {
  gitk HEAD --argscmd='git rev-list --no-walk --branches --glob=refs/stash* --glob=refs/exported --tags --remotes'
}
fn tigaaa {
  tig HEAD (git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported)
}
fn tigaa {
  tig HEAD (git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported refs/tags)
}
fn tiga {
  tig HEAD (git for-each-ref --format="%(refname)" refs/heads refs/stash refs/exported refs/tags refs/remotes)
}

fn random-cowfile {
  cows = [(cowsay -l | drop 1 | each [s]{splits ' ' $s})]
  put $cows[(randint 0 (count $cows))]
}

try {
  fortune -as | cowsay -W 74 -f (random-cowfile)
} except e { put $e }
