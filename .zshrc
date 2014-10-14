# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/halvor/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall


### Custom aliases
alias audiomixer="alsamixer -c 1"
alias atbVoll="/usr/bin/atbQuery getRealTime 16010553"
alias atbGlosN="/usr/bin/atbQuery getRealTime 16010333"
alias atbOBerg="/usr/bin/atbQuery getRealTime 16010567"
alias atbPrinsK="/usr/bin/atbQuery getRealTime 16011011"
### load colors
BASE16_SCHEME="railscasts"
BASE16_SHELL="$HOME/.shell/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

export PATH=/home/halvor/.cabal/bin:$PATH

### turn off autocorrect
DISABLE_CORRECTION="true"
unsetopt correct
unsetopt correct_all

### dirstack
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

## Remove duplicate entries
setopt pushdignoredups

## This reverts the +/- operators.
setopt pushdminus

export VIRTUALENV_PYTHON=/usr/bin/python2.7
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2.7
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

export PGDATA=/home/halvor/.postgres/data



# source /usr/share/zsh/scripts/antigen/antigen.zsh

# antigen use oh-my-zsh

# antigen bundle zsh-users/zsh-syntax-highlighting
# antigen bundle git
# antigen bundle vagrant
# antigen bundle virtualenvwrapper
# antigen bundle gradle
# antigen bundle sudo
# antigen bundle wd

# antigen theme pygmalion

export EDITOR=emacs
# source /usr/bin/virtualenvwrapper.sh

# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor root)
# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export ANDROID_HOME=/home/halvor/apps/android-sdk-linux
export ANDROID_NDK_ROOT=/home/halvor/apps/android-ndk-r10-32
export ANDROID_SDK_ROOT=/home/halvor/apps/android-sdk-linux

export QMAKE_PATH=/home/halvor/apps/Qt/5.3/android_x86/bin/qmake
