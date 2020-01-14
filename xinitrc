# start ssh
eval $(ssh-agent -s)

# turn off bells.
xset b off
set bell none

# set abnt2 keyboard layout
setxkbmap -model abnt2 -layout br -variant abnt2
xmodmap $HOME/.xmodmap

# init Xmonad
exec xmonad
