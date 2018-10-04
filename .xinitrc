export ALSA_CARD=PCH
xrdb -merge ~/.Xresources
pulseaudio --start
#compton -b
xsetroot -cursor_name left_ptr
setxkbmap -layout no -option ctrl:nocaps -option "nbsp:none"
xmodmap ~/.Xmodmap
stalonetray &
nm-applet &
volumeicon &
cbatticon &
wmname LGD
xscreensaver &
exec xmonad
#exec i3
