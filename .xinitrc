#xsetroot -cursor_name left_ptr
setxkbmap -layout no
pulseaudio --start
feh --bg-fill /home/halvor/wallpaper.jpg
/usr/bin/xscreensaver -no-splash &
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
stalonetray &
nm-applet &
exec xmonad
