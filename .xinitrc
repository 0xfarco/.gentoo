slstatus &
picom &
dunst &
#xautolock -time 5 -locker slock &
#xautolock -detectsleep -time 5 -locker slock -killtime 20 -killer "loginctl suspend" &
feh --bg-scale ~/Pictures/wallpaper/gentoo.png &
sxhkd -c ~/.config/sxhkd/sxhkdrc &
exec dwm
