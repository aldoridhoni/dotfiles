#!/bin/sh

export GDK_BACKEND=x11
export CLUTTER_BACKEND=x11
export XDG_SESSION_TYPE=x11
export WAYLAND_DISPLAY=:0
export DISPLAY=:0

dbus-launch $1
