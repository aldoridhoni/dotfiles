#!/bin/bash

LOCATION='/usr/share/applications/org.gnome.Terminal.desktop'

if [[ -n $LOCATION ]]; then
    sudo sed -ie 's/Exec=gnome-terminal%f/Exec=gnome-terminal --maximize --hide-menubar %f/g' $LOCATION
fi
