#!/bin/bash

LOCATION='/usr/share/applications/emacs.desktop'

if [[ -n $LOCATION ]]; then
    sudo sed -ie 's/emacs %f/emacsclient -c -a emacs %f/g' $LOCATION
fi
