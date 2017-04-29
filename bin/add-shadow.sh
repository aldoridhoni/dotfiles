#!/bin/bash

pathname="$1"
noext="${pathname%.*}"

convert $pathname \( +clone -background black -shadow 80x40+0+15 \) \
        +swap -background transparent -layers merge +repage "$noext-shadow.png"
