#!/bin/bash

DIR="$HOME/.local/share/fonts/HackGen"

if [ ! -d $DIR ]; then
    mkdir -p $DIR
fi
cp HackGen* $DIR

sudo apt install fontconfig
fc-cache -vf
