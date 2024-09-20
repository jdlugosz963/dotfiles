#!/bin/sh

BOLD="\e[1";
ENDCOLOR="\e[0m";
Green="32";
LightRed="31";

if ! [ $1 ]; then
    echo ""
    echo -e $BOLD";"";"$LightRed"m Please provide music url!"$ENDCOLOR
    echo ""
    exit
fi

MUSIC_DIR="~/Documents/Music/%(artist)s/%(album)s/%(title)s-%(id)s.%(ext)s"
if [ $2 ]; then
    MUSIC_DIR="${a}/%(artist)s/%(album)s/%(title)s-%(id)s.%(ext)s"
fi

echo ""
echo -e $BOLD";"";"$Green"m Music will download to ${MUSIC_DIR} directory!"$ENDCOLOR
echo ""

sleep 1

yt-dlp -x --audio-format mp3 --embed-metadata -o $MUSIC_DIR  $1
