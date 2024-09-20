#!/bin/sh

BOLD="\e[1";
ENDCOLOR="\e[0m";
Green="32m";
LightRed="31m";

echo ""
echo -e $BOLD";"";"$Green" FILE UPLOADER"$ENDCOLOR
echo ""

if ! [ $1 ]; then
    echo ""
    echo -e $BOLD";"";"$LightRed" Please provide file path!"$ENDCOLOR
    echo ""
    exit
fi

if [ $1 ]; then
    echo -e " Are u sure u want upload this file: "$BOLD";"";"$Green" '$1'"$ENDCOLOR"? [y/N]: "
    read -p " " input
    echo ""
    # read -r -p " " input
    case $input in
	[yY][eE][sS]|[yY])
	    URL=$(curl -s -F "file=@$1" https://0x0.st);
	    echo -e " URL: "$BOLD";"";"$Green"$URL"$ENDCOLOR;
	    ;;
	[nN][oO]|[nN])
	    echo -e $BOLD";"";"$Green" OK"$ENDCOLOR;
	    ;;
	*)
	    echo -e $BOLD";"";"$LightRed" Invalid input..."$ENDCOLOR;
	    exit 1
	    ;;
    esac
    echo ""
fi;
