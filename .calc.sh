#!/bin/bash
#xsel -o | dmenu -p Kalk: | xargs echo | calc -p | xargs dmenu -p


IN="ANS"
while [ "$IN" != ""  ]
do
    # copy result to clipboard
    echo $IN | xsel
    # dmenu
    IN=$(echo $IN | dmenu -b -l 0 -fn "-*-dejavu sans mono-medium-r-*-*-20-*-*-*-*-*-*-*" -p "CALC:" | xargs echo | calc -p)

done
