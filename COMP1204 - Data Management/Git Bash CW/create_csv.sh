#!/bin/bash

#message for the user
echo "Converting "$1" -> "$2""

#text runs through a pipeline of commands and is modified until it is 
#presentable (see page 2)
grep -w 'UTC\|N\|mb\|knots' "$1" | sed -e '/name/d' -e '/dtg/d' -e 's/;.*//g' -e 's/<tr><td>//g' -e 's/<.td><.tr>//g' -e 's/<B>//g' -e 's/<.B>//g' | awk  '(NR%4==1){time=$1; month=$3; day=$4} (NR%4==2){latitude=$1; longitude=$3} (NR%4==3){pressure=$1} (NR%4==0){knots=$1} (NR%4==0){print time" UTC "month" "day","latitude" N,"longitude" W,"pressure" mb,"knots" knots"}' | sed '1 i Timestamp,Latitude,Longitude,MinSeaLevelPressure,MaxIntensity' > "$2" 

#message for the user
echo "Done!"
