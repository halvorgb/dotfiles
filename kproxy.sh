#!/bin/bash
set -euo pipefail

#Black        0;30     Dark Gray     1;30
#Red          0;31     Light Red     1;31
#Green        0;32     Light Green   1;32
#Brown/Orange 0;33     Yellow        1;33
#Blue         0;34     Light Blue    1;34
#Purple       0;35     Light Purple  1;35
#Cyan         0;36     Light Cyan    1;36
#Light Gray   0;37     White         1;37

readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly GRAY='\033[0;37m'
readonly BASE_MESSAGE='LAUNCHING PROXY'

readonly CURRENT_CONTEXT=$(kubectl config current-context)

color=
if [[ "$CURRENT_CONTEXT" = "hoopladev.cloud" ]]
then
   color=$GREEN
elif [[ "$CURRENT_CONTEXT" = "hooplab.cloud" ]]
then
   color=$RED
else
   color=$YELLOW
fi
echo -e "${color} ${BASE_MESSAGE} ${CURRENT_CONTEXT} ${GRAY}"
echo -e "${color} ${BASE_MESSAGE} ${CURRENT_CONTEXT} ${GRAY}"
echo -e "${color} ${BASE_MESSAGE} ${CURRENT_CONTEXT} ${GRAY}"
kubectl proxy
