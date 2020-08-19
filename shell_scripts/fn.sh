#!/bin/bash
#Demonstrate usage of a function within a shell script
#note that functions must be defined before their use

#Robert Grumbine
#7 October 1999

set -x
function fn() {
  if [ $1 -lt 10 ] ; then
    tmp=0$1
  else
    tmp=$1
  fi
  echo $tmp
} 

x=5
y=`fn $x`
echo $x
echo $y

