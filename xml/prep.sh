#!/bin/sh

#Cron-able script for seaspara to generate a full set of html files
#  given a suite of .jpg and .xml files
#Robert Grumbine 15 March 2004 

#Modified now so as to fill in sub-directories for an experiment properly
# -- by copying over the .xsl and .sh files from the reference directory 

export LD_LIBRARY_PATH=/usr/local/lib/
export base_dir=/data/WWW/ofs/developmental/main/
cd ${base_dir}/ref

#-- Should need no changes below here --------------
if [ $# -eq 2 ] ; then
  fday=$1
  vday=$2
else
  echo error! wrong arg count, quitting
  exit 1
fi

#Presumed that the directories are created and filled by the calling script. 
if [ ! -d ${base_dir}/$fday/$vday ] ; then
  echo directory ${base_dir}/$fday/$vday was not created
  exit 1
fi

#Copy scripts and xsl to the target directory:
cp *.sh *.xsl ${base_dir}/$fday/$vday

#Now invoke the main constructor in the target directory:
${base_dir}/$fday/$vday/all.sh ${base_dir}/$fday/$vday

