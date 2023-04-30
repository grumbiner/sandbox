#!/bin/sh
#Script to process ice concentration files into eta ice files
#Robert Grumbine

DATBASE=~/disk1/

tag=19991231

while [ $tag -le 20020331 ] 
do
  echo $tag
  cc=`echo $tag | cut -c1-2`
  yy=`echo $tag | cut -c3-4`
  mm=`echo $tag | cut -c5-6`
  dd=`echo $tag | cut -c7-8`
  stag=${yy}${mm}${dd}

  if [ ! -f index.${cc}$yy ] ; then
    wgrib ${DATBASE}/ice.all/ice.${cc}$yy > index.${cc}$yy
  fi

  if [ ! -f ice.$stag ] ; then
    grep $stag index.${cc}$yy | wgrib -i ${DATBASE}/ice.all/ice.${cc}$yy -nh -o ice.$stag 
  fi
  if [ -f ice.$stag ] ; then
    ./toeta_grumbine.half ice.$stag eta.$stag $yy $mm $dd 00
  fi

  ./changetypeftc eta.$stag ${DATBASE}/regional.reanalysis/char.$stag

  rm ice.$stag eta.$stag
 
#  if [ -f eta.$stag ] ; then
#    if [ ! -f eta$stag.xpm ] ; then
#      ./etaxpm eta.$stag etaland eta$stag.xpm
#    fi
#    if [ ! -f eta$stag.gif ] ; then
#      xpmtogif eta$stag.xpm
#      rm eta$stag.xpm
#    fi
#  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done 
