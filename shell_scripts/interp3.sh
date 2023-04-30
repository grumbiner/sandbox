#!/bin/sh
#Script to process ice concentration files into eta ice files
#Robert Grumbine

DATBASE=~/disk1/

tag=951031

while [ $tag -le 991230 ] 
do
  echo $tag
  yy=`echo $tag | cut -c1-2`
  mm=`echo $tag | cut -c3-4`
  dd=`echo $tag | cut -c5-6`

  if [ ! -f index.19$yy ] ; then
    wgrib ${DATBASE}/ice.all/ice.19$yy > index.19$yy
  fi

  if [ ! -f ice.$tag ] ; then
    grep $tag index.19$yy | wgrib -i ${DATBASE}/ice.all/ice.19$yy -nh -o ice.$tag 
  fi
  if [ -f ice.$tag ] ; then
    ./toeta_grumbine.half ice.$tag eta.$tag $yy $mm $dd 00
  fi

  ./changetypeftc eta.$tag ${DATBASE}/regional.reanalysis/char.$tag

  rm ice.$tag eta.$tag
 
#  if [ -f eta.$tag ] ; then
#    if [ ! -f eta$tag.xpm ] ; then
#      ./etaxpm eta.$tag etaland eta$tag.xpm
#    fi
#    if [ ! -f eta$tag.gif ] ; then
#      xpmtogif eta$tag.xpm
#      rm eta$tag.xpm
#    fi
#  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done 
