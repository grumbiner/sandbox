#!/bin/sh

#Script to cycle through the history of parameters and build backlog for
#  hires ice analysis images
#Robert Grumbine 7 December 2000

tag=20000225
while [ $tag -le 20001206 ] 
do
  for pole in north south global
  do
    p=`echo $pole | cut -c1`
    if [ -f ${pole}.output/${p}conc.$tag ] ; then
      ${pole}xpm ${pole}.output/${p}conc.$tag ${p}land.new ${pole}ref.xpm
      xpmtogif ${pole}ref.xpm 2> /dev/null
      rcp ${pole}ref.gif rmg3@polar:/data/WWW/seaice/expt.analy/history/${pole}ref.$tag.gif
    else
      echo $tag needs rerun for ref $pole
    fi
  
    if [ -f ${pole}.output/parm13.$pole.$tag ] ; then
      ${pole}xpm ${pole}.output/parm13.$pole.$tag ${p}land.new ${pole}new.xpm
      xpmtogif ${pole}new.xpm 2> /dev/null
      rcp ${pole}new.gif rmg3@polar:/data/WWW/seaice/expt.analy/history/${pole}new.$tag.gif
    else
      echo $tag needs rerun for parm13 $pole
    fi
  
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
 
