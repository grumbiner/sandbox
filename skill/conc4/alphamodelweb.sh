#!/bin/sh

PATH=${PATH}:/usr/local/bin

export basenwprod=${basenwprod:-/u/Robert.Grumbine/desk}
export basecom=${basecom:-/u/Robert.Grumbine/noscrub}

export com=${com:-$basecom/com/}
export nwprod=${nwprod:-$basenwprod/model}

export model=${model:-kiss}
export version=${version:-v1.0.1}
export PDY=${PDY:-`date +"%Y%m%d"`}

cd ${com}/mmab/developer/${model}_${version}.${PDY}
if [ $? -ne 0 ] ; then
  echo alphamodelweb could not cd to ${com}/mmab/developer/${model}_${version}.${PDY}
  exit 1
fi

# Make file with forecast 'from' and 'valid' dates ----------------------------
i=1
  PDYp=`expr $PDY + 1`
  PDYp=`dtgfix3 $PDYp `
  echo $PDY $PDYp > dates
i=2
while [ $i -le 8 ]
do
  PDYp=`expr $PDYp + 1`
  PDYp=`dtgfix3 $PDYp `
  echo $PDY $PDYp >> dates
  i=`expr $i + 1`
done

# Ensure that we have graphic files to process:
if [ -z $EXECsice ] ; then
  EXECsice=${nwprod}/${model}_${version}/exec
else 
  echo execdir = $EXECsice
fi
echo running seaice_graphics programs
if [ ! -f graphic_fields.bin.1 ] ; then
  $EXECsice/seaice_graphics running.out.1 12   0 graphic_fields.bin.1
fi
if [ ! -f graphic_fields.bin.2 ] ; then
  $EXECsice/seaice_graphics running.out.2 12  72 graphic_fields.bin.2
fi
if [ ! -f graphic_fields.bin.3 ] ; then
  $EXECsice/seaice_graphics running.out.3  8 144 graphic_fields.bin.3
fi


# New and prettier version--------------------------------------------------------
echo now work on grads graphics:
for f in icegraphics.gs cbarn.gs icegraphics.ctl model.$version domains parameter.$version icegraphics1.gs icegraphics2.gs icegraphics3.gs icegraphics1.ctl icegraphics2.ctl icegraphics3.ctl
do
  if [ ! -f $f ] ; then
    cp ${nwprod}/${model}_${version}/auxiliary/$f .
  fi
done
ln -sf parameter.$version parameter
ln -sf model.$version model
time grads -lcb icegraphics1.gs
time grads -lcb icegraphics2.gs
time grads -lcb icegraphics3.gs

#Make animations:
for domain in alaska nhemi shemi global asian
do
  ls -1 conc_${domain}_?.gif > a
  whirlgif -o conc_${domain}_anim.gif -i a
done


# Remove these because they're large, and, unlike the running output, are
#   not used again later.
rm graphic_fields.bin.?

#Don't do this while polar is out of commission 21 October 2013
#exit
#
#copy to web--------------------------------------------------------------------
ssh -l seaice emcrzdm mkdir -p /home/www/polar/develop/icemodel/${model}_${version}/${model}_${version}.$PDY
chmod a+r *.gif
scp -p *.gif seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version} 
ssh -l seaice emcrzdm cp -p /home/www/polar/develop/icemodel/${model}_${version}/*.gif \
                            /home/www/polar/develop/icemodel/${model}_${version}/${model}_${version}.$PDY
# Old and nasty-looking version--------------------------------------------------------
for parm in thick conc
do
  hh=006
  while [ $hh -le 192 ]
  do
    convert -flip ${parm}${hh}.xpm ${parm}${hh}.png
    rm ${parm}${hh}.xpm
    chmod a+r ${parm}${hh}.png
    scp -p ${parm}${hh}.png seaice@emcrzdm:/home/www/polar/develop/icemodel/${model}_${version} 
    hh=`expr $hh + 6`
    if [ $hh -lt 100 ] ; then
      hh=0$hh
    fi
  done
done
rm *.xpm

ssh -l seaice emcrzdm cp -p /home/www/polar/develop/icemodel/${model}_${version}/*.png \
                            /home/www/polar/develop/icemodel/${model}_${version}/${model}_${version}.$PDY
