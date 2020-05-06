#QSUB -lM 4Mw
#QSUB -lT 1000
#QSUB -me

CDIR=$HOME41/control/
cd $HOME40/ssmi2

set -x
tag=`date +'%y%m%d'`
tagm=`expr $tag - 1`
tagm=`dtgfix3 $tagm`
jday=`date +'%j'`
refyear=`date +'%Y' `



ls -1 /ptmpb/wd23kz/ssmi_data/PRD.SPPROD.SDRS7.* > alpha
./fdcp.proc < alpha > gamma

exit
ja
./ssmi delta ${CDIR}/nland.map ${CDIR}/sland.map \
             n3ssmi.$tag s3ssmi.$tag umasknorth.$tag umasksouth.$tag \
             b3north.$tag b3south.$tag $jday $refyear

ja -chlst
exit
#Exit here for testing

#Make up the grib files for the polar stereographic data
echo `date +'%y' ` > psin
echo `date +'%m' ` >> psin
echo `date +'%d' ` >> psin

for pole in north south
do
  assign -a umask$pole.$tag -s unblocked fort.10
  assign -a ${pole}psg.$tag -s unblocked fort.11
  ${SDIR}/psgrib.$pole < psin
done
cp *psg.$tag   ${ADIR1}
cp *psg.$tag   ${ADIR2}

######################################################
#Construct the graphic files for further display
######################################################
${SDIR}/rtxpm umasknorth.$tag $CDIR/nland.map nh.xpm \
              umasksouth.$tag $CDIR/sland.map sh.xpm



##############################
#Filter, re-process, etc. the data files
##############################
#$CDIR/comb2.jcl
#Get the current operational SST field
fetch oned -mVS -fTR -t'dsn=nmc.prod.sstoi.analysis,disp=shr'
assign -a oned -F null -N ibm fort.11
assign -a sstout -s unblocked fort.12
$SDIR/oisst 

#Filter the ice concentration field (in lat-long space only so far)
$SDIR/filtbg sstout umasknorth.$tag umasksouth.$tag latlon.$tag \
              nps.$tag sps.$tag 2.0

#fill in the ice concentration field with older data as required
$SDIR/tfill $ADIR/fill.$tagm latlon.$tag $ADIR/age.$tagm age.$tag fill.$tag

#Engrib the lat-long file
echo $tag > ein
assign -a fill.$tag   -s unblocked fort.10
assign -a eng.$tag    -s unblocked fort.12
$SDIR/mkllglob < ein
cp eng.$tag    ${ADIR1}
cp eng.$tag    ${ADIR2}

#Remove the temporaries
if [ -s latlon.$tag ] ; then
  cp latlon.$tag $ADIR1
  mv latlon.$tag $ADIR2
  rm $ADIR1/latlon.$tagm
  rm $ADIR2/latlon.$tagm
fi
if [ -s age.$tag ] ; then
  cp age.$tag $ADIR1
  mv age.$tag $ADIR2
  rm $ADIR1/age.$tagm
  rm $ADIR2/age.$tagm
fi
if [ -s fill.$tag ] ; then
  cp fill.$tag $ADIR1
  rcp fill.$tag polar:/data/WWW/seaice/analysis/filled
  mv fill.$tag $ADIR2
  rm $ADIR1/fill.$tagm
  rm $ADIR2/fill.$tagm
fi


##############################
#Distribute the files
##############################
#J916 file disposition
rcp ?3ssmi.$tag  cray4:${RDIR}
rcp b3???th.$tag cray4:${RDIR}
rcp eng.$tag     cray4:${RDIR}
rcp *psg.$tag    cray4:${RDIR}

#polar file disposition

rcp ?3ssmi.$tag  polar:${RDIR2}
rcp b3???th.$tag polar:${RDIR2}
rcp b3???th.$tag polar:/home/ftp/pub/ice
rcp eng.$tag     polar:${RDIR2}
rcp eng.$tag     polar:/home/ftp/pub/cdas
rcp *psg.$tag    polar:${RDIR2}
rcp *psg.$tag    polar:/home/ftp/pub/ice

#Create the gif files for home page use -- do in background to 
#  avoid hanging all processing in the event of an hp20 lockup.
nohup $CDIR/www.jcl > $HOME3/runout/wwwout &


ja -chlst
