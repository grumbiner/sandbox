#QSUB -o  /wd2/wd21/wd21rg/ssmi.getout
#QSUB -lT 150
#QSUB -lM   8.0Mw
#QSUB -me

#Script for handling the production and dissemination of all ssmi-derived
#  sea ice concentration files
#Bob Grumbine 1 March 1995.
#Modified for producing reanalysis files 28 November 1995.  Bob Grumbine

SDIR=/wd2/wd21/wd21rg/execs
ADIR=/dm/wd21rg/ssmi/fifteenth
CDIR=/wd2/wd21/wd21rg/control
RDIR=/marine/ice/ice.analy
RDIR2=/data/forecasts/ice.analy

export SDIR ADIR CDIR RDIR RDIR2

#Nothing below here should need changing on a new system

cd /tmp/wd21rg

tag=`date +'%y%m%d'`
jday=`date +'%j'`
refyear=`date +'%Y' `
if [ ! -f ssmi ] ; then
  cp ${SDIR}/ssmi .
fi

ja

# Get the SSMI orbit by orbit file
# F-13 orbit file, change 5/14/95
#if [ ! -s unblock.$tag ] ; then
  fetch x -fBB -mVS -t"dsn=com.spprod.ss7idb,disp=shr" 
  uscpblock -u -fBB x   unblock.$tag
  rm x
#fi

# Run the analysis on the files
${SDIR}/ssmi unblock.$tag ${CDIR}/nland.map ${CDIR}/sland.map \
             n3ssmi.$tag s3ssmi.$tag umasknorth.$tag umasksouth.$tag \
             b3north.$tag b3south.$tag $jday $refyear

##############################
#Filter, re-process, etc. the data files
##############################


${SDIR}/rtxpm umasknorth.$tag $CDIR/nland.map nh.xpm \
              umasksouth.$tag $CDIR/sland.map sh.xpm

exit
echo `date +'%y' ` > psin
echo `date +'%m' ` >> psin
echo `date +'%d' ` >> psin

for pole in north south
do
  assign -a umask$pole.$tag -s unblocked fort.10
  assign -a ${pole}psg.$tag -s unblocked fort.11
  ${EXDIR}/psgrib.$pole < psin
done


##############################
#Distribute the files
##############################

#C-90 file disposition
cp ?3ssmi.$tag ${ADIR}
cp b3???th.$tag    ${ADIR}
cp eng.$tag    ${ADIR}
cp *psg.$tag   ${ADIR}

#J916 file disposition
rcp ?3ssmi.$tag  cray4:${RDIR}
rcp b3???th.$tag cray4:${RDIR}
rcp eng.$tag     cray4:${RDIR}
rcp *psg.$tag    cray4:${RDIR}

#polar file disposition
rcp ?3ssmi.$tag  polar:${RDIR2}
rcp b3???th.$tag polar:${RDIR2}
rcp b3???th.$tag polar:/home/ftp/pub
rcp eng.$tag     polar:${RDIR2}
rcp eng.$tag     polar:/home/ftp/pub/cdas
rcp *psg.$tag    polar:${RDIR2}
rcp *psg.$tag    polar:/home/ftp/pub

#Create the gif files for home page use -- do in background to 
#  avoid hanging all processing in the event of an hp20 lockup.
nohup $CDIR/www.jcl &

#remsh hp20 'rcp cray3:/tmp/wd21rg/nh.xpm nh.xpm'
#remsh hp20 'rcp cray3:/tmp/wd21rg/sh.xpm sh.xpm'
#remsh hp20 convert -display :0 nh.xpm nh.gif
#remsh hp20 'rcp nh.gif polar:/data/WWW/nh.gif'
#remsh hp20 'rcp nh.gif polar:/home/ftp/pub/nh.gif'
#remsh hp20 convert -display :0 sh.xpm sh.gif
#remsh hp20 'rcp sh.gif polar:/data/WWW/sh.gif'
#remsh hp20 'rcp sh.gif polar:/home/ftp/pub/sh.gif'

ja -chlst
