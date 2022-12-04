#QSUB -s /bin/sh
#QSUB -lT 1800
#QSUB -lM 8Mw
#QSUB -a "Tomorrow-8:30"
#QSUB -eo
#QSUB -r obs

WORKHOME=$HOME2/ssmi.bufr
WORKTMP=/tmp/$LOGNAME/ssmibtmp
mkdir -p $WORKTMP
cd $WORKTMP
rm *

tag=`date +"%y%m%d"`

ja
#Run dumpscript to retrieve a day's data:
/nwprod/bufr/scripts/dumpjb ${tag}00 12 ssmit

#Process in to a readable form, note that ssmit.cos is a name set by the
#  dumpscript.
assign -a $WORKTMP/ssmit.cos    -Fcos fort.14
assign -a bufrout -s unblocked fort.50
$WORKHOME/ssmi.bufr.x > btmp.data 2>>btmp.err

#Process the flattened bufr in to an ssmi file
echo bufrout > alpha
$WORKHOME/ssmi alpha $WORKHOME/nland.map $WORKHOME/sland.map $WORKHOME/nout $WORKHOME/sout $WORKHOME/nrawout $WORKHOME/srawout $WORKHOME/noutc $WORKHOME/soutc 235

ja -chlst
