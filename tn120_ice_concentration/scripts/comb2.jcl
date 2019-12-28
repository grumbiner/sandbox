REANL=$HOME3/execs
SDIR=$DHOME/ssmi/fifteenth

tag=`date +"%y%m%d"`
echo $tag
tagm=`expr $tag - 1 `
tagm=`dtgfix3 $tagm`
#tag=$tagm
#tagm=`expr $tag - 1 `
#tagm=`dtgfix3 $tagm`


#cd /tmp/wd21rg/redo
cd /tmp/wd21rg/

#Get the current operational SST field
fetch oned -mVS -fTR -t'dsn=nmc.prod.sstoi.analysis,disp=shr'
assign -a oned -F null -N ibm fort.11
assign -a sstout -s unblocked fort.12
$REANL/oisst 

#Filter the ice concentration field (in lat-long space only so far)
$REANL/filtbg sstout umasknorth.$tag umasksouth.$tag latlon.$tag \
              nps.$tag sps.$tag 2.0

#fill in the ice concentration field with older data as required
$REANL/tfill $SDIR/fill.$tagm latlon.$tag $SDIR/age.$tagm age.$tag fill.$tag


#Engrib the lat-long file
echo $tag > ein
assign -a fill.$tag   -s unblocked fort.10
assign -a eng.$tag    -s unblocked fort.12
$REANL/mkllglob < ein

if [ -s latlon.$tag ] ; then
  mv latlon.$tag $SDIR
  rm $SDIR/latlon.$tagm
fi
if [ -s age.$tag ] ; then
  mv age.$tag $SDIR
  rm $SDIR/age.$tagm
fi
if [ -s fill.$tag ] ; then
  mv fill.$tag $SDIR
  rm $SDIR/fill.$tagm
fi
