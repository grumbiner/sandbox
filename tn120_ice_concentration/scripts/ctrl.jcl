#QSUB -me
#QSUB -lT 18000
#QSUB -lM 1Mw
#QSUB -o /eldisk1/outputs


if [ ! -d /tmp/wd21rg ] ; then
  mkdir /tmp/wd21rg
fi
cd /tmp/wd21rg
cp /jdsk40/wd21rg/last.reanl/recomp .
cp /jdsk40/wd21rg/last.reanl/badmap .

base=/eldisk1/wd21rg/ssmi/ssmi
for fn in 9501.tar 9502.tar 9503.tar 9504.tar 9505.tar 9506.tar 9507.tar 
do
  dbase=`echo $fn | cut -c1-4`
  tar xvf ${base}$fn 
  chmod u+w * 
  rm a*
  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
            21 22 23 24 25 26 27 28 29 30 31
  do
    if [ -f nssmi.${dbase}$dy -a -f sssmi.${dbase}$dy ] ; then
      ./recomp nssmi.${dbase}$dy sssmi.${dbase}$dy nconc.${dbase}$dy sconc.${dbase}$dy
      rm nssmi.${dbase}$dy sssmi.${dbase}$dy 
     else
      echo No file for ${dbase}$dy
      ./badmap nconc.${dbase}$dy sconc.${dbase}$dy
    fi
  done
  tar cvf /eldisk1/wd21rg/conc$dbase [ns]conc.${dbase}*
  rm [ns]conc.${dbase}*
done

#####################################################
# Switch to the f-13 archives (n3)
#####################################################
for fn in 9504.tar 9505.tar 9506.tar 9507.tar
do
  dbase=`echo $fn | cut -c1-4`
  tar xvf ${base}$fn 
  chmod u+w ?3ssmi.*
  for dy in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
            21 22 23 24 25 26 27 28 29 30 31
  do
    if [ -f n3ssmi.${dbase}$dy -a -f s3ssmi.${dbase}$dy ] ; then
      ./recomp n3ssmi.${dbase}$dy s3ssmi.${dbase}$dy n3conc.${dbase}$dy s3conc.${dbase}$dy
      rm n3ssmi.${dbase}$dy s3ssmi.${dbase}$dy 
     else
      echo No file for ${dbase}$dy
      ./badmap n3conc.${dbase}$dy s3conc.${dbase}$dy
    fi
  done
  tar cvf /eldisk1/wd21rg/c3onc$dbase [ns]3conc.${dbase}*
  rm [ns]3conc.${dbase}*
done
