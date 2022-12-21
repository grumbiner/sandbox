#! /bin/sh
# sort_eddy.sh 
# this script greps for EDDY in files 
# William O'Connor Sep 2003 
#---------------------------------------------------
wkdir='/migr/data/cfspom/data/gspath' 
cd ${wkdir}
pwd
#------------------------------------------------------
/bin/rm -f ${wkdir}/WARM*   
/bin/rm -f ${wkdir}/COLD*   
#----------------------------------------------------
for no in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 \
          16 17 18 19 20 21 22   
do

# grep for WARM eddies
grep "WARM EDDY ${no}" jan_eddy.dat >>  ${wkdir}/WARM${no}
grep "WARM EDDY ${no}" feb_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" mar_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" apr_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" may_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" jun_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" jul_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" aug_eddy.dat >>  ${wkdir}/WARM${no} 
grep "WARM EDDY ${no}" sep_eddy.dat >>  ${wkdir}/WARM${no} 


# grep for COLDeddies
grep "COLD EDDY ${no}" jan_eddy.dat >>  ${wkdir}/COLD${no}
grep "COLD EDDY ${no}" feb_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" mar_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" apr_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" may_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" jun_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" jul_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" aug_eddy.dat >>  ${wkdir}/COLD${no} 
grep "COLD EDDY ${no}" sep_eddy.dat >>  ${wkdir}/COLD${no} 

done  
#-------------------------------------------------
# plot eddy tracks with IDL   

for no in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 \
          16 17 18 19 20 21 22   
do
# plot WARM eddies 
if [ -s ${wkdir}/WARM${no} ]
then     
echo '.run plot_gs_rings.pro'  >  idlfile
echo  WARM${no}                >>  idlfile
wc -l WARM${no}                >>  idlfile
echo 'exit'                    >>  idlfile
idl                            <   idlfile 
lpr -Pphaser4 < plot_gs_rings.ps 
fi 
#
# plot COLD eddies
if [ -s ${wkdir}/COLD${no} ]
then     
echo '.run plot_gs_rings.pro'  >  idlfile
echo  COLD${no}                >>  idlfile
wc -l COLD${no}                >>  idlfile
echo 'exit'                    >>  idlfile
idl                            <   idlfile 
lpr -Pphaser4 < plot_gs_rings.ps 
fi 

done 

# end 
