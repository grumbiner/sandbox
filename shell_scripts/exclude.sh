fn=$1
for s in gdas1.t..z.pgrb gdas1.t..z.sf gblav.t..z drfmr.t..z. pub.data.nccf.com.eta com.ingest.prod pub.data.nccf.com.ruc var.ftp.pub.emc.mmb ftp.pub.emc.gmb ftp.pub.emc.cmb ftp.pub.cpc ftp.pub.hpc pub.nco.datastreme pub.data.nccf_backup eta_pcpn_anal ukmet pub.data.nccf.images com.mrf.prod.ens rucs.dbnet wafsavn com.nawips nccf.ptmp nccf.pcom nccf.com.sref nccf.com.ngm pub.data2.NCDC avn..........bufr.t..z.bufr3 gdas2 saicmrf mrf.prod.ecmwf data2.NASA
do
  grep -v $s $fn > a
  mv a $fn
done
