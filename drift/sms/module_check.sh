module purge
for f in envvars EnvVars prod_envir python ips impi bacio bufr grib_util prod_util util_shared w3emc w3nco
do
  module spider $f 2>> spider.out
  module avail $f 2>> avail.out
done

exit

module load EnvVars/1.0.3
module load prod_envir/1.1.0 
module load python/3.6.3
module load ips/18.0.5.274 
module load impi/18.0.1

#   ips/18.0.1.163    ips/18.0.5.274    ips/19.0.5.281 (D)
for f in bacio bufr grib_util prod_util util_shared w3emc w3nco
do
  module spider $f 2>> spider.out2
  module avail $f 2>> avail.out2
done

module load bacio/2.0.2
module load bufr/11.3.1
module load grib_util/1.1.1
module load prod_util/1.1.3
module load util_shared/1.1.2
module load w3emc/2.3.0
module load w3nco/2.0.6

module list

