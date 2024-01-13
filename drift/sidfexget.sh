#!/bin/bash --login
#PBS -N sidfexget
#PBS -o sidfexout
#PBS -j oe
#PBS -A XFER-DEV
#PBS -q dev_transfer
#PBS -l walltime=6:00:00
#PBS -l select=1:ncpus=1

set -x

if [ ! -d /u/robert.grumbine/noscrub/drift/sidfex ] ; then
	mkdir -p /u/robert.grumbine/noscrub/drift/sidfex
fi
cd /u/robert.grumbine/noscrub/drift/sidfex

export base=/NCEPDEV/emc-marine/5year/Robert.Grumbine/
hsi mget $base/sidfex/*

exit

for f in drift.202110t12.tar drift.202104t6.tar drift.202202.tar drift.dev.202204.tar drift.202201.tar drift.dev.202203.tar
do
	hsi get $base/drift/$f
done
exit

