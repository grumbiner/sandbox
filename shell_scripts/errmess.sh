#!/bin/sh
#Collect statistics on 404 messages per product group
#Robert Grumbine 1 December 2000

set -x
tag=$1
getstats -M -d -w -m -ra -fa -dt -l 200 > 404.stats
grep waves 404.stats > waves.errs
mail -s "waves 404 messages" hendrik.tolman@noaa.gov < waves.errs
rm waves.errs
grep seaice 404.stats | mail -s "$tag seaice 404 stats" rmg3
grep winds 404.stats | mail -s "$tag winds web 404 stats" vladimir.krasnopolsky@noaa.gov william.gemmill@noaa.gov

