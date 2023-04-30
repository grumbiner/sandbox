#!/usr/bin/sh

INCDIR=../includes
LIBS='../lib/omblibc ../lib/omblibf'
CPP=/usr/bin/CC 
PLATFORM=SGI

for fn in basetest.C mathtest.C metricvector.test.C pointh.test.C \
          test.gridbase.C test.gridmath.C test.metric.C vec.test.C \
          gridtype.tests.C
do
  $CPP $fn -I $INCDIR $LIBS -O2 -D$PLATFORM -o ${fn}.x 
  ./${fn}.x > ${fn}.out
done
