#!/bin/sh

    for infile in AWIP3203.19870*.sfc
    do
        outfile=`expr './copygb98/'$infile'_g98'`
        copygb -xg98 $infile $outfile
    done
