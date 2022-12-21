#!/bin/sh
base=`pwd`

fn=`basename $1`

d=`echo $fn | cut -f1 -d\.`
mkdir $d
cd $d

f77split ../$fn 
touch -r ../$fn *

cd $base
rm -f $fn
