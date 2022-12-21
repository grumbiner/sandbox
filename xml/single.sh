#!/bin/sh
export LD_LIBRARY_PATH=/usr/local/lib/

  fn=$1
  base=`echo $fn | cut -c3-900 | cut -f1 -d.`
  Xalan -o ${base}.html -a $base.xml 
