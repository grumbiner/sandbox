#!/usr/bin/python
import os
import urllib
#Or -- from dkrz sidfex site:
#wget -r -H -N --cut-dirs=4 --include-directories="/v1/" "https://swiftbrowser.dkrz.de/public/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_index/observations/?show_all"

imeis = ['127317', '139939', '300234060430010', '300234060434550', '300234060436000', '300234060834110', '300234062738010', '300234063803010', '300234063991680', '300234065495020', '300234066030190', '300234066030330', '300234066031190', '300234066036110', '300234066711310', '300234066713470', '300234066830700', '300234062880820', '300234065801030', '300234065802030', '300234061872720', '300234066417330', '300234067509680', '300234067700680', '300234067700760', '300234066089220', '300234066087220', '300234066084230', '300234065498190', '300234068312210', '300234065709990' ]

for i in imeis:
    web=urllib.urlopen('http://iabp.apl.washington.edu/WebData/'+i+'.dat')
    if (not os.path.exists('SIDFEX_DATA')):
      #os.mkdir('SIDFEX_DATA',0755)
      print "need to make data dir"
      exit(1)
    outfile=open('SIDFEX_DATA/'+i+'.dat','w+')
    data=web.read()
    outfile.write(data)
    outfile.close()
    web.close()
