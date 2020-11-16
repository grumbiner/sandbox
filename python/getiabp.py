#!/usr/bin/python
import os
import urllib

imeis=['300234063991680','300234063803010','300234065495020','300234066036110','300234066030330','300234066030190','300234066031190','300234066830700'] 

for i in imeis:
        web=urllib.urlopen('http://iabp.apl.washington.edu/WebData/'+i+'.dat')
        if (not os.path.exists('SIDFEX_DATA')):
          os.mkdir('SIDFEX_DATA',0755)
        outfile=open('SIDFEX_DATA/'+i+'.dat','w+')
        data=web.read()
        outfile.write(data)
        web.close()
        outfile.close()
