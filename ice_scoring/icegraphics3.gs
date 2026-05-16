*Robert Grumbine
*30 Sep 2013

*Universal:
'open icegraphics3.ctl'
'clear'
'set display color white'
'set mpdset hires'

step = 1
*Read in the model tag information
res = read("model")
retcode = sublin(res,1)
if (retcode = 0) 
  modelsource = sublin(res, 2)
endif
res = read("model")
retcode = sublin(res,1)
if (retcode = 0) 
  modelname   = sublin(res, 2)
endif
say 'model source = 'modelsource
say 'model name 'modelname

dretcode = 0
count = 1
while (dretcode = 0) 
* Start looping over all domains:
  res = read("domains")
  dretcode = sublin(res,1)
  if (dretcode = 0 )
    latlonbox = sublin(res,2)
    tag    = subwrd(latlonbox,1)
    proj   = subwrd(latlonbox,2)
    minlon = subwrd(latlonbox,3)
    maxlon = subwrd(latlonbox,4)
    minlat = subwrd(latlonbox,5)
    maxlat = subwrd(latlonbox,6)
    say latlonbox
  else
    'quit'
  endif
  'set mproj 'proj
  'set lat 'minlat' ' maxlat
  'set lon 'minlon' ' maxlon

* Start looping over all parameters:
  pcode = 0
  while (pcode = 0)
    res = read("parameter")
    pcode = sublin(res,1)
    if (pcode = 0) 
      parmline   = sublin(res,2)
      parm = subwrd(parmline,1)
      parmtag = subwrd(parmline,2)
      gtype   = subwrd(parmline,3)
      scaling = subwrd(parmline,4)
      oscaling = subwrd(parmline,5)

      res = read("parameter")
      levelsline = sublin(res,2)
      res = read("parameter")
      titleline  = sublin(res,2)
    else
      break
    endif

* loop over step for each parameter, add that info to titleline
*   skip ahead to correct date in file
    dskip=6
    index = 0
    while (index < dskip) 
      res = read("dates")
      index = index + 1
    endwhile

    while (step <= 2) 
      'set t 'step*4
      'set grads off'
      'set strsiz 0.2'
      'draw string 0.5 8.25 'modelsource
      'draw string 3.5 7.95 'modelname
      'draw string 2.5 7.65 'titleline
      res = read("dates")
      dateline = sublin(res,2)
      fromdate = subwrd(dateline,1)
      todate   = subwrd(dateline,2)
      'draw string 2.5 7.35 Forecast from 'fromdate' valid on 'todate 
 
      'set gxout 'gtype
      if (gtype = "grfill") 
        'set clevs 'levelsline
        'd 'parm'*'scaling 
        'run cbarn'
      else
        'set arrscl 0.5 'scaling
*      'd skip(u,30,30);v '
        'd skip(u,'oscaling','oscaling');v '
      endif
  
      'printim 'parmtag'_'tag'_'step+dskip'.gif'
      'clear'
      step = step + 1
    endwhile
    close("dates")
    step=1
  endwhile
  close("parameter")

endwhile
'quit'
