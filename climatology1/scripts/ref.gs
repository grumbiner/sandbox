'open ref.ctl'
'set gxout grfill'
'clear'
#'enable print b.grads'
'set display color white'
'set mpdset hires'
#'set lon 30 390'
'set mproj nps'
'set lon -350 10'
'set lat 30 90'
#'set mproj sps'
#'set lon -240 120'
#'set lat -90 -50'
#
'set grads off'
'set strsiz 0.2'
'draw string 5.00 7.75 Average'
'set clevs 0 0.05 0.15 0.50 0.70 0.80 0.85 0.90 0.95 '
'd avg'
'run cbarn'
'printim avg.gif'
'clear'
#
'quit'