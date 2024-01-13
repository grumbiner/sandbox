'open all.ctl '
'set gxout grfill '
'set mpdset hires '
#'set lat  35  55'
#'set lon 280 315'
#
'clear '
'set grads off '
'set display color white '
'set strsiz 0.2'
'draw string 3.00 7.875 East Coast Ages'
'd orig'
'run cbarn'
'printim origa.gif'
#
'clear '
'set grads off '
'set display color white '
'set strsiz 0.2'
'draw string 3.00 7.875 East Coast Ages'
'd age'
'run cbarn'
'printim age.gif'
#
#step=5
#while (step < 20) 
  'clear '
  'set grads off '
  'set display color white '
  'set strsiz 0.2'
  'draw string 3.00 7.875 East Coast conc 7'
  'd maskout(orig,7-age)'
  'run cbarn'
  'printim orig'7'a.gif'
#  step=step+1
#endwhile
#
'quit '
