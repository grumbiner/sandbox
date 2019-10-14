tempor = bytarr(360, 180)
window, 0, xsize=360, ysize=180
wset, 0

!order = 1
xloadct
xinteranimate, set=[360, 180, 31]

a = findgen(5)
sbase='/tmp_mnt/export/hp20/wd21rg/iscp/cout.8708'
for j = 1, 31 do begin
  if (j LT 10) THEN BEGIN
   sbase2=sbase+'0'+strtrim(string(j),2)
  endif else begin
   sbase2 = sbase + strtrim(string(j),2)
  endelse

  fname=sbase2
  print, j, string(j)
  openr, unit, fname, /get_lun
  readu, unit, tempor
  tv, tempor
  xinteranimate, frame=j-1, window=!D.window
  free_lun, unit

endfor

end
