tempor = bytarr(304, 448)
temp2  = bytarr(304, 448)
temp3  = bytarr(304, 448)
window, 1, xsize=304, ysize=448
wset, 1

nframe=31
xinteranimate, set=[304, 448, nframe]

a = findgen(5)
sbase='c:\models\inverse\data\9101'
stag='.e4b'
stag2='.e6'
for j = 1, nframe do begin
  if (j LT 10) THEN BEGIN
   sbase2=sbase+'0'+strtrim(string(j),2)
  endif else begin
   sbase2 = sbase + strtrim(string(j),2)
  endelse

  fname1=sbase2+stag
  fname2=sbase2+stag2
  print, j, string(j)
  openr, unit, fname1, /get_lun
  readu, unit, tempor
  openr, unit2, fname2, /get_lun
  readu, unit2, temp2

  temp3 = (tempor - temp2)+128
  tv, temp3
  xinteranimate, frame=j-1, window=!D.window
  free_lun, unit
  free_lun, unit2
endfor
end
