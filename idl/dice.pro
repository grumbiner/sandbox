tempor = bytarr(304, 448)
window, 0, xsize=304, ysize=448
wset, 0

xloadct
xinteranimate, set=[304, 448, 31]

a = findgen(5)
sbase='9112'
for j = 3, 31 do begin
  if (j LT 10) THEN BEGIN
   sbase2=sbase+'0'+strtrim(string(j),2)
  endif else begin
   sbase2 = sbase + strtrim(string(j),2)
  endelse

fname=sbase2 + '.del'
print, j, string(j)
openr, unit, fname, /get_lun
readu, unit, tempor
tv, tempor
xinteranimate, frame=j-1, window=!D.window
free_lun, unit

endfor
end
