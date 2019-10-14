tempor = bytarr(385, 465)
window, 0, xsize=385, ysize=465
wset, 0

xloadct
xinteranimate, set=[385, 465, 26]

a = findgen(5)
sbase='vara.9408'
for j = 1, 26 do begin
  if (j LT 10) THEN BEGIN
   sbase2=sbase+'0'+strtrim(string(j),2)
  endif else begin
   sbase2 = sbase + strtrim(string(j),2)
  endelse

fname=sbase2+'00'
print, j, string(j)
openr, unit, fname, /get_lun
readu, unit, tempor
tv, tempor
xinteranimate, frame=j-1, window=!D.window
free_lun, unit

endfor
end
