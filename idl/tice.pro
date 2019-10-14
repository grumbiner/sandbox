tempor = bytarr(316, 332)
window, 0, xsize=316, ysize=332
wset, 0

loadct, 13
xinteranimate, set=[316, 332, 31]
loadct, 13

a = findgen(5)
sbase='9101'
stag='.TOT'
for j = 1,31 do begin
  if (j LT 10) THEN BEGIN
   sbase2=sbase+'0'+strtrim(string(j),2)
  endif else begin
   sbase2 = sbase + strtrim(string(j),2)
  endelse

fname=sbase2+stag
print, j, string(j)
openr, unit, fname, /get_lun
readu, unit, tempor
tv, tempor
xinteranimate, frame=j-1, window=!D.window
free_lun, unit

endfor
end
