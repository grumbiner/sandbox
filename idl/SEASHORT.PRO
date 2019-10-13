;data = bytarr(152, 224, 30)
tempor = bytarr(304, 448)
;window, 0, /pixmap, xsize=152*30, ysize = 224
window, 1, xsize=304, ysize=448
wset, 1

nframe=31
xinteranimate, set=[304, 448, nframe]

a = findgen(5)
sbase='c:\models\inverse\data\9101'
stag='.e1'
for j = 1, nframe do begin
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
