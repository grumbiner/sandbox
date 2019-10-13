tempor = bytarr(304, 448)
temp2 = intarr(304,448)

window, 1, xsize=304, ysize=448
wset, 1

nframe=2
xinteranimate, set=[304, 448, nframe]

a = findgen(5)
sbase='c:\models\inverse\data\9101'
stag='.tot'
temp2 = 0
for j = 1, 31 do begin
if (j LT 10) THEN BEGIN
 sbase2=sbase+'0'+strtrim(string(j),2)
endif else begin
 sbase2 = sbase + strtrim(string(j),2)
endelse

fname=sbase2+stag
print, j, string(j)
openr, unit, fname, /get_lun
readu, unit, tempor
temp2 = temp2 + tempor
free_lun, unit

endfor
temp2 = temp2/31
tempor = temp2
tv, tempor
tv, tempor
xinteranimate, frame=0, window=!D.window

end
