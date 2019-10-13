tempor = bytarr(385, 465)
window, 0, xsize=385, ysize=465
wset, 0

xloadct
xinteranimate, set=[385, 465, 1]

a = findgen(5)
sbase='/tmp_mnt/export/hp20/wd21rg/ssmi/a2north.950421'
for j = 1,1 do begin

fname=sbase
print, j, string(j)
openr, unit, fname, /get_lun
readu, unit, tempor
tv, tempor
xinteranimate, frame=j-1, window=!D.window
free_lun, unit

endfor
end
