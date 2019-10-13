tempor = bytarr(345, 355)
window, 0, xsize=345, ysize=355
wset, 0

xloadct
xinteranimate, set=[345, 355, 32]

days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,  31]

a = findgen(5)
for k = 5, 12 do begin
;for k = 6, 6 do begin

  if (k LT 10) THEN BEGIN
    mo='0'+strtrim(string(k),2)
  endif else begin
    mo=strtrim(string(k),2)
  endelse

  sbase='/tmp_mnt/export/hp20/wd21rg/conc2/ssmi' + '/sconc.94' + mo
  for j = 1, days(k-1) do begin

    if (j LT 10) THEN BEGIN
     sbase2=sbase+'0'+strtrim(string(j),2)
    endif else begin
     sbase2 = sbase + strtrim(string(j),2)
    endelse
  
    fname=sbase2
    print, 94, k, j
    openr, unit, fname, /get_lun
    readu, unit, tempor
    tv, tempor
    xinteranimate, frame=j-1, window=!D.window
    free_lun, unit
    
  endfor

endfor

end
