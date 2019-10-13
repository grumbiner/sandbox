data = bytarr(256, 256, 10)
window, 0, /pixmap, xsize=256*10, ysize = 256
window, 1, xsize=256, ysize=256
wset, 1

a = findgen(5)
for j = 0,9 do begin
plot, a, /nodata, yrange = [0,10]
bar_plot, randomu(seed, 5)*9.0, /overplot
temp=tvrd()
wset, 0
tvscl, temp, 256*j, 0
wset, 1
endfor
end
