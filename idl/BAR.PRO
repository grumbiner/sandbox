data = bytarr(256, 256, 10)
window, /free, xsize=256, ysize = 256
a = findgen(5)
for j = 0,9 do begin
plot, a, /nodata, yrange = [0,10]
bar_plot, randomu(seed, 5)*9.0, /overplot
data(*,*,j) = tvrd()
erase
endfor
end
