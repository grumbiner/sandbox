pro image
;; Read in an image
;; Then use a cursor selection to create x and y sections through the cursor location
a=bytarr(512,512)

;openr, unit, 'trbl.512', /get_lun
;readu, unit, a
;free_lun, unit
a=25

;; plot it
window, 0, xsize=512, ysize=512
loadct, 5
tv, a


;repeat
;; select the cross section
wshow, 0
print, 'Click on a location to get cross sections (x and y)'
print, 'Press the right button to exit loop'
;
wset,0
cursor, x, y,/down, /device
b=bytarr(512)
c=bytarr(512)
b=a(x,*)
c=a(*,y)
print, 'x column number is ',x
print, 'y row number is ', y

;; window to plot sections in, multiple plots
window, 1, xsize=768, ysize=384, xpos=0, ypos=256
!p.multi=[0, 2, 1, 0, 0]
d=indgen(512)
plot, d, b
plot, d, c
wshow, 1

;until (!err EQ 4)

RETURN
END
