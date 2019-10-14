function plotfn, p1, p2, color=color, symbol=yes
np=n_params()

CASE np of
  0 : BEGIN
        print, 'No arguments were passed, I need 1 or 2'
        RETURN, 0
      END
  1 : BEGIN
        print, 'Only one argument was passed'
        depend = p1
        indep = findgen(n_elements(p1))
      END
  2 : BEGIN
        print, 'Two arguments were passed'
        depend = p2
        indep  = p1
      END
  ELSE : BEGIN 
           print, 'Too many arguments passed '
           RETURN, 0
         END
ENDCASE

IF ( N_ELEMENTS(color) EQ 0) THEN  color = !d.n_colors-1

LOADCT, 5

plot, indep, depend, /nodata
oplot, indep, depend, color=color, psym=keyword_set(yes)

a=fltarr(2)
a(0) = max(depend)
a(1) = min(depend)
return, a

END
