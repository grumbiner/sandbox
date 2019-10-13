pro plotit, p1, p2, color=color, symbol=yes, help=helpvar
np=n_params()
IF (keyword_set(help) EQ 0) THEN BEGIN
  PRINT, 'You should pass the y values, and optionally the x.'
  PRINT, 'You may also specify the color, color=color number'
  PRINT, ' and whether you want symbols used /symbol '
  RETURN
END

CASE np of
  0 : BEGIN
        print, 'No arguments were passed, I need 1 or 2'
        RETURN
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
           RETURN
         END
ENDCASE

IF ( N_ELEMENTS(color) EQ 0) THEN  color = !d.n_colors-1

LOADCT, 5

plot, indep, depend, /nodata
oplot, indep, depend, color=color, psym=keyword_set(yes)

END
