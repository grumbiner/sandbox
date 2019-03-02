C***********************************************************----------!!
	SUBROUTINE spiral(x, y, npts, xnot, ynot,turns, alpha)
	INTEGER npts, xnot, ynot, x(npts), y(npts)
	REAL turns,alpha
	
	x(1) = xnot
	y(1) = ynot
	DO 1000 i = 2, npts
	  x(i) = INT( xnot + FLOAT(i)*alpha*
     1         cos(2.*3.14159*turns*FLOAT(i)/FLOAT(npts-1)) )
	  y(i) = INT( ynot + FLOAT(i)*alpha*
     1         sin(2.*3.14159*turns*FLOAT(i)/FLOAT(npts-1)) )
CX	  PRINT *,x(i), y(i)
 1000 CONTINUE
      RETURN
      END
