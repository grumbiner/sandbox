      PROGRAM a
      REAL x,y
      INTEGER i
      INTEGER start, newpos, size, number, nactual, fdes
      CHARACTER*80 fname
      INTEGER banio, oper

      x = 23.0
      y = 5.0
      size = 4
      number = 1
      WRITE(fname,9001)
 9001 FORMAT ("george")
      oper = 4
CD      i = st(fname)
      i = banio(oper, start, newpos, size, number, nactual, fdes, 
     1                       fname, x)
      PRINT *,'After open i = ',i
      PRINT *,start, newpos, size, number, nactual, fdes, fname, x
      IF (i .NE. 0) STOP "failed to open file correctly"

      i = banio(32, start, newpos, size, number, nactual, fdes, 
     1                       fname, x)  
      PRINT *,'After write i = ',i
      PRINT *,start, newpos, size, number, nactual, fdes, fname, x

      i = banio(16, 0, newpos, size, number, nactual, fdes, fname, y)
      PRINT *,'After write i = ',i
      PRINT *,start, newpos, size, number, nactual, fdes, fname, y
      
      i = banio(8, start, newpos, size, number, nactual, fdes,
     1                       fname, x)
      
      STOP
      END
