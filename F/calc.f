      SUBROUTINE calc(xmax, xmin, ymax, ymin, xtic, ytic, 
     1           size, xraw, yraw, x, y)

C     Subroutine to process the data into the requested form, compute
C       the maximum and minimum values of x and y to be plotted, and
C       compute the tic intervals as needed.

C     Declare the arguments
      REAL xmax, xmin, ymax, ymin, xtic, ytic
      INTEGER size
      REAL xraw(size), yraw(size), x(size), y(size)

C     Declare the local variables.
      INTEGER i, ostyle
      REAL xr, yr, dx, dy

C**********************************************************----------!!
C     Process the raw data as needed.
      PRINT *,'How do you want the data plotted: '
      PRINT *,'     1 =     y vs     x'
      PRINT *,'     2 = LOG y vs     x'
      PRINT *,'     3 =     y vs LOG x'
      PRINT *,'     4 = LOG y vs LOG x'
      READ (*,9001) ostyle
      IF (ostyle .EQ. 2  .OR. ostyle .EQ. 4) THEN
        DO 1000 i = 1, size
          y(i) = ALOG10( yraw(i) )
 1000   CONTINUE
       ELSE
        DO 1100 i = 1, size
          y(i) = yraw(i)
 1100   CONTINUE
      ENDIF

      IF (ostyle .EQ. 3 .OR. ostyle .EQ. 4) THEN
        DO 2000 i = 1, size
          x(i) = ALOG10( xraw(i) )
 2000   CONTINUE
       ELSE
        DO 2100 i = 1, size
          x(i) = xraw(i)
 2100   CONTINUE
      ENDIF

C     Compute xmin, xmax and ymin, ymax, if needed.
C***********************************************************----------!!
      PRINT *,' '
      PRINT *,'To have the computer choose the max, min values, enter th
     1e same value         for the max and min.'
      PRINT *,' '
      PRINT *,'What is the minimum x value you want plotted?'
      READ (*,9002) xmin
      PRINT *,'What is the maximum "                       "'
      READ (*,9002) xmax
      PRINT *,'What is the minimum y value you want plotted?'
      READ (*,9002) ymin
      PRINT *,'What is the maximum y "                     "'
      READ (*,9002) ymax
      PRINT *,' '
     
      PRINT *,'Enter 0. for the tic mark interval if you want the comput
     1er to choose.'
      PRINT *,'     (It tries to get ten tic marks across the page.)'
      PRINT *,'What tic mark interval do you want in the x direction?'
      READ (*,9002) xtic
      PRINT *,'What tic mark interval do you want in the y direction?'
      READ (*,9002) ytic

      IF (xmax .LE. xmin) CALL armxmn(x, size, xmax, xmin)

      IF (ymax .LE. ymin) CALL armxmn(y, size, ymax, ymin)

C     Compute the tic mark interval if needed.
      IF (xtic .EQ. 0.0) THEN
        xr = xmax - xmin
        dx = 10.0**( ALOG10(xr) - FLOAT(INT(ALOG10(xr) )))
        dx = FLOAT ( INT ( dx + .5) )
        xtic = dx * 10.0**( FLOAT( INT(ALOG10(xr))) -1.)
      ENDIF

      IF (ytic .EQ. 0.0) THEN
        yr = ymax - ymin
        dy = 10.0 **( ALOG10(yr) - FLOAT(INT(ALOG10(yr) )))
        dy = FLOAT ( INT ( dy + .5) )
        ytic = dy * 10.0**( FLOAT( INT(ALOG10(yr))) -1.)
      ENDIF

C     May want to make further modifications to xtic, ytic.
C**********************************************************----------!!

      WRITE (*,9003) xmax, xmin, xtic
      WRITE (*,9004) ymax, ymin, ytic

 9001 FORMAT (I1)

 9002 FORMAT (E13.6)

 9003 FORMAT ('xmax=',E13.6,'xmin=',E13.6,'xtic=',E13.6)

 9004 FORMAT ('ymax=',E13.6,'ymin=',E13.6,'ytic=',E13.6)

      RETURN
      END
