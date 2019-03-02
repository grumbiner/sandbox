C***********************************************************----------!!
      SUBROUTINE axes(xmax, xmin, ymax, ymin, xtic, ytic) 

C     Subroutine to open the graphics, draw the axes and tics, and write
C       the title, and axis labels.

C     Declare the arguments.
      REAL xmax, xmin, ymax, ymin, xtic, ytic

C     Declare the local variables.
      CHARACTER*60 title
      CHARACTER*20 xlab, ylab
      INTEGER i, tic1, tic2
      REAL dx, fractx, dy, fracty

C***********************************************************----------!!
C     Section to draw the axes and tic marks.  

C     Set up the window.
C     Allow the user to specify the fraction of the window that the graph
C       will occupy.  6-25-86

      PRINT *,'What fraction of the screen do you want the plot to occup
     1y in the           horizontal?'
      READ (*,9002) fractx

      PRINT *,'What fraction of the screen do you want the plot to occup
     1y in the           vertical?'
      READ (*,9002) fracty

      dx = xmax-xmin
      dy = ymax-ymin

      CALL JWIND ( xmin - dx* .5* (-1. +1./fractx), 
     1             xmax + dx* .5* (-1. +1./fractx), 
     2             ymin - dy* .5* (-1. +1./fracty),
     3             ymax + dy* .5* (-1. +1./fracty) )

C     Draw the axes.
      CALL J2MOV ( xmin, AMAX1(0.0, ymin) )
      CALL J2DRW ( xmax, AMAX1(0.0, ymin) )

      CALL J2MOV ( AMAX1(0.0, xmin), ymin )
      CALL J2DRW ( AMAX1(0.0, xmin), ymax )

C     Put in the tic marks.
      tic1 = INT (xmin / xtic) 
      tic2 = INT (xmax / xtic) 
      DO 1000 i = tic1, tic2
        CALL J2MOV( xtic * i, AMAX1(0.,ymin) + ytic/ 6.0)
        CALL J2DRW( xtic * i, AMAX1(0.,ymin) )
 1000 CONTINUE

      tic1 = INT (ymin / ytic) 
      tic2 = INT (ymax / ytic) 
      DO 2000 i = tic1, tic2
        CALL J2MOV (AMAX1(0.,xmin) + xtic/6., ytic * i)
        CALL J2DRW (AMAX1(0.,xmin) , ytic * i)
 2000 CONTINUE


C***********************************************************----------!!
C     Label the axes.
      CALL JDFNT (1, 0.0, 1)
      CALL JFONT (1)
      CALL JCSIZ (.035*dx/fracty, 0.05*dy/fracty, 0.0)

C     Try to center the x label on the screen.  Put label below axis.
      PRINT *,'What do you want to label the x axis?'
      READ (*,9003) xlab

      CALL J2MOV ( xmax/6.0+4.*xmin/3., AMAX1(0.,ymin)-ytic)
      CALL JTEXH ( 20, xlab)

      PRINT *,'What do you want to label the y axis?'
      READ (*,9003) ylab

      CALL J2MOV ( AMAX1( 0.0+xtic/3.0, xmin+xtic),
     1                    ymax - 1.5*ytic)
      CALL JTEXH ( 20, ylab)

      PRINT *,'What do you want to title the graph?'
      READ (*,9004) title

      CALL J2MOV ( xmin+xtic, ymax - .5*ytic ) 
      CALL JTEXH ( 60, title)

C     Label the tic marks (?).

C     Make the picture current.
      CALL JMCUR

 9001 FORMAT (I1)

 9002 FORMAT (E15.7)

 9003 FORMAT (A20)

 9004 FORMAT (A60)

      RETURN
      END
