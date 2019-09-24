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
      CHARACTER*8 ticlab
	CHARACTER*256 str255
      
C     Get hold of the MacIntosh drawing routines
      INCLUDE HD.I40:MacFortran020:Include Files:quickdraw.inc
      INTEGER toolbx

C     Section to draw the axes and tic marks.  

C     Set up the window.
C     Allow the user to specify the fraction of the window that the graph
C       will occupy.  6-25-86

CM      PRINT *,'What fraction of the screen do you want the plot to occup
CM     1y in the           horizontal?'
CM      READ (*,9002) fractx

CM      PRINT *,'What fraction of the screen do you want the plot to occup
CM     1y in the           vertical?'
CM      READ (*,9002) fracty

      fractx = 1.0
	fracty = 1.0
      dx = xmax-xmin
      dy = ymax-ymin

CH      CALL JWIND ( xmin - dx* .5* (-1. +1./fractx) - xtic, 
CH     1             xmax + dx* .5* (-1. +1./fractx), 
CH     2             ymin - dy* .5* (-1. +1./fracty),
CH     3             ymax + dy* .5* (-1. +1./fracty) )

C     Draw the axes.
CM	PRINT *,'Ready to draw the axes'
      CALL toolbx (MOVETO, INT(xmin), INT(AMAX1(0.0, ymin)) )
      CALL toolbx (LINETO, INT(xmax), INT(AMAX1(0.0, ymin)) )

      CALL toolbx (MOVETO, INT(AMAX1(0.0, xmin)), INT(ymin) )
      CALL toolbx (LINETO, INT(AMAX1(0.0, xmin)), INT(ymax) )

C     Put in the tic marks.
C     Tic mark labelling added 4-14-87. BG
      tic1 = INT (xmin / xtic)
      tic2 = INT (xmax / xtic)
	CALL toolbx(MOVETO, 60, 60)
CM	PRINT *,'Now putting in tic marks'
      DO 1000 i = tic1, tic2
        CALL toolbx(MOVETO, INT(xtic * i),
     1                      INT(AMAX1(0.,ymin) + ytic/ 6.0))
        CALL toolbx(LINETO, INT(xtic * i),
     1                      INT(AMAX1(0.,ymin) ))
        CALL toolbx(MOVETO, INT(xtic *(i-.75)),
     1                      INT(AMAX1(0.,ymin)-ytic/2.0))
        WRITE (ticlab, 9005) xtic*i
        CALL toolbx(DRAWSTRING,str255(ticlab))  
 1000 CONTINUE

      tic1 = INT (ymin / ytic) 
      tic2 = INT (ymax / ytic)
      DO 2000 i = tic1, tic2
        CALL toolbx(MOVETO, INT(AMAX1(0.,xmin) + xtic/6. ),
     1                      INT(ytic * i)                     )
        CALL toolbx(LINETO, INT(AMAX1(0.,xmin)           ),
     1                      INT(ytic * i)                     )
        CALL toolbx(MOVETO, INT(AMAX1(0.,xmin) - xtic*2.0),
     1                      INT(ytic*(i-.20))                 )
        WRITE (ticlab, 9005) ytic*i
        CALL toolbx(DRAWSTRING,str255(ticlab))
 2000 CONTINUE

C***********************************************************----------!!
C     Label the axes

C     Try to center the x label on the screen.  Put label below axis.
	CALL toolbx(MOVETO, 60, 60)
CM    PRINT *,'What do you want to label the x axis?'
CM      READ (*,9003) xlab
C      xlab='x axis              '

C      CALL toolbx(MOVETO, INT(xmax/6.0+4.*xmin/3.),
C     1                    INT(AMAX1(0.,ymin)-ytic*1.1) )
C      CALL toolbx(DRAWSTRING,str255(xlab))
	
C     	CALL toolbx(MOVETO, 40, 40)
CM	PRINT *,'What do you want to label the y axis?'
CM      READ (*,9003) ylab
C      ylab='y axis              '

C      CALL toolbx(MOVETO, INT(AMAX1( 0.0+xtic/3.0, xmin-xtic)),
C     1                    INT(ymax - 1.5*ytic)                 )
C      CALL toolbx(DRAWSTRING,str255(ylab))

C      CALL toolbx(MOVETO, 60, 60)
CM	PRINT *,'What do you want to title the graph?'
CM      READ (*,9004) title
C      title=
C     1'title                                                      '
C      CALL toolbx(MOVETO, INT(xmin+xtic), INT(ymax - .5*ytic) )
C      CALL toolbx(DRAWSTRING,str255(title))
      
 9001 FORMAT (I1)

 9002 FORMAT (E13.6)

 9003 FORMAT (A20)

 9004 FORMAT (A60)
 
 9005 FORMAT (F8.0)

      RETURN
      END
