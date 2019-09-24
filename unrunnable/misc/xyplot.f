      PROGRAM xyplot

C     Author: Bob Grumbine
C     Date  : 5-9-86

C     This program is designed to be a flexible means of drawing x-y plots.
C     It should accept data in either formatted, or unformatted form, and
C       in xy pairs, x vector then y vector, or x and y in separated files.
C     The output may be selected as any of: x-y, Log x- y, x- Log y, Log-Log.
C     The program should accept the parameters in either a batch, or interactive
C       mode.
C     The output may be on the screen, the plotter, or both.

C     Option of scatter plot added 6-26-86  BG.
C     Spindle plot added 7-9-86 BG.
      IMPLICIT none

C     Declare the variables which are passed to and from other routines.
      INTEGER emode, size
      REAL xmin, xmax, ymin, ymax, xtic, ytic
      REAL x(1000), y(1000), xraw(1000), yraw(1000)

C     Get hold of the MacIntosh drawing routines
      INCLUDE HD.I40:MacFortran020:Include Files:quickdraw.inc
      INCLUDE HD.I40:MacFortran020:Include Files:window.inc
	INTEGER PTR
      PARAMETER (PTR=Z'C0000000')
      INTEGER toolbx
      INTEGER*4 grafptr, window
      INTEGER*2 grafport(54),rect(4)
	CHARACTER*256 str255
      
      INTEGER*4 view_window
      INTEGER*1 view_record(154)
	INTEGER*4 text_window
      INTEGER*1 text_record(154)
      
C     Locally used variables:
      INTEGER   gtype, i
      CHARACTER respon
      LOGICAL yes
      SAVE x, y, xraw, yraw
      
C     Variables and constants for increasing the stack size

	INTEGER*4 appllimit,newlimit,SETAPPLLIMIT	
	PARAMETER (appllimit=z'00000130')
	PARAMETER (SETAPPLLIMIT=Z'02D80008')
	
C     Begin the program.
C***********************************************************----------!!

      window = toolbx(FRONTWINDOW)
      CALL toolbx(CLOSEWINDOW,window)
	
      text_window = toolbx(PTR,text_record)
 	rect(1) =  40
 	rect(2) =   7
 	rect(3) = 475
 	rect(4) = 631
 	text_window = toolbx(NEWWINDOW,text_window,rect,str255("Text"),
     1		         .true.,0,-1,.true.,1)

 	CALL toolbx(SETPORT,text_window)
	CALL toolbx(TEXTFONT,4)
	CALL toolbx(TEXTSIZE,9)
  	PRINT *,' '
  	PRINT *,' setting text attributes'

 2000 CONTINUE
 1000 PRINT *, 'Do you want to run in interactive (1), or batch (2) mode
     1?'
      READ (*,9001) emode
      IF (emode .GT. 2 .OR. emode .LT. 1) GO TO 1000
 

C     If this is the first pass, read in the data.
      IF (emode .NE. 2) PRINT *,'Would you like to read in a(nother) dat
     1a file?'
      IF (yes(.FALSE.)) THEN
        IF (emode .NE. 2) PRINT *,'How many data points are there?'
        READ (*,9001) size
        CALL readin (xraw, yraw, size)
      ENDIF
      
C     Set up a dummy data file
CM      size = 150
CM	xraw(1) = 330.
CM	yraw(1) = 250.
CM	DO 9999 i = 2, size
CM	  xraw(i) = xraw(1) +
CM     1            FLOAT(i)*cos(5.*3.14159*FLOAT(i)/FLOAT(size-1))
CM	  yraw(i) = yraw(1) +
CM     1            FLOAT(i)*sin(5.*3.14159*FLOAT(i)/FLOAT(size-1))
CM	  PRINT *,xraw(i), yraw(i)
CM 9999 CONTINUE
 
C     Scatter plot option added 6-25-86.
 3000 IF (emode .NE. 2) PRINT *,'What type of graph do you want :'
      IF (emode .NE. 2) PRINT *,'     1  Line graph '
      IF (emode .NE. 2) PRINT *,'     2  Scatter plot '
      READ (*,9001) gtype

 	view_window = toolbx(PTR,view_record)
 	rect(1) =  40
 	rect(2) =   7
 	rect(3) = 475
 	rect(4) = 631
 	view_window = toolbx(NEWWINDOW,view_window,rect,str255("View"),
     1		         .true.,16,-1,.true.,0)
 	CALL toolbx(SETPORT,view_window)
	CALL toolbx(SHOWPEN)
	CALL toolbx(PENSIZE,2,2)
	CALL toolbx(TEXTFONT,4)
	CALL toolbx(TEXTSIZE,9)
      emode = 2

C     Compute data to be plotted, and graphics information.
      CALL calc (xmax, xmin, ymax, ymin, xtic, ytic, size,
     1           xraw, yraw, x, y)
      IF (emode .NE. 2) PRINT *,'Do you want to (re)draw the axes?' 
C      IF (yes(.FALSE.)) 
       CALL axes (xmax, xmin, ymax, ymin, xtic, ytic) 

C     Graph the data.
      IF (gtype .EQ. 1) THEN
        CALL graph (x, y, size)

       ELSE IF (gtype .EQ. 2) THEN 
        CALL scater (x, y, size)
       
       ELSE 
        CALL toolbx(DRAWSTRING,str255("unimplemented style"))

      ENDIF

C     Allow user to re-run. 
      IF (emode .NE. 2) PRINT *, 'Would you like to try again?'
C      READ (*,9002) respon
      respon = 'n'
      IF (respon .EQ. 'y') GOTO 2000

 9001 FORMAT (I6)

 9002 FORMAT (A1)
 
      PAUSE

      END
