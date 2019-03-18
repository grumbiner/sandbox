      SUBROUTINE parms(emode, ifmt, istyle, ostyle, fname, locat, 
     1                 xmin, xmax, ymin, ymax, xtic, ytic, 
     2                 title, xlab, ylab, size, npass)

C     This subroutine gets all the control parameters for the program.

      INTEGER emode, ifmt, istyle, ostyle, locat, size, npass
      REAL xmin, xmax, ymin, ymax, xtic, ytic
      CHARACTER*60 title
      CHARACTER*40 fname
      CHARACTER*20 xlab, ylab

      IF (emode .EQ. 2) THEN
C       Batch input
        READ (*,9001) ostyle 
        READ (*,9001) size 
        READ (*,9002) xmin 
        READ (*,9002) xmax 
        READ (*,9002) ymin
        READ (*,9002) ymax
        READ (*,9002) xtic
        READ (*,9002) ytic

       ELSE
C       Interactive input

        PRINT *,'How do you want the data plotted:'
        PRINT *,'  1 =     y vs.     x'
        PRINT *,'  2 = Log y vs.     x'
        PRINT *,'  3 =     y vs. Log x'
        PRINT *,'  4 = Log y vs. Log x'
        READ (*,9001) ostyle

        IF (npass .LT. 1) THEN
          PRINT *,'How many data points do you have?'
          READ (*,9001) size
        ENDIF

        PRINT *,'What is the minimum x value you want?  (If you want the
     1 computer to choose, set xmin=xmax.)'
        READ (*,9002) xmin

        PRINT *,'What is the maximum value of x you want plotted?'
        READ (*,9002) xmax

        PRINT *,'What is the minimum value of y you want plotted?'
        READ (*,9002) ymin

        PRINT *,'What is the maximum value of y you want plotted?'
        READ (*,9002) ymax

        PRINT *,'What tic mark interval do you want in the x direction?'  
        PRINT *,' (Enter 0 to have the computer choose.  It will try to 
     1 put in ten tic marks.)'
        READ (*,9002) xtic

        PRINT *,'What tic mark interval do you want in the y direction?'
        READ (*,9002) ytic

      ENDIF

 9001 FORMAT (I6)

 9002 FORMAT (E15.7)

C9003 FORMAT (A60)

C9004 FORMAT (A40)

C9005 FORMAT (A20)

      RETURN
      END      
