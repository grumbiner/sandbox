      SUBROUTINE supmst(inter, pl1, pl2, pl3, pl4)
C     Subroutine to set up the map, via a call to SUPMAP.
C     By Bob Grumbine 7-22-86.

      INTEGER iproj, jlts, jgrid, iusout, idot, ier
      REAL polat, polong, rot, pl1, pl2, pl3, pl4
      INTEGER*2 color
      LOGICAL inter

      IF (inter)
     1  PRINT *,'Which type of projection do you want?'
      READ (*,9001) iproj

      IF (inter)
     1PRINT *,'How do you want the coordinate pairs you enter next to be
     1 interpreted    (2 is good).'
      READ (*,9001) jlts

      IF (inter)
     1  PRINT *,'Enter the lat., long. coords. of the first point.'
      READ (*,9002) pl1, pl2
      IF (inter)
     1PRINT *,'"                                  " second point.'
      READ (*,9002) pl3, pl4

      IF (inter)
     1PRINT *,'Enter the latitude and longitude of the reference point.'
      READ (*,9002) polat, polong

      IF (inter)
     1PRINT *,'What is the angular difference between the v axis and nor
     1th? (degrees)'
      READ (*,9002) rot

      IF (inter)
     1PRINT *,'At what degree interval do you want grid lines drawn?'
      IF (inter)
     1PRINT *,'  (This must be an integer.  If the number is <= 0, no li
     2nes will be     drawn.)'
      READ (*,9001) jgrid

      IF (inter)
     1PRINT *,'Do you want the lines dotted 1, or continuous 0?'
      READ (*,9001)  idot

      ier    = 0
      iusout = 4

      IF (inter)
     1PRINT *,'What color do you want the map drawn in?'
      READ (*,9001) color
      
      CALL jcolr(color)

C     Now call the mapping program.
      CALL supmap(iproj, polat, polong, rot, pl1, pl2, pl3, pl4, jlts,
     1  jgrid, iusout, idot, ier)

      IF (ier .NE. 0) PRINT *,'Error in supmap, results are suspect.'

9001  FORMAT (I5)

9002  FORMAT (2E15.7)

      RETURN
      END
