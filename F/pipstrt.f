      SUBROUTINE pipstrt(conc, unit, thickr)
C
C     Taken from PIPS model.  This will read in the PIPS restart file,
C       and modify the ice thickness field to suit the input concentration
C       file.
C     Bob Grumbine 15 June 1994.

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=46,NY=24,NX1=47,NY1=25) 
       DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),UICEC(NX,NY),
     *          VICEC(NX,NY),HEFF(NX1,NY1,3),AREA(NX1,NY1,3), 
     *          YNEG(NX1,NY1),TICE(NX1,NY1) 
      DIMENSION conc(nx1, ny1)
      INTEGER unit, i, j
C 
C 
C 
        READ(unit) UICE  
        READ(unit) VICE  
        READ(unit) UICEC 
        READ(unit) VICEC 
        READ(unit) HEFF  
        READ(unit) AREA  
        READ(unit) YNEG 
        READ(unit) TICE 

C     Algorithm is to set concentration in model to be that of the
C       observation.  If model has no ice, then add in 'thickr' 
C       thick ice.  If model has some ice, merely change concentration.
C       If model has ice, but obs don't, set concentration to zero.
      DO 1000 j = 1, ny1
        DO 1100 i = 1, nx1
          IF (area(i,j,2) .EQ. 0. .AND. conc(i,j) .NE. 0.) THEN
            heff(i,j,1) = thickr
            heff(i,j,2) = thickr
            heff(i,j,3) = thickr
           ELSE IF (area(i,j,2) .NE. 0. .AND. conc(i,j) .EQ. 0) THEN
            heff(i,j,1) = 0.0
            heff(i,j,2) = 0.0
            heff(i,j,3) = 0.0
           ELSE 
C           do nothing to the thickness
          ENDIF
          area(i,j,1) = conc(i,j)
          area(i,j,2) = conc(i,j)
          area(i,j,3) = conc(i,j)
 1100   CONTINUE
 1000 CONTINUE

      WRITE(unit+1) UICE  
      WRITE(unit+1) VICE  
      WRITE(unit+1) UICEC 
      WRITE(unit+1) VICEC 
      WRITE(unit+1) HEFF  
      WRITE(unit+1) AREA  
      WRITE(unit+1) YNEG 
      WRITE(unit+1) TICE 
 
      RETURN 
      END 
