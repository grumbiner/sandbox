      SUBROUTINE PRINTAF ( SNODEP ) 
C     SUBROUTINE PRINTAF ( SNODEP, MSKAF ) 
C
      IMPLICIT REAL (A-H, O-Z)                      
C                                                     
      REAL          SNODEP(512,512) 
      COMMON /AFMSK/ MSKAF   (512,512)
      INTEGER       IPSNO(512)
C     INTEGER       MSKAF (512,512), IPSNO(512)
C                                                                       
      DATA   IAF/512/, JAF/512/
C
      JAFP1 = JAF + 1
C
      PRINT 50
   50 FORMAT(/' LAND-SEA MASK OF USAF GRID '/
     1    ' ',' ( SEA/LAKE=1, LAND=2, COASTAL-LAND=4, OFFWORLD=9 )'/)
C
      DO 100, JJ=1,JAF,8
      J = JAF + 1 - JJ
      PRINT 70, J, (MSKAF(I,J), I=69,IAF,6)
   70 FORMAT(' ', ' J=', I3, 1X, 74I1)
  100 CONTINUE
C
      PRINT 120
  120 FORMAT(/' SNOW/ICE ON USAF GRID'/
     1' (ICE-FREE SEA=0, SNOW-FREE LAND=1, SNOWDEPTH=2-8 IN., ICE=9 )'/
     2' ','*** SNOWDEPTH CAPPED HERE AT 8 INCHES FOR DISPLAY ONLY ***'/)
C
      DO 200, JJ=1,JAF,8
      J = JAF + 1 - JJ
      DO 150, I=1,IAF
      SR=SNODEP(I,J)*100./2.54
C CAP SEA-ICE AT 9 FOR PRINT PURPOSES
      IPSNO(I)=MIN(9,NINT(SR))
C CAP SNOWDEPTH AT 8 INCHES FOR PRINT PURPOSES
      IF (MSKAF(I,J) .GE. 2) IPSNO(I)=MAX(1,MIN(8,NINT(SR)))
C FORCE PRINT OF ASTERISK FOR OFF-WORLD
      IF (MSKAF(I,J) .EQ. 9) IPSNO(I)=10
  150 CONTINUE    
      PRINT 70, J, (IPSNO(I), I=69,IAF,6)
  200 CONTINUE
C
      RETURN
      END
