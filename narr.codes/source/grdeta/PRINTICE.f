C
      SUBROUTINE PRINTETA (SI,SM)
C
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE "parmeta.res"
C
      REAL           SI(IM,JM),   SM(IM,JM)
      INTEGER        SI2D(IM,JM), SM2D(IM,JM)
      INTEGER        SR
C
      DO J=1,JM
      DO I=1,IM
C                          CONVERT MTRS TO INCHES, CAP AT 9 FOR PRINT
      SINCH=SI(I,J)*100./2.54
      SR= NINT(SINCH)
c     if (sr.eq.2) print*,'i,j,si(i,j)=',i,j,si(i,j)
      SR = MIN (9,SR) 
      SI2D(I,J) = SR 
c     print*,'i,j,si2d(i,j)=',i,j,si2d(i,j)
      SM2D(I,J) = NINT(SM(I,J))
      IF (SM2D(I,J).EQ.0) SI2D(I,J)=MAX(1,MIN(8,SI2D(I,J)))
C   
      ENDDO
      ENDDO
C
      PRINT 50
   50 FORMAT(////' LAND-SEA MASK OF ETA GRID ( SEA/LAKE=1, LAND=0 )'/)
C
      DO 100, JJ=1,JM,4
      J = JM + 1 - JJ
      PRINT 70, J, (SM2D(I,J), I=1,IM,3)
   70 FORMAT(' ', ' J=', I3, 1X, 115I1)
  100 CONTINUE
C
      PRINT 75
   75 FORMAT(/' SNOW/ICE ON ETA GRID '/
     1' ',9X,'(ICE-FREE SEA=0, SNOW-FREE LAND=1, SNOW=2-8 IN., ICE=9)'/
     2' ','*** SNOWDEPTH CAPPED HERE AT 8 INCHES FOR DISPLAY ONLY ***'/)
C
      DO 200, JJ=1,JM,4
      J = JM + 1 - JJ
      PRINT 70, J, (SI2D(I,J), I=1,IM,3)
  200 CONTINUE
C
      RETURN
      END
