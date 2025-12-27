C
      SUBROUTINE PRINTNGM (SI)
C
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE "parmg104"
      INCLUDE 'parmodel'
C
      REAL           SI(200,200)
      INTEGER        SR,SINCH,SI2D(200,200)
C
      COMMON /CONSTS/ IH, IQ, IT, IU, IV, IZ,
     1                NLAT, LEVH, LEVS, LONF, NSIGSTP,
     2                SLAT( INLAT ), STRIPARA( INSIGSTP ,  INLAT )
C
      COMMON /CONSTN/ IMG( INGRDUSE ), JMG( INGRDUSE ),
     1                IAG( INGRDUS1 ), JAG( INGRDUS1 ),
     2                IBG( INGRDUS1 ), JBG( INGRDUS1 ),
     3                IADDRG( INIADDRS ,  INGRDUSE ),
     4                INDEXHU,  INDEXHV,  INDEXHTH, INDEXHQ,
     5                INDEXH,   INDEXST,  INDEXSQ,  INDEXALB,
     6                INDEXSLP, INDEXPSI, INDEXCD,  INDEXCC,  INDEXGSP,
     7                INDEXRAD, INDEXDLW, INDEXDSW, INDEXCPR, INDEXMA,
     8                INDEXSEC, INDEXZ0, INDEXTSS,  INDEXCG,  INDEXDHQ,
     9                INDEXTSD, KM, NH, NGRDUSE,
     1                SPECS( INSPECS ), DELSIG( IKM ),
     2                POVH( IKM ), PIOVHK( IKM ),
     3                CMUU  ( INGRDUSE ),
     4                XPOLEH( INGRDUSE ), YPOLEH( INGRDUSE ),
     5                XPOLEU( INGRDUSE ), YPOLEU( INGRDUSE ),
     6                XPOLEV( INGRDUSE ), YPOLEV( INGRDUSE ),
     7                DLAMNGM,
     8                BITSEA( IIJMAX ,  INGRDUSE ),
     9                BITSNO( IIJMAX ,  INGRDUSE ),
     1                BITWVL( IIJMAX ,  INGRDUSE )

C
      IM = IMG(2)
      JM = JMG(2)

      DO J=1,JM
        DO I=1,IM
          IF(SI(I,J).LE.100.) SINCH=0
          IF(SI(I,J).GT.101. AND. SI(I,J).LE.250.) SINCH=1
          IF(SI(I,J).GT.251. AND. SI(I,J).LE.500.) SINCH=2
          IF(SI(I,J).GT.501. AND. SI(I,J).LE.750.) SINCH=3
          IF(SI(I,J).GT.751. AND. SI(I,J).LE.1000.) SINCH=4
          IF(SI(I,J).GT.1001. AND. SI(I,J).LE.1250.) SINCH=4
          IF(SI(I,J).GT.1251. AND. SI(I,J).LE.1500.) SINCH=5
          IF(SI(I,J).GT.1501. AND. SI(I,J).LE.1750.) SINCH=6
          IF(SI(I,J).GT.1751. AND. SI(I,J).LE.2000.) SINCH=7
          IF(SI(I,J).GT.2001. AND. SI(I,J).LE.2500.) SINCH=8
          IF(SI(I,J).GT.2501. AND. SI(I,J).LE.3000.) SINCH=9
          IF(SI(I,J).GT.3001.) SINCH=0
          SR= SINCH
          SR = MIN (9,SR) 
          SI2D(I,J) = SR 
        ENDDO
      ENDDO
C
      PRINT 75
   75 FORMAT(/' TERRAIN HEIGHT ON NGM GRID '/)
C
      DO 200, JJ=1,JM
      J = JM + 1 - JJ
      PRINT 70, J, (SI2D(I,J), I=1,IM)
70    FORMAT(' ', ' J=',I3,1X,147I1)
  200 CONTINUE
C
      RETURN
      END
