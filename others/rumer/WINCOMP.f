      SUBROUTINE WINCOMP ( TT , DTX )
      INCLUDE "glgrid.inc"

      DIMENSION TAUAX(NX,NY) , TAUAY(NX,NY) , JO(NX,NY) , JOFN(NX,NY) ,
     *          WX(NX,NY) , WY(NX,NY) , WIN(3) , WAN(3) , TEMP(3) ,
     *          TTEMP(3) , PHI(3) , PSI(3) , VAX(3) , VAY(3) ,
     *          WXX(3) , WYY(3) , COMA(NX,NY) , COMB(NX,NY)
      COMMON / WINTEMP / COMA , COMB
      COMMON / WATER / WX , WY
      COMMON / WIND / WIN, WAN, TEMP
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / WINDEX / PRINTM, WINDINT, DISKINT, IRATIO, IW,
     *                  IPR, MELT
      DATA WX , WY / 3276 * 0. /
      DATA TAUAX , TAUAY / 3276 * 0. /
C
      FVAX ( A , B ) = A * COS ( B * PI / 180. )
      FVAY ( A , B ) = A * SIN ( B * PI / 180. )
      FTAUA ( A , B ) = C1 * ABS ( A ) * B
      FPHI ( A ) = - 0.012 * A + 1.
      FPSI ( A ) = 1.132 + 2.202 / A
      FOMEGA ( A, B, C, D, E, F ) = A * B + C * D + E * F
C
      IHOUR = TT / 3600
      IMIN = ( TT - IHOUR * 3600 ) / 60
      ISEC = 1
      WRITE ( 6 , 107 ) IHOUR, IMIN, ISEC
      PI = 4. * ATAN ( 1.)
      WINMAX = 0.0
      DO 1 I = 1 , 3
      WIN (I) = WIN (I) * 0.514
      WAN (I) = 246.5 - WAN (I)
      TTEMP (I) = TEMP (I) - 32.0
      WAN45 = WAN(I) - 45.
      IF ( TTEMP (I) .LE. 0. ) TTEMP (I) = 0.
      PHI (I) = FPHI (TTEMP(I))
      IF ( WIN (I) .NE. 0. ) GO TO 2
      PSI (I) = 0.
      GO TO 3
    2 PSI (I) = FPSI (WIN(I))
    3 WPHS = WIN (I) * PHI (I) * PSI (I)
      VAX (I) = FVAX ( WPHS , WAN(I) )
      VAY (I) = FVAY ( WPHS , WAN(I) )
      WXX (I) = FVAX ( WIN(I) , WAN45 )
      WYY (I) = FVAY ( WIN(I) , WAN45 )
      WINMAX = AMAX1 ( WINMAX , ABS(VAX(I)) , ABS(VAY(I)) )
    1 CONTINUE
      DO 100 I = 1 , N
      DO 100 J = 1 , M
      IF ( JO(I,J) .EQ. 0 ) GO TO 100
      TOLX = I + 3
      TOLY = J - 15
      CLEX = I - 24
      CLEY = J + 1
      BUFX = I - 79
      BUFY = J - 10
      RDISTOL = 1.0 / ( TOLX ** 2 + TOLY ** 2 )
      RDISCLE = 1.0 / ( CLEX ** 2 + CLEY ** 2 )
      RDISBUF = 1.0 / ( BUFX ** 2 + BUFY ** 2 )
      SUM = RDISTOL + RDISCLE + RDISBUF
      OMEGAT = RDISTOL / SUM
      OMEGAC = RDISCLE / SUM
      OMEGAB = RDISBUF / SUM
      TAUAX (I,J) =  FOMEGA ( VAX(1), OMEGAT, VAX(2), OMEGAC,
     *               VAX(3), OMEGAB )
      TAUAY (I,J) =  FOMEGA ( VAY(1), OMEGAT, VAY(2), OMEGAC,
     *               VAY(3), OMEGAB )
      WX (I,J) = 0.03 * FOMEGA ( WXX(1), OMEGAT, WXX(2), OMEGAC,
     *           WXX(3), OMEGAB )
      WY (I,J) = 0.03 * FOMEGA ( WYY(1), OMEGAT, WYY(2), OMEGAC,
     *           WYY(3), OMEGAB )
      COMA (I,J) = FOMEGA ( WIN(1), OMEGAT, WIN(2), OMEGAC, WIN(3),
     *          OMEGAB )
      COMB (I,J) = FOMEGA ( TEMP(1), OMEGAT, TEMP(2), OMEGAC,
     *          TEMP(3), OMEGAB )
  100 CONTINUE
      DTX = 60.
      IF ( MELT .EQ. 3HYES ) GO TO 4
      IF ( WINMAX .LE. 10.0 ) DTX = DTX + 60.
      IF ( WINMAX .LE.  7.0 ) DTX = DTX + 60.
      IF ( WINMAX .LE.  4.5 ) DTX = DTX + 60.
    4 CONTINUE
      IF ( DTX .GT. PRINTM ) PRINTM = DTX
      IF ( DTX .GT. WINDINT ) WINDINT = DTX
      IRATIO = DISKINT / PRINTM
      IF ( IRATIO .LT. 1 ) IRATIO = 1
      IW = WINDINT
      IPR = PRINTM
      WRITE ( 6 , 101 ) ( ( WIN (I) , WAN (I) ) , I = 1 , 3 )
      WRITE ( 6 , 102 ) ( TEMP (I) , I = 1 , 3 )
      WRITE ( 6 , 106 ) DTX
      WRITE ( 6 , 103 )
C     DO 200 I = 1 , N
C 200 WRITE ( 6 , 105 ) ( TAUAX(I,J) , J = 1 , M )
C     WRITE ( 6 , 104 )
C     DO 300 I = 1 , N
C 300 WRITE ( 6 , 105 ) ( TAUAY(I,J) , J = 1 , M )
      DO 400 I = 1 , N
      DO 400 J = 1 , M
      IF ( JO (I,J) .EQ. 0 ) GO TO 400
      TAUAX (I,J) = FTAUA ( COMA(I,J) , TAUAX(I,J) )
      TAUAY (I,J) = FTAUA ( COMA(I,J) , TAUAY(I,J) )
  400 CONTINUE
C
  101 FORMAT ( //,20X,'WIND AT TOLEDO    =',F5.1,1X,'M/SEC',2X,
     *         F6.1,1X,'DEG.',/,20X,'WIND AT CLEVELAND =',F5.1,
     *         1X,'M/SEC',2X,F6.1,1X,'DEG.',/,20X,'WIND AT ',
     *         'BUFFALO   =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.' )
  102 FORMAT (//,20X,'AIR TEMPERATURE AT TOLEDO    =',F5.1,1X,'DEG. F'
     *       ,/,20X,'AIR TEMPERATURE AT CLEVELAND =',F5.1,1X,'DEG. F'
     *       ,/,20X,'AIR TEMPERATURE AT BUFFALO   =',F5.1,1X,'DEG. F' )
  103 FORMAT ( //,10X,'WIND FIELD OVER LAKE ERIE  ( X-DIRECTION',
     *         ' COMPONENT )',/ )
  104 FORMAT ( //,10X,'WIND FIELD OVER LAKE ERIE  ( Y-DIRECTION',
     *         ' COMPONENT )',/ )
  105 FORMAT ( / , 5X , 21F5.1 )
  106 FORMAT ( //, 20X, 'DT = ', F6.0, 'SEC', // )
  107 FORMAT ( //, 20X, 'METEOROLOGICAL CONDITIONS AT ', I5, ' HOURS',
     *         I5, ' MINUTES', I5, ' SECONDS' )
C
      RETURN
      END
