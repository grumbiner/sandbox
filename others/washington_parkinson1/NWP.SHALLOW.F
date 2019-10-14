      PROGRAM SHALLOW
C ***
C *** BENCHMARK WEATHER PREDICTION PROGRAM FOR COMPARING THE PERFORMANCE
C *** OF CURRENT SUPERCOMPUTERS. THE MODEL IS BASED ON THE PAPER, "THE
C *** DYNAMICS OF FINITE-DIFFERENCE MODELS OF THE SHALLOW-WATER
C *** EQUATIONS," BY R. SADOURNY, J. ATM. SCI., VOL.32, NO.4, APRIL 1975
C *** CODE BY PAUL N. SWARZTRAUBER, NATIONAL CENTER FOR ATMOSPHERIC
C *** RESEARCH, BOULDER, COLORADO   OCTOBER 1984.
C ***
      DIMENSION U(32,16),V(32,16),P(32,16),UNEW(32,16),VNEW(32,16),
     1          PNEW(32,16),UOLD(32,16),VOLD(32,16),POLD(32,16),
     2          CU(32,16),CV(32,16),Z(32,16),H(32,16),PSI(32,16)

      CHARACTER*1 LABEL(3)
      LABEL(1) = 'P'
      LABEL(2) = 'U'
      LABEL(3) = 'V'

      IMAX = 32
      JMAX = 16

      A = 1.E6
      
      DT  = 90.
      TDT = 90.
      TIME = 0.

      DX = 1.E5
      DY = 1.E5

      ALPHA  = .001
      ITMAX  = 120
      MPRINT = 120

      M = 31
      N = 15
      MP1 = M+1
      NP1 = N+1

      PI  = 4.*ATAN(1.)
      TPI = PI+PI

      DI = TPI/FLOAT(M)
      DJ = TPI/FLOAT(N)

C *** INITIAL VALUES OF THE STREAMFUNCTION
      DO 50 J = 1,NP1
      DO 50 I = 1,MP1
        PSI(I,J) = A*SIN((FLOAT(I)-.5)*DI)*SIN((FLOAT(J)-.5)*DJ)
   50 CONTINUE

C *** INITIALIZE VELOCITIES
      DO 60 J = 1,N
      DO 60 I = 1,M
        U(I+1,J) = -(PSI(I+1,J+1)-PSI(I+1,J))/DY
        V(I,J+1) = (PSI(I+1,J+1)-PSI(I,J+1))/DX
   60 CONTINUE

C *** PERIODIC CONTINUATION
      DO 70 J = 1,N
        U(1,J) = U(M+1,J)
        V(M+1,J+1) = V(1,J+1)
   70 CONTINUE

      DO 75 I = 1,M
        U(I+1,N+1) = U(I+1,1)
        V(I,1) = V(I,N+1)
   75 CONTINUE
      U(1,N+1) = U(M+1,1)
      V(M+1,1) = V(1,N+1)

      DO 86 J = 1,NP1
      DO 86 I = 1,MP1
        UOLD(I,J) = U(I,J)
        VOLD(I,J) = V(I,J)
        POLD(I,J) = 50000.
        P(I,J) = 50000.
   86 CONTINUE

C *** PRINT INITIAL VALUES
      WRITE(*,390) N,M,DX,DY,DT,ALPHA
  390 FORMAT('1NUMBER OF POINTS IN THE X DIRECTION',I8/
     1       ' NUMBER OF POINTS IN THE Y DIRECTION',I8/
     2       ' GRID SPACING IN THE X DIRECTION    ',F8.0/
     3       ' GRID SPACING IN THE Y DIRECTION    ',F8.0/
     4       ' TIME STEP                          ',F8.0/
     5       ' TIME FILTER PARAMETER              ',F8.3)
      MNMIN = MIN0(M,N)
      WRITE(*,391) (POLD(I,I),I = 1,MNMIN)
  391 FORMAT(/' INITIAL DIAGONAL ELEMENTS OF P ' //,(8E16.6))
      WRITE(*,392) (UOLD(I,I),I = 1,MNMIN)
  392 FORMAT(/' INITIAL DIAGONAL ELEMENTS OF U ' //,(8E16.6))
      WRITE(*,393) (VOLD(I,I),I = 1,MNMIN)
  393 FORMAT(/' INITIAL DIAGONAL ELEMENTS OF V ' //,(8E16.6))

      NCYCLE = 0
   90 NCYCLE = NCYCLE + 1

C *** COMPUTE CAPITAL U, CAPITAL V, Z, AND H
      FSDX = 4./DX
      FSDY = 4./DY
      DO 100 J = 1,N
      DO 100 I = 1,M
        CU(I+1,J)  = .5*(P(I+1,J)+P(I,J))*U(I+1,J)
        CV(I,J+1)  = .5*(P(I,J+1)+P(I,J))*V(I,J+1)
        Z(I+1,J+1) = (FSDX*(V(I+1,J+1)-V(I,J+1))-FSDY*(U(I+1,J+1)
     1          -U(I+1,J)))/(P(I,J)+P(I+1,J)+P(I+1,J+1)+P(I,J+1))
        H(I,J)     = P(I,J)+.25*(U(I+1,J)*U(I+1,J)+U(I,J)*U(I,J)
     1               +V(I,J+1)*V(I,J+1)+V(I,J)*V(I,J))
  100 CONTINUE

C *** PERIODIC CONTINUATION
      DO 110 J = 1,N
        CU(1,J)     = CU(M+1,J)
        CV(M+1,J+1) = CV(1,J+1)
        Z(1,J+1)    = Z(M+1,J+1)
        H(M+1,J)    = H(1,J)
  110   CONTINUE
      DO 115 I = 1,M
        CU(I+1,N+1) = CU(I+1,1)
        CV(I,1)     = CV(I,N+1)
        Z(I+1,1)    = Z(I+1,N+1)
        H(I,N+1)    = H(I,1)
  115 CONTINUE
      CU(1,N+1)  = CU(M+1,1)
      CV(M+1,1)  = CV(1,N+1)
      Z(1,1)     = Z(M+1,N+1)
      H(M+1,N+1) = H(1,1)
C *** COMPUTE NEW VALUES U, V, AND P
      TDTS8  = TDT/8.
      TDTSDX = TDT/DX
      TDTSDY = TDT/DY

      DO 200 J = 1,N
      DO 200 I = 1,M
        UNEW(I+1,J) = UOLD(I+1,J)+
     1    TDTS8*(Z(I+1,J+1)+Z(I+1,J))*(CV(I+1,J+1)+CV(I,J+1)+CV(I,J)
     2       +CV(I+1,J))-TDTSDX*(H(I+1,J)-H(I,J))
        VNEW(I,J+1) = VOLD(I,J+1)-TDTS8*(Z(I+1,J+1)+Z(I,J+1))
     1       *(CU(I+1,J+1)+CU(I,J+1)+CU(I,J)+CU(I+1,J))
     2       -TDTSDY*(H(I,J+1)-H(I,J))
        PNEW(I,J) = POLD(I,J)-TDTSDX*(CU(I+1,J)-CU(I,J))
     1       -TDTSDY*(CV(I,J+1)-CV(I,J))
  200 CONTINUE

C *** PERIODIC CONTINUATION
      DO 210 J = 1,N
        UNEW(1,J)     = UNEW(M+1,J)
        VNEW(M+1,J+1) = VNEW(1,J+1)
        PNEW(M+1,J)   = PNEW(1,J)
  210 CONTINUE
      DO 215 I = 1,M
        UNEW(I+1,N+1) = UNEW(I+1,1)
        VNEW(I,1)     = VNEW(I,N+1)
        PNEW(I,N+1)   = PNEW(I,1)
  215 CONTINUE
      UNEW(1,N+1)   = UNEW(M+1,1)
      VNEW(M+1,1)   = VNEW(1,N+1)
      PNEW(M+1,N+1) = PNEW(1,1)

      IF(NCYCLE .GT. ITMAX) WRITE (*,220)
  220 FORMAT('0   *****  END OF PROGRAM SHALLOW  *****')
      IF(NCYCLE .GT. ITMAX) STOP
      TIME = TIME + DT

      IF(MOD(NCYCLE,MPRINT) .NE. 0) GO TO 370
      PTIME = TIME/3600.
      WRITE(*,350) NCYCLE,PTIME
  350 FORMAT(//,' CYCLE NUMBER',I5,' MODEL TIME IN  HOURS',F6.2)
      WRITE(*,355) (PNEW(I,I),I = 1,MNMIN)
  355 FORMAT(/,' DIAGONAL ELEMENTS OF P ' //,(8E16.6))
      WRITE(*,360) (UNEW(I,I),I = 1,MNMIN)
  360 FORMAT(/,' DIAGONAL ELEMENTS OF U ' //,(8E16.6))
      WRITE(*,365) (VNEW(I,I),I = 1,MNMIN)
  365 FORMAT(/,' DIAGONAL ELEMENTS OF V ' //,(8E16.6))

      CALL PLOT (P,IMAX,JMAX,NCYCLE,LABEL(1))
      CALL PLOT (U,IMAX,JMAX,NCYCLE,LABEL(2))
      CALL PLOT (V,IMAX,JMAX,NCYCLE,LABEL(3))

  370 IF(NCYCLE .LE. 1) GO TO 310
      DO 300 J = 1,N
      DO 300 I = 1,M
        UOLD(I,J) = U(I,J)+ALPHA*(UNEW(I,J)-2.*U(I,J)+UOLD(I,J))
        VOLD(I,J) = V(I,J)+ALPHA*(VNEW(I,J)-2.*V(I,J)+VOLD(I,J))
        POLD(I,J) = P(I,J)+ALPHA*(PNEW(I,J)-2.*P(I,J)+POLD(I,J))
        U(I,J) = UNEW(I,J)
        V(I,J) = VNEW(I,J)
        P(I,J) = PNEW(I,J)
  300 CONTINUE
C *** PERIODIC CONTINUATION
      DO 320 J = 1,N
        UOLD(M+1,J) = UOLD(1,J)
        VOLD(M+1,J) = VOLD(1,J)
        POLD(M+1,J) = POLD(1,J)
        U(M+1,J) = U(1,J)
        V(M+1,J) = V(1,J)
        P(M+1,J) = P(1,J)
  320 CONTINUE
      DO 325 I = 1,M
        UOLD(I,N+1) = UOLD(I,1)
        VOLD(I,N+1) = VOLD(I,1)
        POLD(I,N+1) = POLD(I,1)
        U(I,N+1) = U(I,1)
        V(I,N+1) = V(I,1)
        P(I,N+1) = P(I,1)
  325 CONTINUE
      UOLD(M+1,N+1) = UOLD(1,1)
      VOLD(M+1,N+1) = VOLD(1,1)
      POLD(M+1,N+1) = POLD(1,1)
      U(M+1,N+1) = U(1,1)
      V(M+1,N+1) = V(1,1)
      P(M+1,N+1) = P(1,1)
      GO TO 90
  310 TDT = TDT+TDT
      DO 400 J = 1,NP1
      DO 400 I = 1,MP1
        UOLD(I,J) = U(I,J)
        VOLD(I,J) = V(I,J)
        POLD(I,J) = P(I,J)
        U(I,J) = UNEW(I,J)
        V(I,J) = VNEW(I,J)
        P(I,J) = PNEW(I,J)
  400 CONTINUE
      GO TO 90
      END
C
      SUBROUTINE PLOT (FIELD,IMAX,JMAX,NCYCLE,FTITLE)
C *** SUBROUTINE TO MAKE TWO-DIMENSIONAL PLOTS OF FIELDS
      DIMENSION FIELD (32,16)
      CHARACTER*1 FTITLE
      DO 1  II = 1,IMAX,8
        I1 = II
        I2 = I1+7
        WRITE (*,22) FTITLE,I1,I2,NCYCLE
   22   FORMAT ('0FIELD=',A1,'  I1=',I2,'  I2=',I2,'  NCYCLE=',I3)
        DO 2  J = 1,JMAX
          L = JMAX+1-J
    2   WRITE (*,21) (FIELD(I,L),I = I1,I2)
    1 CONTINUE
   21 FORMAT(1X,8E9.2)
      RETURN
      END
