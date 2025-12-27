       SUBROUTINE EGVLVE(A,RR,RI,EVECTR,EVECTI,N,IVECT,LWORK,
     *                     AWORK,EVWORK,IERROR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    EGVLVE      COMPUTE EIGENVALUES OR ONE EIGENVECTOR.
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-09-08
C
C ABSTRACT: MATRIX MULTIPLY.  MUST HAVE NA=MB, MA=MC, NB=NC.
C
C PROGRAM HISTORY LOG:
C   88-09-08  PARRISH
C
C USAGE:    CALL EGVLVE(A,RR,RI,EVECTR,EVECTI,N,IVECT,LWORK,
C    *                     AWORK,EVWORK,IERROR)
C   INPUT ARGUMENT LIST:
C     A        - INPUT MATRIX (PRESERVED)
C     RR       - REAL PART OF EIGENVALUES
C     RI       - IMAG PART OF EIGENVALUES
C     N        - ORDER OF MATRIX A
C     IVECT    - =0, THEN OBTAIN EIGENVALUES,
C              - >0, THEN OBTAIN THE IVECT EIGENVECTOR
C
C   OUTPUT ARGUMENT LIST:
C     RR       - REAL PART OF EIGENVALUES
C     RI       - IMAG PART OF EIGENVALUES
C     EVECTR   - REAL PART OF EIGENVECTOR
C     EVECTI   - IMAG PART OF EIGENVECTOR
C     LWORK    - TEMP INTEGER STORAGE OF LENGTH 2*N
C     AWORK    - TEMP REAL STORAGE OF LENGTH N*N
C     EVWORK   - TEMP REAL STORAGE OF LENGTH 4*N
C     IERROR   - RETURNED WHEN COMPUTING EIGENVECTOR, =0 MEANS OK.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         REAL A(N,N)
         REAL RR(N),RI(N),EVECTR(N),EVECTI(N),AWORK(N,N)
         REAL EVWORK(N,4)
         INTEGER LWORK(N,2)
         NA=N
C--------
         IF(IVECT.GT.0) GO TO 500
C--------
C----------COMPUTE ALL EIGENVALUES.
C----------
           DO 100 J=1,N
             DO 90 I=1,NA
               AWORK(I,J)=A(I,J)
90           CONTINUE
100        CONTINUE
           CALL HSBG(N,AWORK,NA)
           CALL ATEIG(N,AWORK,RR,RI,LWORK,NA)
C----------
C----------ORDER EIGENVALUES FROM SMALLEST TO LARGEST IN ABS VALUE.
C----------
           DO 300 J=1,N
             DO 200 I=J,N
               ABSJ=RR(J)**2+RI(J)**2
               ABSI=RR(I)**2+RI(I)**2
               IF(ABSI.GE.ABSJ) GO TO 200
                 RRT=RR(J)
                 RR(J)=RR(I)
                 RR(I)=RRT
                 RIT=RI(J)
                 RI(J)=RI(I)
                 RI(I)=RIT
200          CONTINUE
300        CONTINUE
           RETURN
500      CONTINUE
         IERROR=1
C--------
C--------USE INVERSE ITERATION TO COMPUTE EIGENVECTOR CORRESPONDING
C--------TO EIGENVALUE (RR(IVECT)+I*RI(IVECT)).
C--------FIRST OBTAIN PERTURBED EIGENVALUE.
C--------
         EMAX2=0.
         DO 600 I=1,N
           EMAX2=AMAX1(EMAX2,RR(I)**2+RI(I)**2)
600      CONTINUE
         IP=MIN0(IVECT+1,N)
         IM=MAX0(1,IVECT-1)
         DELTA=AMAX1(1.E-18*EMAX2,AMIN1((RR(IP)-RR(IVECT))**2+(RI(IP)-
     *      RI(IVECT))**2,(RR(IM)-RR(IVECT))**2+(RI(IM)-RI(IVECT))**2))
         DELTA=SQRT(DELTA)*.001
         ALPHA=RR(IVECT)+DELTA
         BETA=RI(IVECT)+DELTA
         DO 700 I=1,N
           EVECTR(I)=1.
           EVECTI(I)=1.
700      CONTINUE
C----------COMPUTE C=(A-ALPHA*I)**2+BETA**2
           DO 1100 J=1,N
             DO 1000 I=1,N
               AWORK(I,J)=-2.*ALPHA*A(I,J)
               IF(I.EQ.J) AWORK(I,J)=AWORK(I,J)+ALPHA**2+BETA**2
               DO 900 K=1,N
                 AWORK(I,J)=AWORK(I,J)+A(I,K)*A(K,J)
900            CONTINUE
1000         CONTINUE
1100       CONTINUE
         CALL MINV(AWORK,N,DETA,LWORK,LWORK(1,2))
         DO 2000 ITER=1,5
C----------COMPUTE RIGHTHAND SIDE
           DO 710 I=1,N
             EVWORK(I,3)=-ALPHA*EVECTR(I)-BETA*EVECTI(I)
             EVWORK(I,4)=BETA*EVECTR(I)-ALPHA*EVECTI(I)
710        CONTINUE
           DO 800 I=1,N
             DO 750 J=1,N
               EVWORK(J,3)=EVWORK(J,3)+A(J,I)*EVECTR(I)
               EVWORK(J,4)=EVWORK(J,4)+A(J,I)*EVECTI(I)
750          CONTINUE
800        CONTINUE
C----------SOLVE FOR NEW ESTIMATE OF EIGENVECTOR
           DO 850 I=1,N
             EVWORK(I,1)=0.
             EVWORK(I,2)=0.
             DO 840 J=1,N
               EVWORK(I,1)=EVWORK(I,1)+AWORK(I,J)*EVWORK(J,3)
               EVWORK(I,2)=EVWORK(I,2)+AWORK(I,J)*EVWORK(J,4)
840          CONTINUE
850        CONTINUE
C----------NORMALIZE
           EVMAX=0.
           DO 1200 I=1,N
             EVMAX=AMAX1(EVMAX,ABS(EVWORK(I,1)),ABS(EVWORK(I,2)))
1200       CONTINUE
           DO 1300 I=1,N
             EVECTR(I)=EVWORK(I,1)/EVMAX
             EVECTI(I)=EVWORK(I,2)/EVMAX
1300       CONTINUE
2000     CONTINUE
         IERROR=0
       RETURN
       END
