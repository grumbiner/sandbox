      real a1(171*172),a2(171*172),av(171)
      open(11,file='sig1',form='unformatted',status='old')
      read(11)
      read(11)
      read(11)
      read(11) a1
      open(12,file='sig2',form='unformatted',status='old')
      read(12)
      read(12)
      read(12)
      read(12) a2
      call spvar(0,170,a1-a2,av)
      print '(f8.3)',8.e3*sqrt(sum(av))
      end
C-----------------------------------------------------------------------
      SUBROUTINE SPVAR(I,M,Q,QVAR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPVAR       COMPUTE VARIANCE BY TOTAL WAVENUMBER
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE VARIANCES BY TOTAL WAVENUMBER
C           OF A SCALAR FIELD IN SPECTRAL SPACE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPVAR(I,M,Q,QVAR)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     Q        - REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C
C   OUTPUT ARGUMENT LIST:
C     QVAR     - REAL (0:(I+1)*M) VARIANCES
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL Q((M+1)*((I+1)*M+2))
      REAL QVAR(0:(I+1)*M)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      L=0
      DO N=0,M
        KS=L*(2*M+(I-1)*(L-1))+2*N
        QVAR(N)=0.5*Q(KS+1)**2
      ENDDO
      DO N=M+1,(I+1)*M
        QVAR(N)=0.
      ENDDO
      DO N=0,(I+1)*M
        DO L=MAX(1,N-M),MIN(N,M)
          KS=L*(2*M+(I-1)*(L-1))+2*N
          QVAR(N)=QVAR(N)+Q(KS+1)**2+Q(KS+2)**2
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
