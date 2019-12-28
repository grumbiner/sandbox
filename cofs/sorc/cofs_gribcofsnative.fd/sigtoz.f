C
      SUBROUTINE SIGTOZ(ZZ,H,T,TLEV,ZLEV,FSM,FSM1,IM,JM,KB)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SIGTOZ
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: THIS LINEARLY INTERPOLATES TLEV AT THE LEVEL, ZLEV, FROM
C   T LOCATED ON SIGMA LEVELS, ZZ. A NEW MASK ,FSM, IS CREATED.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL FIXDAT (IYEAR,IMONTH,IDAY,IHHP,IYYF,IMMF,IDDF,IHHF)
C   INPUT ARGUMENT LIST:
C     ZZ(KB) - vector of sigma values at which the field is given
C     H(IM,JM)    - bottom depth 
C     T(IM,JM,KB) - the field
C     ZLEV        - the required depth to interpolate to
C     IM,JM,KB    - array dimensions
C     FSM(IM,JM)  - model sea mask (common to all sigma surfaces)
C
C   OUTPUT ARGUMENT LIST:
C     TLEV(IM,JM) - target interpolated field
C     FSM1(IM,JM) - sea mask at a given depth
C
C   SUBPROGRAMS CALLED
C     UNIQUE:
C
C ATTRIBUTES:
C   LANGUAGE: IBM 370 VS FORTRAN
C   MACHINE:  NAS, CRAY C-90, IBM SP
C
C$$$
C
C-------------------------------------------------------------------
C     NOTE THAT A NEW MASK ,FSM, IS CREATED.
C     07/18/94: if zlev is greater (higher) than the uppermost
C               model level, extrapolation with a constant value
C               taken from the latter is performed.
C               Also, original argument FSM is replaced by FSM1,
C               and a new argument FSM is added - a purely input
C               array retaining its values; if initially FSM(i,j)=0 ,
C               then on output always FSM1(i,j)=0 .
C
C-------------------------------------------------------------------
      DIMENSION ZZ(KB),H(IM,JM),T(IM,JM,KB),TLEV(IM,JM),FSM(IM,JM),
     >FSM1(IM,JM)
      DO J=1,JM
      DO I=1,IM
        TLEV(I,J)=0.
        FSM1(I,J)=0.
      END DO
      END DO
      K=1
      DO 200 J=1,JM
      DO 200 I=1,IM
  100   CONTINUE
        IF(K.LT.1) then 
          TLEV(i,j)=T(i,j,1)
          FSM1(i,j)=1.
          K=1
C          WRITE (*,*) 'WEIRD'
          GO TO 200
        end if
        IF(K.GT.(KB-1)) THEN
          K=KB-2
C          WRITE (*,*) 'WEIRD'
          GO TO 200
        ENDIF
C
C  FIND K AND K+1 INTERVAL THAT BRACKETS ZLEV, THEN INTERPOLATE
C
        IF(ZLEV.GE.(ZZ(K)*H(I,J))) THEN
          IF(ZLEV.LE.(ZZ(K+1)*H(I,J))) THEN
            TLEV(I,J)=T(I,J,K)+(ZLEV-ZZ(K)*H(I,J))
     1      *(T(I,J,K+1)-T(I,J,K))/((ZZ(K+1)-ZZ(K))*H(I,J))
            FSM1(I,J)=1.
C            WRITE (*,*) 'OK, INTPOL BTWN ',ZZ(K)*H(I,J),ZZ(K+1)*H(I,J)
            GO TO 200
          ELSE
            K=K+1
            GO TO 100
          ENDIF
        ELSE
          K=K-1
          GO TO 100
        ENDIF
  200 CONTINUE
      FSM1=FSM*FSM1
      RETURN
      END
C
C------------ END OF SUBROUTINE SIGTOZ -----------------------------
