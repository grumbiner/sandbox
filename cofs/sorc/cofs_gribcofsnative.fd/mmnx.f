C
       SUBROUTINE MNMX (A,N,RN,RX,IN,IX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MNMX
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: CALCULATES MINIMUM AND MAXIMUM IN THE A(N) VECTOR
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL MNMX (A,N,RN,RX,IN,IX)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
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
C     CALCULATES MINIMUM AND MAXIMUM IN THE A(N) VECTOR      
      DIMENSION A(N)
      IX=1
      IN=1
      RN=A(1)
      RX=RN
      DO I=2,N
        IF (A(I).LT.RN) THEN
          RN=A(I)
          IN=I
        ENDIF  
        IF (A(I).GT.RX) THEN
          RX=A(I)
          IX=I
        ENDIF  
      ENDDO
      END
