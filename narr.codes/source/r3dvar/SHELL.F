C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SHELL       SHELL SORT BASED ON V.
C   PRGMMR: W. COLLINS       ORG: W/NMC22    DATE: 12-24-91
C
C ABSTRACT: SHELL SORT, BASED UPON THE VALUES OF V.
C   IV IS THE ORIGINAL INDEX OF EACH ELEMENT OF V.
C
C PROGRAM HISTORY LOG:
C   91-12-24  W. COLLINS
C
C USAGE:    CALL SHELL(V,IV,MAX)
C   INPUT ARGUMENT LIST:
C     V        - VARIABLE
C     MAX      - DIMENSION OF V
C     IREV     = 0 FOR ASCENDING ORDER
C             <> 0 FOR DESCENDING ORDER
C
C   OUTPUT ARGUMENT LIST:
C     IV       - ORIGINAL INDEX OF VARIABLE
C     V        - VARIABLE, SORTED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      SUBROUTINE SHELL(V,IV,MAX,IREV)
      REAL V(*)
      INTEGER IV(*)
      DO 10 I=1,MAX
        IV(I) = I
   10 CONTINUE
      IOFSET = MAX/2
   20 CONTINUE
      LIM = MAX - IOFSET
   30 CONTINUE
      ISW = 0
      DO 40 I=1,LIM
        IF(V(I).GT.V(I+IOFSET)) THEN
          VT = V(I)
          V(I) = V(I+IOFSET)
          V(I+IOFSET) = VT
          IVT = IV(I)
          IV(I) = IV(I+IOFSET)
          IV(I+IOFSET) = IVT
          ISW = I
        ENDIF
   40 CONTINUE
      LIM = ISW - IOFSET
      IF(ISW.NE.0) GO TO 30
      IOFSET = IOFSET/2
      IF(IOFSET.GT.0) GO TO 20

      IF(IREV.NE.0) THEN
C        REVERSE SORT ORDER...
         NH = MAX/2
         DO I=1,NH
            ITEMP = IV(I)
            IV(I) = IV(MAX+1-I)
            IV(MAX+1-I) = ITEMP
            TEMP = V(I)
            V(I) = V(MAX+1-I)
            V(MAX+1-I) = TEMP
         ENDDO
      ENDIF
      RETURN
      END

