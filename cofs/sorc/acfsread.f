      SUBROUTINE ACFSREAD(fname, Z, ZZ, ALON, ALAT, H)

      INTEGER KB, IM, JM
      PARAMETER (KB = 19)
      PARAMETER (IM = 181)
      PARAMETER (JM = 101)

      CHARACTER*60 fname
      INTEGER KBB
      INTEGER IMM, JMM

      REAL Z(KB), ZZ(KB)
      REAL ALON(IM, JM), ALAT(IM, JM), H(IM, JM)
      INTEGER i, j, k
      INTEGER dum1, dum2

      OPEN (14, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1000 k = 1, KB
        READ (14,9001) i, Z(k)
 1000 CONTINUE
      DO 1001 k = 1, KB
        READ (14,9001) i, ZZ(k)
 1001 CONTINUE

 9001 FORMAT (I8, F10.5)

      DO 1100 j = 1, JM
        DO 1200 i = 1, IM
          READ (14,9002) dum1, dum2, ALON(i,j), ALAT(i,j), H(i,j)
 1200   CONTINUE
 1100 CONTINUE

 9002 FORMAT (I8, 1x, I8, 3(F8.3,1x) )
      
      CLOSE (14)

      RETURN
      END
