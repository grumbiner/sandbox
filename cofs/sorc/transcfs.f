      PROGRAM transcfs
C     translate cfs in to ascii for other machines to read
      INTEGER KB, IM, JM
      PARAMETER (KB = 19)
      PARAMETER (IM = 181)
      PARAMETER (JM = 101)
      REAL Z(KB), ZZ(KB), ALON(IM, JM), ALAT(IM, JM), H(IM, JM)

      CALL CFSREAD("inhts.cfs", Z, ZZ, ALON, ALAT, H)

      END

      
      SUBROUTINE CFSREAD(fname, Z, ZZ, ALON, ALAT, H)

      INTEGER KB, IM, JM
      PARAMETER (KB = 19)
      PARAMETER (IM = 181)
      PARAMETER (JM = 101)

      CHARACTER*60 fname
      INTEGER KBB
      INTEGER IMM, JMM

      REAL Z(KB), ZZ(KB), DZ(KB), DZZ(KB)
      REAL ALON(IM, JM), ALAT(IM, JM), DX(IM, JM), DY(IM, JM), H(IM, JM)

      OPEN (14, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
      READ (14) KBB, Z, ZZ, DZ, DZZ, IMM, JMM, ALON, ALAT, DX, DY, H 
      CLOSE (14)

      DO 1000 k = 1, KB
        WRITE (*,9001) k, Z(k)
 1000 CONTINUE
      DO 1001 k = 1, KB
        WRITE (*,9001) k, ZZ(k)
 1001 CONTINUE

 9001 FORMAT (I8, F10.5)

      DO 1100 j = 1, JM
        DO 1200 i = 1, IM
          WRITE (*,9002) i, j, ALON(i,j), ALAT(i,j), H(i,j)
 1200   CONTINUE
 1100 CONTINUE

 9002 FORMAT (I8, 1x, I8, 3(F8.3,1x) )
      
      RETURN
      END
