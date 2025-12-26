      SUBROUTINE WRTETA(TATHPTS,UATUPTS,VATUPTS,QATHPTS,
     &   PSATHPTS,ZSATHPTS,NUNIT)
C
      INCLUDE "parmlbc"
                             D I M E N S I O N
     & TATHPTS(KB,KMAX), QATHPTS(KB,KMAX),
     &     PSATHPTS(KB), ZSATHPTS(KB),
     & UATUPTS(KB,KMAX), VATUPTS(KB,KMAX)

       DIMENSION UB(KB,KMAX),VB(KB,KMAX),
     &   TB(KB,KMAX),QB(KB,KMAX),PB(KB),ZB(KB)
C
C   WRITE BOUNDARY DATA FOR PETABC 
C
       DO K = 1, KMAX
        DO L = 1, KB
          IF(K. EQ. 1) THEN
            PB(L) = PSATHPTS(L)
            ZB(L) = ZSATHPTS(L)
          ENDIF
          UB(L,K) = UATUPTS(L,K)
          VB(L,K) = VATUPTS(L,K)
          TB(L,K) = TATHPTS(L,K)
          QB(L,K) = QATHPTS(L,K)
        ENDDO
       ENDDO
C
      WRITE(NUNIT)TB,UB,VB,QB,PB,ZB
C
c     DO K = 1, KB
c      IF(K.EQ.1 .OR. MOD(K,20).EQ.0) THEN
c        WRITE(6,1001)K,PB(K),ZB(K)
c001     FORMAT(1x,'in wrteta ntimes= ',i4,2(1x,e12.5))
c        DO L=1,KMAX
c         WRITE(6,1002)L,TB(K,L),UB(K,L),VB(K,L),
c    &     QB(K,L)
c002      FORMAT(1x,i2,4(1x,e12.5))
c        ENDDO
c      ENDIF
c     ENDDO
C
      RETURN
      END
