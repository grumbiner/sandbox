      SUBROUTINE aulist(au1, au2, au3, nref, maxsz, npter)
C     Robert Grumbine 15 Dec 1994

      IMPLICIT none
C     Declare the main data structures.
      INTEGER maxsz
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      INTEGER npter(maxsz), nref(maxsz)
 
C     Local and temporary variables.
      INTEGER maxtmp
      PARAMETER (maxtmp = 2400)
      CHARACTER*24 tau(maxtmp)
      INTEGER ntau(maxtmp)
      INTEGER sumref, numau, j, m 
      CHARACTER*60 fname
      INTEGER i, n
      LOGICAL yes, match

C***********************************************************
   
C     Make list of cited authors 
      j = 1
      tau(1)  = au1(1)
      ntau(1) = nref(1)
      DO 1000 i = 2, maxsz
        IF ( au1(i) .EQ. tau(j) ) THEN
          ntau(j) = ntau(j) + nref(i)
         ELSE
          j = j + 1
          ntau(j) = nref(i)
          tau(j)  = au1(i)
        ENDIF
 1000 CONTINUE
      numau = j

C     Now write out the information.
      CALL tsort(tau, ntau, numau)
      PRINT *,'File name for authors?'
      READ (*,9004) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      WRITE (12, 9001) numau
      sumref = 0
      DO 3000 i = 1, numau
        WRITE(12, 9005) tau(i), ntau(i)
CD      WRITE(* , 9005) tau(i), ntau(i)
        sumref = sumref + ntau(i)
 3000 CONTINUE
      WRITE (12,9001) numau, sumref
      CLOSE (12, STATUS='KEEP')

C************************************************************

 9001 FORMAT (I6)
 
 9004 FORMAT (A60)

 9005 FORMAT (2x,A24,I6) 

      RETURN
      END
