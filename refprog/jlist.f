
      SUBROUTINE jlist(jou, nref, maxsz, npter)

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen =   24)
      PARAMETER (tlen   =  196)
      PARAMETER (notlen =   16)
      
C     Declare the main data structures.
      CHARACTER*24  jou(maxsz)
      INTEGER npter(maxsz), nref(maxsz)
      
C     Local and temporary variables.
      INTEGER nloc
      PARAMETER (nloc = 4000)
      CHARACTER*24 tjou(nloc)
      INTEGER njou(nloc)
      INTEGER sumref, numau, numjou, j, m 
      CHARACTER*60 fname
      INTEGER i, n, nmin
      LOGICAL yes, match

C***********************************************************   
C     Make list of cited journals:
      m = 1
      tjou(1) = jou(1)
      njou(1) = nref(1)
      DO 2000 i = 2, maxsz
        match = .FALSE.
        DO 2100 j = 1, m
          IF (jou(i) .EQ. tjou(j)) THEN
            njou(j) = njou(j) + nref(i)
            match = .TRUE.
          ENDIF
 2100   CONTINUE
        IF (.NOT. match) THEN
          m = m + 1
          tjou(m) = jou(i)
          njou(m) = nref(i)
        ENDIF
 2000 CONTINUE
      numjou = m 

      CALL tsort(tjou, njou, numjou)
      PRINT *,'File name for journals?'
      READ (*,9004) fname
      OPEN (13, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      WRITE (13, 9001) numjou
      DO 3100 i = 1, numjou
        WRITE (13, 9005) tjou(i), njou(i)
 3100 CONTINUE
      CLOSE (13, STATUS='KEEP')
C***********************************************************

 9001 FORMAT (I6)
 
 9004 FORMAT (A60)

 9005 FORMAT (2x,A24,I6) 

      RETURN
      END
