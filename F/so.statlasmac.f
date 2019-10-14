      PROGRAM statlas
C     Try to read the southern ocean atlas data set.
C     Robert Grumbine 27 Sep 1995
 
      INTEGER levtot, reclen, NREC
      PARAMETER (levtot = 58)
      PARAMETER (reclen = 1688)
      PARAMETER (NREC   = 6313)
      INTEGER*4 INTGER, recs
      REAL D(levtot),T(levtot),S(levtot),O2(levtot),
     1  SIO3(levtot),PO4(levtot),ANO2(levtot)
	  REAL IBUF1(15)

      LOGICAL CHECK
      CHARACTER*60 fname
	  INTEGER*4 m, n, p
	  CHARACTER*4 alpha, beta, gamma
	  EQUIVALENCE (alpha, m)
	  EQUIVALENCE (beta, n)
	  EQUIVALENCE (gamma, p)

      ZIN2FL(INTGER)=FLOAT(INTGER)/1000.-200.
 
C     Open data file and an output file
      PRINT *,'What is the name of the input file?'
      READ (*,9009) fname
      OPEN (10,FILE=fname,FORM='FORMATTED',STATUS='OLD')
      PRINT *,'What is the name of the output file?'
      READ (*,9009) fname
      OPEN (11,FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9009 FORMAT (A60)

C     Get time in ticks on macintosh
CD      PRINT *,LONG(362)
	  
      DO 1000 recs = 1, NREC
	  
        CHECK = .TRUE.
		READ (10,9003) m, n, p
		WRITE (*,9006) alpha, beta, gamma
		WRITE (11,9006) alpha, beta, gamma
		READ (10,9004) (IBUF1(J),J=4,15)
		WRITE (11,9004) (IBUF1(J),J=4,15)

        DO 1100 K = 1, levtot
         READ (10,9005)  D(K),T(K),S(K),O2(K),
     1                    SIO3(K),PO4(K),ANO2(K)
         WRITE (11,9005) D(K),T(K),S(K),O2(K),
     1                    SIO3(K),PO4(K),ANO2(K)
 1100   CONTINUE
 
 1000 CONTINUE
 
CD      PRINT *,LONG(362)
 
 9003 FORMAT (3(I12,1x))
 9004 FORMAT (3F6.0,F6.2/,2F9.2,F8.0,F4.0,4F6.0)
 9005 FORMAT (7F9.3)
 9006 FORMAT (3(A4,1x))
 
      PAUSE
      END
