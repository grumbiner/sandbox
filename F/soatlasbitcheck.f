      PROGRAM bitcheck
C     Make some simple checks on whether it seems likely that bits have been 
C       switched.

      INTEGER*4 IBUF1(421)
      INTEGER*1 dumb(1688)
      CHARACTER*1 bytes(1688)
      INTEGER*4 i, recs
      EQUIVALENCE (dumb(1), IBUF1(1))
      EQUIVALENCE (dumb(1), bytes(1))
      INTEGER*4 hexs(2*1688)
      INTEGER*1 lohex, hihex
      INTEGER*4 hfreq(0:15), cfreq(0:255)
      

C     Open data file and an output file
      OPEN (10,FILE='statl.ebcidic.binary',FORM='UNFORMATTED'
     1                                    ,STATUS='OLD')
      OPEN (11,FILE='bitsout',FORM='FORMATTED',STATUS='NEW')
      
C     Get time in ticks on macintosh
      DO 100 i = 0, 15
       hfreq(i) = 0
  100 CONTINUE
      DO 200 i = 0, 255
        cfreq(i) = 0
  200 CONTINUE
      lohex = z0F
      hihex = zF0
      
      DO 1000 recs = 1, 6313
        READ (10) dumb
        DO 1100 i = 1, 1688
          hfreq(IAND(dumb(i),lohex))    = 
     1                       hfreq(IAND(dumb(i),lohex)) + 1
          hfreq(IAND(dumb(i),hihex)/16) = 
     1                       hfreq(IAND(dumb(i),hihex)/16) + 1
          cfreq(ICHAR(bytes(i)))        = 
     1                       cfreq(ICHAR(bytes(i))) + 1
 1100   CONTINUE
 1000 CONTINUE
      
      DO 2000 i = 0,15
        WRITE (*,9001) i,hfreq(i)
        WRITE (11,9001) i,hfreq(i) 
 2000 CONTINUE
      DO 2100 i = 0, 255
        WRITE (*,9001) i,cfreq(i)
        WRITE (11,9001) i,cfreq(i)
 2100 CONTINUE
 
 9001 FORMAT (I5,I13)
 
      PAUSE
      END