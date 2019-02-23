      PROGRAM ba
      INTEGER mode, start, size, no, nactual, fdes
      CHARACTER*80 fname
      CHARACTER*1 dataary(3072)
      INTEGER BAOPEN_RONLY, BAOPEN_WONLY, BACLOSE, BAREAD, BAWRITE  
      PARAMETER( BAOPEN_RONLY = 1)
      PARAMETER( BAOPEN_WONLY = 2)
      PARAMETER( BAOPEN_RW    = 4)
      PARAMETER( BACLOSE      = 8)
      PARAMETER( BAREAD      = 16)
      PARAMETER( BAWRITE     = 32)
      INTEGER jret, bacio 
  
      mode = BAOPEN_RW + BAREAD + BACLOSE
      WRITE (fname, 9001) "file1"
 9001 FORMAT (A20)

      start = 0
      size  = 1
      no    = 1024 * 3
      PRINT *,'fname = ',fname
      jret = bacio(mode, start, size, no, nactual, fdes, 
     1                fname, dataary)

      PRINT *,'jret, nactual = ',jret, nactual
    
      IF (jret .GE. 0 .AND. nactual .GE. 0) THEN 
        DO 1000 i = 1, 3072
CD          PRINT *,i,ICHAR(dataary(i))
 1000   CONTINUE
      ENDIF

      WRITE (fname, 9001) "file1.out"
      start = 0
      size = 1
      mode = BAOPEN_WONLY + BAWRITE + BACLOSE
      PRINT *,'fname = ',fname
      jret = bacio(mode, start, size, no, nactual, fdes, 
     1                fname, dataary)

      END
