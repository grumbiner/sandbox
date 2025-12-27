      PROGRAM alpha
      CHARACTER(len=255) :: ncpu
      INTEGER n
      CALL getenv("NCPU",ncpu)
      READ(ncpu,*) n
      PRINT *,'n = ',n
      END

