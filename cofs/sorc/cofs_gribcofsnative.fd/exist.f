      LOGICAL FUNCTION EXIST (FNAME)
      CHARACTER*80 FNAME 
      LOGICAL E
      INQUIRE (FILE=FNAME,EXIST=E)
      EXIST=E
      RETURN
      END
