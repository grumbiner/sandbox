      PROGRAM a
      CHARACTER*5 text
      OPEN(10, FORM="FORMATTED")
      READ (10,*) text
      PRINT *,text
      END
