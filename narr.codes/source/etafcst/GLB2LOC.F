      SUBROUTINE GLB2LOC(IGLB,JGLB,ICOL,JROW,MYPE)
C----------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------
      JBASE=JM/JNPES
      JEXTRA=NINT(JNPES*(REAL(JM)/JNPES-JBASE)) 
C
      JBNDRY=JEXTRA*(JBASE+1)
      IF(JGLB.LE.JBNDRY)THEN
        JROWPE=JGLB/(JBASE+1)+1
        JROW=MOD(JGLB,JBASE+1)
        IF(JROW.EQ.0)THEN
          JROWPE=JROWPE-1
          JROW=JBASE+1
        ENDIF
      ELSE
        JDIF=JGLB-JBNDRY
        JROWPE=JEXTRA+JDIF/JBASE+1
        JROW=MOD(JDIF,JBASE)
        IF(JROW.EQ.0)THEN
          JROWPE=JROWPE-1
          JROW=JBASE
        ENDIF
      ENDIF
C***
      IBASE=IM/INPES
      IEXTRA=NINT(INPES*(REAL(IM)/INPES-IBASE))
C
      IBNDRY=IEXTRA*(IBASE+1)
      IF(IGLB.LE.IBNDRY)THEN
        ICOLPE=IGLB/(IBASE+1)+1
        ICOL=MOD(IGLB,IBASE+1)
        IF(ICOL.EQ.0)THEN
          ICOLPE=ICOLPE-1
          ICOL=IBASE+1
        ENDIF
      ELSE
        IDIF=IGLB-IBNDRY  
        ICOLPE=IEXTRA+IDIF/IBASE+1
        ICOL=MOD(IDIF,IBASE)
        IF(ICOL.EQ.0)THEN
          ICOLPE=ICOLPE-1
          ICOL=IBASE
        ENDIF
      ENDIF
C
      MYPE=(JROWPE-1)*INPES+ICOLPE-1
C
      WRITE(0,100)IGLB,JGLB
      WRITE(0,101)ICOL,JROW,MYPE
  100 FORMAT(' GLOBAL (',I3,',',I3,')')
  101 FORMAT('  LOCAL (',I3,',',I3,')',' ON PE',I3)

C
      RETURN
      END
