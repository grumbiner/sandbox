      PROGRAM bufr_readsst
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Original program by Bert Katz X/X/2005
C  Modified 11 Dec 2008 by Robert Grumbine
C                                                                      C
C     IN THE SCRIPT THAT RUNS THIS PROGRAM, ONE MUST ISSUE THE         C
C     COMMANDS:                                                        C
C                                                                      C
C     export XLFRTEOPTS="unit_vars=yes"                                C
C     export XLFUNIT_11="NAME OF INPUT FILE (INCLUDING FULL PATH)"     C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !RG IMPLICIT NONE

      CHARACTER*80  IDST1,IDST2,IDCHN

      DATA IDST1  /'YEAR MNTH DAYS HOUR MINU SECO SSTYPE SSTSRC SAID '/
      DATA IDST2  /'CLAT CLON SST1 IREL IRMS SOZA SAZA   SOLAZI OPTH '/
      DATA IDCHN  /'INCN ALBD TMBR '/

      DATA BMISS  /10E10/

      REAL*8 XDATA(18),XCHAN(3,5)
      DOUBLE PRECISION dst(18), chan(3,5)
      CHARACTER*8  SUBSET
      INTEGER outunit, creturns

      DATA IUNT /11/

      outunit = 51
      creturns = openout(outunit)

      CALL OPENBF(IUNT,'IN',IUNT)

      CALL READMG(IUNT,SUBSET,IDATE,IERR)
      IF(IERR.NE.0) GO TO 8888

      IREC = 0

10    CONTINUE

      CALL READSB(IUNT,IERR)
      IF(IERR.NE.0) THEN
        CALL READMG(IUNT,SUBSET,IDATE,IERR)
        IF(IERR.NE.0) GO TO 8888
      ELSE
        CALL UFBINT(IUNT,XDATA( 1),9,1,IRET,IDST1)
        CALL UFBINT(IUNT,XDATA(10),9,1,IRET,IDST2)
        CALL UFBREP(IUNT,XCHAN    ,3,5,IRET,IDCHN)
        IREC = IREC + 1
        do lll = 1, 9
          dst(lll)   = XDATA(lll)
          dst(lll+9) = XDATA(lll+9)
        enddo
        chan = XCHAN

        !CALL avhrrout(idst1, idst2, idchn)
        CALL avhrrout(dst, chan)

      ENDIF
      GO TO 10

C  I/O ERROR EXIT
C  --------------

 8888 CONTINUE
      WRITE(6,1001) IREC
 1001 FORMAT(' EOF ENCOUNTERED IN SST INPUT FILE AFTER RECORD NO. ',I8)
      STOP

C  NORMAL EXIT
C  -----------

      END
