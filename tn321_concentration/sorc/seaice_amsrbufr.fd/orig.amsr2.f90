PROGRAM amsr2lr
      IMPLICIT none
!From Vera Gerald program of 18 Dec 2013
!F90 and other modifications R. Grumbine
      
      INTEGER iunt
      PARAMETER ( iunt = 12 )

      CHARACTER(8) SUBSET
      CHARACTER(80)  HS1, HS2, HS3, Htmp

      CHARACTER(20)   FLT
      PARAMETER     ( FLT = 'bufr.tab' )

      REAL(8) HDR(9), XLOC(2,1), PRDLR(5,12), PRDHR(5,2)
      DATA HS1, HS2, HS3, Htmp &
    / 'SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM               ', &
      'CLATH CLONH                                                ', &
      'SCCF ALFR VIIRSQ ANPO TMBR                                 ', &
      'MTYP                                                       '/

      INTEGER icount, iu, ntank, is, outfreq
      REAL sccf, alfr, virsq, anpo, btmp
      DOUBLE PRECISION slon, slat
  
      INTEGER itk, lubfi, idate, iret, isad, issid, myr, mon, mda, mhr
      INTEGER minu, iorbn, iscanl, idatc
      INTEGER nchan, err

      INTEGER openout, outunit, creturns
      LOGICAL ok, hfok, lfok, weatherok
!.................................

      OPEN  ( UNIT = iunt, FILE = FLT )

      CALL W3TAGB('DCODAMSR2',0095,0333,0077,'NP11   ')
      
      outunit = 52
      OPEN (outunit, FORM="UNFORMATTED", STATUS="NEW")
      creturns = openout(outunit)

      icount = 0
      iu = 11
      ntank = 1

      DO ITK = 1, NTANK
!-----------------------------------------------------------------------
!                       DECODE BUFR FILE
!-----------------------------------------------------------------------
!  OPEN THE BUFR INPUT FILE
!  ------------------------
      LUBFI=IU

      CALL OPENBF(LUBFI,'IN',iunt)
      CALL READMG(LUBFI,SUBSET,IDATE,iret)
      IF(iret.NE.0) THEN
        CALL   closbf(lubfi)
        IF (icount.lt.1) then
          WRITE(6, *) 'READMG ERROR W/ BUFR MSG:iret=', iret
          err = 23
          STOP
!          CALL exit(23)
          go to 99
        ENDIF
      ENDIF

      PRINT*, 'READING DATA FOR ', IDATE

!  READ A SUBSET - READ A MESSAGE WHEN NO SUBSETS - END WHEN NO MESSAGES
!  ---------------------------------------------------------------------
  150 CONTINUE
      CALL READSB(LUBFI, iret)
      IF(iret.NE.0) THEN
         CALL READMG(LUBFI, SUBSET, IDATE, iret)
         IF(iret.NE.0) GOTO 100
         GOTO 150
      ENDIF
!
      IF (icount .GE. 5e8) THEN
        PRINT *, ' ***** NUMBER OF OBS EXCEEDED 5e8 *****'
        STOP 
      ENDIF

!  -----------------------------------------------------------------
!  CALL UFBINT, UFBREP TO GET THE DATA
!    SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM 
!    CLATH CLONH                                                ',
!    SCCF ALFR VIIRSQ ANPO TMBR  
!  -----------------------------------------------
      CALL UFBINT(LUBFI,HDR, 9,  1,iret, HS1)
      CALL UFBrep(LUBFI,XLOC,2,  1,iret, HS2)
      CALL UFBrep(LUBFI,PRDLR,5,12,iret, HS3)
      nchan = iret

      isad   = NINT(hdr(1))
      issid  = NINT(hdr(2))
      myr    = NINT(hdr(3))
      mon    = NINT(hdr(4))
      mda    = NINT(hdr(5))
      mhr    = NINT(hdr(6))
      minu   = NINT(hdr(7))
      iorbn  = NINT(hdr(8))
      iscanl = NINT(hdr(9))

      SLAT = XLOC(1,1)
      SLON = XLOC(2,1)

      if(slon.lt.-180.0.or.slon.gt.180.0) go to 151

          icount = icount + 1
! Add some checking for quality and being in latitudes suitable for sea
! ice.  31 July 2017 Robert Grumbine
          ok = .TRUE.
          IF (nchan .EQ. 2) THEN
            ok = hfok(PRDLR)
          ELSE
            ok = lfok(PRDLR)
          ENDIF 
          ok = ok .AND. (slat > 25.0 .OR. slat < -40.0) !
    
          IF (ok) THEN
            CALL headerout(isad,myr,mon,mda,mhr,minu,slat,slon, nchan)
            DO is = 1,nchan
               sccf  = PRDLR(1,is)
               alfr  = PRDLR(2,is)
               virsq = PRDLR(3,is)
               anpo  = PRDLR(4,is) 
               btmp  = PRDLR(5,is) 
               CALL spotout(sccf, alfr, virsq, anpo, btmp)
            ENDDO
          ENDIF

         IF (MOD(icount,INT(1e5)) .EQ. 0) THEN
           WRITE(51,*)isad,myr,mon,mda,mhr,minu,slat,slon, nchan
         ENDIF
  50     FORMAT(1x,i3,1x,i4,3i2,i3,1x,2f11.5,I3)
!         WRITE(52+icount/5000000) sccf/1.e9,alfr,virsq,anpo,btmp
!         IF (MOD(icount,INT(1e5)) .EQ. 0) THEN
!           WRITE(51,54) sccf/1.e9,alfr,virsq,anpo,btmp
!         ENDIF
   54 FORMAT(1x,F6.2,1x,4F13.4)

  151             continue

!       get next report
      GO TO 150

  100 CONTINUE
!
         CALL   closbf(lubfi)
         IF (icount.lt.1) then
           WRITE(6,*) 'READMG ERROR W/ BUFR MSG:iret=',iret
           err = 23
!           CALL exit(23)
           STOP
           go to 99
         endif
!
      WRITE(6,*) 'AFTER WOOLENS INTERFACE:  icount= ',icount
!
      ENDDO
!
!.................
      WRITE(6,692) icount
  692 FORMAT(1x,'END OF JOB','count= ',i8)

      CALL W3TAGE('DCODNET3')

   99 CONTINUE
END
