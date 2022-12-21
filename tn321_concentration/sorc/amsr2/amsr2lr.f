      PROGRAM amsr2lr
      IMPLICIT none
!From Vera Gerald program of 18 Dec 2013
!F90 and other modifications R. Grumbine
      
      INTEGER iunt
      PARAMETER ( iunt = 12 )

      CHARACTER*8 SUBSET
      CHARACTER*80  HS1, HS2, HS3, Htmp

      CHARACTER*(*)   FLT
      PARAMETER     ( FLT = 'bufr.tab' )

      REAL HDR(9), XLOC(2,1), PRDLR(5,12), PRDHR(5,2)
      DATA HS1, HS2, HS3, Htmp
     ./
     .'SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM               ',
     .'CLATH CLONH                                                ',
     .'SCCF ALFR VIIRSQ ANPO TMBR                                 ',
     .'MTYP                                                       '/
C
      INTEGER icount, iu, ntank, is
      REAL sccf, alfr, virsq, anpo, btmp
      REAL slon, slat
  
      INTEGER itk, lubfi, idate, iret, isad, issid, myr, mon, mda, mhr
      INTEGER minu, iorbn, iscanl, idatc
      INTEGER nchan, err

      INTEGER openout, outunit, creturns
C.................................

       OPEN  ( UNIT = iunt, FILE = FLT )

       CALL W3TAGB('DCODAMSR2',0095,0333,0077,'NP11   ')
      
      outunit = 52
      OPEN (outunit, FORM="UNFORMATTED", STATUS="NEW")
      creturns = openout(outunit)

      icount = 0
      iu = 11
      ntank = 1

      DO 500 ITK = 1, NTANK
C-----------------------------------------------------------------------
C                       DECODE BUFR FILE
C-----------------------------------------------------------------------
C  OPEN THE BUFR INPUT FILE
C  ------------------------
      LUBFI=IU

      CALL OPENBF(LUBFI,'IN',iunt)
      CALL READMG(LUBFI,SUBSET,IDATE,iret)
      IF(iret.NE.0) THEN
        CALL   closbf(lubfi)
        IF (icount.lt.1) then
          WRITE(6, *) 'READMG ERROR W/ BUFR MSG:iret=', iret
          err = 23
          CALL exit(23)
          go to 99
        ENDIF
      ENDIF
C
      PRINT*, 'READING DATA FOR ', IDATE
C
C  READ A SUBSET - READ A MESSAGE WHEN NO SUBSETS - END WHEN NO MESSAGES
C  ---------------------------------------------------------------------
  150 CONTINUE
      CALL READSB(LUBFI, iret)
      IF(iret.NE.0) THEN
         CALL READMG(LUBFI, SUBSET, IDATE, iret)
         IF(iret.NE.0) GOTO 100
         GOTO 150
      ENDIF
C
      if(icount .LE. 5e8) go to 250
      PRINT *, ' ***** NUMBER OF OBS EXCEEDED 5e8 *****'
      STOP 

  250 CONTINUE
C  -----------------------------------------------------------------
C  CALL UFBINT, UFBREP TO GET THE DATA
c
c   SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM 
c   CLATH CLONH                                                ',
c   SCCF ALFR VIIRSQ ANPO TMBR  
C  -----------------------------------------------
      CALL UFBINT(LUBFI,HDR, 9,  1,iret, HS1)
      CALL UFBrep(LUBFI,XLOC,2,  1,iret, HS2)
      CALL UFBrep(LUBFI,PRDLR,5,12,iret, HS3)
!      PRINT *,'iret hs3 = ',iret
      nchan = iret
c
      isad   = NINT(hdr(1))
      issid  = NINT(hdr(2))
      myr    = NINT(hdr(3))
      mon    = NINT(hdr(4))
      mda    = NINT(hdr(5))
      mhr    = NINT(hdr(6))
      minu   = NINT(hdr(7))
      iorbn  = NINT(hdr(8))
      iscanl = NINT(hdr(9))

C
        SLAT = XLOC(1,1)
        SLON = XLOC(2,1)

        if(slon.lt.-180.0.or.slon.gt.180.0) go to 151

           icount = icount + 1

!         WRITE(52+icount/10000000) isad,myr,mon,mda,mhr,minu,slat,slon, 
!     1                              nchan
         CALL headerout(isad,myr,mon,mda,mhr,minu,slat,slon, nchan)

         IF (MOD(icount,INT(1e5)) .EQ. 0) THEN
           WRITE(51,50)isad,myr,mon,mda,mhr,minu,slat,slon, nchan
         ENDIF
  50     FORMAT(1x,i3,1x,i4,3i2,i3,1x,2f11.5,I3)
C
C   SCCF ALFR VIIRSQ ANPO TMBR
C
      DO is = 1,nchan
         sccf  = prdlr(1,is)
         alfr  = prdlr(2,is)
         virsq = prdlr(3,is)
         anpo  = prdlr(4,is) 
         btmp  = prdlr(5,is) 
 
         CALL spotout(sccf, alfr, virsq, anpo, btmp)
!         WRITE(52+icount/5000000) sccf/1.e9,alfr,virsq,anpo,btmp
!         IF (MOD(icount,INT(1e5)) .EQ. 0) THEN
!           WRITE(51,54) sccf/1.e9,alfr,virsq,anpo,btmp
!         ENDIF
      ENDDO
   54 FORMAT(1x,F6.2,1x,4F13.4)

  151             continue

c       get next report
      GO TO 150

  100 CONTINUE
c
         CALL   closbf(lubfi)
         IF (icount.lt.1) then
           WRITE(6,*) 'READMG ERROR W/ BUFR MSG:iret=',iret
           err = 23
           CALL exit(23)
           go to 99
         endif
C
      WRITE(6,*) 'AFTER WOOLENS INTERFACE:  icount= ',icount
c
      WRITE(6,617)
  617 FORMAT(1x,'  TIMES AND TOTAL NUMBER OF WINDS OVER THE AREA')
C
  500 CONTINUE
C
C.................
      WRITE(6,692) icount
  692 FORMAT(1x,'END OF JOB','count= ',i8)

      CALL W3TAGE('DCODNET3')

   99 CONTINUE
      END
