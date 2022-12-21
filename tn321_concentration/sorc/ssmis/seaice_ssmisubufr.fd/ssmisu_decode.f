      PROGRAM ssmis
C ---------------------------------------------------------
c
c
C Program to decode the ssmi/su tank
      IMPLICIT none

      REAL bmiss
      PARAMETER (bmiss=10.0E6)
      CHARACTER*8 SUBSET
      REAL*8 hdr(9),ident(3), ssmischn(4,24)
C
      INTEGER ichan, jj, ii, isiid, ntank, lubfi, kt, jt
      INTEGER iorbn, islnm, isaid, idate, iret
      INTEGER mhr, mins, secs, mda, myr, mmo
      INTEGER icount, err, freq
      REAL xlat, xlon

      INTEGER openout, outunit, creturns, textout
C.................................
c
        CALL W3TAGB('SSMISU_Decode',0095,0333,0077,'NP11   ')
c
        CALL DATELEN(10)
c
        lubfi = 11
        icount = 0
        kt = 0
        ntank = 1
c
      freq = 10 * 1000 !Text output frequency
      textout = 52

      outunit = 51
      creturns = openout(outunit)
      PRINT *,'ssmisubufr creturned = ',creturns


      DO 500 JT = 1, ntank
C-----------------------------------------------------------------------
C                       DECODE BUFR FILE
C-----------------------------------------------------------------------
C
C  OPEN THE BUFR INPUT FILE
C  ------------------------
C
      CALL OPENBF(lubfi,'IN',lubfi)
      CALL READMG(lubfi,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 100
      PRINT*,'READING DATA FOR ',IDATE

C  READ A SUBSET - READ A MESSAGE WHEN NO SUBSETS - END WHEN NO MESSAGES
C  ---------------------------------------------------------------------
  150 CONTINUE
      CALL READSB(lubfi,IRET)
c          print*,' readsb iret ',iret
      IF(IRET.NE.0) THEN
         CALL READMG(lubfi,SUBSET,IDATE,IRET)
c          print*,' readms iret ',iret
         IF(IRET.NE.0) GOTO 100
         GOTO 150
      ENDIF
C
C  -----------------------------------------------------------------
C  CALL UFBINT, UFBREP TO GET THE DATA
C
C     MNEMONICS are embedded in CALLS
C
C  -----------------------------------------------
      !For testing:
      ! IF (icount .GT. 100*freq) GO TO 100
C
      call ufbint(lubfi,hdr,9,1,iret,
     *            'SAID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON')
c     WRITE(6,*) 'IRET= ',IRET
      call ufbint(lubfi,ident,3,1,iret,'SAID ORBN SLNM')
c
      isaid = NINT(ident(1))
      iorbn = NINT(ident(2))
      islnm = NINT(ident(3))
c
      isiid = NINT(hdr(1))
      myr   = NINT(hdr(2))
      mmo   = NINT(hdr(3))
      mda   = NINT(hdr(4))
      mhr   = NINT(hdr(5))
      mins  = NINT(hdr(6))
      secs  = NINT(hdr(7))
      xlat  = hdr(8)
      xlon  = hdr(9)
c
c    "SSMISCHN"24  =  24 btmps
C    each one is "CHNM  TMBR  WTCA   CTCA "
C      TCA are target calibrations, warm and cold
c
      call ufbseq(lubfi,ssmischn,4,24,iret,'SSMISCHN')
c
               icount = icount + 1
c
      if(mod(icount,freq) .eq. 0) then
c
        write(textout,50)isiid,myr,mmo,mda,mhr,mins,secs, xlat,
     *    xlon,isaid,islnm,iorbn
   50   format(1x,i3,1x,i4,1x,5i2,1x,2f12.5,1x,2i6,1x,i8)
c
        do ii = 1,4
         do jj = 1,24
          if(ssmischn(ii,jj).ge.bmiss)ssmischn(ii,jj)= -999.99
         enddo
        enddo
c
        do jj = 1, 24
          if(ssmischn(1,jj).le.-999.99) then
              ichan = -999
            else
             ichan = NINT(ssmischn(1,jj))
          endif 
          write(textout,502)ichan,ssmischn(2,jj)
        enddo
  502   format('chan # ',i4,' tmbr ',f8.2)
c

c
      endif

C Output in binary all steps' info:
          CALL ssmisout(hdr, ident, ssmischn)

C
c
  151             continue
C
c       get next report
c
      GO TO 150
  100 CONTINUE
c
         call   closbf(lubfi)
c
          if(icount.lt.1) then
           wRITE(6,*) 'READMG ERROR W/ BUFR MSG:IRET=',IRET
           err = 23
            call exit(23)
c          
           go to 99
          endif
C
c
        WRITE(6,*) 'AFTER WOOLENS INTERFACE:  icount= ',icount
c
      WRITE(6,617)
  617 FORMAT(1x,'  TIMES AND TOTAL NUMBER OF btmps OVER THE AREA')
CD     WRITE(6,618) IDAT1,IDAT2,icount
CD  618 FORMAT(2X,3I10)
C
  500 CONTINUE
C
C.................
      WRITE(6,692)icount
  692 FORMAT(1x,'END OF JOB','count= ',i8)
c
        CALL W3TAGE('DCODNET3')
C
   99 STOP
      END
