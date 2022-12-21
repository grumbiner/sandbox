      PROGRAM amsre
C ---------------------------------------------------------
C  Read amsre brightness temperature tanks and write out in form for the sea ice
C    concentration analysis
C
C  Original Code from Vera Gerald -- 2009
C  Original Implementation Robert Grumbine 5/2009
C
      IMPLICIT none

      REAL bmiss
      PARAMETER (bmiss=10.0E6)
      CHARACTER*8 SUBSET
      CHARACTER*80  HS1, HS2, HS3
      REAL*8 hdr(9),ident(3), amsri(4,20)
      REAL*8 posit(9,5)
C
      INTEGER ichan, jj, ii, isiid, ntank, lubfi, kt, jt
      INTEGER iorbn, islnm, isaid, idate, iret
      INTEGER mhr, mins, secs, mda, myr, mmo
      INTEGER icount, err, freq
      REAL xlat, xlon

      INTEGER openout, outunit, creturns
      INTEGER ix, iy
C.................................
c
        CALL W3TAGB('DCODNET3',0095,0333,0077,'NP11   ')
c
        CALL DATELEN(10)
c
        lubfi = 11
        icount = 0
        kt = 0
        ntank = 1
c
      freq = 10000 !Text output frequency
      outunit = 51
      OPEN (outunit, FORM="UNFORMATTED")
      creturns = openout(outunit)


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
C
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
!superceded by posit      call ufbint(lubfi,hdr,9,1,iret,
!superceded by posit     *            'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH')
c     WRITE(6,*) 'IRET= ',IRET
      call ufbint(lubfi,ident,3,1,iret,'SAID ORBN SLNM')
c
      isaid = NINT(ident(1))
      iorbn = NINT(ident(2))
      islnm = NINT(ident(3))
c
!superceded by posit      isiid = NINT(hdr(1))
!superceded by posit      myr = NINT(hdr(2))
!superceded by posit      mmo = NINT(hdr(3))
!superceded by posit      mda = NINT(hdr(4))
!superceded by posit      mhr = NINT(hdr(5))
!superceded by posit      mins = NINT(hdr(6))
!superceded by posit      secs = NINT(hdr(7))
!superceded by posit      xlat = hdr(8)
!superceded by posit      xlon = hdr(9)


      call ufbrep(lubfi,posit,9,5,iret,
     *    'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH')
c
      do ix = 1,9
       do iy = 1,5
        if(posit(ix,iy).eq.bmiss)posit(ix,iy)= -888.88
       enddo
      enddo

c
c    "AMSRCHAN"12  =  12 btmps
C    "AMSRDICE"4   X  "AMSRCHAN"2 = 8 btmps
c
c..Call ufbseq unpk AMSR-e "2" channel "4" spot sounding sequence(89 GHz/HORN-B) data
c 
c  Nmenomic string AMSRCHAN  = 'CHNM LOGRCW ACQF TMBR'
c
!changed by posit use      call ufbseq(lubfi,amsri,4,20,iret,'AMSRCHAN')
      call ufbrep(lubfi,amsri,4,20,iret,'CHNM LOGRCW ACQF TMBR')

c
               icount = icount + 1
c
      if (mod(icount,freq).eq.00) then
c
!superceded by posit       write(51,50)isiid,myr,mmo,mda,mhr,mins,secs, xlat,
!superceded by posit     *    xlon,isaid,islnm,iorbn
!superceded by posit   50   format(1x,i3,1x,i4,1x,5i2,1x,2f12.5,1x,2i6,1x,i8)
c
         do ii = 1,4
          do jj = 1,20
           if(amsri(ii,jj).ge.bmiss)amsri(ii,jj)= -999.99
          enddo
         enddo
c
          do jj = 1, 10
            if (amsri(1,jj).le.-999.99) then
                ichan = -999
              else
               ichan = NINT(amsri(1,jj))
            endif 
!          write(51,502)ichan,amsri(2,jj),amsri(4,jj)
          enddo
c
c c    index 17-20  =  89 GHz/HORN-B
c
          do jj = 17,20
            if (amsri(1,jj).le.-999.99) then
                ichan = -999
              else
               ichan = NINT(amsri(1,jj))
            endif 
!          write(51,502)ichan,amsri(2,jj),amsri(4,jj)
          enddo
  502     format('chan # ',i4,' logrcw ',f8.2,' btmp ',f8.2)
c
C Write out the new, additional information from posit:
          WRITE(51, 503) (posit(ix,1),ix=1,9)
          WRITE(51, 503) (posit(ix,4),ix=1,9)
          WRITE(51, 503) (posit(ix,5),ix=1,9)
  503     FORMAT("posit ",9F8.2)
        endif

C Output in binary all steps' info:
        CALL amsreout(posit, ident, amsri)

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
c     WRITE(6,618) IDAT1,IDAT2,icount
  618 FORMAT(2X,3I10)
C
  500 CONTINUE
C
C.................
      WRITE(6,692)icount
  692 FORMAT(1x,'END OF JOB','count= ',i8)
c
        CALL W3TAGE('DCODNET3')
C
   99  stop
      END
