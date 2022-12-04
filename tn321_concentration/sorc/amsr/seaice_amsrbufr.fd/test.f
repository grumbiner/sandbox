C ---------------------------------------------------------
c
c
      PARAMETER (bmiss=10.0E6)
      CHARACTER*8 SUBSET
      CHARACTER*80  HS1, HS2, HS3
      REAL*8 HDR(8),amsrb(4,12),ident(3),flag(3),
     *       amsri(4,20)
C
C.................................
c
        CALL W3TAGB('DCODNET3',0095,0333,0077,'NP11   ')
c
        CALL DATELEN(10)
c
            icount = 0
c
         LUBFI = 11
             kt = 0
          ntank = 1
c
         nn = 0
      TTIME = 0.0
c
      DO 500 JT = 1, NTANK
C-----------------------------------------------------------------------
C                       DECODE BUFR FILE
C-----------------------------------------------------------------------
C
C  OPEN THE BUFR INPUT FILE
C  ------------------------
C
      CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL READMG(LUBFI,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 100
      PRINT*,'READING DATA FOR ',IDATE
C
C  READ A SUBSET - READ A MESSAGE WHEN NO SUBSETS - END WHEN NO MESSAGES
C  ---------------------------------------------------------------------
  150 CONTINUE
      CALL READSB(LUBFI,IRET)
c          print*,' readsb iret ',iret
      IF(IRET.NE.0) THEN
         CALL READMG(LUBFI,SUBSET,IDATE,IRET)
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
      call ufbint(lubfi,hdr,8,1,iret,
     *            'SIID YEAR MNTH DAYS HOUR MINU CLATH CLONH')
c     WRITE(6,*) 'IRET= ',IRET
      call ufbint(lubfi,ident,3,1,iret,'SAID ORBN SLNM')
c
      isaid = nint(ident(1))
      iorbn = nint(ident(2))
      islnm = nint(ident(3))
c
      isiid = NINT(hdr(1))
      MYR = nint(hdr(2))
      mmo = NINT(hdr(3))
      mda = NINT(hdr(4))
      mhr = nint(hdr(5))
      mins = nint(hdr(6))
      xlat = hdr(7)
      xlon = hdr(8)
c
      call ufbrep(lubfi,amsrb,4,12,iret,
     *            'CHNM LOGRCW ACQF TMBR')
c
        do ii = 1,4
         do jj = 1,12
        if(amsrb(ii,jj).ge.bmiss)amsrb(ii,jj)= -999.99
         enddo
        enddo
c
      call ufbseq(lubfi,amsri,4,20,iret,
     *      'AMSRCHAN')
c
               icount = icount + 1
c
      if(mod(icount,1000).eq.00)then
c
c         print*,'amsri',amsri
c
       write(51,50)isiid,myr,mmo,mda,mhr,mins,xlat,
     *    xlon,isaid,islnm,iorbn
   50   format(1x,i3,1x,i4,1x,4i2,1x,2f12.5,1x,2i6,1x,i8)
c
        do jj = 1,12
         write(51,501)amsrb(1,jj),amsrb(2,jj),amsrb(3,jj),amsrb(4,jj)
  501    format('chnm ',f4.0,1x,2f8.2,' tmbr ',f8.2)
        enddo
c
         do ii = 1,4
          do jj = 1,20
           if(amsri(ii,jj).ge.bmiss)amsri(ii,jj)= -999.99
          enddo
         enddo
c
          do jj = 1,20
          write(51,502)amsri(1,jj),amsri(2,jj),amsri(3,jj),amsri(4,jj)
  502     format('sitp ',f4.0,' tmbr ',3f8.2)
          enddo
c
          endif
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
