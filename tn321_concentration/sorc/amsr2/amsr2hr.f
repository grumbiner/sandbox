cvmg
      PARAMETER       ( IUNT = 12 )
cvmg
      PARAMETER (BUFMAX=10.0E6)
      CHARACTER*8 SUBSET
      CHARACTER*80  HS1, HS2, HS3
cvng
      CHARACTER*(*)   FLT
      PARAMETER       ( FLT = 'bufr.tab' )
cvmg
      REAL HDR(9), xloc(2,1), PRDS(5,24)
      DATA HS1,HS2,HS3
     ./
     .'SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM               ',
     .'CLATH CLONH                                                ',
     .'SCCF ALFR VIIRSQ ANPO TMBR                                  '/
C
C.................................
c
c..vbg
             OPEN  ( UNIT = IUNT, FILE = FLT )
cvmg
        CALL W3TAGB('DCODAMSR2',0095,0333,0077,'NP11   ')
c
            icount = 0
c
co         iu = 12
               iu = 11
c
             kt = 0
          ntank = 1
c
         nn = 0
      TTIME = 0.0
c
      DO 500 JJ = 1, NTANK
C-----------------------------------------------------------------------
C                       DECODE BUFR FILE
C-----------------------------------------------------------------------
C
C  OPEN THE BUFR INPUT FILE
C  ------------------------
      LUBFI=IU
cvmg          CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL OPENBF(LUBFI,'IN',iunt)
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
      if(icount.le.500000)go to 250
      PRINT 1002
 1002 FORMAT(' ***** NUMBER OF OBS EXCEEDED 500000 *****')
      STOP 200
  250 CONTINUE
C  -----------------------------------------------------------------
C  CALL UFBINT, UFBREP TO GET THE DATA
c
c   SAID SIID YEAR MNTH DAYS HOUR MINU ORBN SLNM 
c   CLATH CLONH                                                ',
c   SCCF ALFR VIIRSQ ANPO TMBR  
C  -----------------------------------------------
      CALL UFBINT(LUBFI,HDR,9,  1,IRET,HS1)
      CALL UFBINT(LUBFI,XLOC,2,1,IRET,HS2)
      CALL UFBrep(LUBFI,PRDS,5,2,IRET,'SCCF ALFR VIIRSQ ANPO TMBR')
c     WRITE(6,*) 'IRET= ',IRET
c
      isad = NINT(hdr(1))
      issid = NINT(hdr(2))
      myr = NINT(hdr(3))
      mon = nint(hdr(4))
      mda = nint(hdr(5))
      mhr = nint(hdr(6))
      min = nint(hdr(7))
      iorbn = nint(hdr(8))
      iscanl = nint(hdr(9))

c
      IDATC = MYR*1000000 + NINT(hdr(3))*10000
     $         + nint(hdr(4))*100 + nint(hdr(5))
c            if(idatc.lt.idtgb.or.idatc.gt.idtge) go to 150
C
C.........       IS RETRIEVAL ON LAND
C
        do is = 1,1
            SLAT = xloc(1,is)
            SLON = xloc(2,is)
        enddo
c
        if(slon.lt.-180.0.or.slon.gt.180.0) go to 151
c
c   SCCF ALFR VIIRSQ ANPO TMBR
c
c        sccf = prds(1,is)
c        alfr = prds(2,is)
c        virsq = prds(3,is)
c        ANPO  = prds(4,is)
c
         btmp1 =  prds(5,1)
         btmp2 = prds(5,2)
c
c       if(btmp.gt.-3.00.and.btmp.lt.40.00)then
                 icount = icount + 1
c          print*,hdr
c          print*,xloc(1,1),xloc(2,1)
c          print*,prds(1,1),prds(2,1),prds(3,1),prds(4,1),prds(5,1)
c
           write(51,50)isad,myr,mon,mda,mhr,min,slat,
     *     slon,btmp1,btmp2,iorbn,iscanl
c
   50   format(1x,i3,1x,i4,3i2,i3,1x,4f11.5,2i6)
c          endif
c
  151             continue
c
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
        WRITE(6,*) 'AFTER WOOLENS INTERFACE:  icount= ',icount
c
      WRITE(6,617)
  617 FORMAT(1x,'  TIMES AND TOTAL NUMBER OF WINDS OVER THE AREA')
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
