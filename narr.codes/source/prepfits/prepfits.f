C----------------------------------------------------------------------
C----------------------------------------------------------------------
      PROGRAM prepfits
C**********************************************************************
C	INPUTS:
C
C	UNIT		File description
C
C	 5	Records of model name  GRIB file / name  index file
C	11	LEVCAT namelist input file.  Example:
C                 &LEVCAT
C                 NUMLEV=39
C                 FIT=.T.,.T.,.F.,.F.,.T.,.T.,.T.,.F.,.F.,.F.
C                 &END
C	20	INPUT BUFR file created by editbufr
C	21	GRIB file for forecast
C	22	PREPFITS BUFR table file
C	23	GRIB index file for forecast
C	50	Output BUFR file with obs & forecast profiles
C***********************************************************************
C 
      INCLUDE 'parm.inc'
 
      REAL*8 hdr(10), cat(255), obs(10,255), qms(10,255)
      REAL*8 bak(10,255)
      REAL*8 vdata,vdate,bdate

      COMMON /observ/ hdr, cat, obs, qms, nlev
      COMMON /obstrs/ headr, obstr, qmstr, subset, idate, nsub
      COMMON /backgv/ bak, nbak
      COMMON /counts/ kntgsf

      CHARACTER*8 target
      COMMON /debug/ target, indux
      
      CHARACTER*80 headr, obstr, qmstr
      CHARACTER*128 file(mxb), fndx(mxb)
      DIMENSION mate(mxr)
      COMMON /guesfc/ psi(mxr,mxb), zsi(mxr,mxb), tsi(mxr,mxb),
     +            usi(mxr,mxb), vsi(mxr,mxb), qsi(mxr,mxb),
     +            pmi(mxr,mxb), cpi(mxr,mxb), cni(mxr,mxb),
     +            pxi(mxr,mxb)
      real*8 psi,zsi,tsi,usi,vsi,qsi,pmi,cpi,cni,pxi
      REAL*8 xyz(2,mxr)
      COMMON /guser/ xyz, nrep, ibak
      COMMON /vdates/ vdata, vdate(mxb), fhr(mxb), nofo(mxb)
 
      CHARACTER*8 subset, cnf(2,255), sslast
      CHARACTER*8 src(mxb)
      REAL*8 snf(2,255) 
      DIMENSION iges(mxb), irepv(mxb)
      EQUIVALENCE (cnf,snf)
      CHARACTER*8 staid
      EQUIVALENCE (hstid,staid)
      LOGICAL fit(0:9), valid, valix
      LOGICAL onlysf
      REAL Q(100),P(100),T(100),PINT(101)
      REAL q1(100),p1(100),t1(100)
 
c     DATA headr /'SID XOB YOB DHR ELV TYP T29 ITP           '/
c     DATA obstr /'POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI  '/
c     DATA qmstr /'PQM QQM TQM ZQM WQM                       '/
 
      DATA bmiss /10E10/
      NAMELIST /levcat/ numlev, fit
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      target = 'KEOD'
C
c     NAMELIST /levcat/ numlev, fit
C
      headr='SID XOB YOB DHR ELV TYP T29 ITP           '
      obstr='POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI  '
      qmstr='PQM QQM TQM ZQM WQM                       '

      numlev = 19
C     categories to fit
C     0 - surface
      fit(0) = .true.
C     1 - mandatory level
      fit(1) = .true.
C     2 - sig level T & moisture
      fit(2) = .false.
C     3 - winds by pressure
      fit(3) = .false.
C     4 - winds by height
      fit(4) = .false.
C     5 - tropopause
      fit(5) = .false.
C     6 - ANY single level
      fit(6) = .true.
C     7 - auxiliary
      fit(7) = .true.
C     8 & 9 - reserved
      fit(8) = .false.
      fit(9) = .false.

      call datelen(10)

      iread=11
      iwrite=6
      READ (iread,levcat,END=20)
      WRITE (iwrite,levcat)
 
      lubfi = 20
      luges = 21
      lundx = 22
      lugbi = 23
      lubfo = 50

      kntgsf = 0
 
      PRINT *
      PRINT *, '******  BEGINNING PREPFITS PROCESSING ******'
      PRINT *
 
C     GET THE DATE FROM A PREPBUFR FILE AND A LIST OF LAT/LON'S
C     SUBROUTINE UFBXY3 RETURNS A LIST OF UNIQUE LON/LAT/LOCATIONS
C     ------------------------------------------------------------
 
      CALL datebf(lubfi,iy,im,id,ih,idate)
      PRINT '(" DATA VALID AT ",I4,3I2.2)', iy, im, id, ih
C     COMPUTE VALID DATE OF DATA ONCE AND FOR ALL !!!!
      dhr = 0.0
      CALL raddate(idate,dhr,vdata)
      PRINT *, 'VDATA=', vdata
C     
      IF (idate.ge.0) THEN
        CALL ufbxy3(lubfi,xyz,mate,2,mxr,nrep)
	PRINT *, 'Debug index = ', indux
        PRINT *, 'MAKING FITS FOR ', nrep, ' REPORTS'
c       PRINT *, 'MATE:', (mate(n),N=1,nrep)
      ELSE
        GO TO 30
      END IF
 
C     READ A LIST OF INPUT BACKGROUND & INDEX FILES TO PROCESS
C     --------------------------------------------------------
 
      DO n = 1, mxb+1
        READ (5,'(A8,1X,A128)',END=10) src(n), file(n)
        READ (5,'(A8,1X,A128)',END=10) src(n), fndx(n)
      END DO
  
c     CALL abort('PREPFITS - TOO MANY BACKGROUND FIELDS')
      print*,'PREPFITS - TOO MANY BACKGROUND FIELDS'
      CALL abort
   10 nbak = n - 1
      PRINT *, 'NBAK=', nbak
 
C     MAKE SURE THE BACKGROUNDS ARE CHRONOLOGICAL AND TRANSFORM THEM
C     --------------------------------------------------------------
 
      DO ibak = 1, nbak
        CLOSE (luges)
        CLOSE (lugbi)
c       ishl = ishell('assign -a '//file(ibak)//' -s unblocked fort.21')
c       ishi = ishell('assign -a '//fndx(ibak)//' -s unblocked fort.23')
c       ishl = system('ln -s -f '//file(ibak)//'  fort.21')
c       ishi = system('ln -s -f '//fndx(ibak)//'  fort.23')
c       call system('ln -s -f '//file(ibak)//'  fort.21',ishl)
c       call system('ln -s -f '//fndx(ibak)//'  fort.23',ishi)
c       open(21,file=file(ibak),form='unformatted',iostat=ishl)
c       open(23,file=fndx(ibak),form='unformatted',iostat=ishi)
        call baopen(21,file(ibak),ishl)
        call baopen(23,fndx(ibak),ishi)
c       ishl=0
c       ishi=0
     
C       For each background, call GETBAK and then call GETPROF
C       To get a background profile at each ob location
C       -------------------------------------------------
        IF (ishl.eq.0.and.ishi.eq.0) THEN
          CALL getbak(luges,lugbi,numlev,vdate(ibak),fhr(ibak),
     +                iearth,iges(ibak))
          IF (iges(ibak).eq.0) CALL getprof(iearth)
        PRINT*,'DONE WITH IBAK=',IBAK,FILE(IBAK),IGES(IBAK)
        ELSE
          PRINT *
          PRINT *, 'MISSING: ', file(ibak)
          PRINT *
          iges(ibak) = -1
        END IF
C       
C       Here we should determine if time interpolation is desirable
C       BUT I'm WIRING it to NOT do ANY !!!!!
C       
        nofo(ibak) = 1
      END DO
 
C     OPEN THE INPUT FILE TO READ THROUGH
C     -----------------------------------

      CALL openbf(lubfi,'IN ',lubfi)
      irep = 0
      irepo = 0
      irepv = 0

C     OPEN THE OUTPUT FILE (POSITION MOD IF THE FILE EXISTS)
C     ------------------------------------------------------
 
      CALL datebf(lubfo,iy,im,id,ih,idate)
      IF (idate.ge.0) CALL openbf(lubfo,'APN',lubfo)
      IF (idate.lt.0) CALL openbf(lubfo,'OUT',lundx)


C     READ THROUGH THE PREPBUFR MESSAGES
C     ----------------------------------
 
      DO WHILE (ireadmg(lubfi,subset,idate).eq.0)
        nsub = nmsub(lubfi)
        PRINT *, 'SUBSET,NSUB,IDATE: ', subset, nsub, idate
 
C       READ A PREPBUFR REPORT
C       ----------------------
  
        sslast = subset
        DO WHILE (ireadpb(lubfi).eq.0)
          IF (subset.ne.sslast) THEN
            PRINT *, 'SUBSET CHANGED TO: ', subset, 
     +                  ' BY READPB AT IREP', irep
            PRINT *, 'SUBSET,NSUB,IDATE: ', subset, nsub, idate
          END IF
          irep = irep + 1
22        continue
          IF (hdr(2).ne.xyz(1,irep).or.hdr(3).ne.xyz(2,irep)) THEN
            irep=irep-1
c           PRINT *, 'IREP: ob vs xyz', irep, hdr(2), xyz(1,irep), 
c    +                  hdr(3), xyz(2,irep)
c           PRINT *, 'IREP-1 & IREP+1 xyz', xyz(1,irep-1), 
c    +                  xyz(2,irep-1), xyz(1,irep+1), xyz(2,irep+1)
c           GO TO 20
            go to 22
          END IF
          hstid = hdr(1)
C-------------------------------------------------------------
C         READPB (ABOVE) REPLACES THE FOLLOWING BLOCK FOR READING DATA
C-------------------------------------------------------------
C         DO WHILE(IREADSB(LUBFI).EQ.0)
C-------------------------------------------------------------
C         CALL UFBINT(LUBFI,HDR,10,  1,IRET,HEADR)
C         CALL UFBINT(LUBFI,CAT, 1,255,NLEV,'CAT')
C         CALL UFBINT(LUBFI,QMS,10,255,NLEV,QMSTR)
C         CALL UFBINT(LUBFI,OBS,10,255,NLEV,OBSTR)
C------------------------------------------------------------
C         READPB RETURN A COMBINED MASS/WIND/REPORT IN THE OB ARRAYS
C------------------------------------------------------------
c         IF(CAT(1).GT.7) THEN
c         PRINT*,'IREP,HDR: ',IREP,STAID,(HDR(I),I=2,8)
c         PRINT*,'NLEV,CAT: ',NLEV,(CAT(N),N=1,NLEV)
c         PRINT*,'NLEV=1,OBS: ',(OBS(I,1),I=1,6)
c         PRINT*,'NLEV=1,QMS: ',(QMS(I,1),I=1,5)
c         ENDIF
c         DHR = HDR(4)
c         ADATE = IDATE
c         CALL RADDATE(ADATE,DHR,VDATA)
C         PRINT*,'IREP,VDATA: ',IREP,VDATA

      ILEV=0
      do k=1,100
       p(k)=0.
       p1(k)=0.
       q(k)=0.
       q1(k)=0.
       t(k)=0.
       t1(k)=0.
      enddo
      slindx=0.
      cape=0.
      cin=0.
      peql=0.
      plcl=0.
      ifcape=0.
      do iii=8,10
        bak(iii,1)=bmiss
        obs(iii,1)=bmiss
      enddo
      if(subset(:6).eq.'ADPUPA') then
      DO K=1,100
      if(obs(2,k).ne.100000000000..and.
     *    obs(3,k).ne.100000000000.) then
        ilev=ilev+1
        P1(ilev)=OBS(1,k)*100.
        Q1(ilev)=OBS(2,k)/1000000.
        T1(ilev)=OBS(3,k)+273.15
      endif
      ENDDO
      do k=1,ilev
       p(k)=p1(ilev-k+1)
       q(k)=q1(ilev-k+1)
       t(k)=t1(ilev-k+1)
      enddo
      if(ilev.gt.1) then
c     print*,'Entering CALCAPE'
      CALL CALCAPE(T,Q,P,pint,ILEV,1,1,ILEV,
     *   CAPE,CIN,PLCL,PEQL,PLI)
c     print*,'irep=',irep
c     print*,'cpi(irep,1),cni(irep,1),pxi(irep,1)=',
c    * cpi(irep,1),cni(irep,1),pxi(irep,1)
c     if(cpi(irep,1).ne.100000000000..and.
c    *   cni(irep,1).ne.100000000000..and.
c    *   pxi(irep,1).ne.100000000000.) then
      if(cpi(irep,1).ne.99999997952.0000000.and.
     *   cni(irep,1).ne.99999997952.0000000.and.
     *   pxi(irep,1).ne.99999997952.0000000) then
      print*,'OB CAPE,CIN,SLINDX=',CAPE,CIN,PLI
      print*,'IREP=',IREP
c     print*,'HDR(2),HDR(3)=',HDR(2),HDR(3)
      print*,'MDL CAPE,CIN,SLINDX=',CPI(IREP,1),CNI(IREP,1),
     *    PXI(IREP,1)
      obs(8,1)=cape
      obs(9,1)=cin
      obs(10,1)=pli
      bak(8,1)=cpi(irep,1)
      bak(9,1)=cni(irep,1)
      bak(10,1)=pxi(irep,1)
      endif
      endif
      endif

C         FILTER THE DATA TO FIT BY CATEGORY
C         ----------------------------------
 
          mlev = 0
          DO l = 1, nlev
            kat = nint(cat(l))
            IF (kat.gt.9.or.kat.lt.0) kat = 9
            IF (fit(kat)) THEN
              mlev = mlev + 1
              cat(mlev) = cat(l)
              IF (kat.eq.6.and.(subset(:6).eq.'ADPSFC'.or.subset(:6).eq.
     +                    'SFCSHP')) cat(mlev) = 0
              DO i = 1, 10
                obs(i,mlev) = obs(i,l)
                qms(i,mlev) = qms(i,l)
              END DO
            END IF
          END DO
	  IF ( mlev .eq. 1 .and. subset (:6) .eq. 'ADPSFC' .or.
     +			         subset (:6) .eq. 'SFCSHP' ) THEN
	      mlev = 2
	      cat (mlev) = cat (1)
              DO i = 1, 10
                obs(i,mlev) = obs(i,1)
                qms(i,mlev) = qms(i,1)
              END DO
	      onlysf = .true.
	   ELSE
	      onlysf = .false.
	   END IF
	   
C----------------------------------------------------------------------
C         IF(MOD(IREP,350).EQ.0 .OR. MLEV.EQ.0) THEN
          IF (mlev.eq.0) THEN
            PRINT *, 'IREP,NLEV,MLEV: ', irep, nlev, mlev
            PRINT *, 'IRET,HDR: ', iret, staid, (hdr(i),I=2,8)
            PRINT *, 'NLEV,CAT: ', nlev, (cat(n),N=1,nlev)
            PRINT *, 'NLEV=1,OBS: ', (obs(i,1),I=1,7)
            PRINT *, 'NLEV=1,QMS: ', (qms(i,1),I=1,5)
          END IF
 
          nlev = mlev

C         MAKE SURE THERE IS DATA TO WRITE OUT
          IF (nlev.gt.0) THEN
            irepo = irepo + 1
C           MAKE SURE A MESSAGE OF THE PROPER TYPE AND DATE IS OPEN FOR OUTPUT
C           USING THE CURRENT SUBSET NAME (SSLAST) IN CASE IREADPB CHANGED IT
C           ------------------------------------------------------------------

            call datelen(10)
            CALL openmb(lubfo,sslast,idate)

C           WRITE OUT THE REGISTRATION DATA FOR EACH REPORT
C           -----------------------------------------------
 
            CALL ufbint(lubfo,hdr,10,1,iret,headr)
 
C           WRITE OUT THE REGISTRATION DATA FOR EACH LEVEL
C           ----------------------------------------------
 
            CALL ufbint(lubfo,cat,1,nlev,iret,'CAT')
            CALL ufbint(lubfo,obs,10,nlev,iret,'PRC')
            CALL ufbint(lubfo,qms,10,nlev,iret,qmstr)

C           WRITE THE OBSERVED VALUES FOR EACH LEVEL
C           ----------------------------------------
 
            DO l = 1, nlev
              cnf(1,l) = 'PRP'
              snf(2,l) = bmiss
            END DO
  
            if(subset(:6).ne.'ADPUPA') then
               obs(8,1)=bmiss
               obs(9,1)=bmiss
               obs(10,1)=bmiss
            endif

            CALL ufbint(lubfo,snf,2,nlev,iret,'SRC FHR')
            CALL ufbint(lubfo,obs,10,nlev,iret,obstr)

C           WRITE OUT THE INTERPOLATED BACKGROUND(S) INTO THE FIT FILE
C           ----------------------------------------------------------
            valix = .false.
            DO n = 1, nbak
              IF (n.lt.nbak.and.nofo(n).ne.1) THEN
                icdate = idint(vdate(n))
                CALL raddate(icdate,abs(fhr(n)-fhr(n+1)),bdate)
                valid = vdate(n) .le. vdata .and. vdate(n+1) .ge. vdata
     +                      .and. iges(n) .eq. 0 .and. iges(n+1) .eq. 0
     +                      .and. vdate(n+1) .eq. bdate .and. .not. 
     +                      valix
                valix = valid
              ELSE IF (n.eq.nbak.or.nofo(n).eq.1) THEN
                valid = vdate(n) .eq. vdata .and. iges(n) .eq. 0
              END IF
              IF (valid) THEN
                irepv(n) = irepv(n) + 1
                CALL getfct(irep,n,fhour,onlysf)
                if (subset(:6).ne.'ADPUPA') then
                  do iii=8,10
                    bak(iii,1)=bmiss
                  enddo
                endif
                DO l = 1, nlev
                  cnf(1,l) = src(n)
                  snf(2,l) = fhour
                END DO
                CALL ufbint(lubfo,snf,2,nlev,iret,'SRC FHR')
c               do j=1,50
c               if(bak(2,j).ne.bmiss) then
c               print*,'j,bak(2,j)=',j,bak(2,j)
c               endif
c               enddo
                CALL ufbint(lubfo,bak,10,nlev,iret,obstr)
              ELSE
                PRINT *, 'VALID IS FALSE!!  IREP,N,IGES(N)', irep, n, 
     +                      iges(n)
                PRINT *, 'VALID IS FALSE!!  VDATE(N),VDATA', vdate(n), 
     +                      vdata
              END IF
            END DO
 
C           BEFORE END OF READ LOOPS - WRITE THE PREPFITS SUBSET
C           ----------------------------------------------------

            CALL writsb(lubfo)
          END IF
          sslast = subset
 
C       END OF READ LOOPS
        END DO
      END DO
 
C     CLOSE THE BUFR FILES
C     --------------------
 
      CALL closbf(lubfi)
      CALL closbf(lubfo)
      PRINT *, '************************************'
      PRINT *, 'WROTE OUT FITS FOR ', irepo, ' REPORTS'
      PRINT *, '************************************'
      PRINT *, (irepv(i),I=1,nbak)
      PRINT *, '************************************'
      PRINT *, ' # of non-missing frcst reports for ONLYSF = ',
     +         kntgsf
      PRINT *, '************************************'
 
C     END OF PREPFITTING
C     ------------------
 
   20 CONTINUE
      STOP
c  30 CALL abort('PREPFITS - BAD OR MISSING PREPBUFR DATE')
   30 print*,'PREPFITS - BAD OR MISSING PREPBUFR DATE'
      CALL abort
      END
