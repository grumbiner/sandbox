      SUBROUTINE getbak(lugb,lugi,numlev,vdate,fhr,iearth,istat)
C                .      .    .                                       .
C SUBPROGRAM:    GETBAK      RETRIEVE Background Gridded FIELDS
C   PRGMMR: Rogers/DiMego    ORG: W/NP22     DATE: 97-06-23
C
C ABSTRACT: RETRIEVES surface & UPPER AIR Background Gridded FIELDS
C
C PROGRAM HISTORY LOG:
C   97-06-14  Geoff DiMego   Modified Morone's GETETA for PREPFITS
C   97-06-20  Eric Rogers    Modified and TESTED and Re-named GETBAK
C   99-02-25  Keith Brill    Modified to make elon1 & elonv consistently
C                            0 -> 360 degrees.
C   99-05-18  Perry Shafran  Added CAPE, CIN, and LI
C   02-07-12  Eric Rogers    Initialized masks to zero instead of one
C                            to ensure that data outside grid domain is
C                            not used
C
C USAGE:    CALL GETBAK( LUGB, LUGI, NUMLEV, IBAK, ISTAT)
C
C   INPUT FILES:
C     LUGB     - INTEGER unit for GRIB data file
C     LUGI     - INTEGER unit for GRIB Index file
C
C   INPUT ARGUMENT LIST:
C     NUMLEV   - INTEGER NUMBER OF desired LEVELS
C
C   OUTPUT ARGUMENT LIST:
C     VDATE    - VERIFYING DATE OF BACKGROUND FIELDS
C     FHR      - LENGTH OF FORECAST 
C     ISTAT    - INTEGER =0  MEANS SUCCESSFUL COMPLETION
C                        =-1 MEANS GRID COULD NOT BE RETURNED
C   OUTPUT VIA COMMON BLOCK /GRID /
C     COMMON /GRID /PGD(ILIM,JLIM,MAXLEV),
C    1 Z(ILIM,JLIM,MAXLEV), MASKZ(ILIM,JLIM,MAXLEV),
C    2 T(ILIM,JLIM,MAXLEV), MASKT(ILIM,JLIM,MAXLEV),
C    3 U(ILIM,JLIM,MAXLEV), MASKU(ILIM,JLIM,MAXLEV),
C    4 V(ILIM,JLIM,MAXLEV), MASKV(ILIM,JLIM,MAXLEV),
C    5 Q(ILIM,JLIM,MAXLEV), MASKQ(ILIM,JLIM,MAXLEV),
C    6 ALNQ(ILIM,JLIM,MAXLEV)
C
C             REAL & INTEGER ARRAYS dimensioned (ILIM,JLIM,MAXLEV) 
C             Containing 3-D fields on arrays of (IMAX,JMAX,NUMLEV)
C             And its associated MASKx INTEGER ARRAY(IMAX,JMAX,NUMLEV)
C             MASK=0 INDICATES A GRIDPOINT WITH MISSING DATA 
C                PRESENT CONTENTS:
C                PGD PRESSURE (PA) no associated mask
C                Z   GEOPOTENTIAL HEIGHT (M)
C                T   TEMPERATURE (K)
C                U   WIND U-COMPONENT (M/S)
C                V   WIND V-COMPONENT (M/S)
C                Q   Specific HUMIDITY (G/G)
C                ALNQ Natural logarithm of Q no associated mask
C
C   OUTPUT VIA COMMON BLOCK /SURFCE/
C
C     COMMON /SURFCE/ 
C    1 PS(ILIM,JLIM),MSKPS(ILIM,JLIM),ZS(ILIM,JLIM),MSKZS(ILIM,JLIM),
C    2 TS(ILIM,JLIM),MSKTS(ILIM,JLIM),QS(ILIM,JLIM),MSKQS(ILIM,JLIM), 
C    3 US(ILIM,JLIM),MSKUS(ILIM,JLIM),VS(ILIM,JLIM),MSKVS(ILIM,JLIM),
C    4 PM(ILIM,JLIM),MSKPM(ILIM,JLIM),
C    5 CAPE(ILIM,JLIM),MSKCP(ILIM,JLIM),
C    6 CIN(ILIM,JLIM),MSKCN(ILIM,JLIM),
C    7 PLI(ILIM,JLIM),MSKLI(ILIM,JLIM)
C
C                REAL & INTEGER ARRAYS dimensioned (ILIM,JLIM) 
C                Containing surface fields on arrays of (IMAX,JMAX)
C                And its associated MSKxx INTEGER ARRAY(IMAX,JMAX)
C                IF =0 INDICATES A GRIDPOINT WITH MISSING DATA 
C                PRESENT CONTENTS:
C                PS SURFACE PRESSURE (PA)
C                ZS SURFACE GEOPOTENTIAL HEIGHT (M)
C                TS SURFACE TEMPERATURE (K)
C                   (ACTUALLY TEMP AT 2 METERS)
C                US SURFACE WIND U-COMPONENT (M/S)
C                   (ACTUALLY U AT 10 METERS)
C                VS SURFACE WIND V-COMPONENT (M/S)
C                   (ACTUALLY V AT 10 METERS)
C                QS SURFACE Specific HUMIDITY (G/G)
C                   (ACTUALLY Q AT 2 METERS)
C                CAPE SURFACE based CAPE (J/kg)
C                CIN SURFACE based Conv. inhibition (J/kg)
C                PLI  SURFACE to 500-MB lifted index (K)
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE 'parm.inc'
      LOGICAL latlong, lambert, polarstereo
      COMMON /gridef/ imax, jmax, kmax, alat1, elon1, dxx, dyy, elonv, 
     +            alatan, latlong, lambert, polarstereo
C
      COMMON /grid/ pgd(ilim,jlim,maxlev), z(ilim,jlim,maxlev), 
     +            maskz(ilim,jlim,maxlev), t(ilim,jlim,maxlev), 
     +            maskt(ilim,jlim,maxlev), u(ilim,jlim,maxlev), 
     +            masku(ilim,jlim,maxlev), v(ilim,jlim,maxlev), 
     +            maskv(ilim,jlim,maxlev), q(ilim,jlim,maxlev), 
     +            maskq(ilim,jlim,maxlev), alnq(ilim,jlim,maxlev)
C
      COMMON /surfce/ ps(ilim,jlim), mskps(ilim,jlim), zs(ilim,jlim), 
     +            mskzs(ilim,jlim), ts(ilim,jlim), mskts(ilim,jlim), 
     +            qs(ilim,jlim), mskqs(ilim,jlim), us(ilim,jlim), 
     +            mskus(ilim,jlim), vs(ilim,jlim), mskvs(ilim,jlim), 
     +            pm(ilim,jlim), mskpm(ilim,jlim), cape(ilim,jlim),
     +            mskcp(ilim,jlim), cin(ilim,jlim), mskcn(ilim,jlim),
     +            pli(ilim,jlim), mskli(ilim,jlim)
c     real*8 z,t,u,v,q,alnq
C
      DIMENSION grid(itot)
      DIMENSION RICDAT(5),IFDATE(8),IVDATE(8)
      INTEGER jpds(200), jgds(200), kpds(200), kgds(200)
c      INTEGER levs(40,6), ivar(6)
      INTEGER levs(40,7), ivar(6)
      CHARACTER*8 modesc(110)
      LOGICAL*1 mask(itot)
C
      DATA levs /1000, 975, 950, 925, 900, 875, 850, 825, 800, 775, 750,
     +            725, 700, 675, 650, 625, 600, 575, 550, 525, 500, 475,
     +            450, 425, 400, 375, 350, 325, 300, 275, 250, 225, 200,
     +            175, 150, 125, 100, 75, 50, 25, 1000, 975, 950, 925, 
     +            900, 875, 850, 825, 800, 775, 750, 725, 700, 675, 650,
     +            625, 600, 575, 550, 525, 500, 475, 450, 17 * 0, 1000,
     +            950, 900, 850, 800, 750, 700, 650, 600, 550, 500, 450,
     +            400, 350, 300, 250, 200, 150, 100, 50, 20 * 0, 1000, 
     +            925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 70,
     +            50, 27 * 0, 1000, 850, 700, 500, 400, 300, 250, 200, 
     +            150, 100, 30 * 0, 1000, 850, 700, 500, 400, 300, 250,
     +            200, 32 * 0, 1000, 975, 950, 925, 900, 875, 850, 825, 
     *            800, 775, 750, 725, 700, 650, 600, 550, 500, 450, 400, 
     *            350, 300, 275, 250, 225, 200, 175, 150, 125, 100, 
     *            11*0/
C
      DATA ivar /7, 11, 33, 34, 52, 51/
C
      DATA modesc /38 * 'xxxxxxxx', '80KM NGM', 37 * 'xxxxxxxx', 
     +            'T126 AVN', 'T126 MRF', 'xxxxxxxx', 'T62 MRF ', 2 * 
     +            'xxxxxxxx', '80KM ETA', '32KM ETA', '29KM ETA', 
     +            '60KM RUC', 2 * 'xxxxxxxx', '48KM ETA', 15 * 
     +            'xxxxxxxx', '40KM RUC', 4 * 'xxxxxxxx', '10KM ETA'/
C
      CHARACTER cbuf(mbuf)
      LOGICAL*1 lb(jf)
      REAL f(jf)
      REAL*8 vdate
      INTEGER jens(200), kens(200)

      istat = 0
C     
C     READ INDEX FILE TO GET GRID SPECS 
C     
      irgi = 1
      irgs = 1
      kmax = 0
      jr = 0
      kskip = 0
      CALL getgi(lugi,kskip,mbuf,cbuf,nlen,nnum,irgi)
      REWIND lugi
C     
C     NNUM IS THE NUMBER OF GRIB MESSAGES IN THE FILE. READ THE
C     FIRST PDS ONLY TO GET THE GRID NUMBER AND DATE
C     
C     NOTE: SINCE NGM HAS MANY GRIDS COMBINED INTO ONE GRIB FILE
C     SEARCH THROUGH THE INDEX FILE UNTIL WE GET TO A GRID 104
C     RECORD, THEN JUMP OUT OF LOOP AND SAVE GRID 104 INFO
C     IN COMMON BLOCK
C     
      DO k = 1, nnum
        jr = k - 1
        jpds = -1
        jgds = -1
        CALL getgb1s(cbuf,nlen,nnum,jr,jpds,jgds,jens,kr,kpds,kgds,kens,
     +              lskip,lgrib,irgs)
C       
C       USE  F I R S T  PDS TO GET START DATE & FORECAST LENGTH
C       
        IF (k.eq.1) THEN
          iys = kpds(8)
          fhr = kpds(14)
          ifdate(2) = kpds(9)
          ifdate(3) = kpds(10)
          ifdate(5) = kpds(11)
          ricdat(2) = kpds(14)
          icent = kpds(21)
          if(ifdate(2).lt.100) then
            ifdate(1) = (icent-1)*100 + iys
          else
            ifdate(1) = icent * 100
          endif
        END IF
C       IF  N O T  NGM, THEN THIS PDS & GDS ARE FINE
        IF (kpds(2).ne.39) THEN
          WRITE (6,1000) (kgds(ker),ker = 1,14)
 1000     FORMAT (1X,14I8)
          WRITE (6,1100) (kpds(ker),ker = 1,22)
 1100     FORMAT (1X,22I5)
          GO TO 10
        ELSE
          IF (kpds(3).eq.104) THEN
            WRITE (6,1000) (kgds(ker),ker = 1,14)
            WRITE (6,1100) (kpds(ker),ker = 1,22)
            GO TO 10
          END IF
        END IF
      END DO
   10 CONTINUE
C     
C     FILL IN GRIDEF COMMON BLOCK
C     THE FOLLOWING DEFINED REGARDLESS OF GRID PROJECTION
C     
      imax = kgds(2)
      jmax = kgds(3)
C     
C     USE KGDS(1) TO DETERMINE GRID PROJECTION
C     
C     KGDS(1) = 0 ----> LATITUDE/LONGITUDE
C     KGDS(1) = 1 ----> MERCATOR (NOT YET UESD)
C     KGDS(1) = 3 ----> POLAR STEREOGRAPHIC
C     KGDS(1) = 5 ----> LAMBERT CONFORMAL
C     
      IF (kgds(1).eq.0) THEN
        latlong = .true.
        lambert = .false.
        polarstereo = .false.
      ELSE IF (kgds(1).eq.3) THEN
        latlong = .false.
        lambert = .true.
        polarstereo = .false.
      ELSE IF (kgds(1).eq.5) THEN
        latlong = .false.
        lambert = .false.
        polarstereo = .true.
      ELSE
        iret = 99
        WRITE (6,*) ' KGDS(1) = ', kgds(1)
        WRITE (6,*) ' GRID CAN NOT BE USED IN THIS CODE IRET= ', iret
        istat = -1
        RETURN
C     STOP777
      END IF

      jj1 = 1
      jjinc = 1

C     
C     SET THE REST OF THE GRID PARAMETERS BASED ON PROJECTION TYPE
C     
      IF (latlong) THEN
        alat1 = kgds(4) * 0.001
        elon1 = kgds(5) * 0.001
        IF (elon1.lt.0.0) elon1 = elon1 + 360.
        elonv = 0.0
        alatan = 0.0
        dxx = kgds(9) * 0.001
        dyy = kgds(10) * 0.001
C       IF we have a damn global grid with pt 1,1 at the NP, flip it
        IF (alat1.ge.89.999) THEN
          alat1 = alat1 - dyy * (jmax-1)
          alat1 = max(-90.,alat1)
          jj1 = jmax
          jjinc = -1
        END IF
      END IF
C     
      IF (lambert) THEN
        alat1 = kgds(4) * 0.001
        elon1 = kgds(5) * 0.001
        IF (elon1.lt.0.0) elon1 = elon1 + 360.
        elonv = kgds(7) * 0.001
        IF (elonv.lt.0.0) elonv = elonv + 360.
        alatan = kgds(12) * 0.001
        dxx = kgds(8) * 0.001
        dyy = kgds(9) * 0.001
      END IF
C     
      IF (polarstereo) THEN
        alat1 = kgds(4) * 0.001
        elon1 = kgds(5) * 0.001
        IF (elon1.lt.0.0) elon1 = elon1 + 360.
        elonv = kgds(7) * 0.001
        IF (elonv.lt.0.0) elonv = elonv + 360.
        alatan = 0.0
        dxx = kgds(8) * 0.001
        dyy = kgds(9) * 0.001
      END IF
C     
C     ADD IGF FORECAST HOURS TO START DATE TO GET VALID DATE
C     ADDDATE IS MODIFIED VERSION OF W3ADDATE
C     
      call w3movdat(RICDAT,IFDATE,IVDATE)
      vdate = (ivdate(1)*1000000+ivdate(2)*10000+ivdate(3)*100+
     1          ivdate(5))
      print *,ifdate,ricdat
      print *,ivdate
      print *,vdate
C     
C     GET GRID NUMBER FROM PDS
C     
      igdnum = kpds(3)
C     
      WRITE (6,*) ' WELCOME TO THE PREPFITS GRID DECODER '
      WRITE (6,*) ' THE GRID YOU HAVE CHOSEN IS NUMBER ', igdnum
      IF (latlong) THEN
        WRITE (6,*) ' A LAT/LON GRID WITH RES= ', dxx, ' BY ', dyy, 
     +              ' DEG'
        WRITE (6,*) ' AND ORIGIN (1,1) @ ', alat1, ' LAT & ', elon1, 
     +              ' LONG'
      ELSE IF (polarstereo) THEN
        WRITE (6,*) ' A POLAR STEREO GRID CENTERED AT ', elonv, ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      ELSE IF (lambert) THEN
        WRITE (6,*) ' A LAMBERT CONFORMAL GRID CENTERED AT ', elonv, 
     +              ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      END IF
      WRITE (6,*) ' HORIZONTAL DIMENSIONS OF ', imax, ' X', jmax
      modgen = kpds(2)
      WRITE (6,*) ' THE MODEL GENERATION NUMBER IS ', modgen
c     WRITE (6,*) ' WHICH IS THE ', modesc(modgen)
C     
C     PROCESS THE GRIB FILE
C     
      numval = imax * jmax
C     initialize mask arrays
      maskz = 0
      maskt = 0
      masku = 0
      maskv = 0
      maskq = 0
      mskzs = 0
      mskts = 0
      mskus = 0
      mskvs = 0
      mskqs = 0
      mskps = 0
      mskpm = 0
      mskcp = 0
      mskcn = 0
      mskli = 0 
C     default thin mandatory level set NUMLEV<=10
      levset = 5
      kmax = min(10,numlev)
C     full mandatory level set
      IF (numlev.ge.11) THEN
        levset = 4
        kmax = min(13,numlev)
      END IF
C     full mandatory level set but no 150 or 100 mb
      IF (numlev.ge.8) THEN
        levset = 6
        kmax = min(10,numlev)
      END IF
C     full mandatory level set
      IF (numlev.ge.11) THEN
        levset = 4
        kmax = min(13,numlev)
      END IF
C     every 50mb
      IF (numlev.ge.19) THEN
        levset = 3
        kmax = min(20,numlev)
      END IF
C     every 25mb but low levels only
      IF (numlev.ge.23) THEN
        levset = 2
        kmax = min(23,numlev)
      END IF
c RR set
      if (numlev.eq.29) then
       levset=7
       kmax = min(29,numlev)
      endif 
C     every 25mb
      IF (numlev.ge.37) THEN
        levset = 1
        kmax = min(40,numlev)
      END IF
C     
C     Fill pressure array
C     
      DO l = 1, kmax
        DO j = 1, jmax
          DO i = 1, imax
            pgd(i,j,l) = levs(l,levset)
          END DO
        END DO
      END DO
C     
      WRITE (6,1200) imax, jmax, numlev, kmax, levset
 1200 FORMAT (' IMAX,JMAX,NUMLEV,KMAX,LEVSET ',5I4)

C     -== GET SURFACE FIELDS ==-
      l = 0
      iv = 0
C     SURFACE PRESSURE (MB)

c     to start each new file with its index, set J=-1 for sfc pressure
      j = -1
      jpds = -1
      jpds(3) = igdnum
      jpds(5) = 1
      jpds(6) = 1
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          ps(ii,jj) = grid(kk) * 0.01
          IF (mask(kk)) THEN
            mskps(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 20
          END IF
        END DO
   20   CONTINUE
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
 1300   FORMAT (' IV, IVAR, L, IRET:  ',4I5)
      END IF

C     SEA-LEVEL PRESSURE (MB)

      j = 0
      jpds = -1
      jpds(3) = igdnum
C     SHUELL MSL PRESSURE IS 2 (the default)
      jpds(5) = 2
C     THE RUC'S MSL PRESSURE IS 129
      IF (modgen.eq.86.or.modgen.eq.105) jpds(5) = 129
C     FEDOR'S MSL PRESSURE IS 130
      IF (modgen.eq.83.or.modgen.eq.84.or.modgen.eq.85.or.modgen.eq.89
     +            .or.modgen.eq.110) jpds(5) = 130
      jpds(6) = 102
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          pm(ii,jj) = grid(kk) * 0.01
          IF (mask(kk)) THEN
            mskpm(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 30
          END IF
        END DO
   30   CONTINUE
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
      END IF

C     SURFACE GEOPOTENTIAL HEIGHT (M)

      j = 0
      jpds = -1
      jpds(3) = igdnum
      jpds(5) = 7
      jpds(6) = 1
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          zs(ii,jj) = grid(kk)
          IF (mask(kk)) THEN
            mskzs(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 40
          END IF
        END DO
   40   CONTINUE
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
      END IF

C     SURFACE TEMPERATURE (K) (ACTUALLY TEMP AT 2 METERS)

      j = 0
      jpds = -1
      jpds(3) = igdnum
      jpds(5) = 11
      jpds(6) = 105
      jpds(7) = 2
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          ts(ii,jj) = grid(kk)
          IF (mask(kk)) THEN
            mskts(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 50
          END IF
        END DO
   50   CONTINUE
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
      END IF

C     SURFACE RELATIVE OR SPECIFIC HUMIDITY (ACTUALLY AT 2 METERS)

      j = 0
      jpds = -1
      jpds(3) = igdnum
      jpds(5) = 52
C     ETA OUTPUTS SPECIFIC HUMIDITY
      IF (modgen.eq.83.or.modgen.eq.84.or.modgen.eq.85.or.modgen.eq.89
     +            .or.modgen.eq.110) jpds(5) = 51
      jpds(6) = 105
      jpds(7) = 2
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          IF (jpds(5).eq.52) THEN
            IF (mskps(ii,jj).eq.1.and.mskts(ii,jj).eq.1) THEN
              rh = max(0.001,grid(kk)*.01)
              rh = min(1.00,rh)
              vaps = w3fa09(ts(ii,jj)) * 10.
              vap = rh * vaps
              qs(ii,jj) = .622 * vap / (ps(ii,jj)-.378*vap)
            ELSE
              qs(ii,jj) = 0.0
              mask(kk) = .false.
            END IF
          ELSE
            qs(ii,jj) = grid(kk)
          END IF
          IF (mask(kk)) THEN
            mskqs(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 60
          END IF
        END DO
   60   CONTINUE
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
      END IF

C     SURFACE WIND COMPONENTS (M/S) (ACTUALLY AT 10 METERS)

      j = 0
      jpds = -1
      jpds(3) = igdnum
      jpds(5) = 33
      jpds(6) = 105
      jpds(7) = 10
      jpds(13) = 1
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,grid,
     +            iret)
c
c Check to see if the winds are grid-relative or earth-relative
c
      if(btest(kgds(6),3) ) then
       iearth=0  !  grid relative
      else
       iearth=1  !  earth relative
      endif
      IF (iret.eq.0) THEN
        ii = 1
        jj = jj1
        DO kk = 1, itot
          us(ii,jj) = grid(kk)
          IF (mask(kk)) THEN
            mskus(ii,jj) = 1
          END IF
          iprev = ii
          ii = ii + 1
          IF (mod(iprev,imax).eq.0) THEN
            ii = 1
            jj = jj + jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 70
          END IF
        END DO
   70   CONTINUE
C       
        j = 0
        jpds = kpds
        jpds(5) = 34
        CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,
     +              grid,iret)
        IF (iret.eq.0) THEN
          ii = 1
          jj = jj1
          DO kk = 1, itot
            vs(ii,jj) = grid(kk)
            IF (mask(kk)) THEN
              mskvs(ii,jj) = 1
            END IF
            iprev = ii
            ii = ii + 1
            IF (mod(iprev,imax).eq.0) THEN
              ii = 1
              jj = jj + jjinc
              IF (jj.gt.jmax.or.jj.lt.1) GO TO 80
            END IF
          END DO
   80     CONTINUE
        ELSE
          WRITE (6,1300) iv, jpds(5), l, iret
        END IF
      ELSE
        WRITE (6,1300) iv, jpds(5), l, iret
      END IF

C  SURFACE-BASED CAPE
      j=0
      jpds=-1
      jpds(5)=157
      jpds(6)=1
      jpds(7)=0
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,
     +              grid,iret)
      IF (iret.eq.0) THEN
        ii=1
        jj=jj1
        DO kk=1,itot
          cape(ii,jj)=grid(kk)
          IF (mask(kk)) THEN
            mskcp(ii,jj)=1
          ENDIF
          iprev=ii
          ii=ii+1
          IF (mod(iprev,imax).eq.0) THEN
            ii=1
            jj=jj+jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 152
           ENDIF
         ENDDO
152     CONTINUE
        ELSE
         WRITE(6,1300) iv,jpds(5),l,iret
        ENDIF

C  SURFACE-BASED CIN

      j=0
      jpds=-1
      jpds(5)=156
      jpds(6)=1
      jpds(7)=0
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,
     +              grid,iret)
      IF (iret.eq.0) THEN
        ii=1
        jj=jj1
        DO kk=1,itot
          cin(ii,jj)=grid(kk)
          IF (mask(kk)) THEN
            mskcn(ii,jj)=1
          ENDIF
          iprev=ii
          ii=ii+1
          IF (mod(iprev,imax).eq.0) THEN
            ii=1
            jj=jj+jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 153
           ENDIF
         ENDDO
153     CONTINUE
        ELSE
         WRITE(6,1300) iv,jpds(5),l,iret
        ENDIF

C SURFACE-TO-500-MB LIFTED INDEX

      j=0
      jpds=-1
      jpds(5)=24
      jpds(6)=116
      jpds(7)=7680
      CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,
     +              grid,iret)
      IF (iret.eq.0) THEN
        ii=1
        jj=jj1
        DO kk=1,itot
          pli(ii,jj)=grid(kk)
          IF (mask(kk)) THEN
            mskli(ii,jj)=1
          ENDIF
          iprev=ii
          ii=ii+1
          IF (mod(iprev,imax).eq.0) THEN
            ii=1
            jj=jj+jjinc
            IF (jj.gt.jmax.or.jj.lt.1) GO TO 154
           ENDIF
         ENDDO
154     CONTINUE
        ELSE
         WRITE(6,1300) iv,jpds(5),l,iret
        ENDIF

C     -== GET PRESSURE LEVEL VARIABLES Z, T, U, V & Q/RH ==-

      DO 100 iv = 1, 5
        j = 0
        jpds = -1
        jpds(3) = igdnum
        jpds(5) = ivar(iv)
C       ETA OUTPUTS SPECIFIC HUMIDITY
        IF ((modgen.eq.83.or.modgen.eq.84.or.modgen.eq.85.or.modgen.eq.
     +              89.or.modgen.eq.110).and.iv.eq.5) jpds(5) = ivar(6)
c       if (iv.eq.5) jpds(5)=52
        if (iv.eq.5) jpds(5)=51  ! specific humidity
        jpds(6) = 100
        jpds(13) = 1
        DO 90 l = 1, numlev
          jpds(7) = levs(l,levset)
          CALL getgb(lugb,lugi,numval,j,jpds,jgds,kf,k,kpds,kgds,mask,
     +                grid,iret)
c
c Check to see if the winds are grid-relative or earth-relative
c
      if(btest(kgds(6),3) ) then
       iearth=0  !  grid relative
      else
       iearth=1  !  earth relative
      endif
c         if (iv.eq.1.and.l.eq.1) then
c           do kk=1,itot
c           print*,'kk,grid(kk)=',kk,grid(kk)
c           enddo
c         endif
          IF (iret.eq.0) THEN
            ii = 1
            jj = jj1
            DO kk = 1, itot
              IF (iv.eq.1) THEN
                z(ii,jj,l) = grid(kk)
                IF (mask(kk)) THEN
                  maskz(ii,jj,l) = 1
                END IF
              ELSE IF (iv.eq.2) THEN
                t(ii,jj,l) = grid(kk)
                IF (mask(kk)) THEN
                  maskt(ii,jj,l) = 1
                END IF
              ELSE IF (iv.eq.3) THEN
                u(ii,jj,l) = grid(kk)
                IF (mask(kk)) THEN
                  masku(ii,jj,l) = 1
                END IF
              ELSE IF (iv.eq.4) THEN
                v(ii,jj,l) = grid(kk)
                IF (mask(kk)) THEN
                  maskv(ii,jj,l) = 1
                END IF
              ELSE IF (iv.eq.5) THEN
                IF (jpds(5).eq.52) THEN
                  IF (maskt(ii,jj,l).ge.1.) THEN
                    rh = max(0.01,grid(kk)*.01)
                    rh = min(1.00,rh)
                    vaps = w3fa09(t(ii,jj,l)) * 10.
                    vap = rh * vaps
                    q(ii,jj,l) = .622 * vap / (pgd(ii,jj,l)-.378*vap)
                  ELSE
                    q(ii,jj,l) = 0.0
                    mask(kk) = .false.
                  END IF
                ELSE
                  q(ii,jj,l) = grid(kk)
                END IF
                IF (mask(kk)) THEN
                  maskq(ii,jj,l) = 1
                  alnq(ii,jj,l) = alog(q(ii,jj,l))
                END IF
              END IF
              iprev = ii
              ii = ii + 1
              IF (mod(iprev,imax).eq.0) THEN
                ii = 1
                jj = jj + jjinc
                IF (jj.gt.jmax.or.jj.lt.1) GO TO 90
              END IF
            END DO
          ELSE
            WRITE (6,1300) iv, jpds(5), l, iret
          END IF
   90     CONTINUE
  100     CONTINUE

C         PRINT VALUES AT POINT IN MIDDLE OF GRID
          jer = jmax / 2
          ier = imax / 2
          WRITE (6,1400) ier, jer, pm(ier,jer), ps(ier,jer), 
     +                zs(ier,jer), ts(ier,jer), us(ier,jer), 
     +                vs(ier,jer), qs(ier,jer)
 1400     FORMAT (2I4,3X,F7.1,/,3X,F7.1,2X,F7.1,1X,3F8.2,2X,F8.6)
          DO ler = 1, numlev
            WRITE (6,1500) ler, pgd(ier,jer,ler), z(ier,jer,ler), 
     +                  t(ier,jer,ler), u(ier,jer,ler), v(ier,jer,ler),
     +                  q(ier,jer,ler), alnq(ier,jer,ler)
 1500       FORMAT (1X,I2,2X,F5.0,2X,F7.1,1X,3F8.2,2X,F8.6,2X,F7.3)
          END DO

          RETURN
          END
