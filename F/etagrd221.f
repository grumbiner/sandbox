c
      Parameter(imx=349,jmx=277)
      Parameter(jf=imx*jmx)
      character*11 envvar
      character*80 fileb,filei
      logical lb(jf)
      INTEGER PDS,GDS,GRID
      INTEGER JPDS(25),JGDS(22),IGRD(5,3)
      INTEGER KPDS(25),KGDS(22),hours(6)
      real fld(349,277)
      real  uu(349,277),vv(349,277),
     * spch(349,277),airt(349,277),slp(349,277)
c     common/eta/ewsp(3,2000),edir(3,2000),eslp(3,2000),
c    *           ereh(3,2000),eairt(3,2000)
       data hours/00,12,24,36,48,60/
C                U   V   RH  AT  slp
      data IGRD/ 33, 34, 52, 11,  2,
     *          105,105,105,105,102,
     *           10, 10,  2,  2,  0/
c
C            INPUT UNITS FOR DECODING GRIB FILE
C
             do 1000 iu = 1,6
             if(iu.gt.1) go to 1000 
             write(6,601) iu
  601        format(1h ,'  iu ',i5)
c
          LUGB=19 + iu
          LUGI=29 + iu
C
C        Read GRIB data and index file names from the XLFUNIT_nn
C        environment variables, and open the files.
C
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugb
      call getenv(envvar,fileb)
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugi
      call getenv(envvar,filei)
      call baopenr(lugb,fileb,iret1)
      call baopenr(lugi,filei,iret2)
          jg = 0

c
c      print *,'grid 221  getgb iugb lugi ',lugb,lugi
C
C........    DECODE THE FILEDS
C
        DO 30 GRID = 1,5
C
          DO 10 PDS=1,25
          JPDS(PDS) = -1
   10      CONTINUE
C
             DO 20 GDS = 1,22
           JGDS(GDS) = -1
   20     CONTINUE
C
C........   GET SELECT FIELDS
C
           jPDS(5) = IGRD(GRID,1)
           jPDS(6) = IGRD(GRID,2)
           jPDS(7) = IGRD(GRID,3)
C
           CALL GETGB(LUGB,LUGI,JF,jg,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB,Fld,IRET)
C
            WRITE(6,61)KPDS,KF,KGDS
          IF(IRET.NE.0)WRITE(6,60)IRET
   60  FORMAT(1X,' IRET =',I5)
            IF(IRET.NE.0) GO TO 999
c
c           call w3fi68(kpds,vpds)
c           ipds7 = ichar(vpds(7))
c           ipds8 = ichar(vpds(8))
c     JSCALE = ICHAR(VPDS(27)) * 256 + ICHAR(VPDS(28))
c     IF (IAND(JSCALE,32768).NE.0) THEN
c       JSCALE = - IAND(JSCALE,32767)
c     END IF
c     SCALE  = 10.0 ** JSCALE
c
c       print *,' scale ',scale
       WRITE(6,61)KPDS,KF,KGDS
   61  FORMAT(2(/,2X,'PDS=',13I7),
     *        2(/,2X,' GDS=',11I9 ))
c
c....     pass field to proper array
c
      if(grid.eq.1) then
           do in = 1,349
            do jn = 1,277
            uu(in,jn)= fld(in,jn)
           enddo 
          enddo
c
       elseif(grid.eq.2) then
           do in = 1,349
            do jn = 1,277
            vv(in,jn)= fld(in,jn)
           enddo
          enddo
c
C
       elseif(grid.eq.3) then
           do in = 1,349
            do jn = 1,277
            spch(in,jn)= fld(in,jn)
           enddo
           enddo
c
       elseif(grid.eq.4) then
           do in = 1,349
            do jn = 1,277
            airt(in,jn)= fld(in,jn)-273.15
           enddo
            enddo 
c
       elseif(grid.eq.5) then
           do in = 1,349
            do jn = 1,277
            slp(in,jn)= fld(in,jn)*.01
            enddo
           enddo
c
        endif
   30        CONTINUE
C
  999           CONTINUE
           close(lugb)
            close(lugi)
c
c....           test print on analysis
c
            if(iu.eq.1)call mapit(uu,vv,spch,airt,slp)
c
c..          Get Next FCST
c
 1000                continue
c

             stop
             END
