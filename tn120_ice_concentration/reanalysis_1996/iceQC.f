**********************************************************************
*                                                                    *
*          READ SSM/I SEA ICE CONCENTRATION DATA FROM 1992           *
*                                                                    *
*    (Q.C. is performed using the previously analysed ice field)     *
*                                                                    *
*          FT01F001  :  location convertion parameter                *
*          FT1nF001  :  SSM/I SEA ICE CONCENTRATION DATA (N.H.)      *
*          FT3nF001  :  SSM/I SEA ICE CONCENTRATION DATA (S.H.)      *
*          FT50F001  :  PREVIOUS SEA ICE ANALYSIS FIELD              *
*          FT51F001  :  NMC SST ANALYSIS FILE                        *
*          FT60F001  :  SSM/I LAND/SEA MASK                          *
*          FT99F001  :  SEA ICE COVERAGE DATA                        *
*                                                                    *
**********************************************************************
      parameter(pai=3.14159/180.0,imax=360,jmax=180)
      parameter(inmax=304,jnmax=448,ijnmax=inmax*jnmax)
C               -- Dimension for Nortern Hemisphere
      parameter(ismax=316,jsmax=332)
C               -- Dimension for Southern Hemisphere
      parameter(ishift=17)
C               -- Byte shift for the new sea ice data
      parameter(sstlim=2.0)
C               -- Criterion for data rejection (now +2.0C)
      parameter(iofile=50,isfile=51)

      integer*2   ich(ijnmax)
      integer*4   idate(5),jdate(5),kdate(5),inta(ijnmax),
     -            nice(imax,jmax),ice(imax,jmax),ldate(5)
      integer*4   inp(inmax,jnmax),isp(ismax,jsmax)
      real*4      rval(90),fice(imax,jmax),gice(imax),
     -            siceo(imax,jmax),
     -            asst(imax,jmax),csst(imax,jmax),rsst(imax,jmax),
     -            work(imax,jmax),
     -            flatn(inmax,jnmax),flonn(inmax,jnmax),
     -            flats(ismax,jsmax),flons(ismax,jsmax),
     -              fcn(inmax,jnmax),  fcs(ismax,jsmax)
      character*1   cnh(inmax,jnmax),  csh(ismax,jsmax)
      character*2 cunit
      character*7 filename

      namelist /datem/ iym,imm,idm
C        -- year,month,day of the middle day
      namelist /datef/ iyf,imf,idf
C        -- the first date of original(SSM/I) sea ice data
      namelist /datel/ iyl,iml,idl
C        -- the last  date of original(SSM/I) sea ice data
      namelist /efile/ ienfile,iesfile
C        -- file sequence number of the last day's file 
C           for both hemisphere (fort.nn)

      read(5,datem)
      read(5,datef)
      read(5,datel)
      read(5,efile)

        idate(1) = iym     ! date of analysed data
        idate(2) = imm
        idate(3) = idm
        jdate(1) = iyf     ! first date of data
        jdate(2) = imf
        jdate(3) = idf
        kdate(1) = iyl     ! last  date of data
        kdate(2) = iml
        kdate(3) = idl
C        --  these date are used to fill the GRIB format

       write(6,8010) (idate(i),i=1,3),(jdate(i),i=2,3),
     -               (kdate(i),i=2,3)
 8010  format(//1H ,'DATE : ',3I4,2X,2(2X,2I4))

**  --  read LAND/SEA mask  --  **
C     pbopen, pbread is a special subroutines to read a special file
C     these subroutines are only for ECMWF's system

*  --  For Northern Hemisphere  --  *

          filename = 'fort.60'
        call  pbopen (kunit,filename,'r',irtn)
         if (kret.ne.0)  STOP 956
C          -- Open a UNIX file as fort.60

      Do 5 j=1,jnmax
        call pbread (kunit,inta,inmax,irtn)
C          -- read inta bytes and set them to inmax
         if (irtn.le.-1) then
           write(6,*) irtn,j,filename
           STOP 957
         end if
        call copy304(cnh(1,j),inta)
C          -- copy inta to cnh
    5 continue

      call lsmdcd ( cnh,inp,inmax,jnmax )
C          -- set LAND/SEA mask information in inp(i,j) = N.H.

*  --  For Southern Hemisphere  --  *

          filename = 'fort.61'
        call  pbopen (kunit,filename,'r',irtn)
         if (kret.ne.0)  STOP 958

      Do 6 j=1,jsmax
        call pbread (kunit,inta,ismax,irtn)
         if (irtn.le.-1) then
           write(6,*) irtn,j,filename
           STOP 959
         end if
        call copy316(csh(1,j),inta)
    6 continue
      call lsmdcd ( csh,isp,ismax,jsmax )
C          -- set LAND/SEA mask information in isp(i,j) = S.H.

**  --  read SST field  --  **

   11   read(isfile,end=9000) ldate
        read(isfile) asst
        read(isfile) csst
C     -- csst(i,j) : climatological SST at a point (i,j)
C     -- asst(i,j) : SST anomaly        at a point (i,j)
C     --   dimension of SST data is (360,180) = NMC weekly SST

        Do 12 n=1,3
          if (idate(n).ne.ldate(n))  go to 11
   12   Continue

        Do 13 j=1,jmax
        Do 13 i=1,imax
          rsst(i,j) = asst(i,j) + csst(i,j)
   13   Continue

**  --  read previously analysed sea ise field  --  **

        read(iofile,end=98)
        read(iofile) siceo
        go to 99

   98  Do 97 j=1,jmax
       Do 97 i=1,imax
          siceo(i,j) = -1.0
   97  Continue
C       --  if there is no previous sea ice data, they set missing.

   99 Continue

**  --  calculate (i,j) grid lat/lon for N.H. and S.H. --  **
C        latitude/longitude at a point (ii,jj) = satellite sea ice data
C        are calculated in the section.

      Do 110 jj=1,jnmax
      Do 110 ii=1,inmax
        call latlon (1,ii,jj,flatn(ii,jj),flonn(ii,jj))
  110 Continue
C           --  for Nortern Hemisphere
      Do 120 jj=1,jsmax
      Do 120 ii=1,ismax
        call latlon (2,ii,jj,flats(ii,jj),flons(ii,jj))
  120 Continue
C           --  for Southern Hemisphere


**  --  clear sea ice concentration data --  **

      Do 200 j=1,jmax
      Do 200 i=1,imax
        fice(i,j) = 0.0
        nice(i,j) = 0
  200 Continue

**  --  FOR NORTHERN HEMISPHERE  --  **

          idfile =  9
 1000     idfile = idfile + 1
          filename = 'fort.  '
        write(cunit,'(I2)') idfile
          filename(6:7) = cunit
        call  pbopen (kunit,filename,'r',irtn)
         if (kret.ne.0)  STOP 966
C        -- Open a file named "filename" to Read

        call pbread (kunit,inta,inmax-ishift,irtn)
         if (irtn.le.-1) then
           write(6,*) irtn,1,filename
           STOP 967
         end if
C        -- Read (inmax-ishft) bytes and set them to inta

        call copy304s(cnh(1,1),inta,ishift)
C        -- set the first line to cnh considering the byte shift

      Do 1100 j=2,jnmax
        call pbread (kunit,inta,inmax,irtn)
         if (irtn.le.-1) then
           write(6,*) irtn,j,filename
           STOP 967
         end if
        call copy304(cnh(1,j),inta)
 1100 continue

      call icedcd ( cnh,ich,inmax,jnmax )
      call setcon ( ich,fcn,inp,inmax,jnmax )
C         --  decode satellite sea ice data for Northern Hemisphere
C         --  and set sea ice concentration values to fcn(ii,jj)
C             ( LAND/SEA mask(inp) are used to check them )

          nrej = 0
      Do 1200 jj=1,jnmax
      Do 1200 ii=1,inmax

C       (i ,j ) : global co-ordinate         = (360,180)
C       (ii,jj) : satellite data co-ordinate = (304,448)

          val = fcn(ii,jj)
        if ((val.ge.0.0).and.(val.le.100.0)) then
          glat = flatn(ii,jj)
C       -- latitude at a datum point (ii,jj)
          if (glat.lt.37.0)  go to 1200
C       -- latitude at a datum point (ii,jj)
          glon = flonn(ii,jj)
          if (glon.lt.0.0)   go to 1200
          j = 91.0 - glat
          i = glon + 1.0

**  --  Q.C. of SMMR sea ice concentration data   --  **

          if ((val.ge.0.0).and.(val.le.100.0))  then

            if (rsst(i,j).gt.sstlim)  then
              val = 0.0
C --  If SST is greater than a criterion (sstlim) at the datum point,
C     sea ice concentration at the point is set to 0%

*  --  QC against burst data (rarely happened)  --  *

            else if((rsst(i,j).gt.-0.5).and.(val.ge.99.0)) then
              if (siceo(i,j).ge.0.0)  then
                  dif = abs(siceo(i,j) - val)
                if (dif.gt.90.0)  then
                  nrej = nrej + 1
                  val = -1.0
                end if
              end if
            else if ((rsst(i,j).lt.-1.5).and.(rsst(i,j).gt.-3.0).and.
     -               (val.le. 1.0).and.(val.ge. 0.0))   then
              dif = abs(siceo(i,j) - val)
              if (siceo(i,j).ge.0.0)  then
                if (dif.gt.90.0)  then
                  nrej = nrej + 1
                  val = -1.0
                end if
              end if
            end if

          end if

*  --  accumulate sea ice data at a point (i,j)

          if (val.ge.0.0)  then
            fice(i,j) = fice(i,j) + val
            nice(i,j) = nice(i,j) + 1
          end if

        end if
 1200 Continue

*  --  read data of next date  --  *

      if (idfile.lt.ienfile)  go to 1000


**  --  FOR SOUTHERN HEMISPHERE  --  **

**  --  read sea ice data observed by NIMBUS-7  --  **

          idfile = 29
 2000     idfile = idfile + 1
          filename = 'fort.  '
        write(cunit,'(I2)') idfile
          filename(6:7) = cunit
        call  pbopen (kunit,filename,'r',irtn)
         if (kret.ne.0)  STOP 976

        call pbread (kunit,inta,ismax-ishift,irtn)
         if (irtn.le.-1) then
           write(6,*) irtn,1,filename
           STOP 977
         end if
        call copy316s(csh(1,1),inta,ishift)
      Do 2100 j=2,jsmax
        call pbread (kunit,inta,ismax,irtn)
         if (irtn.le.-1) STOP 977
        call copy316(csh(1,j),inta)
 2100 continue

      call icedcd ( csh,ich,ismax,jsmax )
      call setcon ( ich,fcs,isp,ismax,jsmax )

          nrej = 0
      Do 2200 jj=1,jsmax
      Do 2200 ii=1,ismax
          val = fcs(ii,jj)
        if ((val.ge.0.0).and.(val.le.100.0)) then
          glat = flats(ii,jj)
          if (glat.gt.-37.0)  go to 2200
          glon = flons(ii,jj)
          if (glon.lt.0.0)   go to 2200
          j = 91.0 - glat
          i = glon + 1.0

**  --  Q.C. of SMMR sea ice concentration data   --  **

          if ((val.ge.0.0).and.(val.le.100.0))  then
            if (rsst(i,j).gt.sstlim)  then
              val = 0.0
            else if((rsst(i,j).gt.-0.5).and.(val.ge.99.0)) then
              if (siceo(i,j).ge.0.0)  then
                  dif = abs(siceo(i,j) - val)
                if (dif.gt.90.0)  then
                  nrej = nrej + 1
                  val = -1.0
                end if
              end if
            else if ((rsst(i,j).lt.-1.5).and.
     -               (val.le. 1.0).and.(val.ge. 0.0))   then
              dif = abs(siceo(i,j) - val)
              if (siceo(i,j).ge.0.0)  then
                if (dif.gt.90.0)  then
                  nrej = nrej + 1
                  val = -1.0
                end if
              end if
            end if

          end if

          if (val.ge.0.0)  then
            fice(i,j) = fice(i,j) + val
            nice(i,j) = nice(i,j) + 1
          end if

        end if
 2200 Continue

      if (idfile.lt.iesfile)  go to 2000


**  --  calculate mean sea ice concentration  --  **

*   --  There are no satellite data near the north pole = set 99.9%

      Do 3000 j=1,5
      Do 3000 i=1,imax
        fice(i,j) = 99.9
 3000 Continue

*    -- calculate weekly mean sea ice concentration data 

      Do 3100 j=6,175
      Do 3100 i=1,imax
        if (nice(i,j).gt.0)  then
          fice(i,j) = fice(i,j)/nice(i,j)
        else
          fice(i,j) = -999.9
        end if
 3100 Continue

*  --  There are no satellite data near the south pole = set missing

      Do 3200 j=176,180
      Do 3200 i=1,imax
        fice(i,j) = -999.9
 3200 Continue

**  --  write the results in a file (fort.99)  --  **

      write(99) idate,jdate,kdate
      write(99) fice


**  --  check the results  --  **

      Do 4000 j=1,jmax
      Do 4000 i=1,imax
        ice(i,j) = fice(i,j)*0.1
 4000 continue

      Do 4100 i=imax,1,-1
        write(6,8000) i,(ice(i,j),j=1,55),(ice(i,j),j=131,180)
 8000   format(1H ,I3,X,55I1,2X,50I1)
 4100 continue


 9000 STOP
      END


*  ------------------------------------------------------------
      subroutine lsmdcd ( ch,ich,imax,jmax )

*    -- from 1 byre integer to 2 byte integer  --   *

      integer*2 iwork
      integer*4 ich(imax,jmax)
      character*1 ch(imax,jmax)
      Do 1000 j=1,jmax
      Do 1000 i=1,imax
        iwork = 0
        call set1byte ( iwork,ch(i,j) )
        if ((iwork.lt.0).or.(iwork.gt.255))  then
          write(6,*) i,j,iwork
          Stop 997
        end if
        ich(i,j) = iwork

 1000 Continue
      return
      end



* -------------------------------------------------------------
      subroutine icedcd ( ch,ich,imax,jmax )

*    -- from 1 byre integer to 2 byte integer  --   *

      integer*2 ich(imax,jmax),iwork
      character*1 ch(imax,jmax)
      Do 1000 j=1,jmax
      Do 1000 i=1,imax
        iwork = 0
        call set1byte ( iwork,ch(i,j) )
        if ((iwork.lt.0).or.(iwork.gt.255))  then
          write(6,*) i,j,iwork
          Stop 997
        end if
        ich(i,j) = iwork
 1000 Continue
      return
      end


* -------------------------------------------------------------
      subroutine set1byte ( iwork,ic )
      character*1 ic
      character*2 iwork
      iwork(2:2) = ic(1:1)
      return
      END


* -------------------------------------------------------------
      subroutine copy304 (ia,ib)
      character*304 ia,ib
      ia = ib
      return
      end

* --------------------------------------------------------------
      subroutine copy304s (ia,ib,num)
      character ia(304),ib(304)
      Do 1000 n=1,num
        ia(n) = ' '
 1000 Continue
      Do 1100 n=num+1,304
        ia(n) = ib(n-num)
 1100 Continue
      return
      end


* ---------------------------------------------------------------
      subroutine copy316 (ia,ib)
      character*316 ia,ib
      ia = ib
      return
      end


* ----------------------------------------------------------------
      subroutine copy316s (ia,ib,num)
      character ia(316),ib(316)
      Do 1000 n=1,num
        ia(n) = ' '
 1000 Continue
      Do 1100 n=num+1,316
        ia(n) = ib(n-num)
 1100 Continue
      return
      end


* ------------------------------------------------------------------
      subroutine setcon ( ich,fcl,ipo,imax,jmax )

**  --  decode satellite sea ice concentration data  --  **

      integer*2   ich(imax,jmax)
      integer*4   ipo(imax,jmax)
      real*4      fcl(imax,jmax)

      Do 1000 j=1,jmax
      Do 1000 i=1,imax
          ichk = ich(i,j)
        if (ipo(i,j).eq.1)  then
          fcl(i,j) = -1.0
        else
          if (ichk.lt.0)  then
            fcl(i,j) = -1.0
            write(6,*) i,j,ichk
          else if (ichk.le.100)  then
            fcl(i,j) = ichk
          else if (ichk.le.150)  then
            fcl(i,j) = 100.0
          else if (ichk.eq.200)  then
            fcl(i,j) = -999.9
          else if (ichk.eq.224)  then
            fcl(i,j) = -999.9
          else 
            fcl(i,j) = -1.0
            write(6,*) i,j,ichk
          end if
        end if
 1000 Continue

      return
      end


*  ----------------------------------------------------------------
        subroutine  latlon (ihem,i,j,alat,alon)
*****************************************************************
*                                                               *
*       Convert from (i,j) of SSM/I to (lat.,long.)             *
*                                                               *
*    This program is created using sample programs attched on   *
*       DMSP SSM/I Brightness Temperature and                   *
*       Sea Ice Concentration Grids for the Polar Regions       *
*       on CD-ROM User's Guide     (NSIDC)                      *
*                                                               *
*     ihem  : I 4 (I N)  N.H.= 1,  S.H. = 2                     *
*     i,j   : I 4 (I N)  (i,j) of a SSM/I data                  *
*     flat  : R 4 (OUT)  latitude  of the SSM/I data            *
*     flon  : R 4 (OUT)  longitude of the SSM/I data            *
*                                                               *
*      created                       1994   2  21   by  A.B.    *
*****************************************************************
      parameter (slat=70.0,re=6378.273,e2=0.006693883)
      parameter (pi=3.141592654)

      integer*4 numy(2,3)
      real*4    xydist(2,2)

      data numy   /896,664,448,332,224,166/
      data xydist /3850.0,5350.0,3950.0,3950.0/

        e = sqrt(e2)
      if (ihem.eq.1)  then
        sgn   =  1.0
        delta = 45.0
      else
        sgn   = -1.0
        delta =  0.0
      end if

      x  = (i -1)*25.0 - (xydist(1,ihem) - 12.5)
      kk = numy(ihem,2) - (j-1)
      y  = (kk-1)*25.0 - (xydist(2,ihem) - 12.5)

      call mapxy (x,y,alat,alon,slat,sgn,e,re)

      alon = alon*180.0/pi
      alat = alat*180.0/pi
      alon = alon - delta

      if (alon.le.0.0)    alon = alon + 360.0
      if (alon.ge.360.0)  alon = alon - 360.0

      RETURN
      END


      SUBROUTINE MAPXY (X,Y,ALAT,ALONG,SLAT,SGN,E,RE)
      REAL*4 X,Y,ALAT,ALONG,E,E2,CDR,PI

      CDR=57.29577951
      E2=E*E
      PI=3.141592654

      SL = SLAT*PI/180.
  200 RHO=SQRT(X**2+Y**2)
      IF (RHO.GT.0.1) GOTO 250
      ALAT=90.*SGN
      ALONG=0.0
      RETURN

  250 CM=COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
      T =TAN((PI/4.0)-(SL/(2.0)))/((1.0-E*SIN(SL))/
     -   (1.0+E*SIN(SL)))**(E/2.0)

      IF (ABS(SLAT-90.).LT.1.E-5) THEN
        T=RHO*SQRT((1.+E)**(1.+E)*(1.-E)**(1.-E))/2./RE
      ELSE
        T=RHO*T/(RE*CM)
      END IF

      CHI=(PI/2.0)-2.0*ATAN(T)
      ALAT=CHI+((E2/2.0)+(5.0*E2**2.0/24.0)
     -    +(E2**3.0/12.0))*SIN(2*CHI)+
     -    ((7.0*E2**2.0/48.0)+(29.0*E2**3/240.0))*SIN(4.0*CHI)+
     -     (7.0*E2**3.0/120.0)*SIN(6.0*CHI)
      ALAT=SGN*ALAT
      ALONG=ATAN2(SGN*X,-SGN*Y)
      ALONG=SGN*ALONG

      RETURN
      END
