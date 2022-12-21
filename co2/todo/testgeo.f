      PROGRAM testgeo
C     Test the geology program.

      INTEGER nlayer, region, chemu
      PARAMETER (nlayer = 76) 
      PARAMETER (region = 3)
      PARAMETER (chemu  = 2)
      
      DOUBLE PRECISION x(nlayer, chemu, region)
      DOUBLE PRECISION deltaz
      REAL csdept, ishelf, rsalt, time
      INTEGER pchem(chemu)
      
      INTEGER i, j, k
      
      pchem(1) = 13
      pchem(2) = 16
      deltaz   = 50.D0
      rsalt   = 34.73
      time    = -15000.
      csdept  = 500.0
      ishelf  = -340E3


      DO 9999 time = -19000., 0., 1000.

      DO 1000 k = 1, region
        DO 1100 j = 1, chemu
          DO 1200 i = 1, nlayer
            x(i,j,k) = 1.D0
 1200     CONTINUE
 1100   CONTINUE
 1000 CONTINUE
      deltaz   = 50.D0
      CALL geolog(x, pchem, nlayer, chemu, region, 
     1               csdept, ishelf, rsalt, deltaz, 
     2               time)
     
      PRINT *,'x(chem1), x(chem2)', x(3,1,1), x(3,2,1)
      PRINT *,'csdept, ishelf, rsalt',csdept, ishelf, rsalt, deltaz
 
 9999 CONTINUE
 
      PAUSE
      END
C*************************************************----------++++++++++!!
      SUBROUTINE geolog(x, pchem, nlayer, chemu, region, 
     1                   csdept, ishelf, rsalt, deltaz, 
     2                   time)

      IMPLICIT none
     
      INTEGER nlayer, chemu, region
      INTEGER pchem(chemu)
      DOUBLE PRECISION x(nlayer, chemu, region)
      DOUBLE PRECISION deltaz
      REAL csdept, ishelf, time, rsalt
      
      INCLUDE "chmptr.inc"
      
      LOGICAL consrv(nchem)
      INTEGER ntimes
      PARAMETER (ntimes = 20)
      REAL depth(ntimes), shelf(ntimes), dates(ntimes)
      REAL refdep
      PARAMETER (refdep = 3750.0)
      
      INTEGER i, j, k, itime
      DOUBLE PRECISION dznew, height
      DOUBLE PRECISION sum, mult, ic(nchem), fract
      
      REAL area(3), rho(3)
      
      SAVE consrv, depth, dates, shelf, area, rho, ic
      INCLUDE "arrho.inc"
      
      DATA dates(1)  /-19000./
      DATA dates(2)  /-18000./
      DATA dates(3)  /-17000./
      DATA dates(4)  /-16000./
      DATA dates(5)  /-15000./
      DATA dates(6)  /-14000./
      DATA dates(7)  /-13000./
      DATA dates(8)  /-12000./
      DATA dates(9)  /-11000./
      DATA dates(10) /-10000./
      DATA dates(11) /- 9000./
      DATA dates(12) /- 8000./
      DATA dates(13) /- 7000./
      DATA dates(14) /- 6000./
      DATA dates(15) /- 5000./
      DATA dates(16) /- 4000./
      DATA dates(17) /- 3000./
      DATA dates(18) /- 2000./
      DATA dates(19) /- 1000./
      DATA dates(20) /- 0000./
      
      DATA depth(1)  /-120./
      DATA depth(2)  /-120./
      DATA depth(3)  /-119./
      DATA depth(4)  /-116./
      DATA depth(5)  /-112./
      DATA depth(6)  /-107./
      DATA depth(7)  /-102./
      DATA depth(8)  /- 89./
      DATA depth(9)  /- 69./
      DATA depth(10) /- 60./
      DATA depth(11) /- 40./
      DATA depth(12) /- 25./
      DATA depth(13) /- 16./
      DATA depth(14) /- 11./
      DATA depth(15) /-  7./
      DATA depth(16) /-  6./
      DATA depth(17) /-  5./
      DATA depth(18) /-  4./
      DATA depth(19) /-  4./
      DATA depth(20) /-  0./   
      
      DATA shelf(1)  /-0./
      DATA shelf(2)  /-0./
      DATA shelf(3)  /-  31.E3/
      DATA shelf(4)  /-  62.E3/
      DATA shelf(5)  /-  93.E3/
      DATA shelf(6)  /- 124.E3/
      DATA shelf(7)  /- 155.E3/
      DATA shelf(8)  /- 185.E3/
      DATA shelf(9)  /- 216.E3/
      DATA shelf(10) /- 247.E3/
      DATA shelf(11) /- 278.E3/
      DATA shelf(12) /- 309.E3/
      DATA shelf(13) /- 340.E3/
      DATA shelf(14) /- 340.E3/
      DATA shelf(15) /- 340.E3/
      DATA shelf(16) /- 340.E3/
      DATA shelf(17) /- 340.E3/
      DATA shelf(18) /- 340.E3/
      DATA shelf(19) /- 340.E3/
      DATA shelf(20) /- 340.E3/
      
      DATA consrv(He)   /.FALSE./
      DATA consrv(Ne)   /.FALSE./
      DATA consrv(N2)   /.FALSE./
      DATA consrv(O2)   /.FALSE./
      DATA consrv(Ar)   /.FALSE./
      DATA consrv(Kr)   /.FALSE./
      DATA consrv(Xe)   /.FALSE./
      DATA consrv(CO2)  /.FALSE./
      DATA consrv(N2O)  /.FALSE./
      DATA consrv(temp) /.FALSE./
      DATA consrv(salt) /.TRUE./
      DATA consrv(NO3)  /.TRUE./
      DATA consrv(PO4)  /.TRUE./
      DATA consrv(SiO2) /.TRUE./
      DATA consrv(dc14) /.FALSE./
      DATA consrv(tco2) /.FALSE./
      DATA consrv(alk)  /.TRUE./
      
C     Initial Conditions (Present Day ocean mean)
      DATA ic(He)   /1.768D-9 / !B+P, 1982 saturation 0 ¡C
      DATA ic(Ne)   /7.920D-9 / !B+P, 1982 saturation 0 ¡C
      DATA ic(N2)   /6.248D-4 / !B+P, 1982 saturation 0 ¡C
      DATA ic(O2)   /2.000D-4 / !Approx Geosecs Global Mean
      DATA ic(Ar)   /1.702D-5 / !B+P, 1982 saturation 0 ¡C
      DATA ic(Kr)   /4.180D-9 / !B+P, 1982 saturation 0 ¡C
      DATA ic(Xe)   /7.224D-10/ !B+P, 1982 saturation 0 ¡C
      DATA ic(CO2)  /17.64D-6 / !B+P, 1982 saturation 0 ¡C
      DATA ic(N2O)  /1.410D-8 / !B+P, 1982 saturation 0 ¡C
      DATA ic(temp) /3.62D0   / !Levitus
      DATA ic(salt) /34.73D0  / !Levitus
      DATA ic(NO3)  /33.0D-6  /
      DATA ic(PO4)  /2.116 D-6 /!BG analysis of GEOSECS data, SCOPE16
      DATA ic(SiO2) /150.D-6  /
      DATA ic(dc14) /-160.D0  /
      DATA ic(tco2) /2300.D-6 / !Approx Geosecs global mean
      DATA ic(alk ) /2400.D-6 / !Approx Geosecs global mean
      
C*************************************************----------++++++++++!!
C     Now enforce conservation of the selected variables, and
C       evaluate the evolution of the ice shelf location, ocean depth,
C       and ocean salinity.

C     New ocean depth and ice shelf location:
      i = 1
      IF (time .LT. dates(1)) THEN
        PRINT *,'Time is too old.  geolog'
        height = depth(1)
        ishelf = shelf(1)
        GO TO 1000
      ENDIF
      
 100  i = i + 1
      IF (time .GT. dates(i) .AND. i .LT. ntimes) GO TO 100
      
C       The tabled time is less than (older) the chosen time.
C       Having reached this point, we've bracketed the time.
C       Compute the fraction of the two times to use for linearly
C         interpolating between table entries.
        itime = i
        fract = (time - dates(itime-1) )/
     1          (dates(itime)-dates(itime-1))
        height = fract *depth(itime) + 
     1                   (1.-fract)*depth(itime-1)
        ishelf = fract *shelf(itime) + 
     1                   (1.-fract)*shelf(itime-1)

 1000 CONTINUE
      dznew = (refdep+height)/(FLOAT(nlayer-1))

C     Enforce conservation as needed.
      DO 2000 i = 1, chemu
        DO 2100 j = 1, region
        
          IF (consrv(pchem(i)) .EQ. .TRUE.) THEN
C         Have a variable which shouldn't change from start to finish, 
C           aside from sea level effects.
          sum = 0.D0
          DO 2200 k = 1, nlayer
            sum = sum + x(k,i,j)
 2200     CONTINUE
CD          mult = 1.0
          mult = (FLOAT(nlayer)*ic(pchem(i))*refdep/FLOAT(nlayer-1))
     1          /(sum*dznew)
CD          IF (pchem(i) .EQ. PO4 .AND. j .EQ.1)
CD     1         PRINT *,'multiplier',mult,pchem(i)
          DO 2300 k = 3, nlayer-1
            x(k,i,j) = x(k,i,j)*mult
 2300     CONTINUE
         ELSE
C         Skip this chemical
          GO TO 2000
        ENDIF
          
 2100   CONTINUE
 2000 CONTINUE
 
C     Now adjust remaining scalars for sea level changes.  
C       Lower sea level -> higher concentration.
      mult = deltaz/dznew

      DO 3000 j = 1, region
        DO 3100 i = 1, chemu
          IF (consrv(pchem(i)) .EQ. .FALSE.) THEN
            DO 3200 k = 1, nlayer
              x(k,i,j) = x(k,i,j)*mult
 3200       CONTINUE
          ENDIF
 3100   CONTINUE
 3000 CONTINUE

C     Put the new deltaz, salinity in.
      deltaz = dznew
      rsalt = ic(salt)*refdep/(refdep+height)
      csdept = 500.0 + height

CD      PRINT *,'deltaz, rsalt', deltaz, rsalt
      
      RETURN
      END
