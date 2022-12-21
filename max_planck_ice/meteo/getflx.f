      SUBROUTINE getflx(slpf, swdn, lwup, lwdn, t2, tsfc, q2, 
     1    uten, vten, precip, cloud, mask, lugb, lugbi)

C     Test grib reading.
      IMPLICIT none
      INCLUDE "mgrid.inc"

C     Grid variables for de-gribbing.
      INTEGER ijdim
      PARAMETER (ijdim = idim*jdim)
      INTEGER nlong, nlat
      PARAMETER (nlong = 360./dlonm)
      PARAMETER (nlat  = 180./dlatm + 1)

C     Arguments to the get grib routine
      INTEGER lugb, lugbi, j, jf, jpds(25), jgds(22)
      INTEGER k, kf, kpds(25), kgds(22), iret
      LOGICAL lb(ijdim)
      REAL field(ijdim)
      INTEGER ind, ifld(12), ilevel(12), altfld(12), altlev(12)

      REAL tempor(nlong, nlat)

      REAL slpf(nlong, nlat), swdn(nlong, nlat), lwup(nlong, nlat)
      REAL lwdn(nlong, nlat), t2(nlong, nlat), q2(nlong, nlat)
      REAL uten(nlong, nlat), vten(nlong, nlat), precip(nlong, nlat)
      REAL cloud(nlong, nlat), mask(nlong, nlat), tsfc(nlong, nlat)


C     Ifld is the IPDS descriptor for getting a desired field.
C     Some of the values are:
C     001 Pressure                   PA
C     002 Pressure reduced to MSL    PA
C     011 Temperature                K
C     012 Virtual Temperature        K
C     013 Potential temperature      K
C     017 Dew point temperature      K
C     033 U wind component           m/s
C     034 V wind component           m/s
C     049 U current component        m/s
C     050 V current component        m/s
C     051 specific humidity          kg/kg
C     052 relative humidity          %
C     055 vapor pressure             Pa
C     059 Precipitation rate         kg/m2/s
C     061 Total precip               kg/m2
C     065 Water equiv snow depth     m
C     066 Snow depth                 m
C     071 Total cloud cover          %
C     076 Cloud water                kg/m2
C     078 Convective snow            kg/m2
C     079 Large scale snow           kg/m2
C     081 Land-sea mask              0-sea 1-land
C     084 Albedo                     %
C     085 Soil temperature           K

C     067 Mixed layer depth          m
C     080 Water temperature          K
C     088 Salinity                   kg/kg
C     089 Density                    kg/m3
C     091 Ice concentration          fraction
C     092 Ice thickness              m
C     093 Ice drift direction        deg true
C     094 Ice drift speed            m/s
C     095 u-component of ice drift   m/s
C     096 v-component of ice drift   m/s
C     097 ice growth rate            m/s
C     098 ice divergence             1/s
C     099 Snow melt                  kg/m2 

C     111 Net short-wave at surface  w/m2
C     112 Net long wave at surface   w/m2
C     115 Long wave radiation        w/m2
C     116 Short wave radiation       w/m2
C     121 Latent heat net flux       w/m2
C     122 Sensible heat net flux     w/m2
C     124 U momentum flux            Pa
C     125 V momentum flux            Pa
C     
C  Indices above 128 are local to NMC:
C     128 Mean sea level pressure (Standard reduction) Pa
C     129 ditto, MAPS reduction
C     155 Ground heat flux           w/m2
C     201 ice-free water surface     %
C     204 downward short wave radiation, surface w/m2
C     205 downward long wave radiation, surface  w/m2
C     211 upward short wave at surface           w/m2
C     212 upward long wave at surface            w/m2

      ifld(1) = 001 !pmsl
      ilevel(1) = 1
      ifld(2) = 033 !uwind
      ilevel(2) = 105
      ifld(3) = 034 !vwind
      ilevel(3) = 105
      ifld(4) = 051 !q
      ilevel(4) = 105
      ifld(5) = 059 !precip in kg/m2/s
      ilevel(5) = 1
      altfld(5) = 61
      altlev(5) =  1
      ifld(6) = 071 !total cloud cover
      ilevel(6) = 214 ! (at low cloud level)
      altfld(6) = 71
      altlev(6) = 14
      ifld(7) = 081 !slimsk
      ilevel(7) = 1
      ifld(8) = 011 !soil temperature (tsfc)
      ilevel(8) = 1
      ifld(9) = 204 !surface downward shortwave
      ilevel(9) = 1
      ifld(10) = 205 !surface downward longwave
      ilevel(10) = 1
      ifld(11) = 212 !surface upward longwave
      ilevel(11) = 1
      ifld(12) = 011 !temperature, elevation a question.
      ilevel(12) = 105      

      J=-1
      JPDS=-1
      JGDS=-1
      lb = .FALSE.
      field = 0.0

      DO 1000 ind = 1, 12
        JPDS(5) = ifld(ind)
        JPDS(6) = ilevel(ind)
        field = 0.0
        CALL getgb(lugb, lugbi, ijdim, j, jpds, jgds, kf, k, kpds, kgds, 
     1             lb, field, iret)
        IF (iret .NE. 0) THEN 
          PRINT *,'failed, field ind = ',ind, ' pds = ',jpds, 
     1    ' iret = ',iret
          PRINT *,'Trying alternative field'
          JPDS(5) = altfld(ind)
          JPDS(6) = altlev(ind)
          field  = 0.0
          CALL getgb(lugb, lugbi, ijdim, j, jpds, jgds, kf, k, kpds, 
     1               kgds, lb, field, iret)
          IF (iret .NE. 0) THEN
            PRINT *,'Failed to get any data for a required field.'
            PRINT *,'Stopping'
            PRINT *,'failed, field ind = ',ind, ' pds = ',jpds, 
     1    ' iret = ',iret
            STOP
          ENDIF
        ENDIF 
CD         ELSE
CD          PRINT *,'field ind = ',ind, ' pds = ',ind, 
CD     1    ' iret = ',iret
CD          PRINT *,'kpds = ',kpds
CD          PRINT *,'kgds = ',kgds
          CALL ffld(tempor, field, kgds)
CD          PRINT *,'tempor = ',tempor
C        Now parcel out the fields into the return arrays:
         IF (ind .EQ. 1) THEN
           slpf = tempor
         ELSE IF (ind .EQ. 2) THEN
           uten = tempor
         ELSE IF (ind .EQ. 3) THEN
           vten = tempor
         ELSE IF (ind .EQ. 4) THEN
           q2   = tempor
         ELSE IF (ind .EQ. 5) THEN
           precip = tempor
         ELSE IF (ind .EQ. 6) THEN
           cloud = tempor
         ELSE IF (ind .EQ. 7) THEN
           mask = tempor
         ELSE IF (ind .EQ. 8) THEN
           tsfc = tempor
         ELSE IF (ind .EQ. 9) THEN
           swdn = tempor
         ELSE IF (ind .EQ.10) THEN
           lwdn = tempor
         ELSE IF (ind .EQ.11) THEN
           lwup = tempor
         ELSE IF (ind .EQ.12) THEN
           t2   = tempor
         ENDIF  !end of the case statement splitting out the data
CD        ENDIF  !end of if test for a successful read of grib data
 1000 CONTINUE      

      RETURN
      END 
