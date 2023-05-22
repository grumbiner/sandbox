C=====================================================================--
      SUBROUTINE getold(slpf, swdn, lwup, lwdn, t2, q2, uten, vten, 
     1    cloud, mask, iunit )
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine        NMC, Camp Springs, MD            Dec '92
C     Obsolete by 25 October 1994, grib version updated in '93.
C  Purpose:
C     Read selected fields from MRF flux files, which are in GRIB
C       format.  
C  Externals:
C     W3AI08:    NMC W3LIB routine to unpack GRIB data
C     GAU2L:     Jordan Alpert to convert gaussian latitude data
C                  to regular latitude-longitude grids.
C     BUFFERIN:  Cray routine to manage the reading of the GRIB
C                  data fields.  Will run only on Cray.
C=====================================================================--
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=====================================================================--

      INTEGER iunit
      INTEGER nlong, nlat
      PARAMETER (nlong = 360./dlonm)
      PARAMETER (nlat  = 180./dlatm + 1)

      REAL slpf(nlong, nlat)
      REAL swdn(nlong, nlat), lwup(nlong, nlat), lwdn(nlong, nlat)
      REAL t2(nlong, nlat), q2(nlong, nlat)
      REAL uten(nlong, nlat), vten(nlong, nlat)
      REAL cloud(nlong, nlat), mask(nlong, nlat)

C=====================================================================--
C     Variables for de-gribbing.
      INTEGER idim, jdim, ijdim
      INTEGER mxbits, maxgrb
C     Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )
      PARAMETER (ijdim = idim*jdim)
C     Parameters for GRIB extraction
      PARAMETER (mxbits = 16)
      PARAMETER (maxgrb = 82 + ((ijdim+15)/16)*2
     1                       + ((ijdim*mxbits+7)/16)*2 )

C     Variables for de-gribbing the half sized fields
      INTEGER nwave2, idim2, jdim2, ijdim2
      INTEGER maxgrb2
C     Grid parameters.
      PARAMETER (nwave2 = nwave/2 -1)
      PARAMETER (idim2 = 3*nwave2+6     )
      PARAMETER (jdim2 = (3*nwave2+2)/2 )
      PARAMETER (ijdim2 = idim2*jdim2)
C     Parameters for GRIB extraction
      PARAMETER (maxgrb2 = 82 + ((ijdim2+15)/16)*2
     1                        + ((ijdim2*mxbits+7)/16)*2 )

C     Arguments to the GRIB extraction routine W3AI08.
      INTEGER kgds(13), kpds(17), kptr(10), kret
      LOGICAL lbms(ijdim)
      REAL    grid(ijdim)
      CHARACTER*1 pack(maxgrb)

C     Variables which may be used to detect proper reading in
C       the BUFFERIN process which obtains the data from the
C       GRIB packed file.
      REAL rcrgrb    !-1. for a success, 0. for failure (EOF)
      INTEGER lengrb !record length in bytes
      REAL UNIT
      INTEGER LENGTH
C=====================================================================--

C     Pointers to the fields of the surface flux files (s2d files).
      INTEGER utau, vtau, qsense, qlaten, tsa, soilw, sndep
      INTEGER lwdns, lwups, lwupt, swupt, swups, swdns
      PARAMETER (utau   =  3) !N/m2, 7 bits
      PARAMETER (vtau   =  4) !N/m2, 7 bits
      PARAMETER (qsense =  5) !W/m2, 0 bits
      PARAMETER (qlaten =  6) !W/m2, 0 bits
      PARAMETER (tsa    =  7) !K, surface air temperature, 3 bits
      PARAMETER (soilw  =  8) 
      PARAMETER (sndep  =  9)
      PARAMETER (lwdns  = 10) !w/m2, 0 bits
      PARAMETER (lwups  = 11) !w/m2, 0 bits
      PARAMETER (lwupt  = 12) !w/m2, 0 bits
      PARAMETER (swupt  = 13) !w/m2, 0 bits
      PARAMETER (swups  = 14) !w/m2, 0 bits
      PARAMETER (swdns  = 15) !w/m2, 0 bits

      INTEGER hcf, hcbot, hctop, hctemp, mcf, mcbot, mctop, mctemp
      INTEGER lcf, lcbot, lctop, lctemp
      PARAMETER (hcf    = 16)
      PARAMETER (hcbot  = 17)
      PARAMETER (hctop  = 18)
      PARAMETER (hctemp = 19)
      PARAMETER (mcf    = 20)
      PARAMETER (mcbot  = 21)
      PARAMETER (mctop  = 22)
      PARAMETER (mctemp = 23)
      PARAMETER (lcf    = 24)
      PARAMETER (lcbot  = 25)
      PARAMETER (lctop  = 26)
      PARAMETER (lctemp = 27)

      INTEGER rain, crain, ghflux, slimsk, uwind, vwind
      INTEGER t2m, q2m, pslp
      PARAMETER (rain   = 28) !mm,   3 bits
      PARAMETER (crain  = 29) !mm,   3 bits
      PARAMETER (ghflux = 30) !w/m2, 0 bits
      PARAMETER (slimsk = 31) ! ?,   0 bits
      PARAMETER (uwind  = 32) !m/s,  3 bits
      PARAMETER (vwind  = 33) !m/s,  3 bits
      PARAMETER (t2m    = 34) !K,    3 bits
      PARAMETER (q2m    = 35) !.1g/kg, 0 bits
      PARAMETER (pslp   = 36) !hPa (mb), 3 bits, surface level pressure.
      INTEGER nfield
      PARAMETER (nfield = 36)
C=====================================================================--

C     Local variables. 
      INTEGER i, j, k 
      REAL tempor(idim, jdim), tempor2(idim2, jdim2)
C=====================================================================--

C     Loop over all fields.  Copy grid into array for selected
C       arrays.
      DO 1000 k = 1, nfield

C     Call bufferin to get the data to be unpacked into the
C       unpacking array.
       BUFFERIN (iunit, 0) (pack(1), pack(maxgrb*8))
       rcrgrb = UNIT(iunit)
       lengrb = LENGTH(iunit)
        IF (rcrgrb .NE. 0. .AND. k .GT. 2) THEN
          PRINT *,'Read failed, at EOF with field# ',k
          STOP
         ELSE
CD          PRINT *,'Read succeeded at field ',k
        ENDIF

C       Call w3ai08 to extract the data from grib field into
C         data field.  First two calls will fail.
        CALL W3AI08(pack, kpds, kgds, lbms, grid, kptr, kret)
CD        PRINT *,'kpds ',kpds
CD        PRINT *,'kgds ',kgds
CD        PRINT *,'kptr ',kptr
        IF ( kret .NE. 0) THEN
          IF (k .NE. 1 .AND. k .NE. 2) THEN
            PRINT *,'Error in the GRIB extraction process.'
            PRINT *,'Attempting to extract field # ',k
            PRINT *,'Return code ',kret
            PRINT *,'kpds ',kpds
            PRINT *,'kgds ',kgds
            PRINT *,'kptr ',kptr
          ENDIF
        ENDIF

C       Now put data field into useable data field for desired fields.

        IF (k .EQ. t2m) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2000 j = 1, jdim
              DO 2001 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2001         CONTINUE
 2000       CONTINUE
C           Convert data to regular lat-long grid
            CALL GAU2L(tempor, idim, jdim, t2, nlong, nlat)
          ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2100 j = 1, jdim2
              DO 2101 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2101         CONTINUE
 2100       CONTINUE
C           Convert data to regular lat-long grid
            CALL GAU2L(tempor2, idim2, jdim2, t2, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. q2m) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2002 j = 1, jdim
              DO 2003 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2003         CONTINUE
 2002       CONTINUE
            CALL GAU2L(tempor, idim, jdim, q2, nlong, nlat)
          ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2102 j = 1, jdim2
              DO 2103 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2103         CONTINUE
 2102       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, q2, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. lcf) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2016 j = 1, jdim
              DO 2017 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2017         CONTINUE
 2016       CONTINUE
            CALL GAU2L(tempor, idim, jdim, cloud, nlong, nlat)
           ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2116 j = 1, jdim2
              DO 2117 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2117         CONTINUE
 2116       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, cloud, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. slimsk) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2018 j = 1, jdim
              DO 2019 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2019         CONTINUE
 2018       CONTINUE
            CALL GAU2L(tempor, idim, jdim, mask, nlong, nlat)
           ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2118 j = 1, jdim2
              DO 2119 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2119         CONTINUE
 2118       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, mask, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. uwind) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2008 j = 1, jdim
              DO 2009 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2009         CONTINUE
 2008       CONTINUE
            CALL GAU2L(tempor, idim, jdim, uten, nlong, nlat)
           ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2108 j = 1, jdim2
              DO 2109 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2109         CONTINUE
 2108       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, uten, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. vwind) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2010 j = 1, jdim
              DO 2011 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2011         CONTINUE
 2010       CONTINUE
            CALL GAU2L(tempor, idim, jdim, vten, nlong, nlat)
          ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2110 j = 1, jdim2
              DO 2111 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2111         CONTINUE
 2110       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, vten, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. lwdns) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2012 j = 1, jdim
              DO 2013 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2013         CONTINUE
 2012       CONTINUE
            CALL GAU2L(tempor, idim, jdim, lwdn, nlong, nlat)
           ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2112 j = 1, jdim2
              DO 2113 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2113         CONTINUE
 2112       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, lwdn, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. lwups) THEN
          IF (kgds(2) .EQ. idim) THEN
          DO 2022 j = 1, jdim
            DO 2023 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2023       CONTINUE
 2022     CONTINUE
          CALL GAU2L(tempor, idim, jdim, lwup, nlong, nlat)
         ELSEIF (kgds(2) .EQ. idim2) THEN
          DO 2122 j = 1, jdim2
            DO 2123 i = 1, idim2
              tempor2(i,j) = grid((j-1)*idim2+i)
 2123       CONTINUE
 2122     CONTINUE
          CALL GAU2L(tempor2, idim2, jdim2, lwup, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. swdns) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2014 j = 1, jdim
              DO 2015 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2015         CONTINUE
 2014       CONTINUE
            CALL GAU2L(tempor, idim, jdim, swdn, nlong, nlat)
           ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2114 j = 1, jdim2
              DO 2115 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2115         CONTINUE
 2114       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, swdn, nlong, nlat)
          ENDIF
        ENDIF

        IF (k .EQ. pslp) THEN
          IF (kgds(2) .EQ. idim) THEN
            DO 2020 j = 1, jdim
              DO 2021 i = 1, idim
                tempor(i,j) = grid((j-1)*idim+i)
 2021         CONTINUE
 2020       CONTINUE
            CALL GAU2L(tempor, idim, jdim, slpf, nlong, nlat)
          ELSE IF (kgds(2) .EQ. idim2) THEN
            DO 2120 j = 1, jdim2
              DO 2121 i = 1, idim2
                tempor2(i,j) = grid((j-1)*idim2+i)
 2121         CONTINUE
 2120       CONTINUE
            CALL GAU2L(tempor2, idim2, jdim2, slpf, nlong, nlat)
          ENDIF
        ENDIF

C       Now finished de-gribbing.
 1000 CONTINUE

      RETURN
      END
