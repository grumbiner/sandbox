C=====================================================================--
      SUBROUTINE ffld(latlon, field, kgds)
C  Author: Robert Grumbine
C  LAST MODIFIED: 9 November 1994.
C  Purpose: 
C     Convert spectral data to a lat-long grid.
C=====================================================================--
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=====================================================================--

      INTEGER iunit
      INTEGER nlong, nlat
      PARAMETER (nlong = 360./dlonm)
      PARAMETER (nlat  = 180./dlatm + 1)

      REAL field(idim * jdim)
      REAL latlon(nlong, nlat)

C=====================================================================--
C     Grid variables for de-gribbing.
      INTEGER ijdim
      PARAMETER (ijdim = idim*jdim)

C     Grib variables for interpolating from half sized fields
      INTEGER nwave2, idim2, jdim2, ijdim2
      PARAMETER (nwave2 = nwave/2 -1)
      PARAMETER (idim2 = 3*nwave2+6     )
      PARAMETER (jdim2 = (3*nwave2+2)/2 )
      PARAMETER (ijdim2 = idim2*jdim2)

C     Specify wave number of met input
      INTEGER kgds

C=====================================================================--
C     Local variables. 
      INTEGER i, j, k 
      REAL tempor(idim, jdim), tempor2(idim2, jdim2)
C=====================================================================--

        IF (kgds .EQ. idim) THEN
          DO 2000 j = 1, jdim
            DO 2001 i = 1, idim
              tempor(i,j) = field((j-1)*idim+i)
 2001       CONTINUE
 2000     CONTINUE
C         Convert data to regular lat-long grid
          CALL GAU2L(tempor, idim, jdim, latlon, nlong, nlat)
        ELSE IF (kgds .EQ. idim2) THEN
          DO 2100 j = 1, jdim2
            DO 2101 i = 1, idim2
              tempor2(i,j) = field((j-1)*idim2+i)
 2101       CONTINUE
 2100     CONTINUE
C         Convert data to regular lat-long grid
          CALL GAU2L(tempor2, idim2, jdim2, latlon, nlong, nlat)
        ELSE 
          PRINT *,'kgds does not match any known grid ',kgds
          STOP
      ENDIF

      RETURN
      END
