      PROGRAM front
      implicit none
C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      REAL swdown, swup
      REAL lwdown, lwup
      REAL prate  ! kg/m^2/s
      REAL shtfl, lhtfl  !net sensible and latent heat fluxes
C     Meteorology a local flux computation would want:
C       Sensible heat:
      REAL ugrd, vgrd, t2m
C       Latent heat:
      REAL spfh, press
C       Radiation
      REAL albedo
C       Boundary Layer depth
      REAL hpbl
    
      INTEGER i

C     Get meteorological input
      OPEN(10, FILE="meteo", FORM="unformatted")
      DO i = 1, 4*365
        READ(10) shtfl, lhtfl, t2m, lwdown, 
     1          lwup, swup, swdown, prate,
     2          ugrd, vgrd, spfh, press,
     3          hpbl, albedo
        WRITE(*,9000) i, shtfl, lhtfl, t2m, lwdown, lwup, swup, swdown, 
     1          prate, ugrd, vgrd, spfh, press, hpbl, albedo
      ENDDO
 9000 FORMAT(I4,14E10.2)

      STOP
      END
