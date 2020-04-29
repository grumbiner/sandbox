      PROGRAM front
      implicit none
C     Serve as a front end to the 3 layer thermodynamic code of
C     Winton, 2000.

C     From the meteorological model:
      INTEGER nx, ny
      PARAMETER (nx = 768)
      PARAMETER (ny = 384)
      REAL swdown(nx, ny), swup(nx, ny)
      REAL lwdown(nx, ny), lwup(nx, ny)
      REAL prate(nx, ny)  ! kg/m^2/s
      REAL shtfl(nx, ny), lhtfl(nx, ny)  !net sensible and latent heat fluxes
C     Meteorology a local flux computation would want:
C       Sensible heat:
      REAL ugrd(nx, ny), vgrd(nx, ny), t2m(nx, ny)
C       Latent heat:
      REAL spfh(nx, ny), press(nx, ny)
C       Radiation
      REAL albedo(nx, ny)
C       Boundary Layer depth
      REAL hpbl(nx, ny)
      INTEGER refi, refj

C     Get meteorological input
      OPEN(10, FILE="metinput", FORM="unformatted")
      READ (10) shtfl
      READ (10) lhtfl
      READ (10) t2m
      READ (10) lwdown
      READ (10) lwup
      READ (10) swup
      READ (10) swdown
      READ (10) prate
      READ (10) ugrd
      READ (10) vgrd
      READ (10) spfh
      READ (10) press
      READ (10) hpbl
      READ (10) albedo

C     Write out meteorological input 
      refi =  1
      refj = ny - 20
      OPEN(11, FILE="extracted", FORM="unformatted", STATUS="NEW")
      WRITE(11) shtfl(refi,refj), lhtfl(refi,refj), t2m(refi,refj), 
     1  lwdown(refi,refj), lwup(refi,refj), swup(refi,refj), 
     2  swdown(refi,refj), prate(refi,refj), ugrd(refi,refj), 
     3  vgrd(refi,refj), spfh(refi,refj), press(refi,refj), 
     4  hpbl(refi,refj), albedo(refi,refj)
      CLOSE(11, STATUS="KEEP")

      STOP
      END
