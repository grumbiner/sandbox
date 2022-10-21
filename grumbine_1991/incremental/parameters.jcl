/*
/FILE INPUT DATA
  .005       deltat
  .0750      DELTAX  (DX=500KM*DELTAX)
 2.50E-4     e
 1.25E-2     r
 2.00E-2     epsiln
 0.1         sigma
  13         xbox
  12         ybox
 2.000       END TIME IN YEARS
-0.5         initial s
.FALSE.      do you want heavy debugging output?
  10         print out info. every n time steps, this is n.
.TRUE.       DO YOU WANT TO START FROM PRIOR OUTPUT?
 100         end of winter
 150         start of summer
 180         end of summer
   9         yice
 1.0         qmax
/*
FORTVS KILL74 ( OPTIMIZE (2) )
GLOBAL TXTLIB VLNKMLIB VFORTLIB LINPAKS
EXEC LINKTO $C63026$ 302 307 G (W PASS= WPGFD
FILEDEF INPUT DISK INPUT DATA
FILEDEF U DISK U  DATA G
FILEDEF V DISK V  DATA G
FILEDEF S DISK S  DATA G
FILEDEF T DISK T  DATA G
FILEDEF Q DISK Q  DATA G
FILEDEF FLUX DISK FLUX  DATA G
CP QUERY TIME
LOAD KILL74  (START
CP QUERY TIME
/*
