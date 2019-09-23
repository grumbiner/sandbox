C*******************************************************************
C
C     THE PARKINSON SEA-ICE MODEL
C
C     MODEL DESCRIPTION *********
C
C  THIS LARGE-SCALE SEA ICE MODEL SIMULATES THE YEARLY CYCLE OF ICE IN
C  BOTH THE NORTHERN AND SOUTHERN HEMISPHERES ( SEPERATE PRGRM. RUNS -
C  SEE BELOW ).  HORIZONTAL RESOLUTION IS APPROXIMATELY 200 KM., WHILE
C  THE MODEL HAS FOR VERTICAL LAYERS: ICE,SNOW,OCEAN AND ATMOSPHERE.
C  BOTH THERMODYNAMIC ( SUBROUTINE THERMO ) AND DYNAMIC ( SUBROUTINE
C  DNAMIC ) PROCESSES ARE INCORPORATED, THE THERMODYNAMICS BEING BASED O
C  ENERGY BALANCES AT THE VARIOUS INTERFACES AND THE DYNAMICS BEING BASE
C  ON THE FOLLOWING FIVE STRESSES: WIND STRESS, WATER STRESS, CORIOLIS
C  FORCE, INTERNAL ICE RESISTANCE, AND THE STRESS FROM THE TILT OF THE
C  SEA SURFACE ( DYNAMIC TOPOGRAPHY ).  EACH MODEL GRID SQUARE HAS A
C  VARIABLE PERCENTAGE OF ITS AREA ASSUMED ICE FREE, THE GRID SIZES
C  BEING 42X31 FOR THE NORTHERN HEMISPHERE AND 41X41 FOR THE SOUTHERN
C  HEMISPHERE.  FOR FURTHER INFORMATION SEE:
C
C     ' A LARGE-SCALE NUMERICAL MODEL OF SEA ICE '
C     CLAIRE L. PARKINSON AND WARREN M. WASHINGTON
C     JOURNAL OF GEOPHYSICAL RESEARCH,   JANUARY 20, 1979
C
C  ******************************************************************
C
C     TO INITIALIZE THE PROGRAM :
C
C               FOR ARCTIC CASE, SET IHEMI=1
C               FOR ANTARCTIC, SET IHEMI=2
C
C               VERIFY THAT THE PROPER INPUT TAPE IS SPECIFIED IN
C               THE JCL (SEE COMMENT CARDS SURROUNDING JCL).
C               (INPUT FILE = FT08F001).
C
C               SPECIFY THE START AND END YEARS FOR THE
C               SIMULATION USING (NYMIN) AND (NYMAX), RESPECTIVELY.
C
C               JCL CHANGES AND AN APPROPRIATE INPUT TAPE ARE NEEDED
C               FOR RUNS WITH NYMIN > 1 .
C
C               IF OUTPUT ARRAYS ARE TO BE WRITTEN TO TAPE,
C               SET NYTAPE= THE DESIRED YEAR (ARRAYS ARE WRITTEN
C               AT YEAR'S END). JCL CHANGES WILL BE NEEDED.
C
C               FOR NO TAPE OUTPUT, SET NYTAPE > NYMAX, AND DUMMY OUT
C               THE OUTPUT FILE (= FT04F001).
C
C**********************************************************************
C
      IMPLICIT none
      INTEGER IHEMI, NYMIN, NYMAX, NYTAPE, ITOTAL, JTOTAL
      INTEGER IT, JT

      IHEMI=1
      NYMIN=1
      NYMAX=4
      NYTAPE=99
      IF(IHEMI.EQ.2) GO TO 10
      ITOTAL=42
      JTOTAL=31
      GO TO 20
   10 CONTINUE
      ITOTAL=41
      JTOTAL=41
   20 CONTINUE
      IT=ITOTAL
      JT=JTOTAL
      CALL MAINPG(IHEMI,IT,JT,NYMIN,NYTAPE,NYMAX)
      STOP
      END
