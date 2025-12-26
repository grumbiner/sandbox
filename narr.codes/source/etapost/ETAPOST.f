      PROGRAM ETAPOST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C MAIN PROGRAM: ETA_ETAPOST
C   PRGMMR: MANIKIN          ORG: NP22        DATE: 2000-02-01
C     
C ABSTRACT:  
C     THIS PROGRAM DRIVES THE EXTERNAL ETA MODEL POST PROCESSOR.
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON - CODED ETAPOST AS STAND ALONE CODE
C   98-03-06  BALDWIN/BLACK/ROGERS - MODIFIED TO POST MULTIPLE
C             FORECAST HOURS IN ONE EXECUTION; NUMBER AND 
C             FREQUENCY DETERMINED BY UNIT 5 INPUT CARD
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   00-02-04  JIM TUCCILLO - PARALLEL VERSION VIA MPI
C     
C USAGE:    ETAPOST
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - IOUNIT
C                RQSTFLD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM RS/6000 SP
C$$$  
C
C
C============================================================================================================
C
C     This is an MPI code. All array indexing is with respect to the global indices. Loop indices 
C     look as follows for N MPI tasks.
C
C
C
C  Original                                            New
C  Index                                               Index
C
C   JM ----------------------------------------------- JEND
C JM-1 -                                             - JEND_M
C JM-2 -               MPI TASK N-1                  - JEND_M2
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK N-2                  -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C
C                           .
C                           .
C                           .
C
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK 1                    -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             - 
C      -               MPI TASK 0                    - 
C    3 -                                             - JSTA_M2
C    2 -                                             - JSTA_M
C    1 ----------------------------------------------- JSTA
C
C     1                                              IM               
C
C
C     Jim Tuccillo
C     Jan 2000
C
C============================================================================================================
C     
C     INCLUDE ARRAY DIMENSIONS.
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "mpif.h"
C
C     DECLARE VARIABLES.
C     
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "OUTFIL.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "CTLBLK.comm"
C     
C     SET HEADER WRITER FLAGS TO TRUE.
c
      common/tim_info/ETAFLD2_tim,ETA2P_tim,SURFCE2_tim, CLDRAD_tim,
     *                MISCLN_tim,FIXED_tim
C
      common/jjt/time_output, time_e2out
      real(8) time_output, time_e2out, time_initpost, rtc, ist
      time_output = 0.
      time_e2out = 0.
      time_initpost = 0.
C     INITIALIZE MPI
C
      CALL MPI_FIRST
C
      ETAFLD2_tim = 0.0
      ETA2P_tim = 0.0
      SURFCE2_tim = 0.0
      CLDRAD_tim = 0.0
      MISCLN_tim =0.0
      FIXED_tim =  0.0
      bbtim = timef()
c
C     
C
      IF(ME.EQ.0)THEN
        CALL W3TAGB('ETA_ETAPOST',2000,0032,0094,'NP22')
      ENDIF
C
C
C**************************************************************************
C
C     START PROGRAM ETAPOST.
C
      IF(ME.EQ.0)THEN
        READ(5,*)ITAG,NRSTRT,NPINCR
      ENDIF
C
      CALL MPI_BCAST(ITAG  ,1,MPI_INTEGER,0,MPI_COMM_WORLD,IRTN)
      CALL MPI_BCAST(NRSTRT,1,MPI_INTEGER,0,MPI_COMM_WORLD,IRTN)
      CALL MPI_BCAST(NPINCR,1,MPI_INTEGER,0,MPI_COMM_WORLD,IRTN)
C
      CALL MPI_BARRIER(MPI_COMM_WORLD,IRTN)
C
C     LOOP OVER THE RESTRT FILES
C
      DO 1000 NR=1,NRSTRT
      LUNCO=19
      LUNLL=29
C
C     REWIND ALL INPUT FILE UNIT NUMBERS (FOR CONTROL FILE 
C     AND INTERPOLATION WEIGHTS)
C
      REWIND LCNTRL
      DO KER=19,39
        REWIND KER
      ENDDO
C
C     INITIALIZE POST COMMON BLOCKS 
C
      ist = rtc()
      CALL INITPOST
      time_initpost = time_initpost + rtc() - ist
      WRITE(STDOUT,*)'ETAPOST:  INITIALIZED POST COMMON BLOCKS'
C
C     LOOP OVER THE OUTPUT GRID(S).  FIELD(S) AND 
C     OUTPUT GRID(S) ARE SPECIFIED IN THE CONTROL 
C     FILE.  WE PROCESS ONE GRID AND ITS FIELDS 
C     AT A TIME.  THAT'S WHAT THIS LOOP DOES.
C     
 10   CONTINUE
C     
C        READ CONTROL FILE DIRECTING WHICH FIELDS ON WHICH
C        LEVELS AND TO WHICH GRID TO INTERPOLATE DATA TO.
C        VARIABLE IEOF.NE.0 WHEN THERE ARE NO MORE GRIDS
C        TO PROCESS.
C     
         CALL READCNTRL2(IEOF)
         WRITE(STDOUT,*)'ETAPOST:  RETURN FROM READCNTRL.  ',
     1        'IEOF=',IEOF
         IF (IEOF.NE.0) GOTO 20
C     
C        PROCESS SELECTED FIELDS.  FOR EACH SELECTED FIELD/LEVEL
C        WE GO THROUGH THE FOLLOWING STEPS:
C           (1) COMPUTE FIELD IF NEED BE
C           (2) INTERPOLATE FROM E-GRID TO OUTPUT GRID.
C           (3) WRITE FIELD TO OUTPUT FILE IN REQUESTED FORMAT.
C
         CALL PROCESS
         WRITE(STDOUT,*)' '
         WRITE(STDOUT,*)'ETAPOST:  PREPARE TO PROCESS NEXT GRID'
C     
C     PROCESS NEXT GRID.
C     
      GO TO 10
C     
C     ALL GRIDS PROCESSED.
C     
 20   CONTINUE
      WRITE(STDOUT,*)' '
      WRITE(STDOUT,*)'ALL GRIDS PROCESSED.'
      WRITE(STDOUT,*)' '
C
      ITAG=ITAG+NPINCR
 1000 CONTINUE
C
      print*, 'ETAFLD2_tim = ',  ETAFLD2_tim*1.0e-3
      print*, 'ETA2P_tim =  ',ETA2P_tim *1.0e-3
      print*, 'SURFCE2_tim =  ',SURFCE2_tim*1.0e-3
      print*, 'CLDRAD_tim =  ',CLDRAD_tim *1.0e-3
      print*, 'MISCLN_tim = ',MISCLN_tim*1.0e-3
      print*, 'FIXED_tim = ',FIXED_tim*1.0e-3
      print*, 'Total time = ',(timef() - bbtim) * 1.0e-3
      print*, 'Time for OUTPUT = ',time_output
      print*, 'Time for E2OUT = ',time_e2out
      print*, 'Time for INITPOST = ',time_initpost

C     
C     END OF PROGRAM.
C
C
      IF(ME.EQ.0)THEN
        CALL W3TAGE('ETA_ETAPOST')
      ENDIF
C
      CALL MPI_LAST
C
      STOP0
      END

