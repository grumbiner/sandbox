C$$$  sub  PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C sub  PROGRAM:  gribst      UNPACK GLOBAL Grib SST
C   PRGMMR: GERALD           ORG: W/NMC21     DATE: 95-08-01
C
C ABSTRACT: DECODES GLOBAL AVN, FNL, OR MRF GRIB GRIDDED FIELDS.
C
C PROGRAM HISTORY LOG:
C   95-08-01  GERALD
C   96-07-23  PETERS - UPDATED FOR USE WITH ETA MODEL, GETS 
C                      REYNOLDS 1X1 OISST 
C
C
C USAGE:
C   INPUT FILES:
C     DDNAME1  - GENERIC NAMES & USAGE
C     FTNNF001 - START IN COL 7
C              - BUT TAB CONTINUATIONS TO LINE UP LIKE THIS
C     FXN      - LINE UP DASHES WHEN DESCRIBING FILES
C     TAPENN   - CRAY TYPE DESIGNATION
C     UNITNN   - CRAY TYPE DESIGNATION
C     PARM     - IF PARM FIELD IS USED, DESCRIBE HERE
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     FTMMF001 - NAMES & USAGE AS ABOVE IN THE INPUT SECTION
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - ROUTINES THAT ACCOMPANY SOURCE FOR COMPILE
C     LIBRARY:
C       COMMON   - LIST COMMON LIBRARY ROUTINES, E.G., CONSOL
C       W3LIB    -
C       W4LIB    - DELETE THE CORRESPONDING LINE OR LINES
C       GRAPHICS - IF LIBRARY IS UNNEEDED
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - TROUBLE OR SPECIAL FLAG - SPECIFY NATURE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  CRAY
C
C$$$
C
      subroutine gribst(insst,indxst,gsst,ierr,idate)
c
      Parameter(jf=360*180)
      INTEGER PDS,GDS,GRID
      INTEGER JPDS(25),JGDS(22),IGRD(5,3)
      INTEGER KPDS(25),KGDS(22)
      REAl FLD(jf),out(360,180)
      dimension gsst(361,180)
      dimension idate(4)
      logical*1 lb(jf)
      equivalence(out(1,1),fld(1))
c
C
C                sst
      DATA IGRD/ 11, 34,  2, 52, 11,
     *            1,105,102,105,105,
     *            0, 10,  0,  2,  2/
c
C            INPUT UNITS FOR DECODING GRIB FILE
C
          LUGB=INSST
c         LUGI=INDXST
          lugi=0
          ierr = 0
          j = 0
C
C........    DECODE THE FIELDS
C
        DO 30 GRID = 1,1
C
          DO 10 PDS=1,25
          JPDS(PDS) = -1
   10      CONTINUE
C
             DO 20 GDS = 1,22
           JGDS(GDS) = -1
   20     CONTINUE
C
C........   GET SELECT FIELDS
C
           jPDS(5) = IGRD(GRID,1)
           jPDS(6) = IGRD(GRID,2)
           jPDS(7) = IGRD(GRID,3)
           print*,'idate=',idate
           icent = (idate(4) - 1) / 100 + 1
           jpds(8) = idate(4) - (icent-1)*100
           jpds(9) = idate(2)
           jpds(10)= idate(3)
c          jpds(21)=icent
           print*,'sst jpds=',jpds
C
             print *,'call getgb'
           LUGI = 0
           CALL GETGB(LUGB,LUGI,JF,j,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB,Fld,IRET)
C
          IF(IRET.NE.0) THEN
            ierr = 1
            WRITE(6,60)IRET
            GO TO 999
          ENDIF
   60  FORMAT(1X,' IRET =',I5)
       do jj=1,180
       do kk=1,360
       gsst(kk,jj)=out(kk,jj)
       enddo
       enddo
c
c The following lines are commented out for the Regional Reanalysis
c because the climate SST is already oriented S->N.  In operations,
c the SST comes in oriented N->S, so that is why this code exists.
c Perry Shafran - 21 December 2001
c
c..   flip grid to PT(1,1) =(0e,-90.0)
c
c             do  jj= 1,180
c               do  kk= 1,360
c                gsst(kk,180-jj+1) = out(kk,jj)
c               end do
c             end do
c
c...   add greenich to right side of grid
c
             do jj = 1,180
               gsst(361,jj) = gsst(1,jj)
             end do
c
            WRITE(6,61)KPDS,KF,KGDS
   61  FORMAT(2(/,2X,'PDS=',13I7),2(/,2X,' GDS=',11I7 ))
C
c Check to see that point (1,1) is located at (0, 90S)
c
       if(kgds(4).ne.-89500.or.kgds(5).ne.500) then
        print*,'*** WARNING ***'
        print*,'SST ORIGIN POINT NOT AT (0 LON, 90 S LAT)'
        print*,'GRDETA TERMINATING IN GRIBST'
        stop312
       endif
C
   30        CONTINUE
C
  999           CONTINUE
             return
             END
