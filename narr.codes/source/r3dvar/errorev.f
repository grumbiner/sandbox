      SUBROUTINE ERROREV(NFIN,NFOUT,IUNITERR,mype,npes)

      INTEGER      LEV(255), NEV(255), IV(4000), NLEV(0:4)
      REAL(4)      ETIM(4000), EP(4000), ETYP(4000), PCS
      REAL(4)      EQM(4000), ERC(4000)
      REAL(8)      HDR(3), OBSM(13,255), OBSW(6,255)
      LOGICAL      WIND, ENDIN
      CHARACTER*8  EID(4000), CDR(3)
      EQUIVALENCE  (HDR,CDR)

      include 'mpif.h'
      include "my_comm.h"

      write(0,*)' errorev--iuniterr: ',iuniterr

      IF(MYPE.NE.0) RETURN

!  READ THE EVENTS
!  ---------------

      CALL RDEVENTS(NEVENT,EID,ETIM,EP,ETYP,EQM,ERC,IV,IUNITERR,mype,npes)

!  GET SOME DATA OR RETURN IF FINISHED
!  -----------------------------------

   10 CALL INPUT(HDR,OBSM,OBSW,NLEV,PCS,NFIN,NFOUT,ENDIN,WIND)
      IF(ENDIN) THEN
        CALL OUTPUT(NFIN,NFOUT,ENDIN)
        RETURN
      ENDIF

!  SEE IF THERE ARE EVENTS FOR THESE DATA
!  --------------------------------------

      CALL MATCH(LEV,NEV,NIE,HDR,OBSM,OBSW,NLEV,EID,ETIM,EP,  &
     &  ETYP,NEVENT)

!  FORM PROFILE OF EVENTS FOR THIS OBSERVATION
!  -------------------------------------------

      CALL FORMEVENT(LEV,NEV,IV,NIE,NLEV,OBSM,OBSW,EQM,PCS,ERC,NFOUT)

!  COMPLETE WRITING OF THESE DATA
!  ------------------------------

      CALL OUTPUT(NFIN,NFOUT,ENDIN)

!  RETURN FOR MORE DATA
!  --------------------

      GOTO 10

      END

!**********************************************************************

      SUBROUTINE FORMEVENT(LEV,NEV,IV,NIE,NLEV,OBSM,OBSW,EQM,PCS,ERC,NFOUT)

!  FORM EVENT ARRAY
!  ----------------

      INTEGER      LEV(255), NEV(255), IV(4000), NLEV(0:4)
      REAL(4)      EQM(4000), ERC(4000)
      REAL(8)      HDR(3), OBSM(13,255), OBSW(6,255), EVNSW(5,255),  &
     &             EVNP(4,255), EVNT(4,255), EVNZ(4,255), EVNQ(4,255)
      LOGICAL      WINDEVENT
      CHARACTER*40 PEVN,TEVN,ZEVN,TDEVN,WEVN
      DATA PEVN   /'POB PQM PPC PRC                         '/
      DATA TEVN   /'TOB TQM TPC TRC                         '/
      DATA ZEVN   /'ZOB ZQM ZPC ZRC                         '/
      DATA TDEVN  /'TDO QQM QPC QRC                         '/
      DATA WEVN   /'UOB VOB WQM WPC WRC                     '/
      DATA BMISS /10E10/

      EVNP  = BMISS
      EVNT  = BMISS
      EVNZ  = BMISS
      EVNQ  = BMISS
      EVNSW = BMISS

!     PRINT *,'SUBROUTINE FORMEVENT ENTERED'

      IF(NIE.LE.0) RETURN
      WINDEVENT = .FALSE.

      DO L=1,NIE
        N = NEV(L)
        PRINT *,' FORMEVENT--N,IV: ',L,IV(N)
        IF(IV(N).EQ.33) THEN
          EVNSW(1,LEV(L)) = OBSW(3,LEV(L))
          EVNSW(2,LEV(L)) = OBSW(4,LEV(L))
          EVNSW(3,LEV(L)) = EQM(N)
          EVNSW(4,LEV(L)) = PCS
          EVNSW(5,LEV(L)) = ERC(N)
          WINDEVENT = .TRUE.
        ELSEIF(IV(N).EQ.3) THEN
          EVNZ(1,LEV(L)) = OBSM(3,LEV(L))
          EVNZ(2,LEV(L)) = EQM(N)
          EVNZ(3,LEV(L)) = PCS
          EVNZ(4,LEV(L)) = ERC(N)
        ELSEIF(IV(N).EQ.6 .OR. IV(N).EQ.7) THEN
          EVNQ(1,LEV(L)) = OBSM(IV(N),LEV(L))
          EVNQ(2,LEV(L)) = EQM(N)
          EVNQ(3,LEV(L)) = PCS
          EVNQ(4,LEV(L)) = ERC(N)
        ELSEIF(IV(N).EQ.4) THEN
          EVNT(1,LEV(L)) = OBSM(IV(N),LEV(L))
          EVNT(2,LEV(L)) = EQM(N)
          EVNT(3,LEV(L)) = PCS
          EVNT(4,LEV(L)) = ERC(N)
        ENDIF
      ENDDO

!  WRITE EVENTS TO NFOUT
!  ---------------------
        
      IF(WINDEVENT) THEN
        CALL UFBINT(NFOUT,EVNSW,5,NLEV(0),IRET,WEVN)
        WRITE(6,500) NIE,IRET
        DO J=1,NIE
          WRITE(6,502) (EVNSW(I,LEV(J)),I=1,5)
        ENDDO
  500   FORMAT(' WIND EVENT WRITTEN WITH ',I4,' LEVELS.  IRET:',I5)
  502   FORMAT(5(1X,F8.1))
  503   FORMAT(4(1X,F8.1))
      ELSE
        CALL UFBINT(NFOUT,EVNZ,4,NLEV(1),IRET,ZEVN)
        WRITE(6,501) NLEV(1),IRET
        DO J=1,NIE
          WRITE(6,503) (EVNZ(I,LEV(J)),I=1,4)
        ENDDO
        CALL UFBINT(NFOUT,EVNT,4,NLEV(1),IRET,TEVN)
        DO J=1,NIE
          WRITE(6,503) (EVNT(I,LEV(J)),I=1,4)
        ENDDO
        CALL UFBINT(NFOUT,EVNQ,4,NLEV(1),IRET,TDEVN)
        DO J=1,NIE
          WRITE(6,503) (EVNQ(I,LEV(J)),I=1,4)
        ENDDO
  501   FORMAT(' MASS EVENT WRITTEN WITH ',I4,' LEVELS.  IRET:',I5)
      ENDIF

      RETURN
      END

!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    INPUT       READ INPUT FOR ONE STATION
!   PRGMMR: W. COLLINS       ORG: W/NP22     DATE: 1999-08-31
!
! ABSTRACT: Read input for one station.
!
! PROGRAM HISTORY LOG:
! 1999-08-31  W. COLLINS
!
! USAGE:    CALL INPUT(HDR,OBSM,OBSW,NLEV,PCS,NFIN,NFOUT,ENDIN,WIND)
!   INPUT ARGUMENT LIST:
!     NFIN     - INPUT FILE NO.
!     NFOUT    - OUTPUT FILE NO.
!
!   OUTPUT ARGUMENT LIST:
!     OBSM     - MASS OBSERVATION VALUES
!     OBSW     - WIND OBSERVATION VALUES
!     HDR      - HEADER: SID,DHR,TYP
!     NLEV     - NO. OF OBSERVATION LEVELS
!     PCS      - PROGRAM CODE NUMBER, USED IN WRITING EVENTS
!     ENDIN    - LOGICAL, .TRUE. WHEN ALL DATA IS READ
!
!   INPUT FILES:
!     fort.NFIN - INPUT BUFR FILE
!
!   OUTPUT FILES:
!     fort.NFOUT - OUTPUT BUFR FILE
!     fort.06  -  PRINTOUT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$

      SUBROUTINE INPUT(HDR,OBSM,OBSW,NLEV,PCS,NFIN,NFOUT,ENDIN,WIND)
      SAVE
      INTEGER      NLEV(0:4)
      REAL(8)      OBSM(13,255), OBSW(6,255), HDR(3), HHDR(3)
      REAL(8)      OBSM1(6,255), OBSM7(5,255), OBSM12(255), OBSM13(255)
      CHARACTER*8  SUBSET, PRGS(13), OSTRM3, OSTRM4, CDR(3)
      CHARACTER*10 CDATE
      CHARACTER*12 HSTR
      CHARACTER*24 OSTRW, OSTRM1, OSTRM2
      LOGICAL      WIND, START, ENDIN
      EQUIVALENCE (HHDR,CDR)
      DATA HSTR   /'SID DHR TYP '/
      DATA OSTRM1 /'POB PMO ZOB TOB TDO QOB '/
      DATA OSTRM2 /'PWO PW1O PW2O PW3O PW4O '/
      DATA OSTRM3 /'TMSK    '/
      DATA OSTRM4 /'TMBR    '/
      DATA OSTRW  /'POB ZOB UOB VOB DDO FFO '/

      DATA BMISS  /10E10/
      DATA START  /.TRUE./

      WIND  = .FALSE.
      ENDIN = .FALSE.
      OBSM  = BMISS
      OBSM1  = BMISS
      OBSM7  = BMISS
      OBSM12  = BMISS
      OBSM13  = BMISS
      OBSW  = BMISS
      HHDR  = HDR

!  OPEN THE INPUT FILE
!  -------------------

      IF(START) THEN
        CALL DATELEN(10)
        CALL CLOSBF(NFIN)
        REWIND NFIN
        CALL OPENBF(NFIN,'IN',NFIN)
        CALL READMG(NFIN,SUBSET,IDATE,IRETMG)
        IF(IRETMG.NE.0) GOTO 6
        WRITE(CDATE,'(I10)') IDATE
        PRINT*,'DATA VALID AT ',CDATE
        CALL UFBQCD(NFIN,'R3DVAR',PCS)
        PRINT *,' PCS: ',PCS
        CALL CLOSBF(NFIN)
        CALL OPENBF(NFIN,'IN',NFIN)
        CALL OPENBF(NFOUT,'OUT',NFIN)
        START = .FALSE.
        IRETSB = 1
        PRINT *,'FINISHED SETTING UP TO READ INPUT FILE'
      ENDIF

!  READ A REPORT
!  -------------

    5 CONTINUE
      IF(IRETSB.NE.0) THEN
        CALL READMG(NFIN,SUBSET,IDATE,IRETMG)
!       PRINT *,'REPORT READ'
      ENDIF
    6 IF(IRETMG.NE.0) THEN
        ENDIN = .TRUE.
!       CALL CLOSBF(NFIN)
!       CALL CLOSBF(NFOUT)
!       PRINT *,'FILES CLOSED: ',NFIN,NFOUT
        RETURN
      ENDIF

!  UNPACK A SUBSET
!  ---------------

   10 CONTINUE
      CALL OPENMB(NFOUT,SUBSET,IDATE)
      CALL READSB(NFIN,IRETSB)
!     PRINT *,'SUBSET UNPACKED'
      IF(IRETSB.NE.0) GOTO 5
      CALL UFBCPY(NFIN,NFOUT)
!     PRINT *,'SUBSET COPIED FROM ',NFIN,' TO ',NFOUT

      CALL UFBINT(NFIN,HDR,3,  1,IRET,HSTR)
!     PRINT *,'HEADER UNPACKED: ',CDR(1),HDR(2),HDR(3)

      TYP = HDR(3)
!     PRINT *,'TYPE = ',TYP
      IF(TYP.GE.BMISS) STOP 'TYP BAD'

      NLEV = 0
      WIND = TYP.GE.200.
      IF(WIND) THEN
!       PRINT *,'READY TO UNPACK WIND REPORT'
        CALL UFBINT(NFIN,OBSW,6,255,NLEV(0),OSTRW)
!       PRINT *,NLEV,' LEVELS OF WIND REPORT UNPACKED'
      ELSE
!       PRINT *,'READY TO UNPACK MASS REPORT'
        CALL UFBINT(NFIN,OBSM1,6,255,NLEV(1),OSTRM1)
        CALL UFBINT(NFIN,OBSM7,5,255,NLEV(2),OSTRM2)
        CALL UFBINT(NFIN,OBSM12,1,255,NLEV(3),OSTRM3)
        CALL UFBINT(NFIN,OBSM13,1,255,NLEV(4),OSTRM4)
        DO L=1,NLEV(1)
          DO J=1,6
            OBSM(J,L) = OBSM1(J,L)
          ENDDO
        ENDDO
        DO L=1,NLEV(2)
          DO J=1,5
            OBSM(J+6,L) = OBSM7(J,L)
          ENDDO
        ENDDO
        DO L=1,NLEV(3)
          OBSM(12,L) = OBSM12(L)
        ENDDO
        DO L=1,NLEV(4)
          OBSM(13,L) = OBSM13(L)
        ENDDO
!       PRINT *,NLEV,' LEVELS OF MASS REPORT UNPACKED'
      ENDIF

      RETURN
      END

!**********************************************************************

      SUBROUTINE RDEVENTS(NEVENT,EID,ETIM,EP,ETYP,EQM,ERC,IV,IUNITERR,mype,npes)

!  READ THE GROSS ERROR EVENTS AND PUT INTO SINGLE ARRAYS
!  ASSIGN QUALITY MARK AND REASON CODE TO GROSS ERRORS
!  ------------------------------------------------------ 

      REAL(4) ETIM(4000), EP(4000), ETYP(4000), EQM(4000), ERC(4000)
      INTEGER IV(4000), IVV(5)
      CHARACTER*8 EID(4000)
      DATA IVV /3,7,6,4,33/     !     z,pw,q,T,wind for IVAR=1,2,3,4,5

      include 'mpif.h'
         include "my_comm.h"
      write(0,*)' rdevents--npes: ',npes

      J = 0
      NEVENT = 0
      NUM = 0
      write(0,*)' rdevents--before loop to read events from unit', &
        iuniterr,' ...'
      DO N=0,NPES-1
        IUNIT = IUNITERR + N
        open(iunit,form='unformatted')
        write(0,*)' rdevents--n,iunit',n,iunit
   10   CONTINUE
          READ(IUNIT,END=20,ERR=200) NUM, IVAR
          PRINT *,'RDEVENTS--N,NUM,IVAR:',N,NUM,IVAR
          write(0,*)'RDEVENTS--N,NUM,IVAR:',N,NUM,IVAR
          IF(NUM.GT.4000) THEN
            PRINT *,'TOO MANY EVENTS'
            write(0,*)' rdevents--TOO MANY EVENTS'
            STOP
          ENDIF
          READ(IUNIT) (EID(J+I),EP(J+I),ETIM(J+I),ETYP(J+I),I=1,NUM)
          WRITE(6,500) (EID(J+I),EP(J+I),ETIM(J+I),ETYP(J+I),I=1,NUM)
  500     FORMAT(' EVENTS:',/,(1X,A8,F8.1,1X,F8.2,1X,F6.0))
          DO I=1,NUM
            EQM(J+I) = 13.
            ERC(J+I) = 9. + IVAR/10
            IV(J+I)  = IVV(MOD(IVAR,10))
          ENDDO
          J = J+NUM
        GOTO 10
   20   CONTINUE
      ENDDO
      NEVENT = J+NUM
      PRINT *,NEVENT,' EVENTS READ SUCCESSFULLY'
      write(0,*)' rdevents--events read successfully: ',nevent
              close(iunit)
      RETURN

  200 CONTINUE
              close(iunit)
      PRINT *,'ERROR READING STORED GROSS ERRORS for IT =', IT
      write(0,*)' rdevents--ERROR READING STORED GROSS ERRORS for IT =', IT

      RETURN
      END

!**********************************************************************

      SUBROUTINE MATCH(LEV,NEV,NIE,HDR,OBSM,OBSW,NLEV,EID,ETIM,EP,   &
     &  ETYP,NEVENT)         

!  THIS SUBROUTINE MATCHES THIS OBSERVATION TO STORED EVENTS.
!  IT IS NECESSARY TO FIND ALL ERRORS MATCHED FOR THE PRESENT OB
!  THE SUBROUTINE RETURNS LEV, NEV, NIE.
!  NIE TELLS THE NUMBER OF MATCHED EVENTS FOR THIS OBSERVATION
!  LEV IS A LIST OF THE LEVELS MATCHED
!  NEV IS A LIST OF THE EVENT INDICES FOR MATCHES
!  -------------------------------------------------------------

      INTEGER      LEV(255), NEV(255), NLEV(0:4)
      REAL(4)      ETIM(4000), EP(4000), ETYP(4000), TYPLIST(200)
      REAL(8)      HDR(3), OBSM(13,255), OBSW(6,255), HHDR(3)
      CHARACTER*8  EID(4000), CDR(3), CEID(4000)
      LOGICAL      FIRST, WIND
      EQUIVALENCE  (HHDR,CDR)
      DATA FIRST /.TRUE./
      SAVE

      NEV = 0
      LEV = 0
      NIE = 0
      HHDR = HDR

!  FIRST TIME, MAKE MASTER LIST OF TYPES WITH ERRORS
!  -------------------------------------------------

      IF(FIRST) THEN
        NLIST = 0
        TYPLIST = 0.
        IF(NEVENT.GT.0) THEN
          DO I=1,NEVENT
            IF(NLIST.GT.0 .AND. NLIST.LE.200) THEN
              DO L=1,NLIST
                IF(ETYP(I).EQ.TYPLIST(L)) GOTO 10
              ENDDO
            ENDIF
            NLIST = NLIST + 1
            TYPLIST(NLIST) = ETYP(I)
   10     ENDDO
        ENDIF
        FIRST = .FALSE.
        PRINT *,'MASTER LIST OF TYPES OF ERRORS MADE WITH NLIST =',NLIST
        WRITE(6,500) (TYPLIST(I),I=1,NLIST)
  500   FORMAT(10F8.0)
      ENDIF


!  CHECK FOR MATCHING TYPE, STATION ID, TIME, AND PRESSURE
!  -------------------------------------------------------

!     PRINT *,' MATCH--NLIST: ',NLIST
      IE = 0
      IF(NLIST.GT.0) THEN
      DO I=1,NLIST
        IF(ABS(HDR(3)-TYPLIST(I)).LT.0.1) THEN
!         WRITE(6,502) CDR(1),HDR(2),HDR(3)
  502     FORMAT(' ACCEPTABLE TYPE FOR: ',A8,1X,2(F8.2,1X))
          WIND = HDR(3).GE.200.
          DO J=1,NEVENT
            IF(EID(J).EQ.CDR(1) .AND.   &
     &         ABS(ETYP(J)-HDR(3)).LT.0.1 .AND.   &
     &         ABS(ETIM(J)-HDR(2)).LT.0.01) THEN
!             WRITE(6,501) EID(J),ETIM(J),ETYP(J),EP(J)
  501         FORMAT('PARTIAL MATCH FOR: ',A8,1X,F6.2,1X,F6.2,' PRESSURE: ',F8.1)
              IF(WIND) THEN
                DO K=1,NLEV(0)
                  IF(ABS(EP(J)-OBSW(1,K)).LT.0.1) THEN
                    IE = IE + 1
                    IF(IE.GT.255) THEN
                      PRINT *,'EVENT LEVEL GREATER THAN 255'
                      STOP
                    ENDIF
                    NIE = IE
                    LEV(IE) = K
                    NEV(IE) = J
                    WRITE(6,503) EID(J),ETIM(J),ETYP(J),K
  503               FORMAT('WIND MATCH FOR: ',A8,1X,F6.2,1X,F6.2,' LEVEL: ',I4)
                    GOTO 20
                  ENDIF
                ENDDO
              ELSE
                WRITE(6,506) (NLEV(K),K=1,4)
  506           FORMAT(' NLEVs: ',4I5)
                DO K=1,MAX(NLEV(1),NLEV(2),NLEV(3),NLEV(4))
                  IF(ABS(EP(J)-OBSM(1,K)).LT.0.1) THEN
                    IE = IE + 1
                    IF(IE.GT.255) THEN
                      PRINT *,'EVENT LEVEL GREATER THAN 255'
                      STOP
                    ENDIF
                    NIE = IE
                    LEV(IE) = K
                    NEV(IE) = J
                    WRITE(6,505) EID(J),ETIM(J),ETYP(J),K
  505               FORMAT('MASS MATCH FOR: ',A8,1X,F6.2,1X,F6.2,' LEVEL: ',I4)
                    GOTO 20
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
   20     ENDDO
        ENDIF
      ENDDO
      ENDIF
      IF (NIE.GT.0) WRITE(6,504) NIE
  504 FORMAT(' TOTAL EVENTS FOR THIS OBSERVATION: ',I5)

      RETURN
      END

!**********************************************************************

      SUBROUTINE OUTPUT(NFIN,NFOUT,ENDIN)
      LOGICAL ENDIN

!     PRINT *,'OUTPUT--NFIN,NFOUT,ENDIN: ',NFIN,NFOUT,ENDIN
      IF(ENDIN) THEN
        CALL CLOSBF(NFIN)
        CALL CLOSBF(NFOUT)
        PRINT *,'FILES CLOSED: ',NFIN,NFOUT
      ELSE
        CALL WRITSB(NFOUT)
      ENDIF

      RETURN
      END
