      PROGRAM BOCO32
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BCTEND       GENERATE BOUNDARY TENDENCIES FOR ETA
C   PRGMMR: ROGERS           ORG: NP22        DATE: 97-12-30  
C
C ABSTRACT: READS IN VALUES OF BASIC STATE VARIABLES FROM ETA OI
C   ANALYSIS AND AVN FORECAST AT ETA MODEL BOUNDARY POINTS AND
C   COMPUTES BOUNDARY TENDENCIES.  THIS VERISON IS SPECIAL FOR THE
C   EDAS; IT ASSUMES YOU HAVE 3 H AVN FORECAST FILES.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T BLACK
C   95-02-07  E ROGERS ADDED DOCBLOCK
C   98-06-10  E ROGERS CONVERTED TO FORTRAN 90
C
C USAGE:
C
C   INPUT FILES:
C     UNIT17     - BOUNDARY VALUES FROM AVN FORECAST
C     UNIT18     - BOUNDARY VALUES FROM ETA ANALYSIS
C
C   OUTPUT FILES:
C     UNIT51     - PRECOMPUTED BOUNDARY TENDENCIES
C
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - BC6432
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN 90
C   MACHINE: CRAY YMP/C90
C
C$$$
C     ******************************************************************
       include "parmeta.res"
                             P A R A M E T E R
     & (NBCR=15
     &, IMT=IM*2-1,JMT=JM/2+1)
                             P A R A M E T E R
     & (LB=2*JMT+IMT-3
     &, LP1=LM+1,LM1=LM-1
     &, IMJM=IM*JM-JM/2)
                             P A R A M E T E R
     & (H1=1.E0)
C--------------------------------------------------------
      PARAMETER (EPSQ2=0.2)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IDAT(3)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & HPD2  (IMJM)
     &,HT2   (IMJM,LM),HQ2   (IMJM,LM)
     &,HU2   (IMJM,LM),HV2   (IMJM,LM)
     &,PDBA  (LB)
     &,TBA   (LB,LM),QBA   (LB,LM),UBA   (LB,LM),VBA   (LB,LM)
     &,PDBB  (LB)
     &,TBB   (LB,LM),QBB   (LB,LM),UBB   (LB,LM),VBB   (LB,LM)
     &,PDB   (LB,2)
     &,TB    (LB,LM,2),QB    (LB,LM,2),UB    (LB,LM,2),VB    (LB,LM,2)
     &,Q2B   (LB,LM,2),CWMB  (LB,LM,2)
C-----------------------------------------------------------------------
                             C O M M O N /IJHFLDS/
     & HPD2
     &,HT2   ,HQ2   ,HU2   ,HV2   ,RUN   ,IDAT  ,IHRST ,NTSD
C-----------------------------------------------------------------------
                             C O M M O N /BCHFLDS/
     & PDBA  ,TBA   ,QBA   ,UBA   ,VBA
C-----------------------------------------------------------------------
       character*4 envar
                             D A T A
     & TBOCO /3.E0/,SHR   /3600.E0/
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      CALL W3TAGB('BCTEND  ',0097,0364,0070,'NP22   ')
C
C
      RTBOCO=H1/(TBOCO*SHR)
C-----------------------------------------------------------------------
      DO L=1,LM
      DO K=1,LB
        Q2B(K,L,1)=EPSQ2
        CWMB(K,L,1)=0.
      ENDDO
      ENDDO
      DO L=1,LM
      DO K=1,LB
        Q2B(K,L,2)=0.
        CWMB(K,L,2)=0.
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C***
C***  PDB, TB, QB, UB, AND VB ARE BOUNDARY VALUES FROM THE ANALYSIS.
C***
cwas     READ(18) RUN,IDAT,IHRST,TBOCO
      READ(18) RUN,IDAT,IHRST
      READ(18) PDB,TB,QB,UB,VB
      REWIND 18
      WRITE(51) RUN,IDAT,IHRST,TBOCO
C-----------------------------------------------------------------------
                             C A L L   B C 6 4 3 2 (17)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  IF BETWEEN 0 AND 3 HRS, THE TENDENCY SHOULD BE GIVEN BY
C***  AVN(3HR)-ANALYSIS(0HR) NOT AVN(3HR)-AVN(0HR).
C***
C***  If running static off T-12 h AVN, set tmmark=tm00
C***  If running static off T00 h AVN, set tmmark=t00s
C***
      nhupd=3
      nhfcst=3
      ntimes=nhfcst/nhupd+1
      call getenv("tmmark",envar)
	if(envar.eq.'tm12') then
	  rtboco = 1.0 / (3.0 * shr)
        elseif(envar.eq.'tm09') then
	  rtboco = 1.0 / (3.0 * shr)
	  call bc6432 (17)
        elseif(envar.eq.'tm06') then
	  rtboco = 1.0 / (3.0 * shr)
	  call bc6432 (17)
	  call bc6432 (17)
        elseif(envar.eq.'tm03') then
	  rtboco = 1.0 / (3.0 * shr)
	  call bc6432 (17)
	  call bc6432 (17)
	  call bc6432 (17)
        elseif(envar.eq.'tm00') then
	  rtboco = 1.0 / (3.0 * shr)
	  ntimes =35
	  call bc6432 (17)
	  call bc6432 (17)
	  call bc6432 (17)
	  call bc6432 (17)
          nhupd=3
          nhfcst=72
          ntimes=nhfcst/nhupd+1
        elseif(envar.eq.'t00s') then
	  rtboco = 1.0 / (3.0 * shr)
          nhupd=3
          nhfcst=72
          ntimes=nhfcst/nhupd+1
        else
	  print *,"tmmark not set to t00s,tm00,tm03,tm06,tm09 or tm12"
	  call exit(13)
        end if
          print *,"tmmark =",envar
C
        DBCHR=REAL(NHUPD)
        BCHR=-DBCHR
C
	do k = 1,ntimes
          BCHR=BCHR+DBCHR
          if(k.eq.1)  then
            DO N=1,LB
              PDBB(N)=PDB(N,1)
            ENDDO
            DO L=1,LM
            DO N=1,LB
              TBB(N,L)=TB(N,L,1)
              QBB(N,L)=QB(N,L,1)
              UBB(N,L)=UB(N,L,1)
              VBB(N,L)=VB(N,L,1)
            ENDDO
            ENDDO
          ELSE
            DO N=1,LB
              PDBB(N)=PDBA(N)
            ENDDO
            DO L=1,LM
            DO N=1,LB
              TBB(N,L)=TBA(N,L)
              QBB(N,L)=QBA(N,L)
              UBB(N,L)=UBA(N,L)
              VBB(N,L)=VBA(N,L)
            ENDDO
            ENDDO
          ENDIF
C-----------------------------------------------------------------------
          CALL BC6432 (17)
C-----------------------------------------------------------------------
          DO N=1,LB
            IF(K.EQ.1) THEN
              PDB(N,2)=(PDBA(N)-PDBB(N))*RTBOCO
            ELSE
              PDB(N,1) = PDBB(N)
              PDB(N,2)=(PDBA(N)-PDBB(N))*RTBOCO
            ENDIF
          ENDDO
          DO L=1,LM
          DO N=1,LB
            IF(K.EQ.1) THEN
              TB(N,L,2)=(TBA(N,L)-TBB(N,L))*RTBOCO
              QB(N,L,2)=(QBA(N,L)-QBB(N,L))*RTBOCO
              UB(N,L,2)=(UBA(N,L)-UBB(N,L))*RTBOCO
              VB(N,L,2)=(VBA(N,L)-VBB(N,L))*RTBOCO
            ELSE
              TB(N,L,1) = TBB(N,L)
              QB(N,L,1) = QBB(N,L)
              UB(N,L,1) = UBB(N,L)
              VB(N,L,1) = VBB(N,L)
C
              TB(N,L,2)=(TBA(N,L)-TBB(N,L))*RTBOCO
              QB(N,L,2)=(QBA(N,L)-QBB(N,L))*RTBOCO
              UB(N,L,2)=(UBA(N,L)-UBB(N,L))*RTBOCO
              VB(N,L,2)=(VBA(N,L)-VBB(N,L))*RTBOCO
            ENDIF
          ENDDO
          ENDDO
C----------------------------------------------------------------------
C-----------------------------------------------------------------------
          WRITE(51) BCHR
          WRITE(51) PDB
          WRITE(51) TB
          WRITE(51) QB
          WRITE(51) UB
          WRITE(51) VB
          WRITE(51) Q2B
          WRITE(51) CWMB
C-----------------------------------------------------------------------
      ENDDO
C-----------------------------------------------------------------------
      WRITE(51) BCHR
      WRITE(51) PDB
      WRITE(51) TB
      WRITE(51) QB
      WRITE(51) UB
      WRITE(51) VB
      WRITE(51) Q2B
      WRITE(51) CWMB
C-----------------------------------------------------------------------
      CALL W3TAGE('BCTEND  ')
C-----------------------------------------------------------------------
                             STOP
                             END
