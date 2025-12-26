      program rstupt
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: RSTUPT      update eta restart file
C   PRGMMR: ROGERS           ORG: NP22        DATE: 1998-04-06
C
C ABSTRACT: Read in GDAS guess restart file (which has updated TG,SNO,SI)
C   and add these updated variables to the EDAS restrt file for continuous
C   cycling of EDAS
C
C PROGRAM HISTORY LOG:
C   94-07-25  Dennis Deaven wrote original grdeta code
C   95-11-27  Eric Rogers modified grdeta for eta guess
C   96-03-20  Rogers/Mitchell reset stc under new sea ice (get new sea
C             ice mask from nhb file from grdeta)
C   96-04-22  Eric Rogers modified code to get correct TG from nhb file
C             (TG from restrt03.gdas is set to zero in ECONVK)
C   97-04-02  Eric Rogers modified code to update snow at specified input
C             time only
C   97-06-09  Eric Rogers added include of parmsoil for 4-layer soil
C   97-10-17  Eric Rogers added ability to use GDAS for basic state 
C             variables and EDAS for everything else (Partial cycling)
C   99-01-21  Eric Rogers converted to 2-d indexing
C   00-10-02  Eric Rogers removed bug in 22km version which removed
C             full cycling of cloud water and TKE if PARTIAL=TRUE
C   01-02-01  Eric Rogers added logical variable NEWSNOW, which if TRUE
C             will update SNO array with new snow analysis. The decision
C             on what cycle the snow analysis is updated is done in the
C             job script
C   01-02-25  Eric Rogers added new state variables from upgraded land-
C             surface package : 
C                  SH2O (non-frozen soil moisture)
C                  SI (actual snow depth) replaces WET
C                  ALBEDO 
C
C USAGE:
C   INPUT FILES:
C     fort.11  - GDAS guess restrt file
C     fort.12  - EDAS guess restrt file
c     fort.13  - nhb file from grdeta
c     fort.14  - nmcdate file
c     fort.15  - namelist for updating SNO array
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     fort.51  - EDAS restrt file w/updated SI,SNO,TG,ALBEDO
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE:  fortran90
C   MACHINE:   CRAY
C
C$$$
C----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
      INCLUDE "parmeta.res"
      INCLUDE "parmsoil"
C
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::U,V,T,Q,OMEGA,DUM3D
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::CWMEDS,Q2EDS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::UGBL,VGBL,TGBL,QGBL
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::TRAIN,TCUCN,TTND
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::Q2,CWM
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMT=2*IM-1,JMT=JM/2+1)
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3,LP1=LM+1
     &,LMP1=LM+1,LMM1=LM-1)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & KDATE(4),SINT(LP1),SL(LM)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IDAT(3),DETA  (LM),AETA  (LM),ETA   (LP1),DFL   (LP1),IDATGB(3)
C
     &,RES   (IM,JM),FIS   (IM,JM)
     &,SNO   (IM,JM),SST   (IM,JM),SI    (IM,JM)
     &,SM    (IM,JM),LMH   (IM,JM),LMV   (IM,JM)
     &,smc(im,jm,nsoil),stc(im,jm,nsoil),cmc(im,jm)
     &,sice(im,jm)
C-----------------------------------------------------------------------
                             L O G I C A L * 1
     & PARTIAL, NEWSNOW
                             L O G I C A L 
     & RUN, first
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & PDB   (LB,2)
     &,TB    (LB,LM,2),QB    (LB,LM,2)
     &,UB    (LB,LM,2),VB    (LB,LM,2)
     &,PD    (IM,JM)
     &,radin (im,jm)   ,radot (im,jm)
     &,tg    (im,jm)   ,z0    (im,jm)   ,akms  (im,jm)   ,czen  (im,jm)
     &,ths   (im,jm)   ,qs    (im,jm)   ,twbs  (im,jm)   ,qwbs  (im,jm)
     &,akhs  (im,jm)   ,hbot  (im,jm)   ,cfracl(im,jm)   ,thz0  (im,jm)
     &,qz0   (im,jm)   ,uz0   (im,jm)   ,vz0   (im,jm)   ,ustar (im,jm)
     &,htop  (im,jm)   ,cfracm(im,jm)   ,cldefi(im,jm)   ,rf    (im,jm)
     &,pslp  (im,jm)   ,cuppt (im,jm)   ,cfrach(im,jm)   ,soiltb(im,jm)
     &,sfcexc(im,jm)   ,smstav(im,jm)   ,smstot(im,jm)   ,grnflx(im,jm)
     &,pctsno(im,jm)   ,prec  (im,jm)   ,acprec(im,jm)   ,accliq(im,jm)
     &,cuprec(im,jm)   ,acfrcv(im,jm)   ,ncfrcv(im,jm)   ,acfrst(im,jm)
     &,ncfrst(im,jm)   ,acsnow(im,jm)   ,acsnom(im,jm)   ,ssroff(im,jm)
     &,bgroff(im,jm)   ,iwxtyp(im,jm)   ,sfcshx(im,jm)   ,sfclhx(im,jm)
     &,subshx(im,jm)   ,snopcx(im,jm)   ,sfcuvx(im,jm)   ,sfcevp(im,jm)
     &,potevp(im,jm)   ,aswin (im,jm)   ,aswout(im,jm)   ,aswtoa(im,jm)
     &,alwin (im,jm)   ,alwout(im,jm)   ,alwtoa(im,jm)
     &,pdomg(im,jm),resomg(im,jm),resorg(im,jm)
     &,fisorg(im,jm),resout(im,jm)

                             D I M E N S I O N
     & swnet(im,jm), elwdwn(im,jm), czmean(im,jm), sigt4(im,jm)
     &,u00(im,jm),ul(2*lm),lc(im,jm),sr(im,jm),utim(2500)
     &,th10(im,jm),q10(im,jm),u10(im,jm),v10(im,jm)
     &,th30(im,jm),q30(im,jm),u30(im,jm),v30(im,jm)
     &,tshltr(im,jm),pshltr(im,jm),qshltr(im,jm)
     &,dum(im,jm),pdgbl(im,jm)
C
                             D I M E N S I O N
     & TGNEW(IM,JM),SNONEW(IM,JM),SINEW(IM,JM)
     &,RESNEW(IM,JM),FISNEW(IM,JM),SNOEDS(IM,JM),SIEDS(IM,JM)
C
                             D I M E N S I O N
     & SH2O(IM,JM,NSOIL),ALBEDO(IM,JM),ALBNEW(IM,JM),ALBEDS(IM,JM)
                             D I M E N S I O N
     & FQU(IM,JM),FQV(IM,JM),DQFLX(IM,JM)
     &,FCU(IM,JM),FCV(IM,JM),DCFLX(IM,JM)
     &,FQU7(IM,JM),FQV7(IM,JM),DQFLX7(IM,JM)
     &,FCU7(IM,JM),FCV7(IM,JM),DCFLX7(IM,JM)
     &,DQADV(IM,JM),FQNEV1(IM,JM),FQSEV1(IM,JM)
                             D I M E N S I O N
     & VAPINC(IM,JM),VAPINC7(IM,JM),CLDINC(IM,JM),CLDINC7(IM,JM)
C-----------------------------------------------------------------------
                             D A T A
     & NFCSTE/11/,NFCST /12/,NRESTRT /51/, NFC/52/, LIST/06/
C-----------------------------------------------------------------------
      CHARACTER*32 LABEL
C-----------------------------------------------------------------------
      NAMELIST /CYCLE/ PARTIAL
      NAMELIST /UPSNOW/ NEWSNOW
C
      CALL W3TAGB('RSTUPT  ',1998,0096,0080,'NP22   ')
C
      PARTIAL=.FALSE.
      READ(5,CYCLE)
      WRITE(6,CYCLE)
C
      NEWSNOW=.TRUE.
      READ(15,UPSNOW)
      WRITE(6,UPSNOW)
C
C Read in cycle hour from tm12 nmcdate file to determine if we want to use
C new snow analysis or cycle snow from previous EDAS. 
C
      READ(14,100) IHRCYC
100   FORMAT(14X,I2)
C
C     nfcste = gdas restart file
C     nfcst  = edas restart file
C     nrestrt = output file
C
      rewind nfcste
      rewind nfcst
      rewind nrestrt
C
C Read through edas restart file until we get CWM and Q2
C
         nskip = 2+lm+3
         do iskip = 1, nskip
           read(nfcst)
         enddo
C
         ALLOCATE(DUM3D(IM,JM,LM))
         ALLOCATE(CWMEDS(IM,JM,LM))
         ALLOCATE(Q2EDS(IM,JM,LM))
C
         do l = 1, lm
            READ(nfcst) ((DUM3D(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((DUM3D(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((DUM3D(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((DUM3D(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((Q2EDS(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((DUM3D(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((cwmeds(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((dum3d(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((dum3d(I,J,L),I=1,IM),J=1,JM)
         enddo
C
         deallocate(dum3d)
         rewind nfcst
C
C Read in the GDAS guess restart file 
C
         READ(nfcste) RUN,IDATGB,IHRSTG,NTSD,LABEL
         READ(nfcste) PDOMG,RESOMG
C
         ALLOCATE(OMEGA(IM,JM,LM))
         DO L = 1,LM
            READ(nfcste) ((OMEGA(I,J,L),I=1,IM),J=1,JM)
         END DO
C
C If doing partial cycling put date of GDAS guess into IDAT
C
         IF(PARTIAL) THEN
           DO ID = 1, 3
            IDAT(ID) = IDATGB(ID)
           ENDDO
           IHRST = IHRSTG
C
           WRITE(nrestrt) RUN,IDAT,IHRST,NTSD,LABEL
           WRITE(nrestrt) PDOMG,RESOMG
C
C  Read same records from edas file
C
           READ(nfcst) RUN,IDAT,IHRST,NTSD,LABEL
           READ(nfcst) PDOMG,RESOMG
C
           DO L = 1,LM
              WRITE(nrestrt) ((OMEGA(I,J,L),I=1,IM),J=1,JM)
           END DO
C
C  Read same records from edas file
C
           DO L = 1,LM
             READ(nfcst) ((OMEGA(I,J,L),I=1,IM),J=1,JM)
           END DO
C
           DEALLOCATE(OMEGA)
         ELSE
           READ(nfcst) RUN,IDAT,IHRST,NTSD,LABEL
           READ(nfcst) PDOMG,RESOMG
           WRITE(nrestrt) RUN,IDAT,IHRST,NTSD,LABEL
c          print*,'Input IDAT=',IDAT
           WRITE(nrestrt) PDOMG,RESOMG
           DO L = 1,LM
             READ(nfcst) ((OMEGA(I,J,L),I=1,IM),J=1,JM)
           END DO
           DO L = 1,LM
              WRITE(nrestrt) ((OMEGA(I,J,L),I=1,IM),J=1,JM)
           END DO
           DEALLOCATE(OMEGA)
         ENDIF
C
         READ(nfcste) RUN,IDAT,IHRST,NTSD,LABEL,
     &        FIRST,IOUT,NSHDE
         READ(nfcste) PDGBL,RESNEW,FISNEW
         READ(nfcste) PDB,TB,QB,UB,VB
C
         READ(nfcst) RUN,IDAT,IHRST,NTSD,LABEL,
     &        FIRST,IOUT,NSHDE
         write(nrestrt) RUN,IDAT,IHRST,NTSD,LABEL,
     &        FIRST,IOUT,NSHDE
         READ(nfcst) PD,RES,FIS
         READ(nfcst) PDB,TB,QB,UB,VB
CPART
         if (.NOT.PARTIAL) then
           write(nrestrt) PD,RESNEW,FISNEW
         else
           write(nrestrt) PDGBL,RESNEW,FISNEW
         endif
CPART
         write(nrestrt) PDB,TB,QB,UB,VB
CPART
         ALLOCATE(TGBL(IM,JM,LM))
         ALLOCATE(QGBL(IM,JM,LM))
         ALLOCATE(UGBL(IM,JM,LM))
         ALLOCATE(VGBL(IM,JM,LM))
         ALLOCATE(Q2(IM,JM,LM))
         ALLOCATE(TTND(IM,JM,LM))
         ALLOCATE(CWM(IM,JM,LM))
         ALLOCATE(TRAIN(IM,JM,LM))
         ALLOCATE(TCUCN(IM,JM,LM))
C
         DO L = 1,LM
            READ(nfcste) ((TGBL(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((QGBL(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((UGBL(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((VGBL(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((Q2(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((TTND(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((cwm(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((train(I,J,L),I=1,IM),J=1,JM)
            READ(nfcste) ((tcucn(I,J,L),I=1,IM),J=1,JM)
         END DO
C
         IF(PARTIAL) THEN
          DO L = 1,LM
            write(nrestrt) ((TGBL(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((QGBL(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((UGBL(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((VGBL(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((Q2EDS(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((TTND(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((cwmeds(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((train(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((tcucn(I,J,L),I=1,IM),J=1,JM)
          END DO
         ENDIF
C
          DEALLOCATE(TGBL)
          DEALLOCATE(UGBL)
          DEALLOCATE(VGBL)
          DEALLOCATE(QGBL)
          DEALLOCATE(Q2EDS)
          DEALLOCATE(CWMEDS)
          ALLOCATE(T(IM,JM,LM))
          ALLOCATE(Q(IM,JM,LM))
          ALLOCATE(U(IM,JM,LM))
          ALLOCATE(V(IM,JM,LM))
C
         DO L = 1,LM
            READ(nfcst) ((T(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((Q(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((U(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((V(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((Q2(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((TTND(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((cwm(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((train(I,J,L),I=1,IM),J=1,JM)
            READ(nfcst) ((tcucn(I,J,L),I=1,IM),J=1,JM)
         END DO
C
         IF(.NOT.PARTIAL) THEN
          DO L = 1,LM
            write(nrestrt) ((T(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((Q(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((U(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((V(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((Q2(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((TTND(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((cwm(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((train(I,J,L),I=1,IM),J=1,JM)
            write(nrestrt) ((tcucn(I,J,L),I=1,IM),J=1,JM)
          END DO
         ENDIF
C        
         DEALLOCATE(T)
         DEALLOCATE(U)
         DEALLOCATE(V)
         DEALLOCATE(Q)
         DEALLOCATE(Q2)
         DEALLOCATE(CWM)
         DEALLOCATE(TRAIN)
         DEALLOCATE(TCUCN)
         DEALLOCATE(TTND)
C
         READ(nfcste) RUN,IDAT,IHRST,NTSD,LABEL,
     &        RADIN,RADOT,TG,Z0,((AKMS(I,J),I=1,IM),J=1,JM),CZEN
         WRITE(LIST,*)'  READ  ',RUN,IDAT,IHRST,NTSD,LABEL
         READ(nfcste) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
         WRITE(LIST,*)'  READ  ',RUN,IDAT,IHRST,NTSD,LABEL
         READ(nfcste) THZ0,QZ0
     &,             ((UZ0(I,J),I=1,IM),J=1,JM)
     &,             ((VZ0(I,J),I=1,IM),J=1,JM)
     &,             USTAR,HTOP,CFRACM
         WRITE(LIST,*)'  READ  ',RUN,IDAT,IHRST,NTSD,LABEL
         READ(nfcste) SNONEW,SINEW,CLDEFI,RF,PSLP,
     &      CUPPT,CFRACH
C
C  Read 14 records to get to albedo 
C
         do ier = 1, 14
           read(nfcste)
         enddo
         read(nfcste) albnew
C
         print *,'done reading gdas restart file'
         print*,'Am I actually here?'
C
C  Read in remainder of edas guess restrt file
C
         READ(nfcst) RUN,IDAT,IHRST,NTSD,LABEL,
     &        RADIN,RADOT,TG,Z0,((AKMS(I,J),I=1,IM),J=1,JM),CZEN
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) THZ0,QZ0
     &,             ((UZ0(I,J),I=1,IM),J=1,JM)
     &,             ((VZ0(I,J),I=1,IM),J=1,JM)
     &,             USTAR,HTOP,CFRACM
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) SNOEDS,SIEDS,CLDEFI,RF,PSLP,
     &      CUPPT,CFRACH
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) SOILTB,SFCEXC,SMSTAV,SMSTOT,GRNFLX,
     &      PCTSNO
         WRITE(LIST,*)'  READ  ',LABEL
	 READ(nfcst) SWNET,ELWDWN,CZMEAN,SIGT4
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) u00,ul,lc,sr
         WRITE(LIST,*)'  READ  ',LABEL
C
         READ(nfcst) RUN,IDAT,IHRST,NTSD,LABEL,
     &        PREC,ACPREC,ACCLIQ,CUPREC
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) ACFRCV,NCFRCV,ACFRST,NCFRST
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) ACSNOW,ACSNOM,SSROFF,BGROFF
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) SFCSHX,SFCLHX,SUBSHX,SNOPCX,
     &        SFCUVX,SFCEVP,POTEVP
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst)
     &        ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst)
     &        ardsw,ardlw,asrfc,avrain,avcnvc
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) th10,q10,u10,v10,tshltr,qshltr,
     &        pshltr,th30,q30,u30,v30
         WRITE(LIST,*)'  READ  ',LABEL
         READ(nfcst) smc
         READ(nfcst) cmc
         READ(nfcst) stc
         READ(nfcst) sh2o
         READ(nfcst) albeds
         READ(nfcst) FQU,FQV,DQFLX,FCU,FCV,DCFLX,
     &               FQU7,FQV7,DQFLX7,FCU7,FCV7,DCFLX7,
     &               DQADV,FQNEV1,FQSEV1
         READ(nfcst) VAPINC,VAPINC7,CLDINC,CLDINC7
         WRITE(LIST,*)'  READ  ',LABEL
         write(list,*)' done reading edas guess restrt file'
C
         do j = 1, jm
          do i = 1, im
           diffps = abs(pdgbl(i,j) - pd(i,j))
           ref=1./resnew(i,j)
           if(diffps.ge.4000.) then
             print *,i,j,pdgbl(i,j),pd(i,j),ref
           endif
          enddo
         enddo 
c
c  read in new nhb file to get sea ice mask from grdeta
c  and tg
c
         nhibu = 13
         do ker = 1,7
           read(nhibu)
         enddo
         read(nhibu) sice
c
         do ker = 1, lm
           read(nhibu)
         enddo
         do ker = 1, lm
           read(nhibu)
         enddo
         do ker = 1, 17
           read(nhibu)
         enddo
         read(nhibu) tgnew
c
c  reset stc for new sea ice to -4C
c
         do j = 1, jm
          do i = 1, im
          if(sice(i,j).eq.1.0 .and. stc(i,j,1).eq.0.0) then
            do ns = 1, nsoil
             stc(i,j,ns) = 269.16
            enddo
            print *," stc reset for sea ice at i,j = ",i,j
          endif
          enddo
         enddo
c
c   Check to see whether we will use new NESDIS snow analysis or cycling
c   on snow from previous EDAS based on tm12 time read into unit 14.
c
c If AF snow was missing in grdeta, then change the NEWSNOW to .false.
c
         do j=1,jm
         do i=1,im
         if(snonew(i,j).eq.99.9.and.sinew(i,j).eq.99.9) then
           print*,'AF SNOW WAS MISSING, NEWSNOW=.F.'
           newsnow=.false.
           goto 1234
         endif
         enddo
         enddo
1234     continue
         IF(NEWSNOW) THEN
          print *,' use new snow analysis at tm12 = ',IHRCYC,'Z'
          DO J = 1, JM
           DO I = 1, IM
            SNO(I,J) = SNONEW(I,J)
            SI(I,J) = SINEW(I,J)
            ALBEDO(I,J) = ALBNEW(I,J)
           ENDDO
          ENDDO
         ELSE
          print *,' use cycled EDAS snow at tm12 = ',IHRCYC,'Z'
          DO J = 1, JM
           DO I = 1, IM
            SNO(I,J) = SNOEDS(I,J)
            SI(I,J) = SIEDS(I,J)
            ALBEDO(I,J) = ALBEDS(I,J)
           ENDDO
          ENDDO
         ENDIF
C
C-----------------------------------------------------------------------
c Now write back rest of the edas guess restart file with updated SNO,SI, 
C ALBEDO, TG and stc under ice 
c-----------------------------------------------------------------------
C
         write(nrestrt) RUN,IDAT,IHRST,NTSD,LABEL,
     &        RADIN,RADOT,TGNEW,Z0,
     &        ((AKMS(I,J),I=1,IM),J=1,JM),CZEN
c        print*,'wrote IDAT=',IDAT
         write(nrestrt) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
         write(nrestrt) THZ0,QZ0,
     &              ((UZ0(I,J),I=1,IM),J=1,JM),
     &              ((VZ0(I,J),I=1,IM),J=1,JM),
     &              USTAR,HTOP,CFRACM
         write(nrestrt) SNO,SI,CLDEFI,RF,PSLP,
     &        CUPPT,CFRACH
         write(nrestrt) SOILTB,SFCEXC,SMSTAV,SMSTOT,GRNFLX,
     &        PCTSNO
	 write(nrestrt) SWNET,ELWDWN,CZMEAN,SIGT4
         write(nrestrt) u00,ul,lc,sr
C
         write(nrestrt) RUN,IDAT,IHRST,NTSD,LABEL,
     &        PREC,ACPREC,ACCLIQ,CUPREC
         write(nrestrt) ACFRCV,NCFRCV,ACFRST,NCFRST
         write(nrestrt) ACSNOW,ACSNOM,SSROFF,BGROFF
         write(nrestrt) SFCSHX,SFCLHX,SUBSHX,SNOPCX,
     &        SFCUVX,SFCEVP,POTEVP
         write(nrestrt)
     &        ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA
         write(nrestrt)
     &        ardsw,ardlw,asrfc,avrain,avcnvc
         write(nrestrt) th10,q10,u10,v10,tshltr,qshltr,
     &        pshltr,th30,q30,u30,v30
         write(nrestrt) smc
         write(nrestrt) cmc
         write(nrestrt) stc
         write(nrestrt) sh2o
         write(nrestrt) albedo
         write(nrestrt) FQU,FQV,DQFLX,FCU,FCV,DCFLX,
     &                  FQU7,FQV7,DQFLX7,FCU7,FCV7,DCFLX7,
     &                  DQADV,FQNEV1,FQSEV1
         write(nrestrt) VAPINC,VAPINC7,CLDINC,CLDINC7
         WRITE(LIST,*)'  WROTE  ',LABEL
         write(list,*)' done writing edas guess restrt file'
c
c        do l = 1, lm
c          write(6,12345)t(93,53,l),q(93,53,l),u(93,53,l),v(93,53,l)
c2345      format(1x,'at i,j=93,53 ',4(1x,e12.5))
c        enddo
c
      CALL W3TAGE('RSTUPT  ')
c
      stop
      end
