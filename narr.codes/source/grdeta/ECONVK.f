      SUBROUTINE ECONVK(idate)
C     ******************************************************************
C     *                                                                *
C     *  SET UP INITIAL FILES FOR RUNNING ETA MODEL.                   *
C     *                                                                *
C     ******************************************************************
C----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)

      INCLUDE "parmeta.res"
      INCLUDE "parmsoil"
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::T,U,V,Q,DUMMY
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::DUM1D
      REAL(REAL_32)::RDUM=0.0
c
      dimension idate(4)
c
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMT=2*IM-1,JMT=JM/2+1)
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3,LP1=LM+1)
C-----------------------------------------------------------------------
                             C O M M O N  /PTETA/
     & IDAT(3),PT ,DETA  (LM),AETA  (LM),ETA   (LP1),DFL   (LP1)
C
     &,RES   (IM,JM),FIS   (IM,JM),ALBEDO(IM,JM)
     &,SNO   (IM,JM),SST   (IM,JM),SI    (IM,JM)
     &,SM    (IM,JM),LMH   (IM,JM),LMV   (IM,JM)
     &,SMC(IM,JM,NSOIL),STC(IM,JM,NSOIL),CMC(IM,JM),SH2O(IM,JM,NSOIL)
C
      COMMON /TGRND/ TG(IM,JM)
C-----------------------------------------------------------------------
                             L O G I C A L 
     & RUN, FIRST
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & PDB   (LB,2)
     &,TB    (LB,LM,2),QB    (LB,LM,2)
     &,UB    (LB,LM,2),VB    (LB,LM,2)
     &,PD    (IM,JM)
c    &,U(IM,JM,LM),V(IM,JM,LM),T(IM,JM,LM),Q(IM,JM,LM)
c    &,RSWIN (IM,JM)   ,RADOT (IM,JM)
c    &,TG    (IM,JM)   ,Z0    (IM,JM)   ,AKMS  (IM,JM)   ,CZEN  (IM,JM)
c    &,THS   (IM,JM)   ,QS    (IM,JM)   ,TWBS  (IM,JM)   ,QWBS  (IM,JM)
c    &,AKHS  (IM,JM)   ,HBOT  (IM,JM)   ,CFRACL(IM,JM)   ,THZ0  (IM,JM)
c    &,QZ0   (IM,JM)   ,UZ0   (IM,JM)   ,VZ0   (IM,JM)   ,USTAR (IM,JM)
c    &,HTOP  (IM,JM)   ,CFRACM(IM,JM)   ,CLDEFI(IM,JM)   ,RF    (IM,JM)
c    &,PSLP  (IM,JM)   ,CUPPT (IM,JM)   ,CFRACH(IM,JM)
     &,                                                   SOILTB(IM,JM)
c    &,SFCEXC(IM,JM)   ,SMSTAV(IM,JM)   ,SMSTOT(IM,JM)   ,GRNFLX(IM,JM)
c    &,PCTSNO(IM,JM)   ,PREC  (IM,JM)   ,ACPREC(IM,JM)   ,ACCLIQ(IM,JM)
c    &,CUPREC(IM,JM)   ,ACFRCV(IM,JM)   ,NCFRCV(IM,JM)   ,ACFRST(IM,JM)
c    &,NCFRST(IM,JM)   ,ACSNOW(IM,JM)   ,ACSNOM(IM,JM)   ,SSROFF(IM,JM)
c    &,BGROFF(IM,JM)   ,SFCSHX(IM,JM)   ,SFCLHX(IM,JM)
c    &,SUBSHX(IM,JM)   ,SNOPCX(IM,JM)   ,SFCUVX(IM,JM)   ,SFCEVP(IM,JM)
c    &,POTEVP(IM,JM)   ,ASWIN (IM,JM)   ,ASWOUT(IM,JM)   ,ASWTOA(IM,JM)
c    &,ALWIN (IM,JM)   ,ALWOUT(IM,JM)   ,ALWTOA(IM,JM)
c    &,PDOMG(IM,JM),RESOMG(IM,JM)
c    &,OMEGA(IM,JM,LM),PDOMG(IM,JM),RESOMG(IM,JM)

                             D I M E N S I O N
c    & Q2(IM,JM,LM)  ,CWM(IM,JM,LM)
c    & RLWIN(IM,JM), RSWOUT(IM,JM), CZMEAN(IM,JM), SIGT4(IM,JM)
c    &,TCUCN(IM,JM,LM)   ,TRAIN(IM,JM,LM)  ,TTND(IM,JM,LM)
     & UL(2*LM)
c    &,U00(IM,JM),UL(2*LM),LC(IM,JM),SR(IM,JM)
c    &,TH10(IM,JM),Q10(IM,JM),U10(IM,JM),V10(IM,JM)
c    &,TSHLTR(IM,JM),PSHLTR(IM,JM),QSHLTR(IM,JM)
     &,IGDATE(4)
C
       COMMON /OLDALB/ ALBASE(IM,JM)
C-----------------------------------------------------------------------
                             C O M M O N  /SFCTYPES/
     & IVGTPK(IM,JM),ISLTPK(IM,JM),ISPTPK(IM,JM)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       CHARACTER*32 LABEL
C-----------------------------------------------------------------------
                             D A T A
     & NFCSTE/51/,NFCST /53/,NBC   /55/,NRESTRT /56/, LIST/06/
                             D A T A
     & LABEL/'                                '/
C-----------------------------------------------------------------------
      real*8 timef
      real cnsts_tim
C
      ALLOCATE(T(IM,JM,LM))
      ALLOCATE(Q(IM,JM,LM))
      ALLOCATE(U(IM,JM,LM))
      ALLOCATE(V(IM,JM,LM))
      ALLOCATE(DUMMY(IM,JM,LM))
      ALLOCATE(DUM1D(IM,JM))
C
      REWIND NFCSTE
      REWIND NFCST
      REWIND NBC
      READ (NFCSTE) RUN,IDAT,IHRST,NTSD,U,V,SNO,SST,SI
      READ (NFCSTE) T,Q,PD,FIS,SM,RES,ETA,PT,DETA,AETA
     &,DFL,PDB,TB,QB,UB,VB
      print *,run,idat,ihrst,ntsd,pt,u(im,jm,lm),v(im,jm,lm)
      print *,t(im,jm,lm),pd(im,jm),res(im,jm),fis(im,jm)
C-----------------------------------------------------------------------
      print *,'econvk calling subroutine cnsts'
      btim=timef()
      CALL CNSTS(idate)
      cnsts_tim=timef()-btim
      print *,' ok after cnsts, time= ',cnsts_tim
C-----------------------------------------------------------------------
      DO I=1,IM
       DO J=1,JM
        RES(I,J)=1./RES(I,J)
       ENDDO
      ENDDO
      print *,'before nfc write ',RUN,IDAT,IHRST,NTSD
C-----------------------------------------------------------------------
      WRITE(NFCST)RUN,IDAT,IHRST,NTSD
      WRITE(NFCST)PD
      WRITE(NFCST)RES
      WRITE(NFCST)FIS
      DO L=1,LM
        WRITE(NFCST)((U(I,J,L),I=1,IM),J=1,JM)
      END DO
      DO L=1,LM
        WRITE(NFCST)((V(I,J,L),I=1,IM),J=1,JM)
      END DO
      DO L=1,LM
        WRITE(NFCST)((T(I,J,L),I=1,IM),J=1,JM)
      END DO
      DO L=1,LM
        WRITE(NFCST)((Q(I,J,L),I=1,IM),J=1,JM)
      END DO
      WRITE(NFCST)SI
      WRITE(NFCST)SNO
      WRITE(NFCST) SMC
      WRITE(NFCST) CMC
c     do j=1,jm
c     do i=1,im
c        print*,'i,j,1,stc(i,j,1)=',i,j,1,stc(i,j,1)
c        print*,'i,j,2,stc(i,j,2)=',i,j,2,stc(i,j,2)
c        print*,'i,j,3,stc(i,j,3)=',i,j,3,stc(i,j,3)
c        print*,'i,j,4,stc(i,j,4)=',i,j,4,stc(i,j,4)
c     enddo
c     enddo
      WRITE(NFCST) STC
      WRITE(NFCST) SH2O
      WRITE(NFCST) ALBEDO
C-----------------------------------------------------------------------
      TBOCO = 6.0
      print *,'before nbc write ',RUN,IDAT,IHRST,TBOCO
      WRITE(NBC   ) RUN,IDAT,IHRST,TBOCO
      WRITE(NBC   ) PDB,TB,QB,UB,VB
      WRITE(NBC   ) PDB,TB,QB,UB,VB
      WRITE(NBC   ) PDB,TB,QB,UB,VB
C-----------------------------------------------------------------------
C NOW DO RESTART STUFF (USED ONLY IF EDASING AND FIRST TIME AT THAT!!
C FIRST ZERO THE ARRAYS THAT ARE ONLY USED FOR FILLER (PHYSICS..ETC..)
C-----------------------------------------------------------------------
           DUM1D = 0.0
           DUMMY = 0.0
           UL = 0.0
C-----------------------------------------------------------------------
c      do i = 1, im
c       do j = 1, jm
c        if(mod(i,20).eq.0 .and. mod(j,20).eq.0 .or.
c    1     i.eq.im .and. j.eq.jm) then 
c          write(6,1000)i,j,pd(i,j),fis(i,j),res(i,j),sm(i,j)
c000       format(1x,2i4,4(1x,e12.5))
c          do l = 1, lm
c            write(6,1010)l,t(i,j,l),q(i,j,l),u(i,j,l),v(i,j,l)
c010         format(1x,i2,4(1x,e12.5))
c          enddo
c        endif
c       write(3,*)i,j,pd(i,j),res(i,j),fis(i,j)
c       enddo
c      enddo
C
         FIRST = .TRUE.
         IOUT = 0
         NSHDE = 0
         print *,'before nfc write ',RUN,IDAT,IHRST,NTSD,LABEL 
         WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL
         WRITE(NRESTRT) DUM1D,DUM1D
         DO L = 1,LM
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
         END DO
         WRITE(LIST,*)'  READ  ',RUN,IDAT,IHRST,NTSD,LABEL
C
         WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &        FIRST,IOUT,NSHDE
c        do j=380,390
c        do i=110,120
c        print*,'i,j,pd(i,j)=',i,j,pd(i,j)
c        enddo
c        enddo
         WRITE(NRESTRT) PD,RES,FIS
         WRITE(NRESTRT) PDB,TB,QB,UB,VB
         DO L = 1,LM
            WRITE(NRESTRT)((T(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((Q(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((U(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((V(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
            WRITE(NRESTRT)((DUMMY(I,J,L),I=1,IM),J=1,JM)
         END DO
         DEALLOCATE(T)
         DEALLOCATE(U)
         DEALLOCATE(V)
         DEALLOCATE(Q)
         DEALLOCATE(DUMMY)

         WRITE(LIST,*)'  WROTE  ',RUN,IDAT,IHRST,NTSD,LABEL
C
c        WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL,
c    &        RSWIN,RSWOUT,TG,Z0,AKMS,CZEN
c        WRITE(NRESTRT) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
c        WRITE(NRESTRT) THZ0,QZ0,UZ0,VZ0,USTAR,HTOP,CFRACM
c        WRITE(NRESTRT) SNO,SI,CLDEFI,RF,PSLP,CUPPT,CFRACH
c        WRITE(NRESTRT) SOILTB,SFCEXC,SMSTAV,SMSTOT,GRNFLX,PCTSNO
c        WRITE(NRESTRT) RLWIN,RADOT,CZMEAN,SIGT4
c        WRITE(NRESTRT) U00,UL,LC,SR

C
C eliminate redundant SOILTB in favour of TG (see SURFCE.F in etafcst)
C eliminate TG redundancy in NHIBU and RESTRT files (June 23, 2003)
C
         DO J=1,JM
         DO I=1,IM
         SOILTB(I,J)=TG(I,J)
         END DO
         END DO

         WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &        dum1d,dum1d,TG,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) SNO,SI,dum1d,dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) SOILTB,dum1d,dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,UL,dum1d,dum1d

c        WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL,
c    &        PREC,ACPREC,ACCLIQ,CUPREC
c        WRITE(NRESTRT) ACFRCV,NCFRCV,ACFRST,NCFRST
c        WRITE(NRESTRT) ACSNOW,ACSNOM,SSROFF,BGROFF
c        WRITE(NRESTRT) SFCSHX,SFCLHX,SUBSHX,SNOPCX,
c    &        SFCUVX,SFCEVP,POTEVP
c        WRITE(NRESTRT)
c    &        ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA
c        WRITE(NRESTRT)
c    &        ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
c        WRITE(NRESTRT) TH10,Q10,U10,V10,TSHLTR,PSHLTR,QSHLTR

         WRITE(NRESTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &        dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d,
     &        dum1d,dum1d,dum1d
         WRITE(NRESTRT)
     &        dum1d,dum1d,dum1d,dum1d,dum1d,dum1d
         WRITE(NRESTRT)
Cdule     &        dum1d,dum1d,dum1d,dum1d,dum1d
     &        rdum,rdum,rdum,rdum,rdum
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d,
     &        dum1d,dum1d,dum1d,
     &        dum1d,dum1d,dum1d,dum1d     !dule 30m vars
         WRITE(NRESTRT) SMC
         WRITE(NRESTRT) CMC
         WRITE(NRESTRT) STC
         WRITE(NRESTRT) SH2O
         WRITE(NRESTRT) ALBEDO
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d,dum1d,dum1d !dule flx vars
     &,                 dum1d,dum1d,dum1d,dum1d,dum1d,dum1d
     &,                 dum1d,dum1d,dum1d
         WRITE(NRESTRT) dum1d,dum1d,dum1d,dum1d !dule prec. assim. incr.
         WRITE(LIST,*)'  WROTE  ',LABEL
C
c        do j = 1, jm
c          do i = 1, im
c            if(mod(i,10).eq.0 .and .mod(j,10).eq.0) then
c            write(6,12346)i,j,sh2o(i,j,1),smc(i,j,1),stc(i,j,1),
c    &         sh2o(i,j,4),sno(i,j),si(i,j),sm(i,j),
c    &         ivgtpk(i,j),isltpk(i,j),isptpk(i,j)
c2346        format(1x,2i6,7(1x,e12.5),3(1x,i6))
c            endif
C
c            if(i.eq.203 .and. j.eq.196) then
c            write(6,12346)i,j,sh2o(i,j,1),smc(i,j,1),stc(i,j,1),
c    &         sh2o(i,j,4),sno(i,j),si(i,j),sm(i,j),
c    &         ivgtpk(i,j),isltpk(i,j),isptpk(i,j)
c            endif
c          enddo
c        enddo
C
c        do l = 1, lm
c          write(6,12345)t(93,53,l),q(93,53,l),u(93,53,l),v(93,53,l)
c2345      format(1x,'at i,j=93,53 ',4(1x,e12.5))
c        enddo
C
                             RETURN
                             END
