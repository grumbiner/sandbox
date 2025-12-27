      PROGRAM STALST
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------------------
      PARAMETER (IMJM=IM*JM-JM/2,IMT=IM*2-1,JMT=JM/2+1)
      PARAMETER (NSTA=2000)
      PARAMETER (JAM=6+2*(JM-10),LP1=LM+1)
      PARAMETER (D2R=1.745329252E-2,R2D=57.29577951)
C----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C----------------------------------------------------------------------
c     INCLUDE "MAPOT.comm"
C----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C----------------------------------------------------------------------
                            C H A R A C T E R
     & AN*1,AW*1,CID*8,CIDDUM*8,CIDX*8
C----------------------------------------------------------------------
                            D I M E N S I O N
     & RLAT(NSTA),RLON(NSTA),TLAT(NSTA),TLON(NSTA)
     &,RLATD(NSTA),RLOND(NSTA)
     &,IHDUM(NSTA),JHDUM(NSTA),IVDUM(NSTA),JVDUM(NSTA)
     &,IHSTA(NSTA),JHSTA(NSTA),IVSTA(NSTA),JVSTA(NSTA)
     &,KCLAS(NSTA),KCLASD(NSTA),IDDUM(NSTA),ID(NSTA)
     &,CIDDUM(NSTA),CID(NSTA)
C----------------------------------------------------------------------
                            R E A L
     & LAM0
C----------------------------------------------------------------------
      INCLUDE "griddef"
C----------------------------------------------------------------------
      DATA LNHB/12/,LUNLST/15/
C----------------------------------------------------------------------
C***
C***  READ IN THE MASKS THAT ARE NEEDED
C***
      READ(12)
      READ(12)LMH
      READ(12)LMV
      write(6,*)' lmh=',lmh(10,14)
      write(6,*)' lmh=',lmh(10,13),lmh(10,13)
      write(6,*)' lmh=',lmh(10,13),lmh(10,13),lmh(10,13)
      write(6,*)' lmh=',lmh(10,13),lmh(10,13)
      write(6,*)' lmh=',lmh(10,13)
      READ(12)
      READ(12)
      READ(12)
      READ(12)SM
      READ(12)SICE
C***
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
      ENDDO
C
      write(6,*)' tph0d=',tph0d,' tlm0d=',tlm0d
      PHI0=TPH0D*D2R
      LAM0=-1.0*TLM0D*D2R
      DPHI=DPHD*D2R
      DLAM=DLMD*D2R
C
C   SBDB AND WBDB ARE 2 ROWS INSIDE THE DOMAIN BOUNDARY
C   TO TRY TO EXCLUDE POINTS RIGHT ON THE BOUNDARY
C
      SBDB=SBD+2.0*DPHD
      WBDB=WBD+2.0*DLMD
      write(6,*)' sbdb=',sbdb,' wbdb=',wbdb
C***
C***  LOOP THROUGH ALL THE STATIONS TO COUNT HOW MANY INSIDE DOMAIN
C***
      NSUM=0
   25 READ(LUNLST,50,END=100)IDX,RLATX,AN,RLONX,AW,CIDX,KCLS
   50 FORMAT(I6,F6.2,A1,F7.2,A1,1X,A4,I3)
      print *, IDX 
      IF (AN.EQ.'S') RLATX=-1.0*RLATX
      IF (AW.EQ.'E') RLONX=360.0-RLONX
      RLATX=RLATX*D2R
      RLONX=RLONX*D2R
C***
C***  CONVERT GEODETIC TO TRANSFORMED COORDINATES OF THE STATION
C***
      X=COS(PHI0)*COS(RLATX)*COS(RLONX-LAM0)
     1  +SIN(PHI0)*SIN(RLATX)
      Y=-COS(RLATX)*SIN(RLONX-LAM0)
      Z=COS(PHI0)*SIN(RLATX)
     1  -SIN(PHI0)*COS(RLATX)*COS(RLONX-LAM0)
      TLATX=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLONX=R2D*ATAN(Y/X)
      IF(ABS(TLATX).GT.ABS(SBDB).OR.ABS(TLONX).GT.ABS(WBDB))GO TO 25
      NSUM=NSUM+1
      RLATD(NSUM)=RLATX
      RLOND(NSUM)=RLONX
      TLAT(NSUM)=TLATX
      TLON(NSUM)=TLONX
      IDDUM(NSUM)=IDX
      if(idx.eq.725720)then
        write(6,*)' SLC at n=',nsum
        nsave=nsum
      endif
      CIDDUM(NSUM)=CIDX
      KCLASD(NSUM)=KCLS
      GO TO 25
  100 CONTINUE
C
C----------------------------------------------------------------------
      DO 200 N2=1,NSUM
C----------------------------------------------------------------------
C***
C***  FIND THE CLOSEST POINT REGARDLESS OF LAND/SEA MASK
C***
      ROW=TLAT(N2)/DPHD+JMT
      COL=TLON(N2)/DLMD+IM
      NROW=INT(ROW)
      NCOL=INT(COL)
      TLAT(N2)=TLAT(N2)*D2R
      TLON(N2)=TLON(N2)*D2R
C***
C***  IDENTIFY THE ROW AND COLUMN OF THE LOWER LEFT CORNER OF THE BOX
C***  THAT SURROUNDS EACH STATION INSIDE THE DOMAIN
C***
C***  FIRST CONSIDER THE SITUATION WHERE THE STATION X IS AT
C***
C***              H      V
C***
C***
C***                 X
C***              V      H
C***
      IF(MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.1.OR.
     1   MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.0)THEN
        TLAT1=(NROW-JMT+1)*DPHI
        TLAT2=TLAT1-DPHI
        TLON1=(NCOL-IM)*DLAM
        TLON2=TLON1+DLAM
        DLM1=TLON(N2)-TLON1
        DLM2=TLON(N2)-TLON2
        D1=ACOS(COS(TLAT(N2))*COS(TLAT1)*COS(DLM1)
     1         +SIN(TLAT(N2))*SIN(TLAT1))
        D2=ACOS(COS(TLAT(N2))*COS(TLAT2)*COS(DLM2)
     1         +SIN(TLAT(N2))*SIN(TLAT2))
        IF(D1.GT.D2)THEN
          NCOL=NCOL+1
        ELSE
          NROW=NROW+1
        ENDIF
C***
C***  NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
C***
C***              V      H
C***
C***
C***                 X
C***              H      V
C***
      ELSE
        TLAT1=(NROW-JMT)*DPHI
        TLAT2=TLAT1+DPHI
        TLON1=(NCOL-IM)*DLAM
        TLON2=TLON1+DLAM
        DLM1=TLON(N2)-TLON1
        DLM2=TLON(N2)-TLON2
        D1=ACOS(COS(TLAT(N2))*COS(TLAT1)*COS(DLM1)
     1         +SIN(TLAT(N2))*SIN(TLAT1))
        D2=ACOS(COS(TLAT(N2))*COS(TLAT2)*COS(DLM2)
     1         +SIN(TLAT(N2))*SIN(TLAT2))
        IF(D1.GT.D2)THEN
          NROW=NROW+1
          NCOL=NCOL+1
        ENDIF
      ENDIF
C***
C***  NOW WE CAN FIND THE K VALUE
C***
      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2).EQ.1)THEN
        KMIN=KROWS+(NCOL+1)/2
      ELSE
        KMIN=KROWS+IM+NCOL/2
      ENDIF
      JX=(KMIN-1)/IMT+1
      IX=KMIN-(JX-1)*IMT
      IF(IX.LE.IM)THEN
        IDUM=IX
        JDUM=2*JX-1
      ELSE
        IDUM=IX-IM
        JDUM=2*JX
      ENDIF
C
      IF (KCLASD(N2).GE.10.AND.KCLASD(N2).LT.20) THEN
C
C   WE WANT THIS TO BE A LAND POINT, IF NOT, FIND THE CLOSEST LAND POINT
C
        IF (SM(IDUM,JDUM)+SICE(IDUM,JDUM).GT.0.5) THEN
          DMIN=99999.
          DO J=1,JM
            TLAT1=(J-JMT)*DPHI
            DO I=1,IM
             IHV=I*2-MOD(J,2)
             TLON1=(IHV-IM)*DLAM
             DLM1=TLON(N2)-TLON1
             IF(SM(I,J)+SICE(I,J).LT.0.5) THEN
               D1=ACOS(COS(TLAT(N2))*COS(TLAT1)*COS(DLM1)+
     1            SIN(TLAT(N2))*SIN(TLAT1))
               IF(D1.LT.DMIN) THEN
                  DMIN=D1
                  NCOL=IHV
                  NROW=J
               ENDIF
             ENDIF
            ENDDO
          ENDDO
        ENDIF
        KROWS=((NROW-1)/2)*IMT
        IF(MOD(NROW,2).EQ.1)THEN
          KMIN=KROWS+(NCOL+1)/2
        ELSE
          KMIN=KROWS+IM+NCOL/2
        ENDIF
      ENDIF
C
      IF (KCLASD(N2).GE.20.AND.KCLASD(N2).LT.30) THEN
C
C   WE WANT THIS TO BE A SEA POINT, IF NOT, FIND THE CLOSEST SEA POINT
C
        IF (SM(IDUM,JDUM)+SICE(IDUM,JDUM).LT.0.5) THEN
          DMIN=99999.
          DO J=1,JM
            TLAT1=(J-JMT)*DPHI
            DO I=1,IM
             IHV=I*2-MOD(J,2)
             TLON1=(IHV-IM)*DLAM
             DLM1=TLON(N2)-TLON1
             IF(SM(I,J)+SICE(I,J).GT.0.5) THEN
               D1=ACOS(COS(TLAT(N2))*COS(TLAT1)*COS(DLM1)+
     1            SIN(TLAT(N2))*SIN(TLAT1))
               IF(D1.LT.DMIN) THEN
                  DMIN=D1
                  NCOL=IHV
                  NROW=J
               ENDIF
             ENDIF
            ENDDO
          ENDDO
        ENDIF
        KROWS=((NROW-1)/2)*IMT
        IF(MOD(NROW,2).EQ.1)THEN
          KMIN=KROWS+(NCOL+1)/2
        ELSE
          KMIN=KROWS+IM+NCOL/2
        ENDIF
      ENDIF
C***
C***  FIND THE GEODETIC LAT/LON
C***
      TLATS=(NROW-JMT)*DPHI
      TLONS=(NCOL-IM)*DLAM
      A=SIN(TLATS)*COS(PHI0)+COS(TLATS)*SIN(PHI0)*COS(TLONS)
      ALAT=ASIN(A)
      RLATD(N2)=ALAT
      B=COS(TLATS)*COS(TLONS)/(COS(ALAT)*COS(PHI0))
      D=B-TAN(ALAT)*TAN(PHI0)
      IF (ABS(D).GT.1.0) D=ABS(D)/D
      C=ACOS(D)
      C=SIGN(C,-1.0*TLONS)
      RLOND(N2)=LAM0+C
C***
C***  THIS IS THE H POINT, NOW FIND THE CLOSEST NEIGHBORING
C***  V POINT THAT IS ON THE SAME STEP
C***
C***                 2          KMIN+IM-1
C***
C***              1  H  3  KMIN-1  H   KMIN
C***                  
C***                 4           KMIN-IM
C***       
C***
        TLAT1=(NROW-JMT)*DPHI
        TLON1=(NCOL-IM-1)*DLAM
        TLAT2=TLAT1+DPHI
        TLON2=TLON1+DLAM
        TLAT3=TLAT1
        TLON3=TLON2+DLAM
        TLAT4=TLAT1-DPHI
        TLON4=TLON2
        DLM1=TLON(N2)-TLON1
        DLM2=TLON(N2)-TLON2
        DLM3=TLON(N2)-TLON3
        DLM4=TLON(N2)-TLON4
        D1=ACOS(COS(TLAT(N2))*COS(TLAT1)*COS(DLM1)
     1         +SIN(TLAT(N2))*SIN(TLAT1))
        D2=ACOS(COS(TLAT(N2))*COS(TLAT2)*COS(DLM2)
     1         +SIN(TLAT(N2))*SIN(TLAT2))
        D3=ACOS(COS(TLAT(N2))*COS(TLAT3)*COS(DLM3)
     1         +SIN(TLAT(N2))*SIN(TLAT3))
        D4=ACOS(COS(TLAT(N2))*COS(TLAT4)*COS(DLM4)
     1         +SIN(TLAT(N2))*SIN(TLAT4))
        DMIN=99999.
C
        CALL K2IJH(KMIN,IMIN,JMIN)
C
      if(n2.eq.nsave)then
        write(6,*)' d1=',d1,' d2=',d2,' d3=',d3,' d4=',d4
        write(6,*)' imin=',imin,' jmin=',jmin
        write(6,*)' imin1=',imin+ihw(jmin),' jmin1=',jmin
        write(6,*)' imin2=',imin,' jmin2=',jmin+1
        write(6,*)' imin3=',imin+ihe(jmin),' jmin3=',jmin
        write(6,*)' imin4=',imin,' jmin4=',jmin-1
        write(6,*)' lmh=',lmh(imin,jmin)
     1,           ' lmv=',lmv(imin+ihw(jmin),jmin)
        write(6,*)' lmh=',lmh(imin,jmin)
     1,           ' lmv=',lmv(imin,jmin+1)
        write(6,*)' lmh=',lmh(imin,jmin)
     1,           ' lmv=',lmv(imin+ihe(jmin),jmin)
        write(6,*)' lmh=',lmh(imin,jmin)
     1,           ' lmv=',lmv(imin,jmin-1)
      endif
        IF(LMH(IMIN,JMIN).EQ.LMV(IMIN+IHW(JMIN),JMIN).AND.
     1                 D1.LT.DMIN)THEN
          DMIN=D1
          KVMIN=KMIN-1
      if(n2.eq.nsave)then
        write(6,*)' 1 kmin=',kmin,' kvmin=',kvmin
      endif
        ENDIF
C
        IF(LMH(IMIN,JMIN).EQ.LMV(IMIN,JMIN+1).AND.D2.LT.DMIN)THEN
          DMIN=D2
          KVMIN=KMIN+IM-1
      if(n2.eq.nsave)then
        write(6,*)' 2 kmin=',kmin,' kvmin=',kvmin
      endif
        ENDIF
C
        IF(LMH(IMIN,JMIN).EQ.LMV(IMIN+IHE(JMIN),JMIN).AND.
     1                 D3.LT.DMIN)THEN
          DMIN=D3
          KVMIN=KMIN
      if(n2.eq.nsave)then
        write(6,*)' 3 kmin=',kmin,' kvmin=',kvmin
      endif
        ENDIF
C
        IF(LMH(IMIN,JMIN).EQ.LMV(IMIN,JMIN-1).AND.D4.LT.DMIN)THEN
          DMIN=D4
          KVMIN=KMIN-IM
      if(n2.eq.nsave)then
        write(6,*)' 4 kmin=',kmin,' kvmin=',kvmin
      endif
        ENDIF
C
        IF(DMIN.EQ.99999.)THEN
          KVMIN=KMIN
          WRITE(6,180)IDDUM(N2),RLATD(N2)*R2D,RLOND(N2)*R2D,KMIN,
     1    LMH(IMIN,JMIN),LMV(IMIN+IHW(JMIN),JMIN),LMV(IMIN,JMIN+1)
     2,                  LMV(IMIN+IHE(JMIN),JMIN),LMV(IMIN,JMIN-1)
      if(n2.eq.nsave)then
        write(6,*)' 5 kmin=',kmin,' kvmin=',kvmin
      endif
        ENDIF
C
      KH=KMIN
      KV=KVMIN
C     
      JX=(KH-1)/IMT+1
      IX=KH-(JX-1)*IMT
      IF(IX.LE.IM)THEN
        IHDUM(N2)=IX
        JHDUM(N2)=2*JX-1
      ELSE
        IHDUM(N2)=IX-IM
        JHDUM(N2)=2*JX
      ENDIF
C
      JX=(KV-1)/IMT+1
      IX=KV-(JX-1)*IMT
      IF(IX.LE.IM-1)THEN
        IVDUM(N2)=IX
        JVDUM(N2)=2*JX-1
      ELSE
        IVDUM(N2)=IX-IM+1
        JVDUM(N2)=2*JX
      ENDIF
      if(n2.eq.nsave)then
        write(6,*)' ihdum=',ihdum(n2),' jhdum=',jhdum(n2)
        write(6,*)' ivdum=',ivdum(n2),' jvdum=',jvdum(n2)
      endif
C
  180 FORMAT(I6,2F8.2,I8,5I4,' NO NEIGHBOR ON SAME STEP')
  200 CONTINUE
C***
      NDUM=0
      DO I=1,NSUM
        IF(IDDUM(I).NE.-99999)THEN
          NDUM=NDUM+1
          IHSTA(NDUM)=IHDUM(I)
          JHSTA(NDUM)=JHDUM(I)
          IVSTA(NDUM)=IVDUM(I)
          JVSTA(NDUM)=JVDUM(I)
          ID(NDUM)=IDDUM(I)
          RLAT(NDUM)=RLATD(I)
          RLON(NDUM)=RLOND(I)
          CID(NDUM)=CIDDUM(I)
          KCLAS(NDUM)=MOD(KCLASD(I),10)
        ENDIF
      ENDDO
C
      NSUM=NDUM

      DO I=1,NSUM
       DO K=1, NSUM
        IF (IHSTA(I).EQ.JHSTA(J)) THEN
          print *, IHSTA(I), IHSTA(J)
        ENDIF
       ENDDO
      ENDDO 
C-----------------------------------------------------------------------
      WRITE(63)NSUM,ID,RLAT,RLON,IHSTA,JHSTA,IVSTA,JVSTA,CID
      DTR=1.74532925E-2
      WRITE(6,20)NSUM
   20 FORMAT('STALST:  NUMBER OF PROFILE STATIONS ',I5)
      WRITE(6,30)(ID(N),RLAT(N)/DTR,RLON(N)/DTR
     1,               IHSTA(N),JHSTA(N),IVSTA(N),JVSTA(N)
     2,               CID(N),N=1,NSUM)
   30 FORMAT(2X,I6,2F8.2,4I8,1X,A8)
C-----------------------------------------------------------------------
      STOP
      END
      SUBROUTINE K2IJH(K,I,J)
C----------------------------------------------------------
C***
C***  COMPUTE I,J VALUES OF H POINTS FROM A ONE DIMENSIONAL K
C***
C----------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------
C***
C***  CONVERT FROM ONE DIMENSIONAL K TO I,J ON THE E-GRID
C***
      IMT=2*IM-1
      JX=(K-1)/IMT+1
      IX=K-(JX-1)*IMT
      IF(IX.LE.IM)THEN
        I=IX
        J=2*JX-1
      ELSE
        I=IX-IM
        J=2*JX
      ENDIF
C
      RETURN
      END
C
c     BLOCK DATA STN
c     INCLUDE "parmeta"
c     PARAMETER (LP1=LM+1)
c     INCLUDE "MAPOT.comm"
c     INCLUDE "griddef"
c     END BLOCK DATA STN
