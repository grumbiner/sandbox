      SUBROUTINE SPHRT7
     1(IDIR,GRID,WAVE,MLT,FAC,IMAX,JMAX,MAXWV,IROMB)
C
C  THIS ROUTINE PERFORMS INVERSE SPHERICAL TRANSFORM ONLY
C
C  IDIR.LT.0  WAVE TO GRID
C       IDIR= -1 ...... NORMAL TRANSFORM (GAUSSIAN GRID)
C             -2 ...... TRANSFORM WITH DPNM (GAUSSIAN GRID)
C           -101 ...... AS IDIR=-1 BUT FOR EQUIDISTANT LAT/LON GRID
C           -102 ...... AS IDIR=-2 BUT FOR EQUIDISTANT LAT/LON GRID
C
C        1  MULTIPLY 'FAC(NM)' TO WAVE VALUES
C  MLT = 0  NO MULTIPLY
C       -1  MULTIPLY 'I*FAC(NM)' (IMAGINARY) TO WAVE VALUES
C
C  IROMB = 1 FOR RHOMBOIDAL TRUNCATION
C        = 0 FOR TRIANGULAR TRUNCATION
C
C  NOTE
C          GRID   IS REAL*4 GRID VALUES OF   (IMAX,JMAX)
C          WAVE   IS REAL*4 WAVE VALUES OF   (  2,NMMAX)
C          WORK   IS REAL*4 WORKING ARRAY OF (IMAXP,JMAX,2)
C
C  THE FOLLOWING PARAMETER STATEMENT SPECIFIES MAXIMUM POSSIBLE
C  WAVE TRUNCATION HANDLED IN THIS PROGRAM
C
C  LIMWV ... MAXIMUM POSSIBLE WAVE NUMBER
C  LIMGAU .. MAXIMUM POSSIBLE GAUSSIAN LATITUDE
C  LROMB ... =0 TRIANGULAR =1 RHOMBOIDAL TRUNCATION
C
      PARAMETER (LIMWV=90,LIMGAU=140,LIMIMX=270,IXTRA=2,LROMB=0,
     1           LIMWVX=(LIMWV+1)*(LIMWV+2)/2*(1-LROMB)+
     2                  (LIMWV+1)*(LIMWV+1)*LROMB,
     3           MAXWK =(LIMIMX+IXTRA)*LIMGAU*2,LIMWV2=LIMWV*2)
C
      DIMENSION GRID(1),WAVE(1),FAC(1)
C
      REAL*8 PPNM(LIMWVX),HHNM(LIMWVX)
      REAL*8 GW(LIMGAU),GL(LIMGAU),GWCS(LIMGAU)
      REAL*8 DLAT,SL,PI,HFPI
C
      DIMENSION WORK(MAXWK)
      DIMENSION PNM (LIMWVX)
      DIMENSION DDW(LIMGAU,6)
      DIMENSION SYMM(LIMWVX)
      DIMENSION COSCLT(LIMGAU)
      DIMENSION IFAX(10),DTRIGS(300)
C
      DIMENSION FWRK(LIMWV2)
      DIMENSION NFAC(10)
      LOGICAL LSFFT
C
      DATA NFAC/32,64,72,96,128,144,192,216,256,288/
C
      DATA MFP1/0/,IRMB1/-999/,IFP/0/
C
C-----CUT DOWN ON PRINTOUT
C
       DATA IFRST1/1/,IFRST2/1/
C
      LSFFT=.FALSE.
      DO 3 N=1,10
      IF(IMAX.EQ.NFAC(N)) LSFFT=.TRUE.
    3 CONTINUE
C
      IF(IFRST1 .NE.0)THEN
 8001 FORMAT(5X,'IMAX,JMAX,MAXWV,IROMB=',4I8)
      WRITE(6,8001) IMAX,JMAX,MAXWV,IROMB
      IFRST1=0
      ENDIF
C
      IMAXP=IMAX+IXTRA
      JMAXHF=(JMAX+1)/2
      IJHMAX=IMAX*JMAXHF
      IPJHMX=IMAXP*JMAXHF
      IJMAX=IMAX*JMAX
      IPJMAX=IMAXP*JMAX
      MEND1=MAXWV+1
      MEND1T=MEND1*2
      NMMAX=(MEND1+1)*MEND1/2
      IF(IROMB.EQ.1) NMMAX=MEND1*MEND1
      NMMAXT=NMMAX*2
      JMH=JMAX/2
C
      IF(IFRST2 .NE.0)THEN
 8002 FORMAT(5X,'IMAXP,JMAXHF,IJHMAX,IPJHMX,IJMAX,IPJMAX',6I8)
      WRITE(6,8002) IMAXP,JMAXHF,IJHMAX,IPJHMX,IJMAX,IPJMAX
 8003 FORMAT(5X,'MEND1,MEND1T,NMMAX,NMMAXT',4I8)
      WRITE(6,8003) MEND1,MEND1T,NMMAX,NMMAXT
      IFRST2=0
      ENDIF
C
      IF (IDIR .GT. 0) THEN
      WRITE(6,8004) IDIR
 8004 FORMAT(5X,'IDIR =',I4,' IS .GT. 0 - NOT PERMITTED ')
      ENDIF
      IF(NMMAX*2.GT.IMAX*JMAX) THEN
      WRITE(6,8007)
 8007 FORMAT(5X,'*** POSSIBLE ARGUMENT LIST ERROR',/,5X,
     1  '*** WAVE DATA HAS MORE DEGREE OF FREEDOM THAN GRID DATA')
      ENDIF
C
      IF(NMMAX*2.GT.IMAXP*JMAX) THEN
      IXREQ=FLOAT(NMMAX*2)/FLOAT(JMAX)-FLOAT(IMAX)+1
      WRITE(6,8008) IXREQ
 8008 FORMAT(5X,'INCREASE IXTRA OF SUBROUTINE SPHRT7 TO ',I8)
      STOP 'ABEND'
      ENDIF
C
      IF(LSFFT) THEN
      IF(IFP.EQ.IMAX) GO TO 1
      IMODE=2
      CALL SFAX(IFAX,IMAX,IMODE)
      CALL SFTRIG(DTRIGS,IMAX,IMODE)
      IFP=IMAX
    1 CONTINUE
      ENDIF
C
      IF(MFP1.EQ.MAXWV.AND.IRMB1.EQ.IROMB)  GO TO 2
      NM=0
      DO 10 MM=1,MEND1
      NEND1=MEND1
      IF(IROMB.EQ.1) NEND1=MM+MEND1-1
      DO 20 NN=MM,NEND1
      NM=NM+1
      MODNM=MOD(NN-MM,2)
      IF(MODNM.EQ.1) GO TO 11
      SYMM(NM)=1.
      GO TO 20
   11 CONTINUE
      SYMM(NM)=-1.
   20 CONTINUE
   10 CONTINUE
      MFP1=MAXWV
      IRMB1=IROMB
    2 CONTINUE
C
      IF(IDIR.GT.-10) THEN
      CALL GAUAW7(GL,GW,JMAX)
      ELSE
      PI=4.D0*ATAN(1.D0)
      HFPI=0.5D0*PI
      DLAT=PI/DFLOAT(JMAX-1)
      ENDIF
C
C  END OF CONSTANTS
C
C  BACKWARD TRANSFORM (WAVE TO GRID)
C
C  ZERO CLEAR GRIDWK
C
      DO 205 IJ=1,IPJMAX
      WORK(IJ)=0.
  205 CONTINUE
C
      DO 250 J=1,JMAXHF
C
      IF(IDIR.GT.-10) THEN
      CALL LGNDR7(GL(J),MEND1,PPNM,HHNM,IROMB)
C
        IF(IDIR.EQ.-1) THEN
        DO 120 NM=1,NMMAX
        PNM(NM)=SNGL(PPNM(NM))
  120   CONTINUE
        ENDIF
        IF(IDIR.EQ.-2) THEN
        DO 130 NM=1,NMMAX
        PNM(NM)=SNGL(HHNM(NM))
  130   CONTINUE
        ENDIF
C
      ELSE
C
        SL=SIN(HFPI-DFLOAT(J-1)*DLAT)
        CALL LGNDR7(SL,MEND1,PPNM,HHNM,IROMB)
        IF(IDIR.EQ.-101) THEN
        DO 21 NM=1,NMMAX
        PNM(NM)=SNGL(PPNM(NM))
   21   CONTINUE
        ENDIF
        IF(IDIR.EQ.-102) THEN
        DO 22 NM=1,NMMAX
        PNM(NM)=SNGL(HHNM(NM))
   22   CONTINUE
        ENDIF
C
      ENDIF
C
      IX2 =IMAXP*(J-1)
      IX2R=IMAXP*(JMAX-J)
      DO 250 II=1,2
      IX3 =IX2 +II-2
      IX3R=IX2R+II-2
      IY2 =II-2
C
      IF(MLT.NE.0) GO TO 261
      DO 260 NM=1,NMMAX
      WORK(IPJMAX+NM)=PNM(NM)*WAVE(NM*2+IY2)
  260 CONTINUE
      GO TO 269
  261 CONTINUE
      IF(MLT.EQ.-1) GO TO 263
C  MULTIPLY 'FAC'
      DO 262 NM=1,NMMAX
      WORK(IPJMAX+NM)=PNM(NM)*WAVE(NM*2+IY2)*FAC(NM)
  262 CONTINUE
      GO TO 269
C  MULTIPLY 'I*FAC'
  263 CONTINUE
      IF(II.EQ.2) GO TO 266
C  REAL PART
      DO 264 NM=1,NMMAX
      WORK(IPJMAX+NM)=PNM(NM)*(-WAVE(NM*2+IY2+1)*FAC(NM))
  264 CONTINUE
      GO TO 269
C  IMAGINARY PART
  266 CONTINUE
      DO 267 NM=1,NMMAX
      WORK(IPJMAX+NM)=PNM(NM)*WAVE(NM*2+IY2-1)*FAC(NM)
  267 CONTINUE
C
  269 CONTINUE
      NM=0
      DO 270 MM=1,MEND1
      IX4=IX3+2*MM
      NEND1=MEND1
      IF(IROMB.EQ.1) NEND1=MM+MEND1-1
      DO 280 NN=MM,NEND1
      NM=NM+1
      WORK(IX4)=WORK(IX4)+WORK(IPJMAX+NM)
  280 CONTINUE
  270 CONTINUE
C
  275 CONTINUE
C  GLOBAL CASE
      IF(J.GT.JMH) GO TO 250
      NM=0
      DO 470 MM=1,MEND1
      IX4R=IX3R+2*MM
      NEND1=MEND1
      IF(IROMB.EQ.1) NEND1=MM+MEND1-1
      IF(IDIR.EQ.-2.OR.IDIR.EQ.-102) GO TO 485
      DO 480 NN=MM,NEND1
      NM=NM+1
      WORK(IX4R)=WORK(IX4R)+WORK(IPJMAX+NM)*SYMM(NM)
  480 CONTINUE
      GO TO 495
  485 CONTINUE
C
C  CASE OF DPNM
      DO 490 NN=MM,NEND1
      NM=NM+1
      WORK(IX4R)=WORK(IX4R)-WORK(IPJMAX+NM)*SYMM(NM)
  490 CONTINUE
  495 CONTINUE
C
  470 CONTINUE
C
  250 CONTINUE
  200 CONTINUE
C
      IF(LSFFT) THEN
      CALL SFT991(WORK,WORK(IPJMAX+1),DTRIGS,IFAX,1,IMAXP,IMAX,JMAX,+1,
     1            DDW(1,1),DDW(1,2),DDW(1,3),DDW(1,4),DDW(1,5),DDW(1,6))
      DO 290 J=1,JMAX
      IX2=IMAX *(J-1)
      IY2=IMAXP*(J-1)
      DO 290 I=1,IMAX
      GRID(IX2+I)=WORK(IY2+I)
  290 CONTINUE
      ELSE
      IMAXT=IMAX*2
      DO 1212 J=1,JMAX
      IX1=IMAX *(J-1)
      IX2=IMAXP*(J-1)
      DO 1213 I=1,IMAXT
      FWRK(I)=0.
 1213 CONTINUE
      DO 1214 I=1,IMAX
      FWRK(I)=WORK(IX2+I)
 1214 CONTINUE
      DO 1215 I=3,IMAX,2
      FWRK(IMAXT-I+2)= WORK(IX2+I  )
      FWRK(IMAXT-I+3)=-WORK(IX2+I+1)
 1215 CONTINUE
      CALL FOURT(FWRK,IMAX,1,1,1,WORK(IPJMAX+1))
      II=0
      DO 1216 I=1,IMAXT,2
      II=II+1
      GRID(IX1+II)=FWRK(I)
 1216 CONTINUE
 1212 CONTINUE
      ENDIF
C
      RETURN
      END