C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
      SUBROUTINE SSTHIRES (SST,SM,GLAT,GLON,IDAT,LIST,DTR,idate)
C
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE "parmeta.res"
C
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
C
      INTEGER IDATE(4),IDAT(3),MONTH(12)
      DIMENSION SSTLL(361,180),SALTLK(12),SALTLA(2),SALTLO(2)
C
      DIMENSION  SST(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
C
      DATA   INSST/39/
      DATA   INDXST/38/

      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
      DATA SALTLK/273.38,274.27,278.50,283.01,287.33,293.41
     1,           297.13,297.73,294.97,289.58,282.31,275.67/
C
C     CORNERS OF SALT LAKE LATITUDE/LONGITUDE BOX
C     in degrees---> 40.0     42.0            111.0    114.0
      DATA SALTLA/0.698132,0.733038/,SALTLO/1.937315,1.989675/
C
      CALL BAOPENR(INSST,'fort.39',IRETO)
      call baopen(38,'fort.38',ireto)
      IOUTUPRT = LIST
      CALL GRIBST(INSST,INDXST,SSTLL,IERR,idate)
      IF (IERR.NE.0) GOTO 4500
c     print*,'Printout of SST slice at I=190'
c     print*
c     do j=180,1,-1
c     do i=1,361
c      print*,'i,sst(i,180)=',i,sstll(i,180)
c     enddo
C
C----  INTERPOLATE 1-DEG GLOBAL SST TO ETA GRID  -------
C
C-CP NOTE:  THIS SUBROUTINE AND INTERPOLATION ALGORITHM ASSUME
C-CP A 1-DEG GLOBAL SST FIELD IN THE FOLLOWING FORMAT:  
C-CP
C-CP  I=1 AT 0.5 E,  I=2 AT 1.5 E, ... , I=360 at 0.5W
C-CP  J=1 AT 89.5S, J=2 AT 88.5 S, ..., J=180 at 89.5N
C-CP  
C-CP In the interpolation algorithm below, glon is positive westward,
C-CP from 0 to 360, with 0 at the greenwich meridian.  Elon is positive 
C-CP eastward, thus the need to subtract glon from 360 to get the index
C-CP of the correct oisst point.  If your input 1 deg SST field is in
C-CP a different indexing scheme, you will need to change the algorithm
C-CP below - see "grdeta.oldoi"
C-CP
      DO J=1,JM
      DO I=1,IM
      ELAT=H90+GLAT(I,J)/DTR
      ELON=H360-GLON(I,J)/DTR
      IF(ELON.GT.H360)ELON=ELON-H360
c     print*,'i,j,elat,elon=',i,j,elat-90.0,abs(elon-360.0)
      ILON1=INT(ELON)
      DIF=ELON-ILON1
      IF(DIF.GT.D5)ILON1=ILON1+1
      IF(ILON1.EQ.D00)ILON1=360
      ILON2=ILON1+1
      ILAT1=INT(ELAT)
      DIF=ELAT-ILAT1
      IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
c     if(i.eq.119.and.j.eq.387) then
c       print*,'h90,dtr=',h90,dtr
c       print*,'glat(i,j)=',glat(i,j)
c       print*,'glat(i,j)/dtr=',glat(i,j)/dtr
c       print*,'elat,ilat1,dif=',elat,ilat1,dif
c     endif
c     IF(ILAT1.EQ.180.OR.ILAT1.EQ.0)THEN
c       WRITE(6,6788)I,J,GLAT(I,J),GLON(I,J),ELAT,ELON
c6788   FORMAT(' I,J=',2I4,' GLAT=',E12.5,' GLON=',E12.5,
c    1   ' ELAT=',E12.5,' ELON=',E12.5)
c       STOP 333
c     ENDIF
      ILAT2=ILAT1+1
      W1=ELON-ILON1+D5
      IF(W1.LT.D00)W1=W1+H360
      W2=ELAT-ILAT1+D5
      AR1=W1*W2
      AR2=W1*(H1-W2)
      AR3=(H1-W1)*(H1-W2)
      AR4=(H1-W1)*W2
        
      if(ilat2.eq.181) then
       sst(i,j)=(sstll(ilon2,ilat1)+sstll(ilon1,ilat1))*0.5
c      sst(i,j)=271.3
      else
      SST(I,J) = AR1*SSTLL(ILON2,ILAT2)+AR2*SSTLL(ILON2,ILAT1)+
     1            AR3*SSTLL(ILON1,ILAT1)+AR4*SSTLL(ILON1,ILAT2)
      endif
c     print*,'i,j,sst(i,j)=',i,j,sst(i,j)
c     if(i.eq.119.and.j.eq.387) then
c     if(ilat2.gt.180) then
c       print*,'i,j=',i,j
c       print*,'sst(i,j)=',sst(i,j)
c       print*,'ilon1,ilat1=',ilon1,ilat1
c       print*,'ilon2,ilat2=',ilon2,ilat2
c       print*,'ar1,ar2,ar3,ar4=',ar1,ar2,ar3,ar4
c       print*,'sstll(ilon2,ilat2)=',sstll(ilon2,ilat2)
c       print*,'sstll(ilon2,ilat1)=',sstll(ilon2,ilat1)
c       print*,'sstll(ilon1,ilat1)=',sstll(ilon1,ilat1)
c       print*,'sstll(ilon1,ilat2)=',sstll(ilon1,ilat2)
c     endif
      ENDDO
      ENDDO
C***
C***  INSERT TEMPERATURES FOR THE GREAT SALT LAKE
C***
      ID1=IDAT(1)
      ID2=IDAT(2)
      MARG0=ID1-1
      IF(MARG0.LT.1)MARG0=12
      MNTH0=MONTH(MARG0)
      MNTH1=MONTH(ID1)
      IF(ID2.LT.15)THEN
        NUMER=ID2+MNTH0-15
        DENOM=MNTH0
        IARG1=MARG0
        IARG2=ID1
      ELSE
        NUMER=ID2-15
        DENOM=MNTH1
        IARG1=ID1
        IARG2=ID1+1
        IF(IARG2.GT.12)IARG2=1
      ENDIF
      FRAC=NUMER/DENOM
      DO J=1,JM
      DO I=1,IM
        IF(GLAT(I,J).GT.SALTLA(1).AND.GLAT(I,J).LT.SALTLA(2))THEN
          IF(GLON(I,J).GT.SALTLO(1).AND.GLON(I,J).LT.SALTLO(2))THEN
            IF(SM(I,J).GT.0.5)
     1        SST(I,J)=SALTLK(IARG1)+
     2                (SALTLK(IARG2)-SALTLK(IARG1))*FRAC
          ENDIF
        ENDIF
      ENDDO
      ENDDO
C
      RETURN
C
 4500 CONTINUE
C              ERROR OCCURRED WHEN INPUTING SST FROM GRIB.
      WRITE (IOUTUPRT, 4550) INSST
 4550 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN SST        ',
     1             'ON UNIT', I3, ' GRIB ' /
     2        ' ', 'EXECUTION TERMINATING.')
C
      STOP 222
C
      END
