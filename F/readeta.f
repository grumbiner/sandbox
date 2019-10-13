c
      subroutine mapit(uu,vv,spch,airt,slp)
c
           real ipnts(4),jpnts(4)
           dimension  uu(349,277),vv(349,277),
     *     spch(349,277),airt(349,277),slp(349,277)
           dimension aarea(4),parea(4)
        data parea/30.,60.,140.,120./
        data aarea/22.,50.,82.,50./ 
c       data ipnts/1.0,1.0,349.0,349.0/
c       data jpnts/1.0,277.0,277.0,1.0/
c
        SAVE
c
         write(6,601) (parea(i),i=1,4)
         write(6,601) (aarea(i),i=1,4)
  601    format(1h ,' area degrees W',4f6.1)
         do 5 i=3,4
         parea(i) = 360.0 - parea(i)
         aarea(i) = 360.0 - aarea(i)
   5     continue
c
c  ETA LAMBER Conformal grid 211
c
c
c  ALAT1 = KGDS(4) * 0.001
c
       alat1 = 1000 * 0.001
c
c  ELON1 = KGDS(5) * 0.001 + 360.0
c
       elon1 = -145500 * 0.001 + 360.0
c
c  ELONV = KGDS(7) * 0.001 + 360.0
c
       elonv = -107000 * 0.001 + 360.0
c
c  ALATAN = KGDS(12) * 0.001
c
       alatan = 50000 * 0.001
c
c  DXX = KGDS(8) * 0.001
c
       dxx = 32463 * 0.001
c
c  DYY = KGDS(9) * 0.001
c
       dyy = 32463 * 0.001
c
c  DXM = Diameter in meters
c
         DXM = DXX * 1000.
c
c...  Given xi and xj convert to alat,elon of grid 
c
c     Subroutine W3FB12   LAMBERT(I,J) TO LAT/LON FOR GRID
C  
C     ALAT     - LATITUDE IN DEGREES (NEGATIVE IN SOUTHERN HEMI.)
C     ELON     - EAST LONGITUDE IN DEGREES, REAL*4
C     IERR     - .EQ. 0   IF NO PROBLEM
C                .GE. 1   IF THE REQUESTED XI,XJ POINT IS IN THE
C                         FORBIDDEN ZONE, I.E. OFF THE LAMBERT MAP
C                         IN THE OPEN SPACE WHERE THE CONE IS CUT.
C                  IF IERR.GE.1 THEN ALAT=999. AND ELON=999.
C
         NP =0
         NA =0
         nn = 0
         do 50  ii = 1,349
            xi = float(ii)
           do 49 jj = 1,277
               xj = float(jj)
c................................................................
c    Test for reference points
c       do ii = 1,4
c          xi = ipnts(ii)
c              xj = jpnts(ii)
c
         NN = NN + 1
        CALL W3FB12(XI,XJ,ALAT1,ELON1,DXM,ELONV,ALATAN,ALAT,ELON,ierr)
        IF(MOD(NN,100).eq.0)
     *  write(6,605) nn,ierr,xi,xj,parea(1),alat,parea(2),parea(3),
     *  elon, parea(4) 
        IF(MOD(NN,100).eq.0)
     *  write(6,605) nn,ierr,xi,xj,aarea(1),alat,aarea(2),aarea(3),
     *  elon, aarea(4) 
 605    format(1h ,'ETA ',i6,i4,2f6.1,' ALAT,ELON',3f6.1,3x,3f6.1) 
        if(ierr.eq.0)then
              if(alat.ge.parea(1).and.alat.le.parea(2).and.
     *           elon.ge.parea(3).and.elon.le.parea(4)) NP = NP + 1
               if(mod(np,50).ne.0) go to 48 
              if(alat.ge.parea(1).and.alat.le.parea(2).and.
     *           elon.ge.parea(3).and.elon.le.parea(4)) 
     *  write(6,60) np,ii,jj,alat,elon,slp(ii,jj),airt(ii,jj),
     *               uu(ii,jj),vv(ii,jj)               
  48       continue 
              if(alat.ge.aarea(1).and.alat.le.aarea(2).and.
     *           elon.ge.aarea(3).and.elon.le.aarea(4)) NA = NA + 1
               if(mod(na,50).ne.0) go to 49 
              if(alat.ge.aarea(1).and.alat.le.aarea(2).and.
     *           elon.ge.aarea(3).and.elon.le.aarea(4)) 
     *  write(6,60) na,ii,jj,alat,elon,slp(ii,jj),airt(ii,jj),
     *               uu(ii,jj),vv(ii,jj)               
   60       format(1x,i6,2i6,3x,6f8.2) 
c
c... Write for reference points
c               write(6,61) xi,xj,alat,elon
c  61        format(1x,' XI/XJ ',2f4.0,' lat/lon ',2f8.2)
c
           else
              print*,' REQUESTED XI,XJ FORBIDDEN ZONE'
          endif
   49      continue
   50      continue
           write(6,655) np,na
  655      format(1h ,'NP',i6,'   NA',i6) 
c
c... ENDDo for test
c          enddo
c...................................................................
c
c           enddo
c          enddo
             return
              end
