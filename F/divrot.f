      SUBROUTINE DIVROT(U,V,DIV,ROT,IDIM,JDIM,SLAT,DEGINT)
      DIMENSION U(IDIM,JDIM),V(IDIM,JDIM)
      DIMENSION DIV(IDIM,JDIM),ROT(IDIM,JDIM),DEF(IDIM,JDIM)
      DIMENSION DEFC(IDIM,JDIM)
c      COMMON/BB/SLAT,XNLAT,WLON,ELON,DLAT,DLON
C     COMPUTE DIVERGENCE AND RELATIVE VORTICITY FROM THE WIND FIELD
c
      save
c
      write(6,650) slat,degint     
  650 format(1h ,'DIVROT     input SLAT',f6.1,'  DEGINT',f6.1)
      SUMR = 0.0
      SUMR2 = 0.0
      RMAX = -99999999.
      RMIN = 99999999.
      SUMS = 0.0
      SUMS2 = 0.0
      SMAX = -99999999.
      SMIN = 99999999.
      IDM = IDIM - 1
      JDM = JDIM - 1
      DIVMN = 9999.
      DIVMX = -9999.   
      ROTMN = 9999.
      ROTMX = -9999.  
C
      DO 10 I=2,IDM
      DO 10 J=2,JDM
      DELY = 111.1*1000.*DEGINT
      XLAT = float(J-1)*DEGINT + SLAT
      RLAT = XLAT*3.14159/180.
      DELX = 111.1*1000.*COS(RLAT)*DEGINT
      DIV(I,J) = (U(I+1,J) - U(I-1,J))/(2.*DELX)
     *         + (V(I,J+1) - V(I,J-1))/(2.*DELY)
      ROT(I,J) = (V(I+1,J) - V(I-1,J))/(2.*DELX)
     *         - (U(I,J+1) - U(I,J-1))/(2.*DELY)
      DEF(I,J) = (V(I+1,J) - V(I-1,J))/(2.*DELX)
     *         + (U(I,J+1) - U(I,J-1))/(2.*DELY)
      DEFC(I,J)= (U(I+1,J) - U(I-1,J))/(2.*DELX)
     *         - (V(I,J+1) - V(I,J-1))/(2.*DELY)
C ... CONVERT TO INVARIANT DEF
      DEF(I,J) = SQRT(DEF(I,J)**2 + DEFC(I,J)**2)
C
      if(rot(i,j).gt.rotmx) mxri = i
      if(rot(i,j).gt.rotmx) mxrj = j 
      if(rot(i,j).gt.rotmx) rotmx = rot(i,j)
      if(rot(i,j).lt.rotmn) mnri = i
      if(rot(i,j).lt.rotmn) mnrj = j
      if(rot(i,j).lt.rotmn) rotmn = rot(i,j) 
      if(div(i,j).gt.divmx) mxdi = i
      if(div(i,j).gt.divmx) mxdj = j 
      if(div(i,j).gt.divmx) divmx = div(i,j)
      if(div(i,j).lt.divmn) mndi = i
      if(div(i,j).lt.divmn) mndj = j
      if(div(i,j).lt.divmn) divmn = div(i,j)
      if(i.eq.31.and.j.eq.31) write(6,651) i,j,rot(i,j),div(i,j)
  651 format(1h ,'i,j',2i5,'   rot',e12.4,'  div',e12.4)
   10 CONTINUE
C
      DO 20 I=2,IDM
      DIV(I,1) = DIV(I,2)
      ROT(I,1) = ROT(I,2)
      DEF(I,1) = DEF(I,2)
      DEFC(I,1) = DEFC(I,2)
      DIV(I,JDIM) = DIV(I,JDM)
      ROT(I,JDIM) = ROT(I,JDM)
      DIV(I,JDIM) = DIV(I,JDM)
      DEFC(I,JDIM) = DEFC(I,JDM)
   20 CONTINUE
C
      DO 30 J=2,JDIM
      DIV(1,J) = DIV(2,J)
      ROT(1,J) = ROT(2,J)
      DEF(1,J) = DEF(2,J)
      DEFC(1,J) = DEFC(2,J)
      DIV(IDIM,J) = DIV(IDM,J)
      ROT(IDIM,J) = ROT(IDM,J)
      DEF(IDIM,J) = DEF(IDM,J)
      DEFC(IDIM,J) = DEFC(IDM,J)
   30 CONTINUE
C
      DIV(1,1) = DIV(2,2)
      DIV(1,JDIM) = DIV(2,JDM)
      DIV(IDIM,1) = DIV(IDM,2)
      DIV(IDIM,JDIM) = DIV(IDM,JDM)
      ROT(1,1) = ROT(2,2)
      ROT(1,JDIM) = ROT(2,JDM)
      ROT(IDIM,JDIM) = ROT(IDM,JDM)
      DEF(1,1) = DEF(2,2)
      DEF(1,JDIM) = DEF(2,JDM)
      DEF(IDIM,1) = DEF(IDM,2)
      DEF(IDIM,JDIM) = DEF(IDM,JDM)
      DEFC(1,1)= DEFC(2,2)
      DEFC(1,JDIM)=DEFC(2,JDM)
      DEFC(IDIM,1)=DEFC(IDM,2)
      DEFC(IDIM,JDIM)=DEFC(IDM,JDM)
C
      write(6,692)
  692 format(1h ,' ROT')
      write(6,691) mxri,mxrj,rotmx,mnri,mnrj,rotmn
  691 format(1h ,'MAX - i,j',2i5,e12.4,'   MIN - i,j',2i5,e12.4,/)
      write(6,693)
  693 format(1h ,' DIV') 
      write(6,691) mxdi,mxdj,divmx,mndi,mndj,divmn

      WRITE(6,601)
  601 FORMAT(1H0,'  END DIVROT XXX')
      RETURN
      END
