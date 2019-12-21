      SUBROUTINE LAP(X,D2X,IDIM,JDIM,SLAT,DEGINT) 
      DIMENSION X(IDIM,JDIM),D2X(IDIM,JDIM)
ccx      COMMON/BB/SLAT,XNLAT,WLON,ELON,DLAT,DLON
c
      SAVE
c
      write(6,601) IDIM,JDIM,SLAT,DEGINT
  601 format(1h ,'   BEGIN LAP --',/,1x,2i5,2f6.1) 
      DEL = 111.1*1000.*DEGINT 
      IDM1 = IDIM -1
      JDM1 = JDIM -1
      DO 10 I=2,IDM1
      DO 10 J=2,JDM1
      XLAT = (J-1.)*DEGINT + SLAT
      RLAT = XLAT*3.14159/180.
      DDLON = COS(RLAT)
      G2X = (X(I+1,J)+X(I-1,J)-2.*X(I,J))/((DDLON*DEL)**2)
      G2Y = (X(I,J+1)+X(I,J-1)-2.*X(I,J))/((DEL)**2)
      D2X(I,J) = G2X + G2Y
        IDH = IDM1/2 
        JDX = 1./DEGINT 
        if(i.eq.idh.and.mod(j,jdx).eq.0) 
     *   write(6,611) i,j,x(i,j),d2x(i,j)
  611    format(1h ,' i,j',2i4,' X',f6.1,' D2X',e15.5)
   10 CONTINUE
      write(6,699) 
 699  format(1h ,'END of LAP')
      RETURN
      END
