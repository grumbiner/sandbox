      SUBROUTINE CNBLK (JTH1,JTH2,JTH1P,JTH2P,JQ1P,JQ2P)
  
      COMMON/CNB22/M,MM,AT(4,44),BT(4,44),CT(4,44),DT(4,44),ET(4,44)
     1 ,FT(4,44)
      COMMON/CNQSC/QT(4,44) 
      COMMON W(46,50) 
  
      REAL GAM(4,44),A(4,44),Y1(44),Y2(44)
C     MODIFIED TO ACCEPT POINTERS IN THE CALL, RATHER THAN PASS 
C       VARIABLE SIZED ARRAYS. 10/29/83 BG
C     STATEMENT FUNCTIONS REMOVED FOR FTN5 8/5/83.
C     THIS SOLVES A BLOCK TRIDIAGONAL EQUATION
C     THE TYPICAL FORM OF WHICH IS
C     BT(I)TH(I-1)+AT(I)TH(I)+CT(I)TH(I+1)= 
C     DT(I)THM(I-1)+FT(I)THM(I-1)+ET(I)THM(I+1)+QT(I)S(I) 
  
C     THIS IS SUITABLE FOR EXPRESSING A CRANK-NICKELSON TYPE
C     DIFFUSION DIFF. EQ IN 2 COUPLED VARIABLES 
  
C     BT,AT,CT ARE THE ARRAYS FORMING THE LEFT HAND TRIDIAGONAL 
C     WHICH IS SOLVED USING THE METHODS FROM I. AND K.
C     DT,ET,FT ARE THE ARRAYS FORMING THE RIGHT HAND TRIDIAGONAL
C     WHICH CONTRIBUTES TO THE FORCING. 
C     QT PREMULTIPLIES S
C     IN THESE ARRAYS THE FIRST SUBSCRIPT GOES COLUMNWISE THRU
C     THE 4 ELEMENTS OF A GIVEN BLOCK SUBMATRIX.THE SECOND SUBSCRIPT
C     DENOTES A PARTICULAR BLOCK ROW. 
C     TH1,TH2 ARRAYS ARE COMPONENTS OF THE TH VECTORS 
C     THM1,THM2 ARRAYS ARE COMPONENTS OF THE THM VECTORS
C       THM1 AND THM2 CORRESPOND TO W(MZ,JTH1P AND JTH2P) RESPECTIVELY. 
C     S1,S2 ARRAYS ARE COMPONENTS OF THE S VECTORS
C       S11 AND S2 CORRESPOND TO W(MZ,JQ1P AND JQ2P) RESPECTIVELY.
C     SECOND HALF OF M BY 2X2 BLOCK TRIDIAGONAL SOLVER
C     NOTATION IN COMMENTS FOLLOWS ISSACSON AND KELLER P59-61 
  
      DO 200 I=1,M
C       COMPUTE FORCING F VECTOR
C       SUBTRACT B(I)Y(I-1)TERM FROM FORCING IF I.GT.1
       F1=(QT(1,I)*W(I+1,JQ1P)+QT(3,I)*W(I+1,JQ2P))+ET(1,I)*W(I+1,JTH1P) 
     1    +ET(3,I)*W(I+1,JTH2P) 
        F2=QT(2,I)*W(I+1,JQ1P)+QT(4,I)*W(I+1,JQ2P)+ET(2,I)*W(I+1,JTH1P) 
     1    +ET(4,I)*W(I+1,JTH2P) 
        IF (I.EQ.1) GO TO 201 
        F1=F1+DT(1,I)*W(I,JTH1P)+DT(3,I)*W(I,JTH2P)-BT(1,I)*Y1(I-1) 
     1    -BT(3,I)*Y2(I-1)
        F2=F2+DT(2,I)*W(I,JTH1P)+DT(4,I)*W(I,JTH2P)-BT(2,I)*Y1(I-1) 
     1    -BT(4,I)*Y2(I-1)
  
201     IF (I.NE.M) THEN
          F1=F1+FT(1,I)*W(I+2,JTH1P)+FT(3,I)*W(I+2,JTH2P) 
          F2=F2+FT(2,I)*W(I+2,JTH1P)+FT(4,I)*W(I+2,JTH2P) 
        ENDIF 
  
C       COMPUTE Y(I)=(INV A(I))*(F(I)-B(I)*Y(I-1))
        Y1(I)=A(1,I)*F1+A(3,I)*F2 
        Y2(I)=A(2,I)*F1+A(4,I)*F2 
200   CONTINUE
  
C     COMPUTE X(I)=Y(I)-GAMMA(I)X(I+1)
      W(M+1,JTH1)=Y1(M) 
      W(M+1,JTH2)=Y2(M) 
      I=M 
      DO 300 II=1,MM
        I=I-1 
        W(I+1,JTH1)=Y1(I)-GAM(1,I)*W(I+2,JTH1)-GAM(3,I)*W(I+2,JTH2) 
        W(I+1,JTH2)=Y2(I)-GAM(2,I)*W(I+2,JTH1)-GAM(4,I)*W(I+2,JTH2) 
300   CONTINUE
  
      RETURN
  
      ENTRY CNBTRI
C     FIRST HALF OF M BY 2X2 BLOCK TRIDIAGONAL SOLVER 
C     NOTATION IN COMMENTS FOLLOWS ISSACSON AND KELLER P59-61 
      MM=M-1
C     A(1)= SLANT A(1)
      DO 110 L=1,4
110     A(L,1)=AT(L,1)
      I=1 
120   CONTINUE
C     COMPUTE A(I) INVERSE
      DET=A(1,I)*A(4,I)-A(2,I)*A(3,I) 
      TEM=A(1,I)
      A(1,I)=A(4,I)/DET 
      A(2,I)=-A(2,I)/DET
      A(3,I)=-A(3,I)/DET
      A(4,I)=TEM/DET
      IF (I.GE.M) GO TO 122 
  
C     COMPUTE GAMMA(I)=(INV A(I))*(C(I))
      GAM(1,I)=A(1,I)*CT(1,I)+A(3,I)*CT(2,I)
      GAM(2,I)=A(2,I)*CT(1,I)+A(4,I)*CT(2,I)
      GAM(3,I)=A(1,I)*CT(3,I)+A(3,I)*CT(4,I)
      GAM(4,I)=A(2,I)*CT(3,I)+A(4,I)*CT(4,I)
  
      I=I+1 
C     COMPUTE A(I)=SLANT A(I)-B(I)*GAMMA(I-1) 
      A(1,I)=AT(1,I)-BT(1,I)*GAM(1,I-1)-BT(3,I)*GAM(2,I-1)
      A(2,I)=AT(2,I)-BT(2,I)*GAM(1,I-1)-BT(4,I)*GAM(2,I-1)
      A(3,I)=AT(3,I)-BT(1,I)*GAM(3,I-1)-BT(3,I)*GAM(4,I-1)
      A(4,I)=AT(4,I)-BT(2,I)*GAM(3,I-1)-BT(4,I)*GAM(4,I-1)
      GO TO 120 
  
122   CONTINUE
  
  
      RETURN
      END 
