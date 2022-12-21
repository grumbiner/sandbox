      SUBROUTINE TDSC(MS,AQT) 
  
      COMMON/CNB22/M,MM,AT(4,44),BT(4,44),CT(4,44),DT(4,44),ET(4,44)
     1 ,FT(4,44)
      COMMON/CNQSC/QT(4,44) 
  
C     SCALE SUBMATRICES BY AQT INVERSE
      REAL A(4),AQT(4),SV(4)
C     STATEMENT FUNCTION PUT DIRECTLY INTO STATEMENTS. 8/5/83 
C     COMPUTE AQT INVERSE 
  
      DET=AQT(1)*AQT(4)-AQT(2)*AQT(3) 
      A(1)=AQT(4)/DET 
      A(2)=-AQT(2)/DET
      A(3)=-AQT(3)/DET
      A(4)=AQT(1)/DET 
      SV(1)=A(1)*AT(1,MS)+A(3)*AT(2,MS) 
      SV(2)=A(2)*AT(1,MS)+A(4)*AT(2,MS) 
      SV(3)=A(1)*AT(3,MS)+A(3)*AT(4,MS) 
      SV(4)=A(2)*AT(3,MS)+A(4)*AT(4,MS) 
  
      DO 11 I=1,4 
11    AT(I,MS)=SV(I)
  
      SV(1)=A(1)*BT(1,MS)+A(3)*BT(2,MS) 
      SV(2)=A(2)*BT(1,MS)+A(4)*BT(2,MS) 
      SV(3)=A(1)*BT(3,MS)+A(3)*BT(4,MS) 
      SV(4)=A(2)*BT(3,MS)+A(4)*BT(4,MS) 
  
      DO 12 I=1,4 
12    BT(I,MS)=SV(I)
  
      SV(1)=A(1)*CT(1,MS)+A(3)*CT(2,MS) 
      SV(2)=A(2)*CT(1,MS)+A(4)*CT(2,MS) 
      SV(3)=A(1)*CT(3,MS)+A(3)*CT(4,MS) 
      SV(4)=A(2)*CT(3,MS)+A(4)*CT(4,MS) 
  
      DO 13 I=1,4 
13    CT(I,MS)=SV(I)
  
      SV(1)=A(1)*DT(1,MS)+A(3)*DT(2,MS) 
      SV(2)=A(2)*DT(1,MS)+A(4)*DT(2,MS) 
      SV(3)=A(1)*DT(3,MS)+A(3)*DT(4,MS) 
      SV(4)=A(2)*DT(3,MS)+A(4)*DT(4,MS) 
  
      DO 14 I=1,4 
14    DT(I,MS)=SV(I)
  
      SV(1)=A(1)*ET(1,MS)+A(3)*ET(2,MS) 
      SV(2)=A(2)*ET(1,MS)+A(4)*ET(2,MS) 
      SV(3)=A(1)*ET(3,MS)+A(3)*ET(4,MS) 
      SV(4)=A(2)*ET(3,MS)+A(4)*ET(4,MS) 
      DO 15 I=1,4 
15    ET(I,MS)=SV(I)
      SV(1)=A(1)*FT(1,MS)+A(3)*FT(2,MS) 
      SV(2)=A(2)*FT(1,MS)+A(4)*FT(2,MS) 
      SV(3)=A(1)*FT(3,MS)+A(3)*FT(4,MS) 
      SV(4)=A(2)*FT(3,MS)+A(4)*FT(4,MS) 
      DO 16 I=1,4 
16    FT(I,MS)=SV(I)
      SV(1)=A(1)*QT(1,MS)+A(3)*QT(2,MS) 
      SV(2)=A(2)*QT(1,MS)+A(4)*QT(2,MS) 
      SV(3)=A(1)*QT(3,MS)+A(3)*QT(4,MS) 
      SV(4)=A(2)*QT(3,MS)+A(4)*QT(4,MS) 
      DO 17 I=1,4 
17    QT(I,MS)=SV(I)
      RETURN
      END 
