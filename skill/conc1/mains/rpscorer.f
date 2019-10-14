C This program calculates the average and the standard 
C deviation of numbers from 7 different files.

C rpscorer.f
      PROGRAM AB test
      IMPLICIT NONE
 
      REAL A(28),B(28),C(28),SDA,SDB
      REAL D(28),E(28),F(28),SDD,SDE
      REAL G(28),H(28),J(28),SDG,SDH
      REAL K(28),L(28),M(28),SDK,SDL
      REAL O(28),P(28),Q(28),SDO,SDP
      REAL R(28),S(28),U(28),SDR,SDS
      REAL X(28),Y(28),Z(28),SDX,SDY
      REAL average

      INTEGER n
  
      CALL GET1 (a,b,c,n)
      CALL GET2 (d,e,f,n)
      CALL GET3 (g,h,j,n)
      CALL GET4 (k,l,m,n)
      CALL GET5 (o,p,q,n)
      CALL GET6 (r,s,u,n)
      CALL GET7 (x,y,z,n)

      OPEN (7, FILE = 'scores') 
      WRITE (7,9005) average (A,n),average (B,n),SDA (A,n),SDB (B,n)
      WRITE (7,9005) average (D,n),average (E,n),SDD (D,n),SDE (E,n)
      WRITE (7,9005) average (G,n),average (H,n),SDG (G,n),SDH (H,n)
      WRITE (7,9005) average (K,n),average (L,n),SDK (K,n),SDL (L,n)
      WRITE (7,9005) average (O,n),average (P,n),SDO (O,n),SDP (P,n)
      WRITE (7,9005) average (R,n),average (S,n),SDR (R,n),SDS (S,n)
      WRITE (7,9005) average (X,n),average (Y,n),SDX (X,n),SDY (Y,n)
 
 9005 FORMAT (' ',4F7.3)

      END


      REAL FUNCTION average (Y,n)
      INTEGER i,n  
      REAL Y(28), aver

      aver = 0.

      DO 4000 i = 1,n
         aver = aver + Y(i) 
 4000 CONTINUE

      average = aver/n

      RETURN 
      END



      REAL FUNCTION SDY (Y,n)
      INTEGER i,n
      REAL Y(28),Var,averY,average

      averY = average (Y,n)
      Var = 0.
      DO 6000 i = 1,n
         Var = Var + ((Y(i) - averY)**2)
 6000 CONTINUE 

      SDY = SQRT(Var/n) 
      RETURN
      END


      REAL FUNCTION SDX (X,n)
      INTEGER i,n
      REAL X(28),Var,averX,average

      averX = average (Y,n)
      Var = 0.
      DO 6001 i = 1,n
         Var = Var + ((X(i) - averX)**2)
 6001 CONTINUE 

      SDX = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDA (a,n)
      INTEGER i,n
      REAL A(28),Var,averA,average

      averA = average (Y,n)
      Var = 0.
      DO 6002 i = 1,n
         Var = Var + ((A(i) - averA)**2)
 6002 CONTINUE 

      SDA = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDB (b,n)
      INTEGER i,n
      REAL B(28),Var,averB,average

      averB = average (Y,n)
      Var = 0.
      DO 6003 i = 1,n
         Var = Var + ((B(i) - averB)**2)
 6003 CONTINUE 

      SDB = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDD (d,n)
      INTEGER i,n
      REAL D(28),Var,averD,average

      averD = average (Y,n)
      Var = 0.
      DO 6004 i = 1,n
         Var = Var + ((D(i) - averD)**2)
 6004 CONTINUE 

      SDD = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDE (e,n)
      INTEGER i,n
      REAL E(28),Var,averE,average

      averE = average (Y,n)
      Var = 0.
      DO 6005 i = 1,n
         Var = Var + ((E(i) - averE)**2)
 6005 CONTINUE 

      SDE = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDG (g,n)
      INTEGER i,n
      REAL G(28),Var,averG,average

      averG = average (Y,n)
      Var = 0.
      DO 6006 i = 1,n
         Var = Var + ((G(i) - averG)**2)
 6006 CONTINUE 

      SDG = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDH (h,n)
      INTEGER i,n
      REAL H(28),Var,averH,average

      averH = average (Y,n)
      Var = 0.
      DO 6007 i = 1,n
         Var = Var + ((H(i) - averH)**2)
 6007 CONTINUE 

      SDH = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDK (k,n)
      INTEGER i,n
      REAL K(28),Var,averK,average

      averK = average (Y,n)
      Var = 0.
      DO 6008 i = 1,n
         Var = Var + ((K(i) - averK)**2)
 6008 CONTINUE 

      SDK = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDL (l,n)
      INTEGER i,n
      REAL L(28),Var,averL,average

      averL = average (Y,n)
      Var = 0.
      DO 6009 i = 1,n
         Var = Var + ((L(i) - averL)**2)
 6009 CONTINUE 

      SDL = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDO (o,n)
      INTEGER i,n
      REAL O(28),Var,averO,average

      averO = average (Y,n)
      Var = 0.
      DO 6010 i = 1,n
         Var = Var + ((O(i) - averO)**2)
 6010 CONTINUE 

      SDO = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDP (p,n)
      INTEGER i,n
      REAL P(28),Var,averP,average

      averP = average (Y,n)
      Var = 0.
      DO 6011 i = 1,n
         Var = Var + ((P(i) - averP)**2)
 6011 CONTINUE 

      SDP = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDR (r,n)
      INTEGER i,n
      REAL R(28),Var,averR,average

      averR = average (Y,n)
      Var = 0.
      DO 6013 i = 1,n
         Var = Var + ((R(i) - averR)**2)
 6013 CONTINUE 

      SDR = SQRT(Var/n) 
      RETURN
      END

      REAL FUNCTION SDS (s,n)
      INTEGER i,n
      REAL S(28),Var,averS,average

      averS = average (Y,n)
      Var = 0.
      DO 6012 i = 1,n
         Var = Var + ((S(i) - averS)**2)
 6012 CONTINUE 

      SDS = SQRT(Var/n) 
      RETURN
      END

