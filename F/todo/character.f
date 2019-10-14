      PROGRAM grace
	  
      INTEGER Sx,Sy,Sz,Wx,Wy,Wz,Ix,Iy,Iz,Dx,Dy,Dz,Cx,Cy,Cz,A
	  INTEGER Hx,Hy,Hz,S,W,I,D,C,H,ran, seed,choice
	  CHARACTER*60 name
	  
	  seed=LONG(362)
        A=0
 1000   continue
	  
	  A=A+1
	 
	  Sx=ran(6,seed)
	  Sy=ran(6, seed)
	  Sz=ran(6,seed)
	  Wx=ran(6,seed)
	  Wy=ran(6,seed)
	  Wz=ran(6,seed)
	  Ix=ran(6,seed)
	  Iy=ran(6,seed)
	  Iz=ran(6,seed)
	  Dx=ran(6,seed)
	  Dy=ran(6,seed)
	  Dz=ran(6,seed)
	  Cx=ran(6,seed)
	  Cy=ran(6,seed)
	  Cz=ran(6,seed)
	  Hx=ran(6,seed)
	  Hy=ran(6,seed)
	  Hz=ran(6,seed)
	  
	  S=Sx+Sy+Sz
	  W=Wx+Wy+Wz
	  I=Ix+Iy+Iz
	  D=Dx+Dy+Dz
	  C=Cx+Cy+Cz
	  H=Hx+Hy+Hz
	  IF (A .GT. 100) THEN
	       GO TO 100
	    ELSE
	       CONTINUE
	    ENDIF
	  IF( (S .LT.10 .OR.  W .LT. 10 .OR.  I .LT.10 .OR.  D.LT.10 
     1          .OR.  C.LT.10 ) .OR. (S .LT. 16 .AND.
     2           W .LT. 16 .AND. I .LT. 16 .AND. D .LT. 16 .AND.
     3            C .LT. 16) ) THEN 
            GO TO 1000
          ELSE 
	    CONTINUE
	  ENDIF
	  PRINT *,"What is your character's name?"
	  READ(*,9001) name
 9001 FORMAT (A60)
	  PRINT *,"            ",name
	  PRINT *,"Strength",S,  "   Intelligence",I
	  PRINT *,"Wisdom",W, "   Dexterity",D
	  PRINT *,"Constitution",C, "    Charisma",H
       PRINT *,"It took me ",A," tries to generate a usable character."
          A=0
100	  PRINT *,"Type in '1'if you want to create another character."
	  READ(*,9002)choice
9002  FORMAT (I6),(A6)
	  IF (choice  .EQ. 1) THEN
	     GO TO 1000
	  ELSE
	     CONTINUE
	  ENDIF
	      
	  PAUSE
	  END
	  
	  INTEGER FUNCTION ran(n,seed)
	  INTEGER n,temp,seed 
	  REAL temp2
	  
	  temp = MOD( (25173*seed+13849) ,65536)
	  seed = temp
	  
	  temp2 = (temp*n)/65536
	  ran = INT(temp2)+1
C	  PRINT *,temp, temp2, ran
      RETURN
	  END