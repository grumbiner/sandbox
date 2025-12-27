      SUBROUTINE FILT25(ZI,TM,IPASS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    FILTER      FILTERS THE ARRAY ZI                       
C   PRGMMR: DIMEGO           ORG: W/NP22     DATE: 86-07-18             
C                                                                       
C ABSTRACT: FILTERS AN ARRAY USING A 25PT BLECK FILTER IN THE           
C   INTERIOR OF THE DOMAIN                                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   86-07-18  G DIMEGO - ORIGINATOR
C   88-09-23  B SCHMIDT - ADDED THE DOCBLOCK                              
C   90-11-27  G DIMEGO - LEFT Z AS INTERNAL WORK ARRAY ON CRAY
C   93-06-21  R TREADON - STREAMLINED CODE
C   95-06-21  T BLACK - MODIFIED FOR THE E-GRID
C   99-08-25  T BLACK - MODIFIED FOR DISTRIBUTED MEMORY
C                                                                       
C USAGE:    CALL FILTER (IDIM1,IDIM2,JDIM1,JDIM2,ZI,IPASS)                        
C   INPUT ARGUMENT LIST:                                                
C     ZI       - ARRAY CONTAINING THE ARRAY TO BE FILTERED              
C     TM       - ARRAY CONTAINING THE TOPOGRAPHY MASK
C     IPASS    - NUMBER OF PASSES THROUGH THE FILTER                    
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     ZI       - ARRAY CONTAINING THE FILTERED FIELD                    
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C    MACHINE: IBM SP                                                           
C                                                                       
C$$$                                                                    
C----------------------------------------------------------------
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
C----------------------------------------------------------------
      INCLUDE "INDX.comm"
C----------------------------------------------------------------
      REAL ZI(IDIM1:IDIM2,JDIM1:JDIM2),Z(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL TM(IDIM1:IDIM2,JDIM1:JDIM2)
C
      DATA CF1/0.279372/,CF2/0.171943/,CF3/-0.006918/
     1,    CF4/0.077458/,CF5/-0.024693/,CF6/-0.012940/
C----------------------------------------------------------------
C
      IPC=IPASS                                                       
C
      DO J=JDIM1,JDIM2
      DO I=IDIM1,IDIM2
        Z(I,J)=0.
      ENDDO
      ENDDO
C***
C***  FILTER THE INTERIOR POINTS WITH 25-PT BLECK FILTER               
C***
      DO 105 IP=1,IPC                                                 
C
!$omp parallel do private(htot,i,iihe,iihw,rsumh,sumh)
      DO 102 J=MYJS4,MYJE4
      DO 101 I=MYIS2,MYIE2
C
      IIHE=I+IHE(J)
      IIHW=I+IHW(J)
c     if(mype.eq.6.and.i.eq.16.and.j.eq.34)then
c       write(6,12345)tm(i,j),ihe(j),ihw(j),l
c       write(6,12346)tm(IIHW,J-1),tm(IIHE,J-1),tm(IIHE,J+1),
c    1                tm(IIHW,J+1)
c       write(6,12347)tm(I-1,J-2),tm(I+1,J-2),tm(I+1,J+2),
c    1                tm(I-1,J+2)
c       write(6,12348)tm(I,J-2),tm(I,J+2),tm(I+1,J),
c    1                tm(I-1,J)
c       write(6,12349)tm(IIHE,J-3),tm(IIHE+1,J-1),tm(IIHW,J-3),
c    1                tm(IIHE+1,J+1)
c       write(6,12350)tm(IIHW-1,J-1),tm(IIHE,J+3),tm(IIHW-1,J+1),
c    1                tm(IIHW,J+3)
c       write(6,12351)tm(I,J-4),tm(I+2,J),tm(I-2,J),
c    1                tm(I,J+4)
12345   format(' mask=',f2.0,' ihe=',i2,' ihw=',i2,' l=',i2)
12346   format(' 2nd row=',4(1x,f2.0))
12347   format(' 3rd row=',4(1x,f2.0))
12348   format(' 4th row=',4(1x,f2.0))
12349   format(' 5th row=',4(1x,f2.0))
12350   format(' 6th row=',4(1x,f2.0))
12351   format(' 7th row=',4(1x,f2.0))
c     endif
      HTOT=TM(I,J)                              
     1    +TM(IIHW,J-1)+TM(IIHE,J-1)+TM(IIHE,J+1)+TM(IIHW,J+1)
     2    +TM(I-1,J-2)+TM(I+1,J-2)+TM(I+1,J+2)+TM(I-1,J+2)
     3    +TM(I,J-2)+TM(I,J+2)+TM(I+1,J)+TM(I-1,J)
     4    +TM(IIHE,J-3)+TM(IIHE+1,J-1)+TM(IIHW,J-3)+TM(IIHE+1,J+1) 
     5    +TM(IIHW-1,J-1)+TM(IIHE,J+3)+TM(IIHW-1,J+1)+TM(IIHW,J+3)
     6    +TM(I,J-4)+TM(I+2,J)+TM(I-2,J)+TM(I,J+4)
C
      IF(HTOT.GT.0.)THEN
        SUMH=CF1*TM(I,J)                              
     1   +CF2*(TM(IIHW,J-1)+TM(IIHE,J-1)+TM(IIHE,J+1)+TM(IIHW,J+1))
     2   +CF3*(TM(I-1,J-2)+TM(I+1,J-2)+TM(I+1,J+2)+TM(I-1,J+2))
     3   +CF4*(TM(I,J-2)+TM(I,J+2)+TM(I+1,J)+TM(I-1,J))
     4   +CF5*(TM(IIHE,J-3)+TM(IIHE+1,J-1)+TM(IIHW,J-3)+TM(IIHE+1,J+1) 
     5        +TM(IIHW-1,J-1)+TM(IIHE,J+3)+TM(IIHW-1,J+1)+TM(IIHW,J+3))
     6   +CF6*(TM(I,J-4)+TM(I+2,J)+TM(I-2,J)+TM(I,J+4))
C
        RSUMH=1./SUMH
C
      Z(I,J)=(CF1*ZI(I,J)*TM(I,J)                              
     1  +CF2*(ZI(IIHW,J-1)*TM(IIHW,J-1)+ZI(IIHE,J-1)*TM(IIHE,J-1)
     2       +ZI(IIHE,J+1)*TM(IIHE,J+1)+ZI(IIHW,J+1)*TM(IIHW,J+1))
     3  +CF3*(ZI(I-1,J-2)*TM(I-1,J-2)+ZI(I+1,J-2)*TM(I+1,J-2)
     4       +ZI(I+1,J+2)*TM(I+1,J+2)+ZI(I-1,J+2)*TM(I-1,J+2))
     5  +CF4*(ZI(I,J-2)*TM(I,J-2)+ZI(I,J+2)*TM(I,J+2)
     6       +ZI(I+1,J)*TM(I+1,J)+ZI(I-1,J)*TM(I-1,J))
     7  +CF5*(ZI(IIHE,J-3)*TM(IIHE,J-3)+ZI(IIHE+1,J-1)*TM(IIHE+1,J-1)
     8       +ZI(IIHW,J-3)*TM(IIHW,J-3)+ZI(IIHE+1,J+1)*TM(IIHE+1,J+1) 
     9       +ZI(IIHW-1,J-1)*TM(IIHW-1,J-1)+ZI(IIHE,J+3)*TM(IIHE,J+3)
     o       +ZI(IIHW-1,J+1)*TM(IIHW-1,J+1)+ZI(IIHW,J+3)*TM(IIHW,J+3))
     1  +CF6*(ZI(I,J-4)*TM(I,J-4)+ZI(I+2,J)*TM(I+2,J)
     2       +ZI(I-2,J)*TM(I-2,J)+ZI(I,J+4)*TM(I,J+4)))
     3  *RSUMH
      ENDIF
C
  101 CONTINUE                                                    
  102 CONTINUE                                                       
C
!$omp parallel do 
      DO J=MYJS4,MYJE4
      DO I=MYIS2,MYIE2
        IF(TM(I,J).GT.0.5)THEN
          ZI(I,J)=Z(I,J)                                         
        ENDIF
      ENDDO
      ENDDO
C
      CALL EXCH(ZI,1,4,4)
C
  105 CONTINUE                                                          
C
      RETURN                                                            
      END                                                               
