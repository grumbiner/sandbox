      SUBROUTINE WRSD(REC,LEN,ID,IU)
      DIMENSION REC(LEN)
C     REWRITTEN FOR MACHINE INDEPENDANCE 8/10/83.  BOB GRUMBINE 
C     REC IS THE RECORD TO BE WRITTEN 
C     LEN IS THE RECORD LENGTH
C     ID IS AN IDENTIFYING ALPHA CONSTANT 1-7 CHAR
C     IN L FORM (LEFT JUSTIIED BLANK FILL). 
C     IU IS UNIT NUMBER 
      CHARACTER ID*10,IDWORD*10 
      IDWORD=ID 
      WRITE(IDWORD(8:10),'(I3)') LEN
      WRITE(IU) IDWORD,(REC(L),L=1,LEN) 
      RETURN
      END 