      SUBROUTINE LAB2PDS(LAB84,IDGRB)
C
      INTEGER LAB84(27),IDGRB(25)
C
      ICENT = (LAB84(21)-1)/100 + 1
      IDGRB(1) = 28
      IDGRB(2) =  2 
      IDGRB(3) =  7
      IDGRB(4) = 39
      IDGRB(5) = LAB84(17)
      IDGRB(12) = LAB84(21) - (ICENT-1)*100
      IDGRB(13) = LAB84(22)
      IDGRB(14) = LAB84(23)
      IDGRB(15) = LAB84(24)
      IDGRB(16) = 0
      IDGRB(17) = 1
      IDGRB(21) = 0
      IDGRB(22) = 0
      IDGRB(23) = ICENT
      IDGRB(24) = 0
C
C BITMAP SWITCH ALWAYS ZERO IN NGM; GDS SWITCH ALWAYS=1  
C
      IDGRB(6) = 1
      IDGRB(7) = 0
C
      RETURN 
      END
