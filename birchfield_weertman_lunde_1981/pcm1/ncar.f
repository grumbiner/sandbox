      SUBROUTINE NCAR 
C     SUBROUTINE WRITTEN TO COLLECT ROUTINES USED EITHER ONLY FOR 
C       NCAR COMPATABILITY,  OR TO PUT MACHINE DEPENDANCIES IN ONE
C       PLACE.  8/12/83  BOB GRUMBINE 
      COMMON/TIMES/DAYTE, TYME
      CHARACTER DAYTE*10, TYME*10 
      CHARACTER DATE*10, TIME*10
  
C     USE 10 FOR DATE AND TIME AT NU.  USE 8 AND CHANGE TIME TO 
C       CLOCK AT NCAR.
  
      ENTRY CORSET
C     IF THE CORE AT NCAR NEEDS PRESETTING,  DO SO HERE.
      RETURN
  
      ENTRY ENDIT 
C     IF WE WANT TO DO ANYTHING PRIOR TO THE END,  DO IT HERE.
      STOP
  
      ENTRY TIMING
C     THIS GIVES THE VALUES FOR THE TIME OF DAY AND DATE. 
      DAYTE="DATE()"
      TYME="TIME()" 
      RETURN
  
      END 