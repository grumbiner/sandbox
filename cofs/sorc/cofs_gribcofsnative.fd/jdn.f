       INTEGER FUNCTION JDN(IYEAR,MONTH,IDAY) 
       JDN = IDAY - 32075
     &       + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
     &       + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
     &       - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
       RETURN
       END
