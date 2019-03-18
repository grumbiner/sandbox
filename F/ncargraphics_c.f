      FUNCTION C(P1,P2)
      COMMON /CONRE2/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(5120)   ,NR
      C=(P1-CV)/(P1-P2)
      RETURN
      END
