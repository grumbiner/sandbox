            SUBROUTINE GETABC(A,B,C,ZB,ZM,ZT,PB,PM,PT)
        REAL A,B,C,ZB,ZM,ZT,PB,PM,PT
C
        XB = PB
        XM = PM
        XT = PT
        XB = ALOG(XB)
        XM = ALOG(XM)
        XT = ALOG(XT)
        X2B = XB * XB
        X2M = XM * XM
        X2T = XT * XT
C
      ATM = X2T - X2M
      BTM = XT - XM
      CTM = ZT - ZM
C
      ABM = X2B - X2M
      BBM = XB - XM
      CBM = ZB - ZM
C
      BOT =  ATM * BBM - ABM * BTM
C
      A = (CTM*BBM - CBM*BTM)/BOT
      B = (ATM*CBM - ABM*CTM)/BOT
C
      C = ZM - A * X2M - B * XM
C
      RETURN
      END
