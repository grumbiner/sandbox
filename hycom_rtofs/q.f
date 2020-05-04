      PROGRAM qtest
      REAL q, qs, dqdt, t
      qs(t) = 0.622*6.11/1013.25*EXP(LOG(10.)*9.5*(t-273.16)/(t-7.66))
      dqdt(t) = qs(t)*(273.16-7.66)/(t-7.66)**2*LOG(10.)*9.5

      DO t = 220, 280
        PRINT *,t, qs(t), dqdt(t)
      ENDDO

      STOP
      END
