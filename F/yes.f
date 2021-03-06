      FUNCTION yes(defalt, unit)
C     Function to return .TRUE. if the user responds y, .FALSE. if he
C       says n, and the default value otherwise.
C     Robert Grumbine 5 April 1994.

      IMPLICIT none

      LOGICAL yes, defalt
      INTEGER unit
      CHARACTER resp

      READ (unit,9001) resp
 9001 FORMAT(A1)

      yes = (resp .EQ. 'y') .OR. (defalt .AND. resp .NE. 'y'
     1                                   .AND. resp .NE. 'n')

      RETURN
      END
