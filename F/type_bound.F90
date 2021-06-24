module a
  implicit none

  type, public :: line_type
    real :: x, y
  contains
    procedure, public :: length
    procedure, public :: abi
    procedure, public :: s2
  end type line_type

contains
  real function length(ab) result(length_result)
    class(line_type), intent(in) :: ab
    length_result = ab%x + ab%y
  end function length 

  real function abi(alpha) result(rc)
    class(line_type), intent(inout) :: alpha
    alpha%x = 0.0
    alpha%y = 0.0
    rc = 0.0
  end function abi

  subroutine s2(alpha, x, y) 
    class(line_type), intent(inout) :: alpha
    real, intent(in) :: x,y
    alpha%x = x 
    alpha%y = y
  end subroutine s2

end module a

PROGRAM aaa
  use a

  type(line_type) :: x

  x%x = 1.0
  x%y = 1.0
  PRINT *,x%length()

  PRINT *,'abi ',x%abi()
  PRINT *,x%length()

  call x%s2(1.0, 2.0)
  PRINT *,x%length()
END
