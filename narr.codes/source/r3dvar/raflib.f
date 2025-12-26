!						********************
!						* module_pmat1.f90 *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!
!
!   Routines for basic algebraic operations on general matrices and vectors
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine's name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  resolve an ambiguity, the type of result.
!
!  OPERATIONS:
!   DET     evaluate log-determinant
!   DIF     differentiate
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1's along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SWP     swap first two operands
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1's on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortran dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
!------------------------------------------------------------------------------
MODULE MODULE_pmat1
IMPLICIT NONE
INTERFACE pro333  ; MODULE PROCEDURE pro333;                 END INTERFACE
INTERFACE pro333_d; MODULE PROCEDURE dpro333;                END INTERFACE
INTERFACE cro33   ; MODULE PROCEDURE cro33;                  END INTERFACE
INTERFACE cro33_d;  MODULE PROCEDURE dcro33;                 END INTERFACE
INTERFACE norv;     MODULE PROCEDURE norv;                   END INTERFACE
INTERFACE norv_d;   MODULE PROCEDURE dnorv;                  END INTERFACE
INTERFACE norq;     MODULE PROCEDURE norq;                   END INTERFACE
INTERFACE norq_d;   MODULE PROCEDURE dnorq;                  END INTERFACE
INTERFACE swpvv;    MODULE PROCEDURE swpvv;                  END INTERFACE
INTERFACE swpvv_d;  MODULE PROCEDURE dswpvv;                 END INTERFACE
INTERFACE mulmd;    MODULE PROCEDURE mulmd;                  END INTERFACE
INTERFACE mulmd_d;  MODULE PROCEDURE dmulmd;                 END INTERFACE
INTERFACE multd;    MODULE PROCEDURE multd;                  END INTERFACE
INTERFACE multd_d;  MODULE PROCEDURE dmultd;                 END INTERFACE
INTERFACE muldm;    MODULE PROCEDURE muldm;                  END INTERFACE
INTERFACE muldm_d;  MODULE PROCEDURE dmuldm;                 END INTERFACE
INTERFACE muldt;    MODULE PROCEDURE muldt;                  END INTERFACE
INTERFACE muldt_d;  MODULE PROCEDURE dmuldt;                 END INTERFACE
INTERFACE mulpp;    MODULE PROCEDURE mulpp;                  END INTERFACE
INTERFACE mulpp_d;  MODULE PROCEDURE dmulpp;                 END INTERFACE
INTERFACE madpp;    MODULE PROCEDURE madpp;                  END INTERFACE
INTERFACE madpp_d;  MODULE PROCEDURE dmadpp;                 END INTERFACE
INTERFACE msbpp;    MODULE PROCEDURE msbpp;                  END INTERFACE
INTERFACE msbpp_d;  MODULE PROCEDURE dmsbpp;                 END INTERFACE
INTERFACE difp;     MODULE PROCEDURE difp;                   END INTERFACE
INTERFACE difp_d;   MODULE PROCEDURE ddifp;                  END INTERFACE
INTERFACE intp;     MODULE PROCEDURE intp;                   END INTERFACE
INTERFACE intp_d;   MODULE PROCEDURE dintp;                  END INTERFACE
INTERFACE invp;     MODULE PROCEDURE invp;                   END INTERFACE
INTERFACE invp_d;   MODULE PROCEDURE dinvp;                  END INTERFACE
INTERFACE prgv;     MODULE PROCEDURE prgv;                   END INTERFACE
INTERFACE prgv_d;   MODULE PROCEDURE dprgv;                  END INTERFACE
INTERFACE mulcc;    MODULE PROCEDURE mulcc;                  END INTERFACE
INTERFACE mulcc_d;  MODULE PROCEDURE dmulcc;                 END INTERFACE
INTERFACE madcc;    MODULE PROCEDURE madcc;                  END INTERFACE
INTERFACE madcc_d;  MODULE PROCEDURE dmadcc;                 END INTERFACE
INTERFACE msbcc;    MODULE PROCEDURE msbcc;                  END INTERFACE
INTERFACE msbcc_d;  MODULE PROCEDURE dmsbcc;                 END INTERFACE
INTERFACE zerl;     MODULE PROCEDURE zerl;                   END INTERFACE
INTERFACE zerl_d;   MODULE PROCEDURE dzerl;                  END INTERFACE
INTERFACE zeru;     MODULE PROCEDURE zeru;                   END INTERFACE
INTERFACE zeru_d;   MODULE PROCEDURE dzeru;                  END INTERFACE
INTERFACE ldum;     MODULE PROCEDURE ldum;                   END INTERFACE
INTERFACE ldum_d;   MODULE PROCEDURE dldum;                  END INTERFACE
INTERFACE udlmm;    MODULE PROCEDURE udlmm, udlmv;           END INTERFACE
INTERFACE udlmm_d;  MODULE PROCEDURE dudlmm,dudlmv;          END INTERFACE
INTERFACE linvan;   MODULE PROCEDURE linvan;                 END INTERFACE
INTERFACE linvan_d; MODULE PROCEDURE dlinvan;                END INTERFACE
INTERFACE copdm;    MODULE PROCEDURE copdm;                  END INTERFACE
INTERFACE copdm_d;  MODULE PROCEDURE dcopdm;                 END INTERFACE
INTERFACE condm;    MODULE PROCEDURE condm;                  END INTERFACE
INTERFACE condm_d;  MODULE PROCEDURE dcondm;                 END INTERFACE
INTERFACE copsm;    MODULE PROCEDURE copsm;                  END INTERFACE
INTERFACE copsm_d;  MODULE PROCEDURE dcopsm;                 END INTERFACE
INTERFACE consm;    MODULE PROCEDURE consm;                  END INTERFACE
INTERFACE consm_d;  MODULE PROCEDURE dconsm;                 END INTERFACE
INTERFACE addmd;    MODULE PROCEDURE addmd;                  END INTERFACE
INTERFACE addmd_d;  MODULE PROCEDURE daddmd;                 END INTERFACE
INTERFACE submd;    MODULE PROCEDURE submd;                  END INTERFACE
INTERFACE submd_d;  MODULE PROCEDURE dsubmd;                 END INTERFACE
INTERFACE addms;    MODULE PROCEDURE addms;                  END INTERFACE
INTERFACE addms_d;  MODULE PROCEDURE daddms;                 END INTERFACE
INTERFACE subms;    MODULE PROCEDURE subms;                  END INTERFACE
INTERFACE subms_d;  MODULE PROCEDURE dsubms;                 END INTERFACE
INTERFACE l1lm;     MODULE PROCEDURE l1lm;                   END INTERFACE
INTERFACE l1lm_d;   MODULE PROCEDURE dl1lm;                  END INTERFACE
INTERFACE ldlm;     MODULE PROCEDURE ldlm;                   END INTERFACE
INTERFACE ldlm_d;   MODULE PROCEDURE dldlm;                  END INTERFACE
INTERFACE invh;     MODULE PROCEDURE invh;                   END INTERFACE
INTERFACE invh_d;   MODULE PROCEDURE dinvh;                  END INTERFACE
INTERFACE invl;     MODULE PROCEDURE invl;                   END INTERFACE
INTERFACE invl_d;   MODULE PROCEDURE dinvl;                  END INTERFACE
INTERFACE linlv;    MODULE PROCEDURE linlv;                  END INTERFACE
INTERFACE linlv_d;  MODULE PROCEDURE dlinlv;                 END INTERFACE
INTERFACE linuv;    MODULE PROCEDURE linuv;                  END INTERFACE
INTERFACE linuv_d;  MODULE PROCEDURE dlinuv;                 END INTERFACE
INTERFACE powp;     MODULE PROCEDURE powp;                   END INTERFACE
INTERFACE powp_d;   MODULE PROCEDURE dpowp;                  END INTERFACE
INTERFACE polps;    MODULE PROCEDURE polps;                  END INTERFACE
INTERFACE polps_d;  MODULE PROCEDURE dpolps;                 END INTERFACE
INTERFACE polpp;    MODULE PROCEDURE polpp;                  END INTERFACE
INTERFACE polpp_d;  MODULE PROCEDURE dpolpp;                 END INTERFACE
INTERFACE trcm;     MODULE PROCEDURE trcm;                   END INTERFACE
INTERFACE trcm_d;   MODULE PROCEDURE dtrcm;                  END INTERFACE
INTERFACE inv;      MODULE PROCEDURE invmt, linmmt, linmvt;  END INTERFACE
INTERFACE inv_d;    MODULE PROCEDURE dinvmt,dlinmmt,dlinmvt; END INTERFACE

CONTAINS

FUNCTION pro333(d,e,f) RESULT(pro_res) ! TRIPLE PRODUCT OF 3 3-VECTORS
REAL                :: pro_res
REAL,    INTENT(IN) :: d(3), e(3), f(3)
REAL                :: g(3)
CALL CRO33(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION pro333

FUNCTION dpro333(d,e,f) RESULT(pro_res) ! TRIPLE PRODUCT OF 3 3-VECTORS
REAL(8)             :: pro_res
REAL(8), INTENT(IN) :: d(3), e(3), f(3)
REAL(8)             :: g(3)
CALL CRO33_d(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION dpro333

SUBROUTINE cro33(a,b,c) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
REAL,    INTENT(IN) :: a(3), b(3)
REAL,    INTENT(OUT):: c(3)
c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE cro33

SUBROUTINE dcro33(a,b,c) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
REAL(8), INTENT(IN) :: a(3), b(3)
REAL(8), INTENT(OUT):: c(3)
c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE dcro33


FUNCTION norv(d) RESULT(norv_res)! NORM OF VECTOR..
REAL                :: norv_res
REAL,    INTENT(IN) :: d(:)
norv_res=SQRT(DOT_PRODUCT(D,D))
END FUNCTION norv

FUNCTION dnorv(d) ! NORM OF VECTOR..
REAL(8):: dnorv
REAL(8),    INTENT(IN) :: d(:)
dnorv=SQRT(DOT_PRODUCT(d,d))
END FUNCTION dnorv

FUNCTION norq(d) ! Norm of a matrix
REAL:: norq
REAL,INTENT(IN):: d(:,:)
INTEGER m2,i2
m2=SIZE(d,2)
norq=0.; DO i2=1,m2; norq=norq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
norq=SQRT(norq)
END FUNCTION norq

FUNCTION dnorq(d) ! norm of a matrix
REAL(8):: dnorq
REAL(8),INTENT(IN):: d(:,:)
INTEGER m2,i2
m2=SIZE(d,2)
dnorq=0.; DO i2=1,m2; dnorq=dnorq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
dnorq=SQRT(dnorq)
END FUNCTION dnorq


SUBROUTINE swpvv(d,e)
REAL, INTENT(INOUT) :: d(:), e(:)
REAL :: t(SIZE(d))
t = d; d = e; e = t
END SUBROUTINE swpvv

SUBROUTINE dswpvv(d,e)
REAL(8), INTENT(INOUT) :: d(:), e(:)
REAL(8) :: t(SIZE(d))
t = d; d = e; e = t
END SUBROUTINE dswpvv

SUBROUTINE mulmd(a,d,b)
REAL, INTENT(INOUT) :: a(:,:),b(:,:) 
REAL, INTENT(IN)    :: d(*)
INTEGER:: m2,j
m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE mulmd

SUBROUTINE dmulmd(a,d,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:)
REAL(8), INTENT(IN)    :: d(*)
INTEGER:: m2,j
m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE dmulmd

SUBROUTINE multd(a,d,b)
REAL, INTENT(INOUT)    :: a(:,:),b(:,:) 
REAL, INTENT(IN)       :: d(*)
INTEGER:: m2,j
m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE multd

SUBROUTINE dmultd(a,d,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN)    :: d(*)
INTEGER:: m2,j
m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE dmultd

SUBROUTINE muldm(d,a,b)
REAL, INTENT(INOUT)    :: a(:,:),b(:,:) 
REAL, INTENT(IN)       :: d(*)
INTEGER                :: m1,i
m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE muldm

SUBROUTINE dmuldm(d,a,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN)    :: d(*)
INTEGER                :: m1,i
m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE dmuldm

SUBROUTINE muldt(d,a,b)
REAL, INTENT(INOUT)    :: a(:,:),b(:,:) 
REAL, INTENT(IN)       :: d(*)
INTEGER                :: m1,i
m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE muldt

SUBROUTINE dmuldt(d,a,b)
REAL(8), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(8), INTENT(IN)    :: d(*)
INTEGER:: m1,i
m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE dmuldt

SUBROUTINE mulpp(a,b,c) !  multiply polynomials, possibly in place
REAL,    INTENT(IN)    :: a(0:), b(0:)
REAL,    INTENT(INOUT) :: c(0:)
INTEGER                :: m,mcp, i, j
REAL                   :: s, b0
m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = 0.0
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=s
ENDDO
RETURN
ENTRY madpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY msbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS
FUNCTION mcmax(a,b,m) RESULT(mmx_res) ! This fn can be contained in mulpp().
INTEGER             :: mmx_res
INTEGER, INTENT(IN) :: m
REAL,    INTENT(IN) :: a(0:m), b(0:m)
INTEGER             :: ma, mb
mmx_res=0		       ! default for when ALL elements of c are zero
DO ma=m,0,-1	               ! seek last nonzero coefficient of polynomial a
  IF(a(ma) /= 0.)THEN
    DO mb=m,0,-1	       ! seek last nonzero coefficient of polynomial b
      IF(b(mb) /= 0.)THEN
        mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
        RETURN
      ENDIF
    ENDDO
    RETURN
  ENDIF
ENDDO
END FUNCTION mcmax
END SUBROUTINE mulpp

SUBROUTINE difp(a,b) ! Symbolically differentiate polynomial
REAL, INTENT(IN)  :: a(0:)
REAL, INTENT(OUT) :: b(0:)
INTEGER           :: m,mcp, i, j
REAL              :: s, b0
m=SIZE(a)-1
DO i=1,m	! possibly with coincident storage for a and b
  b(i-1)=i*a(i)
ENDDO
b(m)=0.
RETURN
ENTRY intp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1	! possibly with coincident storage for a and b
  b(i)=a(i-1)/i
ENDDO
b(0)=0.
RETURN
ENTRY invp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=1./a(0)	! storage of a and b must not be the same
b(0)=b0
DO i=1,m
  s = SUM(b(i-1:0:-1)*a(1:i))
  b(i)=-b0*s
ENDDO
END SUBROUTINE difp

SUBROUTINE dmulpp(a,b,c) !  multiply polynomials, possibly in place
REAL(8), INTENT(IN)   :: a(0:), b(0:)
REAL(8), INTENT(INOUT):: c(0:)
INTEGER               :: m,mcp, i, j
REAL(8)               :: s, b0
m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = 0.0
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=s
ENDDO
RETURN
ENTRY dmadpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY dmsbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
  s = SUM(a(j-1:0:-1)*b(0:j-1))
  c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS
FUNCTION mcmax(a,b,m) RESULT(mmx_res)
INTEGER              :: mmx_res
INTEGER,  INTENT(IN) :: m
REAL(8), INTENT(IN)  :: a(0:m), b(0:m)
INTEGER              :: ma, mb
mmx_res=0		       ! default for when all elements of c are zero
DO ma=m,0,-1	               ! seek last nonzero coefficient of polynomial a
  IF(a(ma) /= 0.d0)THEN
    DO mb=m,0,-1	       ! seek last nonzero coefficient of polynomial b
      IF(b(mb) /= 0.d0)THEN
        mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
        RETURN
      ENDIF
    ENDDO
    RETURN
  ENDIF
ENDDO
RETURN
END FUNCTION mcmax

END SUBROUTINE dmulpp

SUBROUTINE ddifp(a,b) ! Symbolically differentiate polynomial
REAL(8), INTENT(IN)   :: a(0:)
REAL(8), INTENT(INOUT):: b(0:)
INTEGER               :: m,mcp, i, j
REAL(8)               :: s, b0
m=SIZE(a)-1
DO i=1,m	 ! possibly with coincident storage for a and b
  b(i-1)=i*a(i)
ENDDO
b(m)=0.
RETURN
ENTRY dintp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1	 ! possibly with coincident storage for a and b
  b(i)=a(i-1)/i
ENDDO
b(0)=0.
RETURN
ENTRY dinvp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=1./a(0)	 ! storage of a and b must not be the same
b(0)=b0
DO i=1,m
  s = SUM(b(i-1:0:-1)*a(1:i))
  b(i)=-b0*s
ENDDO
END SUBROUTINE ddifp


SUBROUTINE prgv(d)
REAL, PARAMETER        :: crit=1.E-30
REAL, INTENT(INOUT)    :: d(:)
INTEGER                :: i,m
m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=0.; ENDDO
END SUBROUTINE prgv

SUBROUTINE dprgv(d)
REAL(8), PARAMETER     :: crit=1.D-30
REAL(8), INTENT(INOUT) :: d(:)
INTEGER                :: i,m
m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=0.; ENDDO
END SUBROUTINE dprgv


SUBROUTINE mulcc(a,b,c,m)  ! Multiply circulant matrices of period M
INTEGER, INTENT(IN) :: m
REAL, INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)
INTEGER             :: mm, j
c(0:m-1) = 0.0
ENTRY madcc(a,b,c,m)
mm=m-1
DO j=0,mm
  c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
  c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY msbcc(a,b,c,m)
mm=m-1
DO j=0,mm
  c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
  c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE mulcc

SUBROUTINE dmulcc(a,b,c,m)  ! Multiply circulant matrices of period M
INTEGER, INTENT(IN   ) :: m
REAL(8), INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)
INTEGER                :: mm, j
c(0:m-1) = 0.0d0
ENTRY dmadcc(a,b,c,m)
mm=m-1
DO j=0,mm
  c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
  c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY dmsbcc(a,b,c,m)
mm=m-1
DO j=0,mm
  c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
  c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE dmulcc

SUBROUTINE zerl(a)  ! Zero out the strictly lower triangle of elements
REAL,INTENT(INOUT):: a(:,:)
INTEGER           :: m,j
m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = 0; ENDDO; RETURN

ENTRY zeru(a)       ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = 0; ENDDO
END SUBROUTINE zerl

SUBROUTINE dzerl(a) ! Zero out the strictly lower triangle of elements
REAL(8),INTENT(INOUT):: a(:,:)
INTEGER              :: m,j
m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = 0; ENDDO; RETURN

ENTRY dzeru(a)      ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = 0; ENDDO
END SUBROUTINE dzerl

!------------------------------------------------------------------------------
!   R.J.Purser, NCEP, Washington D.C.	1996
!		    SUBROUTINE	LDUM
!  perform l-d-u decomposition of square matrix a in place with
!
!  <-> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!------------------------------------------------------------------------------
SUBROUTINE ldum(a,ipiv,d)
REAL,    INTENT(INOUT) :: a(:,:) 
REAL,    INTENT(OUT  ) :: d
INTEGER, INTENT(OUT  ) :: ipiv(:)
INTEGER                :: m,i, j, jp, ibig, jm
REAL                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij
m=SIZE(a,1)
DO i=1,m
  aam=0.
  DO j=1,m
    aa=ABS(a(i,j))
    IF(aa > aam)aam=aa
  ENDDO
  IF(aam == 0.)THEN
    PRINT '(" row ",i3," of matrix in ldum vanishes")',i
    STOP
  ENDIF
  s(i)=1./aam
ENDDO
d=1.
ipiv(m)=m
DO j=1,m-1
  jp=j+1
  abig=s(j)*ABS(a(j,j))
  ibig=j
  DO i=jp,m
    aa=s(i)*ABS(a(i,j))
    IF(aa > abig)THEN
      ibig=i
      abig=aa
    ENDIF
  ENDDO
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  IF(ibig /= j)THEN
    d=-d
    CALL swpvv(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  ENDIF
  ajj=a(j,j)
  IF(ajj == 0.)THEN
    jm=j-1
    PRINT '(" failure in ldum:"/" matrix singular, rank=",i3)',jm
    STOP
  ENDIF
  ajji=1./ajj
  DO i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  ENDDO
ENDDO
END SUBROUTINE ldum
SUBROUTINE DLDUM(A,IPIV,D)
REAL(8), INTENT(INOUT) :: a(:,:) 
REAL(8), INTENT(OUT  ) :: d
INTEGER, INTENT(OUT  ) :: ipiv(:)
INTEGER                :: m,i, j, jp, ibig, jm
REAL(8)                :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij
m=SIZE(a,1)
DO i=1,m
  aam=0.
  DO j=1,m
    aa=ABS(a(i,j))
    IF(aa > aam)aam=aa
  ENDDO
  IF(aam == 0.d0)THEN
    PRINT '(" row ",i3," of matrix in dldum vanishes")',i
    STOP
  ENDIF
  s(i)=1./aam
ENDDO
d=1.
ipiv(m)=m
DO j=1,m-1
  jp=j+1
  abig=s(j)*ABS(a(j,j))
  ibig=j
  DO i=jp,m
    aa=s(i)*ABS(a(i,j))
    IF(aa > abig)THEN
      ibig=i
      abig=aa
    ENDIF
  ENDDO
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  IF(ibig /= j)THEN
    d=-d
    CALL swpvv_d(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  ENDIF
  ajj=a(j,j)
  IF(ajj == 0.d0)THEN
    jm=j-1
    PRINT '(" Failure in dldum:"/" matrix singular, rank=",i3)',jm
    STOP
  ENDIF
  ajji=1./ajj
  DO i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  ENDDO
ENDDO
END SUBROUTINE dldum


!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMM
!  use l-u factors in A to back-substitute for mm rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!	    vectors on return
!  --> IPIV array encoding the pivoting sequence
!------------------------------------------------------------------------------
SUBROUTINE udlmm(a,b,ipiv)
INTEGER, INTENT(IN)    :: ipiv(:) 
REAL,    INTENT(IN)    :: a(:,:) 
REAL,    INTENT(INOUT) :: b(:,:) 
INTEGER                :: m,mm,i, k, l
REAL                   :: s,aiii
m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
  DO i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  ENDDO
  b(m,k)=b(m,k)/a(m,m)
  DO i=m-1,1,-1
    aiii=1./a(i,i)
    b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  ENDDO
ENDDO
END SUBROUTINE udlmm
SUBROUTINE dudlmm(a,b,ipiv)
INTEGER, INTENT(IN   ) :: ipiv(:) 
REAL(8), INTENT(IN   ) :: a(:,:) 
REAL(8), INTENT(INOUT) :: b(:,:) 
INTEGER                :: m,mm,i, k, l
REAL(8)                :: s,aiii
m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
  DO i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  ENDDO
  b(m,k)=b(m,k)/a(m,m)
  DO i=m-1,1,-1
    aiii=1./a(i,i)
    b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  ENDDO
ENDDO
END SUBROUTINE dudlmm

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMV
!  use l-u factors in A to back-substitute for mm rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-side on entry, corresponding vector solution
!	    on return
!  --> IPIV array encoding the pivoting sequence
!------------------------------------------------------------------------------
SUBROUTINE udlmv(a,b,ipiv)
INTEGER, INTENT(IN)    :: ipiv(:) 
REAL,    INTENT(IN)    :: a(:,:) 
REAL,    INTENT(INOUT) :: b(:) 
INTEGER                :: m,i, l
REAL                   :: s,aiii
m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=1./a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE udlmv
SUBROUTINE dudlmv(a,b,ipiv)
INTEGER,  INTENT(IN   ) :: ipiv(:) 
REAL(8),  INTENT(IN   ) :: a(:,:) 
REAL(8),  INTENT(INOUT) :: b(:) 
INTEGER                 :: m,i, l
REAL(8)                 :: s,aiii
m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=1./a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE dudlmv

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!		    SUBROUTINE LINVAN
!
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! <-> W:    Generalized Vandermonde matrix in, conditioning indicator out.
! <-> AB:   R.h.s. vector in, solution vector of numerical coefficients out.
!------------------------------------------------------------------------------
SUBROUTINE linvan(w,ab)
INTEGER, PARAMETER  :: nit=20
REAL, INTENT(INOUT) :: w(:,:), ab(:)
REAL                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                       w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER             :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL                :: p, e, dw, c, d, s, d2j
REAL,ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)	    ! in later "clean-up" operation.

d1 = 1.0 	    ! Row scaling factors set to default
d2 = 1.0 	    ! Column scaling factors set to default

C=1.E-16	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
  p=0.
  e=0.
  DO j=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(E == 0.)STOP 'W effectively singular in LINVAN'
  d1(i)=EXP(-p/e)
ENDDO
CALL muldm(d1,w2,w)

DO j=1,nc
  p=0.
  e=0.
  DO i=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(E == 0.)STOP 'W effectively singular in LINVAN'
  d2(j)=EXP(-p/e)
ENDDO
CALL mulmd(w,d2,w)

c=1.e-8  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
DO it=1,nit	    !	perform nit relaxation iterations
  DO i=1,nc	    !	do rows:
    p=1.
    e=0.
    DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(i,:) = w(i,:) * p            ! rescale this row of w..
    d1(i)=d1(i)*p			     ! ..and update d1 consistently
  ENDDO
  DO j=1,nc	    !	do columns:
    p=1.
    e=0.
    d2j=d2(j)
    DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(:,j) = w(:,j) * p        ! rescale this column of w..
    d2(j)=d2(j)*p		       ! ..and update d2 consistently
  ENDDO
ENDDO
c=1.e-3	    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=1.			     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)		      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! rescale solution vector by d2
!  note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of w. we invoke original matrix elements w2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  begin "clean-up" process. substitute solution vector in original
!  equation and leave the residual difference in v
v=v-MATMUL(w2,ab)
v = d1 * v    ! rescale the residual vector by d1
wv(:,1) = v ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv) ! solve linear system with this rhs.
ab=ab+wv(:,1)*d2 ! add residual solution vector, 
                                      ! scaled, to ab

	  DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! when d/p is very small, treat the results with suspicion!

END SUBROUTINE linvan

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DLINVAN
!
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! <-> W:    Generalized Vandermonde matrix in, conditioning indicator out.
! <-> AB:   R.h.s. vector in, solution vector of numerical coefficients out.
!------------------------------------------------------------------------------
SUBROUTINE dlinvan(w,ab)
INTEGER, PARAMETER     :: nit=20
REAL(8), INTENT(INOUT) :: w(:,:), ab(:)
REAL(8)                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                          w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER                :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(8)                :: p, e, dw, c, d, s, d2j
REAL(8),ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)	    ! in later "clean-up" operation.

d1 = 1.0 	    ! Row scaling factors set to default
d2 = 1.0 	    ! Column scaling factors set to default

C=1.E-16	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
  p=0.
  e=0.
  DO j=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(e == 0.d0)STOP 'w effectively singular in linvan'
  d1(i)=EXP(-p/e)
ENDDO
CALL muldm_d(d1,w2,w)

DO j=1,nc
  p=0.
  e=0.
  DO i=1,nc
    dw=ABS(w(i,j))
    IF(dw > c)THEN
      e=e+1.
      p=p+LOG(dw)
    ENDIF
  ENDDO
  IF(e == 0.)STOP 'w effectively singular in linvan'
  d2(j)=EXP(-p/e)
ENDDO
CALL mulmd_d(w,d2,w)

c=1.e-8  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
DO it=1,nit	    !	perform nit relaxation iterations
  DO i=1,nc	    !	do rows:
    p=1.
    e=0.
    DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(i,:) = w(i,:) * p            ! rescale this row of w..
    d1(i)=d1(i)*p			     ! ..and update d1 consistently
  ENDDO
  DO j=1,nc	    !	do columns:
    p=1.
    e=0.
    d2j=d2(j)
    DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
        e=e+1.
        p=p*dw
      ENDIF
    ENDDO
    p=1./(p**(1./e))
    w(:,j) = w(:,j) * p        ! rescale this column of w..
    d2(j)=d2(j)*p		       ! ..and update d2 consistently
  ENDDO
ENDDO
c=1.e-3	    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=1.			     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum_d(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)		      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm_d(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
v=v-MATMUL(w2,ab)
v = d1 * v    ! Rescale the residual vector by D1
wv(:,1) = v ! Convert shape of array
CALL UDLMM_d(w,wv(:,1:1),ipiv) ! Solve linear system with THIS rhs.
ab=ab+wv(:,1)*d2 ! Add residual solution vector, 
                                      ! scaled, to AB

	  DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! When D/P is very small, treat the results with suspicion!

END SUBROUTINE dlinvan

SUBROUTINE copdm(d,a)
REAL,DIMENSION(:),INTENT(IN)::d; REAL,DIMENSION(:,:),INTENT(OUT)::a; INTEGER i
                  a=0.; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY condm(d,a); a=0.; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE copdm

SUBROUTINE dcopdm(d,a)
REAL(8),DIMENSION(:),INTENT(IN)::d; REAL(8),DIMENSION(:,:),INTENT(OUT)::a
INTEGER i
                   a=0.; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY dcondm(d,a); a=0.; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE dcopdm

SUBROUTINE copsm(s,a)
REAL,INTENT(IN) :: s; REAL,DIMENSION(:,:),INTENT(OUT):: a; INTEGER i
                  a=0.; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY consm(s,a); a=0.; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE copsm

SUBROUTINE dcopsm(s,a)
REAL(8),INTENT(IN) :: s; REAL(8),DIMENSION(:,:),INTENT(OUT):: a; INTEGER i
                   a=0.; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY dconsm(s,a); a=0.; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE dcopsm

SUBROUTINE addmd(a,b,d)
REAL,DIMENSION(:,:),INTENT(INOUT):: a,b; REAL,DIMENSION(:),INTENT(IN):: d
REAL s;  INTEGER i
                   b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY submd(a,b,d);b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY addms(a,b,s);b=a; DO I=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY SUBMS(A,B,S);b=a; DO I=1,SIZE(a,1); B(I,I)=B(I,I)-S;    ENDDO;
END SUBROUTINE addmd

SUBROUTINE daddmd(a,b,d)
REAL(8),DIMENSION(:,:),INTENT(INOUT)::A,B;REAL(8),DIMENSION(:),INTENT(IN)::D
REAL(8) s; INTEGER i
                     b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY DSUBMD(A,B,D); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY DADDMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY DSUBMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-s;    ENDDO;
END SUBROUTINE daddmd

SUBROUTINE l1lm(a,b) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
REAL, INTENT(IN)    :: a(:,:)
REAL, INTENT(INOUT) :: b(:,:)
INTEGER             :: m,j, jm, jp, i
REAL                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(S <= 0.)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=1./b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = 0.0
ENDDO
END SUBROUTINE l1lm

SUBROUTINE DL1LM(A,B) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
REAL(8), INTENT(IN)    :: a(:,:) 
REAL(8), INTENT(INOUT) :: b(:,:) 
INTEGER :: m,j, jm, jp, i
REAL(8) :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(s <= 0.d0)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=1./b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = 0.0
ENDDO
RETURN
END SUBROUTINE dl1lm

SUBROUTINE ldlm(a,b,d) ! Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
REAL, INTENT(IN)    :: a(:,:)
REAL, INTENT(INOUT) :: b(:,:)
REAL, INTENT(OUT)   :: d(:)
INTEGER :: m,j, jm, jp, i
REAL :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  
  b(j,j) = 1.
  IF(d(j) == 0.)THEN
    PRINT '(" LDLM detects singularity at diagonal index",i2)',J
    STOP
  ENDIF
  bjji=1./d(j)
  DO i=jp,m
     b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  ENDDO
ENDDO
CALL zeru(b)
RETURN
END SUBROUTINE ldlm

SUBROUTINE dldlm(a,b,d) ! Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
REAL(8), INTENT(IN)    :: a(:,:)
REAL(8), INTENT(INOUT) :: b(:,:)
REAL(8), INTENT(OUT)   :: d(:)
INTEGER                :: m,j, jm, jp, i
REAL(8)                :: s, bjji
m=SIZE(a,1)
DO j=1,m; jm=j-1; jp=j+1
  d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  b(j,j) = 1.
  IF(d(j) == 0.d0)THEN
    PRINT '(" DLDLM detects singularity at diagonal index",i2)',J
    STOP
  ENDIF
  bjji=1./d(j)
  DO i=jp,m
     b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  ENDDO
ENDDO
CALL zeru_d(b)
RETURN
END SUBROUTINE dldlm

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE INVH
!  Inver,t in place, a symmetric matrix
!
!  <-> A    symmetric square matrix, output as inverse of input
!
!  LIMITATION
!     This routine incorporates no pivoting - it is intended for matrices
!     that are already diagonally dominant
!------------------------------------------------------------------------------
SUBROUTINE invh(a)
REAL, INTENT(INOUT)      :: a(:,:) 
INTEGER                  :: m,k, kp, i, ip, j
REAL,DIMENSION(SIZE(a,1)):: d
m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m; kp=k+1
  a(k,k)=1./d(k)
  DO i=kp,m
    a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
    a(i,k)=-a(i,k)
  ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
  a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! Really?
ENDDO
DO i=1,m
  ip=i+1
  DO j=1,i-1
    a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
    a(i,j)=a(j,i)
  ENDDO
  a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE invh

SUBROUTINE dinvh(a)
REAL(8), INTENT(INOUT)      :: a(:,:) 
INTEGER                     :: m,k, kp, i, ip, j
REAL(8),DIMENSION(SIZE(a,1)):: d
m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm_d(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m
  kp=k+1
  a(k,k)=1./d(k)
  DO i=kp,m
    a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
    a(i,k)=-a(i,k)
  ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
  a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! really?
ENDDO
DO i=1,m
  ip=i+1
  DO j=1,i-1
    a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
    a(i,j)=a(j,i)
  ENDDO
  a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE dinvh

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE INVL
!     Invert lower triangular matrix in place if A are same
!------------------------------------------------------------------------------
SUBROUTINE invl(a)
REAL, INTENT(INOUT) :: a(:,:) 
INTEGER             :: m,j, i
REAL                :: s
m=SIZE(a,1)
DO j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  DO i=j+1,m
    s = SUM(a(j:i-1,j)*a(i,j:i-1))
    a(i,j)=-a(i,i)*s
  ENDDO
ENDDO
END SUBROUTINE invl

SUBROUTINE dinvl(a)
REAL(8), INTENT(INOUT) :: a(:,:) 
INTEGER                :: m,j, i
REAL(8)                :: s
m=SIZE(a,1)
DO j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  DO i=j+1,m
    s = SUM(a(j:i-1,j)*a(i,j:i-1))
    a(i,j)=-a(i,i)*s
  ENDDO
ENDDO
END SUBROUTINE dinvl

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINLV
!     Solve linear system involving lower triangular (LINLV) or upper
!     triangular (LINUV) matrix. u is input as right-hand-side, output
!     as the solution vector.
!------------------------------------------------------------------------------
SUBROUTINE linlv(a,u)
REAL, INTENT(IN)   :: a(:,:)
REAL, INTENT(INOUT):: u(:)
INTEGER            :: m,i, j, jp
DO i=1,SIZE(a,1);    u(i)=(u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY linuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j)=(u(j) - SUM(a(jp:m,j)*u(jp:m)))  /a(j,j); ENDDO
END SUBROUTINE linlv
SUBROUTINE dlinlv(a,u)
REAL(8), INTENT(IN)   :: a(:,:)
REAL(8), INTENT(INOUT):: u(:)
INTEGER :: m,i, j, jp
DO i=1,SIZE(a,1); u(i)= (u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY dlinuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j) = (u(j) - SUM(a(jp:m,j)*u(jp:m)))/a(j,j); ENDDO
END SUBROUTINE dlinlv


SUBROUTINE powp(a,b,n)	       ! Raise power series A to the power
INTEGER, INTENT(IN) :: n       ! of N and output as B
REAL,    INTENT(IN) :: a(0:)
REAL,    INTENT(OUT):: b(0:)
REAL,DIMENSION(0:SIZE(a)-1):: t; INTEGER :: k
b(0)=1.; b(1:) = 0.0; DO k=1,n; CALL mulpp(a,b,t); b=t; ENDDO
END SUBROUTINE powp
SUBROUTINE DPOWP(A,B,N)	       ! Raise power series A to the power
INTEGER,  INTENT(IN) :: n      ! of N and output as B
REAL(8), INTENT(IN) :: a(0:)
REAL(8), INTENT(OUT):: b(0:)
REAL(8),DIMENSION(0:SIZE(a)-1):: t; INTEGER :: k
B(0)=1.; b(1:) = 0.0; DO k=1,n; CALL mulpp_d(a,b,t); b=t; ENDDO
END SUBROUTINE dpowp


SUBROUTINE polps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
REAL,INTENT(IN) :: a(0:)
REAL,INTENT(IN) :: s1
REAL,INTENT(OUT):: s2
INTEGER m,k
m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE polps
SUBROUTINE dpolps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
REAL(8),INTENT(IN) :: a(0:)
REAL(8),INTENT(IN) :: s1
REAL(8),INTENT(OUT):: s2
INTEGER m,k
m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE dpolps

SUBROUTINE polpp(a,b,c) ! Apply power series A to power series B and put
                        ! the result out as power-series C.
REAL,INTENT(INOUT)         :: a(0:),b(0:),c(0:)
REAL,DIMENSION(0:SIZE(a)-1):: t
INTEGER m,k
m=SIZE(a)-1; c(0)=a(m); c(1:m) = 0.0
DO k=m-1,0,-1; CALL mulpp(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE polpp
SUBROUTINE dpolpp(a,b,c) ! Apply power series A to power series B and put
                         ! the result out as power-series C.
REAL(8),INTENT(INOUT)         :: a(0:),b(0:),c(0:)
REAL(8),DIMENSION(0:SIZE(a)-1):: t
INTEGER m,k
m=SIZE(a)-1
c(0)=a(m); c(1:m) = 0.0
DO k=m-1,0,-1; CALL mulpp_d(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE dpolpp

FUNCTION trcm(a) RESULT(trc_res)	    ! Trace of square matrix A
REAL             :: trc_res
REAL, INTENT(IN) :: a(:,:)
INTEGER          :: i
trc_res=0.; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION trcm
FUNCTION dtrcm(a) RESULT(trc_res)	    ! Trace of square matrix A
REAL(8)             :: trc_res
REAL(8), INTENT(IN) :: a(:,:)
INTEGER              :: m,i
trc_res=0.; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION dtrcm

SUBROUTINE invmt(a)
REAL,DIMENSION(:,:),INTENT(INOUT):: a
INTEGER m,i,j,jp,l
REAL d
INTEGER,DIMENSION(SIZE(a,1)):: ipiv
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to invmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=1./a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv(a(:,j),a(:,l)); ENDDO
END SUBROUTINE invmt

SUBROUTINE dinvmt(a)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a
INTEGER                             :: m,i,j,jp,l
REAL(8)                             :: d
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to dinvmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum_d(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=1./a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv_d(a(:,j),a(:,l)); ENDDO
END SUBROUTINE dinvmt

SUBROUTINE linmmt(a,b)
REAL,DIMENSION(:,:),INTENT(INOUT):: a,b
INTEGER,DIMENSION(SIZE(a,1))     :: ipiv
INTEGER                          :: m
REAL                             :: d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmmt

SUBROUTINE dlinmmt(a,b)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a,b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
INTEGER                             :: m 
REAL(8)                             :: d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt_d is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmmt

SUBROUTINE linmvt(a,b)
REAL,DIMENSION(:,:),INTENT(INOUT):: a
REAL,DIMENSION(:),  INTENT(INOUT):: b
INTEGER,DIMENSION(SIZE(a,1))     :: ipiv
INTEGER                          :: m
REAL                             :: d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmvt

SUBROUTINE dlinmvt(a,b)
REAL(8),DIMENSION(:,:),INTENT(INOUT):: a
REAL(8),DIMENSION(:),  INTENT(INOUT):: b
INTEGER,DIMENSION(SIZE(a,1))        :: ipiv
INTEGER m; REAL(8) d
m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt_d is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmvt

end module module_pmat1

!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						********************
!						* module_pmat1.f90 *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

!						********************
!						* module_pmat2.f90 *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
MODULE MODULE_pmat2
USE MODULE_pmat1
IMPLICIT NONE

INTERFACE avco;   MODULE PROCEDURE avco;           END INTERFACE
INTERFACE avco_d; MODULE PROCEDURE davco;          END INTERFACE
INTERFACE dfco;   MODULE PROCEDURE dfco;           END INTERFACE
INTERFACE dfco_d; MODULE PROCEDURE ddfco;          END INTERFACE
INTERFACE dfco2;  MODULE PROCEDURE dfco2;          END INTERFACE
INTERFACE dfco2_d;MODULE PROCEDURE ddfco2;         END INTERFACE
INTERFACE clib;   MODULE PROCEDURE clib;           END INTERFACE
INTERFACE clib_d; MODULE PROCEDURE dclib;          END INTERFACE
INTERFACE cad1b;  MODULE PROCEDURE cad1b;          END INTERFACE
INTERFACE csb1b;  MODULE PROCEDURE csb1b;          END INTERFACE
INTERFACE cad2b;  MODULE PROCEDURE cad2b;          END INTERFACE
INTERFACE csb2b;  MODULE PROCEDURE csb2b;          END INTERFACE
INTERFACE copbt;  MODULE PROCEDURE copbt;          END INTERFACE
INTERFACE conbt;  MODULE PROCEDURE conbt;          END INTERFACE
INTERFACE copmb;  MODULE PROCEDURE copmb;          END INTERFACE
INTERFACE conmb;  MODULE PROCEDURE conmb;          END INTERFACE
INTERFACE copbm;  MODULE PROCEDURE copbm;          END INTERFACE
INTERFACE conbm;  MODULE PROCEDURE conbm;          END INTERFACE
INTERFACE mulbb;  MODULE PROCEDURE mulbb;          END INTERFACE
INTERFACE madbb;  MODULE PROCEDURE madbb;          END INTERFACE
INTERFACE msbbb;  MODULE PROCEDURE msbbb;          END INTERFACE
INTERFACE ldub;   MODULE PROCEDURE ldub;           END INTERFACE
INTERFACE ldub_d; MODULE PROCEDURE dldub;          END INTERFACE
INTERFACE l1ubb;  MODULE PROCEDURE l1ubb;          END INTERFACE
INTERFACE l1ubb_d;MODULE PROCEDURE dl1ubb;         END INTERFACE
INTERFACE l1ueb;  MODULE PROCEDURE l1ueb;          END INTERFACE
INTERFACE l1ueb_d;MODULE PROCEDURE dl1ueb;         END INTERFACE
INTERFACE l1lb;   MODULE PROCEDURE l1lb;           END INTERFACE
INTERFACE ldlb;   MODULE PROCEDURE ldlb;           END INTERFACE
INTERFACE ldlb_d; MODULE PROCEDURE dldlb;          END INTERFACE
INTERFACE udub;   MODULE PROCEDURE udub;           END INTERFACE
INTERFACE udub_d; MODULE PROCEDURE dudub;          END INTERFACE
INTERFACE mulbv;  MODULE PROCEDURE mulbv;          END INTERFACE
INTERFACE madbv;  MODULE PROCEDURE madbv;          END INTERFACE
INTERFACE msbbv;  MODULE PROCEDURE msbbv;          END INTERFACE
INTERFACE mulbx;  MODULE PROCEDURE mulbx;          END INTERFACE
INTERFACE madbx;  MODULE PROCEDURE madbx;          END INTERFACE
INTERFACE msbbx;  MODULE PROCEDURE msbbx;          END INTERFACE
INTERFACE mulby;  MODULE PROCEDURE mulby;          END INTERFACE
INTERFACE madby;  MODULE PROCEDURE madby;          END INTERFACE
INTERFACE msbby;  MODULE PROCEDURE msbby;          END INTERFACE
INTERFACE mulvb;  MODULE PROCEDURE mulvb;          END INTERFACE
INTERFACE madvb;  MODULE PROCEDURE madvb;          END INTERFACE
INTERFACE msbvb;  MODULE PROCEDURE msbvb;          END INTERFACE
INTERFACE mulxb;  MODULE PROCEDURE mulxb;          END INTERFACE
INTERFACE madxb;  MODULE PROCEDURE madxb;          END INTERFACE
INTERFACE msbxb;  MODULE PROCEDURE msbxb;          END INTERFACE
INTERFACE mulyb;  MODULE PROCEDURE mulyb;          END INTERFACE
INTERFACE madyb;  MODULE PROCEDURE madyb;          END INTERFACE
INTERFACE msbyb;  MODULE PROCEDURE msbyb;          END INTERFACE
INTERFACE mulbd;  MODULE PROCEDURE mulbd;          END INTERFACE
INTERFACE madbd;  MODULE PROCEDURE madbd;          END INTERFACE
INTERFACE msbbd;  MODULE PROCEDURE msbbd;          END INTERFACE
INTERFACE muldb;  MODULE PROCEDURE muldb;          END INTERFACE
INTERFACE maddb;  MODULE PROCEDURE maddb;          END INTERFACE
INTERFACE msbdb;  MODULE PROCEDURE msbdb;          END INTERFACE
INTERFACE udlbv;  MODULE PROCEDURE udlbv;          END INTERFACE
INTERFACE udlbx;  MODULE PROCEDURE udlbx;          END INTERFACE
INTERFACE udlby;  MODULE PROCEDURE udlby;          END INTERFACE
INTERFACE udlvb;  MODULE PROCEDURE udlvb;          END INTERFACE
INTERFACE udlxb;  MODULE PROCEDURE udlxb;          END INTERFACE
INTERFACE udlyb;  MODULE PROCEDURE udlyb;          END INTERFACE
INTERFACE u1lbv;  MODULE PROCEDURE u1lbv;          END INTERFACE
INTERFACE u1lbx;  MODULE PROCEDURE u1lbx;          END INTERFACE
INTERFACE u1lby;  MODULE PROCEDURE u1lby;          END INTERFACE
INTERFACE u1lvb;  MODULE PROCEDURE u1lvb;          END INTERFACE
INTERFACE u1lxb;  MODULE PROCEDURE u1lxb;          END INTERFACE
INTERFACE u1lyb;  MODULE PROCEDURE u1lyb;          END INTERFACE
INTERFACE linbv;  MODULE PROCEDURE linbv;          END INTERFACE
INTERFACE wrtb;   MODULE PROCEDURE wrtb;           END INTERFACE


CONTAINS

!=============================================================================
SUBROUTINE davco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!		    SUBROUTINE DAVCO
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jpurser@ncep.noaa.gov					      1999
!
!  Compute one row of the coefficients for the compact mid-interval
!  interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!  Where s is the vector of "source" values, t the staggered "target" values.
!
! --> NA:   number of t-points operated on by this row of the A of (*)
! --> NB:   number of s-points operated on by this row of the B of (*)
! --> ZA:   coordinates of t-points used in this row of (*)
! --> ZB:   coordinates of s-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the NA coefficients A for this scheme
! <-- B:    the NB coefficients B for this scheme
!=============================================================================
INTEGER, INTENT(IN )          :: na,nb
REAL(8), INTENT(IN )          :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)          :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                       :: na1,nab,i
REAL(8),DIMENSION(na+nb,na+nb):: w
REAL(8),DIMENSION(na)         :: za0,pa
REAL(8),DIMENSION(nb)         :: zb0,pb
REAL(8),DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE davco

!=============================================================================
SUBROUTINE avco(na,nb,za,zb,z0,a,b) 
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                     :: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE avco 


SUBROUTINE ddfco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jpurser@ncep.noaa.gov					      1999
!		    SUBROUTINE DDFCO
!
!  Compute one row of the coefficients for either the compact differencing or
!  quadrature scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  In either case, d is the derivative of c.
!
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the A-coefficients for this scheme
! <-- B:    the B-coefficients for this scheme
!=============================================================================
INTEGER, INTENT(IN)            :: na,nb
REAL(8), INTENT(IN)            :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)           :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                        :: na1,nab,i
REAL(8), DIMENSION(na+nb,na+nb):: w
REAL(8), DIMENSION(na)         :: za0,pa
REAL(8), DIMENSION(nb)         :: zb0,pb
REAL(8), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;       pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco 

!=============================================================================
SUBROUTINE dfco(na,nb,za,zb,z0,a,b)
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER:: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;       pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco 

!=============================================================================
SUBROUTINE ddfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!		    SUBROUTINE DDFCO2
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   jpurser@ncep.noaa.gov					      1999
!
!  Compute one row of the coefficients for either the compact second-
!  differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  Where d is the second-derivative of c.
!
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! <-- A:    the NA coefficients A for this scheme
! <-- B:    the NB coefficients B for this scheme
!=============================================================================
INTEGER, INTENT(IN )           :: na,nb
REAL(8), INTENT(IN )           :: za(na),zb(nb),z0
REAL(8), INTENT(OUT)           :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER                        :: na1,nab,i
REAL(8), DIMENSION(na+nb,na+nb):: w
REAL(8), DIMENSION(na)         :: za0,pa
REAL(8), DIMENSION(nb)         :: zb0,pb
REAL(8), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;             pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco2 

!=============================================================================
SUBROUTINE dfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
INTEGER, INTENT(IN )        :: na,nb
REAL,    INTENT(IN )        :: za(na),zb(nb),z0
REAL,    INTENT(OUT)        :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER:: na1,nab,i
REAL, DIMENSION(na+nb,na+nb):: w
REAL, DIMENSION(na)         :: za0,pa
REAL, DIMENSION(nb)         :: zb0,pb
REAL, DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0; zb0=zb-z0
pa=1.;     pb=-1.
w=0.;         ab=0.
w(1,1:na)=1.; ab(1)=1.
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;             pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco2 

!=============================================================================
SUBROUTINE clib(a,m1,m2,mah1,mah2) ! Clip the dead space of the band matrix, a
!=============================================================================
INTEGER, INTENT(IN)   :: m1, m2, mah1, mah2
REAL,    INTENT(INOUT):: a(m1,-mah1:mah2)
INTEGER               :: j
IF(m2-m1+mah1 < 0)STOP 'In CLIB, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:j,-j)=0.; ENDDO; DO j=m2-m1+1,mah2; a(m2-j+1:m1,j)=0.; ENDDO
END SUBROUTINE clib

!=============================================================================
SUBROUTINE dclib(a,m1,m2,mah1,mah2) ! Clip dead space of the band matrix, a
!=============================================================================
INTEGER, INTENT(IN)   :: m1, m2, mah1, mah2
REAL(8), INTENT(INOUT):: a(m1,-mah1:mah2)
INTEGER               :: j
IF(m2-m1+mah1 < 0)STOP 'In CLIB_d, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:j,-j)=0.; ENDDO; DO j=m2-m1+1,mah2; a(m2-j+1:m1,j)=0.; ENDDO
END SUBROUTINE dclib

SUBROUTINE cad1b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Incorporate operand symmetry near end-1 of a band matrix operator
!
! <-> A:      Input as unclipped operator, output as symmetrized and clipped.
! m1, m2:     Sizes of implied full matrix
! mah1, mah2: Left and right semi-bandwidths of A.
! mirror2:    2*location of symmetry axis relative to end-1 operand element.
!      Note: although m2 is not used here, it IS used in companion routines
!            cad2b and csb2b; it is retained in the interests of uniformity.
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(0:m1-1,-mah1:mah2)
INTEGER                :: i,i2,jm,jp,jpmax
IF(mirror2+mah1 > mah2)STOP 'In cad1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax <= -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp <= jm)EXIT
      a(i,jp)=a(i,jp)+a(i,jm) ! Reflect and add
      a(i,jm)=0.              ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY     csb1b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Like cad1b, but for antisymmetric operand
IF(mirror2+mah1 > mah2)STOP 'In csb1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax < -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp < jm)EXIT
      a(i,jp)=a(i,jp)-a(i,jm) ! Reflect and subtract
      a(i,jm)=0.              ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad1b

!=============================================================================
SUBROUTINE cad2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
! Incorporate operand symmetry near end-2 of a band matrix operator
!
! <-> A:      Input as unclipped operator, output as symmetrized and clipped.
! m1, m2:     Sizes of implied full matrix
! mah1, mah2: Left and right semi-bandwidths of A.
! mirror2:    2*location of symmetry axis relative to end-2 operand element.
!=============================================================================
INTEGER,  INTENT(IN)   :: m1,m2,mah1,mah2,mirror2
REAL,     INTENT(INOUT):: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)
INTEGER                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cad2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)+a(i,jp) ! Reflect and add
      a(i,jp)=0.              ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY    csb2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In csb2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin > nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm > jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp) ! Reflect and subtract
      a(i,jp)=0.              ! zero the exterior part
   ENDDO
ENDDO
!=============================================================================
ENTRY    cex2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cex2b, mah1 insufficient'
mirror=mirror2/2
IF(mirror*2 /= mirror2)STOP 'In cex2b, mirror2 is not even'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   j0=mirror-i
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp)    ! Reflect and subtract
      a(i,j0)=a(i,j0)+2.*a(i,jp) ! Apply double the coefficient to end
      a(i,jp)=0.                 ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad2b

!=============================================================================
SUBROUTINE copbt(a,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE COPBT
!  Copy transpose of rectangular banded matrix A to B
!  Note: this routine expects A and B always to occupy separate storage.
!
! --> A  input matrix in banded format
! <-- B  output matrix in banded format
! --> M1 number of rows of A, columns of B
! --> M2 number of columns of A, rows of B
! --> MAH1 left-half-bandwidth of A, right-half-bandwidth of B
! --> MAH2 right-half-bandwidth of A, left-half-bandwidth of B
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2
REAL,     INTENT(IN) :: a(m1,-mah1:mah2)
REAL,     INTENT(OUT):: b(m2,-mah2:mah1)
INTEGER              :: j, i
CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 conbt(a,b,m1,m2,mah1,mah2)
CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=-a(i,j); ENDDO
ENDDO
END SUBROUTINE copbt

!=============================================================================
SUBROUTINE copmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,m2),        INTENT(IN) :: afull
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(OUT):: aband
INTEGER                                       :: i1,i2, i, j
CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)= afull(i,j+i); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)=-afull(i,j+i); ENDDO
ENDDO
END SUBROUTINE copmb

!=============================================================================
SUBROUTINE copbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,                           INTENT(IN) :: m1, m2, mah1, mah2
REAL,     DIMENSION(m1,-mah1:mah2),INTENT(IN) :: aband
REAL,     DIMENSION(m1,m2),        INTENT(OUT):: afull
INTEGER                                       :: i1,i2, i, j
afull=0.
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)= aband(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
afull=0.
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)=-aband(i,j); ENDDO
ENDDO
END SUBROUTINE copbm
 
!=============================================================================
SUBROUTINE mulbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
INTEGER,  INTENT(IN)   :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL,     INTENT(IN)   :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL,     INTENT(INOUT):: c(m1,-mch1:mch2)
INTEGER                :: nch1, nch2, j, k, jpk, i1,i2
c=0.0
ENTRY      madbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MULBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE mulbb

!=============================================================================
SUBROUTINE msbbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL,     INTENT(IN) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL,     INTENT(OUT):: c(m1,-mch1:mch2)
INTEGER              :: nch1, nch2, j, k, jpk, i1,i2
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MSBBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)-a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE msbbb

!=============================================================================
SUBROUTINE LDUB(a,m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LDUB
!  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! <-> A: input as the asymmetric band matrix. On output, it contains
!     the [L]*[D**-1]*[U] factorization of the input matrix, where
!     [L] is lower triangular with unit main diagonal
!     [D] is a diagonal matrix
!     [U] is upper triangular with unit main diagonal
! --> M:    The number of rows of array A
! --> MAH1: the left half-bandwidth of fortran array A
! --> MAH2: the right half-bandwidth of fortran array A
!=============================================================================
INTEGER, INTENT(IN)   :: m,mah1, mah2 
REAL,    INTENT(INOUT):: a(m,-mah1:mah2) 
INTEGER               :: j, imost, jmost, jp, i
REAL                  :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)THEN
    PRINT '(" Failure in LDUB:"/" Matrix requires pivoting or is singular")'
    STOP
  ENDIF
  ajji=1./ajj
  a(j,0)=ajji
  DO i=jp,imost
    aij=ajji*a(i,j-i)
    a(i,j-i)=aij
    a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE LDUB

!=============================================================================
SUBROUTINE DLDUB(a,m,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN)   :: m,mah1, mah2 
REAL(8),  INTENT(INOUT):: a(m,-mah1:mah2) 
INTEGER                :: j, imost, jmost, jp, i
REAL(8)                :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)THEN
    PRINT '(" Fails in LDUB_d:"/" Matrix requires pivoting or is singular")'
    STOP
  ENDIF
  ajji=1./ajj
  a(j,0)=ajji
  DO i=jp,imost
    aij=ajji*a(i,j-i)
    a(i,j-i)=aij
    a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE DLDUB

!=============================================================================
SUBROUTINE L1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE L1UBB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M    Number of rows of A and B
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
!=============================================================================
INTEGER, INTENT(IN) ::  m,mah1, mah2, mbh1, mbh2 
REAL, INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)
INTEGER             :: j, imost, jmost, jleast, jp, i
REAL                :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(1,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)STOP 'failure in L1UBB'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE L1UBB

!=============================================================================
SUBROUTINE DL1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
INTEGER                :: m,j, imost, jmost, jleast, jp, i
INTEGER,  INTENT(IN)   ::  mah1, mah2, mbh1, mbh2 
REAL(8),  INTENT(INOUT):: a(m,-mah1:mah2), b(m,-mbh1:mbh2)
REAL(8)                :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(1,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)STOP 'failure in DL1UBB'
  AJJI=1./AJJ
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO I=JP,IMOST
    AIJ=A(I,J-I)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  A(J,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE DL1UBB

!=============================================================================
SUBROUTINE l1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE L1UEB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  all but row zero of the
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!  This is a special adaptation of L1UBB used to process quadarature weights
!  for QEDBV etc in which the initial quadrature value is provided as input
!  instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M    number of rows of B, one less than the rows of A (which has "row 0")
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
!=============================================================================
INTEGER, INTENT(IN) :: m,mah1, mah2, mbh1, mbh2 
REAL, INTENT(INOUT) :: a(0:m,-mah1:mah2), b(m,-mbh1:mbh2)
INTEGER :: j, imost, jmost, jleast, jp, i
REAL :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(0,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0.)STOP 'failure in L1UEB'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE l1ueb

!=============================================================================
SUBROUTINE dl1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
INTEGER,  INTENT(IN)   :: m,mah1, mah2, mbh1, mbh2 
REAL(8),  INTENT(INOUT):: a(0:,-mah1:), b(:,-mbh1:)
INTEGER                :: j, imost, jmost, jleast, jp, i
REAL(8)                :: ajj, ajji, aij
DO j=1,m
  imost=MIN(m,j+mah1)
  jmost=MIN(m,j+mah2)
  jleast=MAX(0,j-mah1)
  jp=j+1
  ajj=a(j,0)
  IF(ajj == 0)STOP 'failure in L1UEB_d'
  ajji=1./ajj
  a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
  DO i=jp,imost
    aij=a(i,j-i)
    a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
  ENDDO
  a(j,0)=1.
  b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE dl1ueb

!=============================================================================
SUBROUTINE L1LB(a,b,m,mah)	! Cholesky LU decomposition of Banded.
!=============================================================================
INTEGER,  INTENT(IN) :: m, mah
REAL,     INTENT(IN) :: a(m,-mah:mah)
REAL,     INTENT(OUT):: b(m,-mah:0)
INTEGER              :: i, j,jmi
REAL                 :: s
CALL clib(b,m,m,mah,0)
DO j=1,m
   s=a(j,0)-DOT_PRODUCT(b(j,-mah:-1),b(j,-mah:-1))
   IF(s <= 0.)THEN
      PRINT '(" L1LB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   s=SQRT(s); b(j,0)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i
      b(i,jmi)=s*(a(i,jmi)-DOT_PRODUCT(b(i,-mah:jmi-1),b(j,-mah-jmi:-1)))
   ENDDO
ENDDO
END SUBROUTINE L1LB

!=============================================================================
SUBROUTINE LDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
INTEGER,  INTENT(IN) :: m, mah
REAL,     INTENT(IN) :: a(m,-mah:mah)
REAL,     INTENT(OUT):: b(m,-mah:0)
REAL,     INTENT(OUT):: d(m) 
INTEGER              :: i, j,k,jmi,lj,li
REAL                 :: s,t
CALL clib(b,m,m,mah,0); b(:,0)=1.
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= 0.)THEN
      PRINT '(" LDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i; li=MAX(-mah,1-i); lj=li-jmi
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=1./d
END SUBROUTINE LDLB

!=============================================================================
SUBROUTINE DLDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
INTEGER,  INTENT(IN) :: m, mah
REAL(8),  INTENT(IN) :: a(m,-mah:mah)
REAL(8),  INTENT(OUT):: b(m,-mah:0)
REAL(8),  INTENT(OUT):: d(m) 
INTEGER              :: i, j,k,jmi,lj,li
REAL(8)              :: s,t
CALL clib_d(b,m,m,mah,0); b(:,0)=1.
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= 0.)THEN
      PRINT '(" DLDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=1./s
   DO i=j+1,MIN(m,j+mah); jmi=j-i;  
      li=MAX(-mah,1-i); 
      lj=li-jmi; 
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=1./d
END SUBROUTINE DLDLB

!=============================================================================
SUBROUTINE UDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
INTEGER,        INTENT(IN) :: m, mah
REAL,           INTENT(IN) :: a(m,-mah:mah)
REAL,           INTENT(OUT):: b(m,0:mah)
REAL,           INTENT(OUT):: d(m) 
REAL, DIMENSION(m,-mah:mah):: at
REAL, DIMENSION(m,-mah:0)  :: bt
REAL, DIMENSION(m)         :: dt
at=a(m:1:-1,mah:-mah:-1); CALL ldlb(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1); d=dt(m:1:-1)
END SUBROUTINE UDUB

!=============================================================================
SUBROUTINE DUDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
INTEGER,           INTENT(IN) :: m, mah
REAL(8),           INTENT(IN) :: a(m,-mah:mah)
REAL(8),           INTENT(OUT):: b(m,0:mah)
REAL(8),           INTENT(OUT):: d(m) 
REAL(8), DIMENSION(m,-mah:mah):: at
REAL(8), DIMENSION(m,-mah:0)  :: bt
REAL(8), DIMENSION(m)         :: dt
at=a(m:1:-1,mah:-mah:-1); CALL ldlb_d(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1);   d=dt(m:1:-1)
END SUBROUTINE DUDUB

!=============================================================================
SUBROUTINE mulbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBV
!  MULtipication of a Banded matrix times a Vector.
!
! --> A is the matrix
! --> V1 the input vector
! <-- V2 the output vector
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2
REAL,     INTENT(IN) :: a(m1,-mah1:mah2), v1(m2)
REAL,     INTENT(OUT):: v2(m1)
INTEGER              :: j, i1,i2 
v2 = 0.0
!=============================================================================
ENTRY	 madbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) + a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) - a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbv

!=============================================================================
SUBROUTINE mulbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBX
!  MULtipication of a Banded matrix times parallel X-Vectors.
!
! --> A is the matrix
! --> V1 the array of input vectors
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY the number of parallel X-vectors
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN) :: a(m1,-mah1:mah2), v1(m2,my)
REAL,     INTENT(OUT):: v2(m1,my)
INTEGER              :: i,j
v2=0.0
!=============================================================================
ENTRY	 madbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)-a(i,j)*v1(i+j,:); ENDDO
ENDDO
END SUBROUTINE mulbx

!=============================================================================
SUBROUTINE mulby(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBY
!  MULtipication of a Banded matrix times parallel Y-Vectors.
!
! --> A is the matrix
! --> V1 the array of input vectors
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and for V2
! --> M2 the number of columns assumed for A and rows for V1
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX the length of each of the parallel Y-vectors
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL,     INTENT(OUT):: v2(mx,m1)
INTEGER              :: i,j
v2(1:mx,1:m1) = 0.0
ENTRY	 madby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)+a(i,j)*v1(:,i+j); ENDDO
ENDDO
RETURN
ENTRY	 msbby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)-a(i,j)*v1(:,i+j); ENDDO
ENDDO
END SUBROUTINE mulby

!=============================================================================
SUBROUTINE MULVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULVB
!  MULtipication of a Vector times a Banded matrix.
!
! --> V1 the input row-vector
! --> A is the matrix
! <-- V2 the output vector
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and for V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2
REAL,     INTENT(IN) :: v1(m1), a(m1,-mah1:mah2)
REAL,     INTENT(OUT):: v2(m2)
INTEGER              :: j, i1,i2
v2=0.0
!=============================================================================
ENTRY	 madvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)-v1(i1:i2)*a(i1:i2,j)
ENDDO
END SUBROUTINE mulvb

!=============================================================================
SUBROUTINE mulxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULXB
!  MULtipication of X-Vectors times Banded matrix.
!
! --> V1 the array of input row-vectors
! --> A is the matrix
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY the number of parallel X-vectors
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2, my
REAL,     INTENT(IN) :: v1(m1,my), a(m1,-mah1:mah2)
REAL,     INTENT(OUT):: v2(m2,my)
INTEGER              :: i,j
v2=0.0
!=============================================================================
ENTRY	 madxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)-v1(i,:)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulxb

!=============================================================================
SUBROUTINE mulyb(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULYB
!  MULtipication of Y-Vectors times a Banded matrix.
!
! --> V1 the array of input row-vectors
! --> A is the matrix
! <-- V2 the array of output vectors
! --> M1 the number of rows assumed for A and columns for V1
! --> M2 the number of columns assumed for A and V2
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX the length of each of the parallel Y-vectors
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2, mx
REAL,     INTENT(IN) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL,     INTENT(OUT):: v2(mx,m2)
INTEGER              :: i,j
v2=0.0
ENTRY	 madyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 msbyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)-v1(:,i)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulyb

!=============================================================================
SUBROUTINE mulbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULBD
! MULtipication of a Banded matrix times a Diagonal
!
! --> A is the input banded-matrix
! --> D the diagonal matrix
! <-- B the output matrix
! --> M1 the number of rows assumed for A and for B
! --> M2 number of columns assumed for A and B, number of elements of D
! --> MAH1 the left half-bandwidth of arrays A and B
! --> MAH2 the right half-bandwidth of arrays A and B
!=============================================================================
INTEGER,  INTENT(IN   ):: m1, m2, mah1, mah2
REAL,     INTENT(IN   ):: d(m2)
REAL,     INTENT(INOUT):: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
INTEGER                :: j, i1,i2
CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j)=a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 madbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)+a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)-a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbd

!=============================================================================
SUBROUTINE muldb(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE MULDB
!  MULtipication of a Banded matrix times a Diagonal
!
! --> D the diagonal matrix
! --> A is the input banded-matrix ! <->  if A and B are actually
! <-- B the output matrix          ! <->  equivalent arrays.
! --> M1 the number of rows assumed for A and for B
! --> M2 number of columns assumed for A and B, number of elements of D
! --> MAH1 the left half-bandwidth of arrays A and B
! --> MAH2 the right half-bandwidth of arrays A and B
!=============================================================================
INTEGER,  INTENT(IN)    :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
INTEGER                 :: j
CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; b(:,j)=d(:)*a(:,j); ENDDO
END SUBROUTINE muldb

!=============================================================================
SUBROUTINE maddb(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN)    :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1)
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
INTEGER                 :: j
DO j=-mah1,mah2; b(:,j)=b(:,j)+d(:)*a(:,j); ENDDO
END SUBROUTINE maddb

!=============================================================================
SUBROUTINE msbdb(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN)    :: m1, m2, mah1, mah2
REAL,     INTENT(IN   ) :: d(m1) 
REAL,     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)
INTEGER                 :: j
DO j=-mah1,mah2; b(:,j)=b(:,j)-d(:)*a(:,j); ENDDO
END SUBROUTINE msbdb


!=============================================================================
SUBROUTINE udlbv(a,v, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBV
!  BACk-substitution step of linear inversion involving
!  Banded matrix and Vector.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! <-> V input as right-hand-side vector, output as solution vector
! --> M the number of rows assumed for A and for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN)   :: m, mah1, mah2
REAL,     INTENT(IN)   :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT):: v(m)
INTEGER                :: i, j
REAL                   :: vj
DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO; v(j)=a(j,0)*vj
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE udlbv

!=============================================================================
SUBROUTINE udlbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBX
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and X-Vectors.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: mx, mah1, mah2, my
REAL,     INTENT(IN)   :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: jx, ix
DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
   v(jx,:) = a(jx,0) * v(jx,:)
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE udlbx

!=============================================================================
SUBROUTINE udlby(a,v, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLBY
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and Y-Vectors.
!
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: my, mah1, mah2, mx
REAL,     INTENT(IN)   :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: iy, jy
DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy) = v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
   v(:,jy)=a(jy,0)*v(:,jy)
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE udlby

!=============================================================================
SUBROUTINE udlvb(v,a, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLVB
!  BACk-substitution step of linear inversion involving
!  row-Vector and Banded matrix.
!
! <-> V input as right-hand-side row-vector, output as solution vector
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> M the number of rows assumed for A and columns for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN)   :: m, mah1, mah2
REAL,     INTENT(IN)   :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT):: v(m)
INTEGER                :: i, j
REAL                   :: vi
DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
   v(i)=vi*a(i,0)
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE udlvb

!=============================================================================
SUBROUTINE udlxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLXB
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and row-X-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: mx, mah1, mah2, my
REAL,     INTENT(IN)   :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: ix, jx
DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
   v(ix,:)=v(ix,:)*a(ix,0)
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1; v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE udlxb

!=============================================================================
SUBROUTINE udlyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE UDLYB
!  BACk-substitution step of parallel linear inversion involving
!  Banded matrix and row-Y-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the (L)*(D**-1)*(U) factorization of the linear-system
!     matrix, as supplied by subroutine LDUB
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: my, mah1, mah2, mx
REAL,     INTENT(IN)   :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: iy, jy
DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
   v(:,iy)=v(:,iy)*a(iy,0)
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1; v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE udlyb

!=============================================================================
SUBROUTINE u1lbv(a,v, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBV
!  BACk-substitution step ((U**-1)*(L**-1)) of linear inversion involving
!  special Banded matrix and right-Vector.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vector, output as solution vector
! --> M the number of rows assumed for A and for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER,  INTENT(IN)   :: m, mah1, mah2
REAL,     INTENT(IN)   :: a(m,-mah1:mah2)
REAL,     INTENT(INOUT):: v(m)
INTEGER                :: i, j
REAL                   :: vj
DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE u1lbv

!=============================================================================
SUBROUTINE u1lbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBX
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and X-right-Vectors.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: mx, mah1, mah2, my
REAL,     INTENT(IN)   :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: ix, jx
DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE u1lbx

!=============================================================================
SUBROUTINE u1lby(a,v, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LBY
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and Y-right-Vectors.
!
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! <-> V input as right-hand-side vectors, output as solution vectors
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: my, mah1, mah2, mx
REAL,     INTENT(IN)   :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: iy, jy
DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE u1lby

!=============================================================================
SUBROUTINE u1lvb(v,a, m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LVB
!  Special BaCk-substitution step of linear inversion involving
!  left-Vector and Banded matrix.
!
! <-> V input as right-hand-side row-vector, output as solution vector
! --> A encodes the special [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> M the number of rows assumed for A and columns for V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
!=============================================================================
INTEGER, INTENT(IN)   :: m, mah1, mah2
REAL,    INTENT(IN)   :: a(m,-mah1:mah2)
REAL,    INTENT(INOUT):: v(m)
INTEGER               :: i, j
REAL                  :: vi
DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE u1lvb

!=============================================================================
SUBROUTINE u1lxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LXB
!  Special BaCk-substitution step of parallel linear inversion involving
!  Banded matrix and X-left-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the special [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> MX the number of rows assumed for A and length of
!     X-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MY number of parallel X-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: mx, mah1, mah2, my
REAL,     INTENT(IN)   :: a(mx,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: ix, jx
DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1;  v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE u1lxb

!=============================================================================
SUBROUTINE u1lyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE U1LYB
!  Special BaCk-substitution step of parallel linear inversion involving
!  special Banded matrix and Y-left-Vectors.
!
! <-> V input as right-hand-side vectors, output as solution vectors
! --> A encodes the [L]*[U] factorization of the linear-system
!     matrix, as supplied by subroutine L1UBB
! --> MY the number of rows assumed for A and length of
!     Y-vectors stored in V
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> MX number of parallel Y-vectors inverted
!=============================================================================
INTEGER,  INTENT(IN)   :: my, mah1, mah2, mx
REAL,     INTENT(IN)   :: a(my,-mah1:mah2)
REAL,     INTENT(INOUT):: v(mx,my)
INTEGER                :: iy, jy
DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1;  v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE u1lyb

!=============================================================================
SUBROUTINE linbv(a,v,m,mah1,mah2)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINBV
!   Solve LINear system with square Banded-matrix and vector V
!
! <-> A system matrix on input, its [L]*[D**-1]*[U] factorization on exit
! <-> V vector of right-hand-sides on input, solution vector on exit
! --> M order of matrix A
! --> MAH1 left half-bandwidth of A
! --> MAH2 right half-bandwidth of A
!=============================================================================
INTEGER, INTENT(IN)    :: m, mah1, mah2
REAL,    INTENT(INOUT) :: a(m,-mah1:mah2), v(m)
CALL ldub(a,m,mah1,mah2)
CALL udlbv(a,v,m,mah1,mah2)
END SUBROUTINE linbv

!=============================================================================
SUBROUTINE wrtb(a,m1,m2,mah1,mah2)
!=============================================================================
INTEGER,  INTENT(IN) :: m1, m2, mah1, mah2
REAL,     INTENT(IN) :: a(m1,-mah1:mah2)
INTEGER              :: i1, i2, i, j1, j2, j, nj1
DO i1=1,m1,20
   i2=MIN(i1+19,m1)
   PRINT '(7x,6(i2,10x))',(j,j=-mah1,mah2)
   DO i=i1,i2
      j1=MAX(-mah1,1-i)
      j2=MIN(mah2,m2-i)
      nj1=j1+mah1
      IF(nj1==0)PRINT '(1x,i3,6(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==1)PRINT '(1x,i3,12x,5(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==2)PRINT '(1x,i3,24x,4(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==3)PRINT '(1x,i3,36x,3(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==4)PRINT '(1x,i3,48x,2(1x,e11.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==5)PRINT '(1x,i3,60x,1(1x,e11.5))',i,(a(i,j),j=j1,j2)
   ENDDO
   READ(*,*)
ENDDO
END SUBROUTINE wrtb

END MODULE MODULE_pmat2


!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						********************
!						* module_pmat2.f90 *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

!						**********************
!						* module_fitcons.f90 *
!						* PURSER 1994/1999   *
!                                               *   FUJITA 1999      *
!						**********************

!============================================================================
module vkind
!============================================================================
!  integer, parameter :: vp=kind(1.0d0)
  integer, parameter :: vp=kind(1.0)
end module vkind

!============================================================================
module module_fitcons
!============================================================================
use vkind
implicit none
integer,parameter             :: noh=3,    nohm=noh-1,   nohp=noh+1,&
                                 no=noh*2, nom=no-1,     nop=no+1,   nnit=7
real(vp),parameter            :: sigc=3._vp,  sigb=2._vp
real(vp),dimension(no)        :: hunit,q,wt,dwt
real(vp),dimension(nom)       :: hunit1,hunit2,q1,wt1,dwt1
real(vp),dimension(-noh:noh)  :: qco
real(vp),dimension(-1-noh:noh):: ico,dco
real(vp)                      :: rcrit,ldsig,ldsig4
!============================================================================

contains
!============================================================================
SUBROUTINE setq(q,x,n) 
!============================================================================
!                SUBROUTINE SETQ
! Precompute the N constant denominator factors of the N-point Lagrange
! polynomial interpolation formula.
!
! <-- Q:    The N denominator constants.
! --> X:    The N abscissae.
! --> N:    The number of points involved.
!============================================================================
  use vkind
  IMPLICIT NONE
  INTEGER,          INTENT(in) :: n
  REAL(vp),DIMENSION(n),INTENT(out):: q
  REAL(vp),DIMENSION(n),INTENT(in) :: x
!-----------------------------------------------------------------------------
  INTEGER                          :: i,j
!=============================================================================
DO i=1,n
   q(i)=1.
   DO j=1,n
      IF(j /= i)q(i)=q(i)/(x(i)-x(j))
   ENDDO
ENDDO
END SUBROUTINE setq 

!============================================================================
SUBROUTINE lagw(x,xt,q,w,dw,n) 
!============================================================================
!      SUBROUTINE LAGW
! Construct the Lagrange weights and their derivatives when target abscissa
! is known and denominators Q have already been precomputed
!
! --> X:    Grid abscissae
! --> XT:   Target abscissa
! --> Q:    Q factors (denominators of the Lagrange weight formula)
! <-- W:    Lagrange weights
! <-- DW:   Derivatives, dW/dX, of Lagrange weights W
! --> N:    Number of grid points involved in the interpolation
!============================================================================
  use vkind
  IMPLICIT NONE
  INTEGER,              INTENT(in) :: n
  REAL(vp),             INTENT(in) :: xt
  REAL(vp),DIMENSION(n),INTENT(in) :: x,q
  REAL(vp),DIMENSION(n),INTENT(out):: w,dw
!-----------------------------------------------------------------------------
  REAL(vp),DIMENSION(n)            :: sdit,d,di
  INTEGER                          :: i,j
  REAL(vp)                         :: p,s,sdil,sdir
!============================================================================
p=1.       ! ...will become product of all the d(i)=xt-x(i)
DO i=1,n
   d(i)=xt-x(i)
   p=p*d(i)
ENDDO

!   test p to reveal whether any of the d(i) vanish:
IF(p == 0._vp)THEN   ! xt coincides with a grid point - use special code:
   p=1.           ! p will become the product of the nonzero d(i),
   s=0.           ! s will become the corresponding sum of q(i)/d(i)
   DO i=1,n
      IF(d(i) == 0._vp)THEN
         j=i            ! identify the grid index corresponding to present xt
         w(j)=1.        ! interpolation weighted entirely to this one.
      ELSE
         w(i)=0.
         p=p*d(i)
         dw(i)=q(i)/d(i)
         s=s+dw(i)
      ENDIF
   ENDDO
   dw(j)=-s*p
   DO i=1,n
      IF(i /= j)dw(i)=dw(i)*p
   ENDDO
ELSE             ! xt is not a grid point - use generic code:
   sdil=0.            ! will become the sum of terms to the left.
   sdir=0.            ! will become the sum of terms to the right.
   DO i=1,n
      di(i)=1./d(i)
      sdit(i)=sdil
      sdil=sdil+di(i)
      w(i)=q(i)*p*di(i)
   ENDDO
   DO i=n,1,-1
      sdit(i)=sdit(i)+sdir
      sdir=sdir+di(i)
      dw(i)=w(i)*sdit(i)
   ENDDO
ENDIF
END SUBROUTINE lagw 

!============================================================================
subroutine infit
!============================================================================
implicit none
integer :: i,l
real(vp):: divq,divd
!============================================================================
! Initialize quantities that relate to interpolations:
do i=1,no; hunit(i)=i-noh; enddo
hunit1=hunit(:nom)    ; hunit2=hunit(2:)
call setq(q,hunit,no) ; call setq(q1,hunit1,nom)
rcrit=SQRT(EPSILON(1._vp))
!------------------------------------
! Initialize coefficients for quadrature, differencing and mdpt interpolation:
divq=967680        ; divd=1024
qco(0)=862564/divq ; dco(0)=1225/divd     ; ico(0)=1225/(2*divd)
qco(1)= 57249/divq ; dco(1)=-245/(3*divd) ; ico(1)=-245/(2*divd)
qco(2)= -5058/divq ; dco(2)=  49/(5*divd) ; ico(2)=  49/(2*divd)
qco(3)=   367/divq ; dco(3)=  -5/(7*divd) ; ico(3)=  -5/(2*divd)
qco(-1:-noh:-1)  = qco(1:noh) ! complete the stencil of quadrature coeffs.
dco(-1:-nohp:-1) =-dco(0:noh) ! complete the stencil of difference coeffs
ico(-1:-nohp:-1) = ico(0:noh) ! complete the stencil of interpolation coeffs.
!------------------------------------
! Initial coefficients related to control of working grid resolution:
ldsig =log(sigc/sigb)
ldsig4=ldsig**4
end subroutine infit
end module module_fitcons

!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						**********************
!						* module_fitcons.f90 *
!						* PURSER 1994/1999   *
!                                               *   FUJITA 1999      *
!						**********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

!						********************
!						* coefrf.f90       *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************

!=============================================================================
subroutine coefrf(sig,nu,n,m,bnf,lnf)
!=============================================================================
! R. J. Purser NCEP 2001
!-----------------------------------------------------------------------------
use module_pmat2
implicit none
integer,              intent(IN   ) :: n,m
real, dimension(n),   intent(IN   ) :: sig,nu
real, dimension(n),   intent(OUT  ) :: bnf
real, dimension(m,n), intent(OUT  ) :: lnf
!-------------------------------------------------------------------------- 
integer, parameter                  :: irmax=6
real, dimension(n,-m:m)             :: s
real, dimension(n,-m:0)             :: sl
real, dimension(n,-m:m,m)           :: k,l
real, dimension(n)                  :: eta
real, dimension(irmax)              :: bcofi,bcofh
integer                             :: i,i1,il,ir,ik
!--------------------------------------------------------------------------
! The coefficients bcofi are the reciprocals of the i=1 entries of TABLE 1
! of NCEP O.N. 431:
data bcofi/1., 12., 90., 560., 3150., 16632./
!=============================================================================
bcofh=.5/bcofi
do i=1,n
   eta(i)=sig(i)*sqrt(nu(i))
enddo
k=0
!-------------------------------------------------------------------------
! Set k(:, -1:1, 1) to be the K-matrix of (4.8)--(4.10) of NCEP O.N. 431: 
!--------------------------------------------------------------------------
do i=1,n-1
   k(i  , 0,1)=k(i,0,1)  +eta(i+1)/eta(i)
   k(i+1, 0,1)=k(i+1,0,1)+eta(i)/eta(i+1)
   k(i  , 1,1)=-1
   k(i+1,-1,1)=-1
enddo

!-------------------------------------------------------------------------
! Set k(:, : , ir) to be the original K-matrix raised to the power of (ir):
!--------------------------------------------------------------------------
do ir=2,m
   il=ir-1
   call mulbb(k(:,-1:1,1),k(:,-il:il,il),k(:,-ir:ir,ir),n,n,1,1,il,il,ir,ir)
enddo

!-------------------------------------------------------------------------
! Pre- and post-multiply each of the m powers of K by the diagonal matrix,
! sigma, of NCEP O.N. 431, where the elements of sigma measure the smoothing
! scale of the quasi-Gaussian filter in grid-space units.
! Also, multiply each of the resulting banded matrices by .5*b_{1,ir} for
! the appropriate index, ir, corresponding to the power by which the original
! K was raised.
!--------------------------------------------------------------------------
do ir=1,m
   call mulbd(k(:,-ir:ir,ir),sig,k(:,-ir:ir,ir),n,n,ir,ir)
   call muldb(sig,k(:,-ir:ir,ir),k(:,-ir:ir,ir),n,n,ir,ir)
   k(:,-ir:ir,ir)=k(:,-ir:ir,ir)*bcofh(ir)
enddo


s=0
s(:,0)=1.

do ir=1,m
   l(:,-ir:ir,ir)=k(:,-ir:ir,ir)
   s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
enddo
do i1=2,m
   do ir=m,i1,-1
      l(:,-ir:ir,ir)=0.
      do ik=1,ir-i1+1
         il=ir-ik
         call madbb(k(:,-ik:ik,ik),l(:,-il:il,il),l(:,-ir:ir,ir), &
              n,n,ik,ik,il,il,ir,ir)
      enddo
      l(:,-ir:ir,ir)=l(:,-ir:ir,ir)/i1
      s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
   enddo
enddo
call ldlb(s,sl,bnf,n,m)
do i1=1,m
do i=1,n
   lnf(i1,i)=sl(i,-i1)
enddo
enddo
end subroutine coefrf


!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						********************
!						* coefrf.f90       *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

!						********************
!						* hgnrf.f90        *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************

!============================================================================
subroutine ldlb1i(nol,lnf,bnf,                                              &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),                       &
           INTENT(INOUT) :: bnf
  REAL, DIMENSION(nol, ims:ime),                  &
           INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,l,m,nola
  real                   :: s
!============================================================================
do i=its,ite
   nola=min(nol,i-its)
   do l=nola,1,-1
      s=lnf(l,i)
      do m=l+1,nola
         s=s-lnf(m,i)*bnf(i-m)*lnf(m-l,i-l)
      enddo
      lnf(l,i)=s/bnf(i-l)
   enddo
   s=bnf(i)
   do l=1,nola
      s=s-lnf(l,i)**2*bnf(i-l)
   enddo
   bnf(i)=s
enddo
end subroutine ldlb1i
   
!============================================================================
subroutine ldlb2i(nol,lnf,bnf,                                              &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: bnf
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,l,m,nola
  real                   :: s
!============================================================================
do j=jts,jte
do i=its,ite
   nola=min(nol,i-its)
   do l=nola,1,-1
      s=lnf(l,i,j)
      do m=l+1,nola
         s=s-lnf(m,i,j)*bnf(i-m,j)*lnf(m-l,i-l,j)
      enddo
      lnf(l,i,j)=s/bnf(i-l,j)
   enddo
   s=bnf(i,j)
   do l=1,nola
      s=s-lnf(l,i,j)**2*bnf(i-l,j)
   enddo
   bnf(i,j)=s
enddo
enddo
end subroutine ldlb2i
   
!============================================================================
subroutine ldlb2j(nol,lnf,bnf,                                              &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: bnf
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,l,m,nola
  real                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do i=its,ite
   do l=nola,1,-1
      s=lnf(l,i,j)
      do m=l+1,nola
         s=s-lnf(m,i,j)*bnf(i,j-m)*lnf(m-l,i,j-l)
      enddo
      lnf(l,i,j)=s/bnf(i,j-l)
   enddo
   s=bnf(i,j)
   do l=1,nola
      s=s-lnf(l,i,j)**2*bnf(i,j-l)
   enddo
   bnf(i,j)=s
   enddo
enddo
end subroutine ldlb2j
   
!============================================================================
subroutine ldlb3i(nol,lnf,bnf,                                              &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k,l,m,nola
  real                   :: s
!============================================================================
do j=jts,jte
do k=kts,kte
do i=its,ite
   nola=min(nol,i-its)
   do l=nola,1,-1
      s=lnf(l,i,k,j)
      do m=l+1,nola
         s=s-lnf(m,i,k,j)*bnf(i-m,k,j)*lnf(m-l,i-l,k,j)
      enddo
      lnf(l,i,k,j)=s/bnf(i-l,k,j)
   enddo
   s=bnf(i,k,j)
   do l=1,nola
      s=s-lnf(l,i,k,j)**2*bnf(i-l,k,j)
   enddo
   bnf(i,k,j)=s
enddo
enddo
enddo
end subroutine ldlb3i
   
!============================================================================
subroutine ldlb3j(nol,lnf,bnf,                                              &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k,l,m,nola
  real                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do k=kts,kte
   do i=its,ite
   do l=nola,1,-1
      s=lnf(l,i,k,j)
      do m=l+1,nola
         s=s-lnf(m,i,k,j)*bnf(i,k,j-m)*lnf(m-l,i,k,j-l)
      enddo
      lnf(l,i,k,j)=s/bnf(i,k,j-l)
   enddo
   s=bnf(i,k,j)
   do l=1,nola
      s=s-lnf(l,i,k,j)**2*bnf(i,k,j-l)
   enddo
   bnf(i,k,j)=s
   enddo
   enddo
enddo
end subroutine ldlb3j
   
SUBROUTINE hbnrf1i(a,nol,lnf,bnf,                                           &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 1-dimensional, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,l,nola
!============================================================================
DO i=its+1,ite
   nola=MIN(nol,i-its)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i)*a(i-l)
   ENDDO
ENDDO
DO i=its,ite
   a(i)=bnf(i)*a(i)
ENDDO
DO i=ite-1,its,-1
   nola=MIN(nol,ite-i)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i+l)*a(i+l)
   ENDDO
ENDDO
END SUBROUTINE hbnrf1i

SUBROUTINE hbnrf2i(a,nol,lnf,bnf,                                           &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 2-dimensional, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,l,nola
!============================================================================
DO j=jts,jte
   DO i=its+1,ite
      nola=MIN(nol,i-its)
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i-l,j)
      ENDDO
   ENDDO
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
   DO i=ite-1,its,-1
      nola=MIN(nol,ite-i)
      DO l=1,nol
         a(i,j)=a(i,j)-lnf(l,i+l,j)*a(i+l,j)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2i

SUBROUTINE hbnrf2j(a,nol,lnf,bnf,                                           &
       ids,ide, jds,jde,                                                    &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 2-dimensional, active index j
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i,j-l)
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j+l)*a(i,j+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2j

SUBROUTINE hbnrf3i(a,nol,lnf,bnf,                                           &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 3-dimensional, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts,kte
      DO i=its+1,ite
         nola=MIN(nol,i-its)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i-l,k,j)
         ENDDO
      ENDDO
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
      DO i=ite-1,its,-1
         nola=MIN(nol,ite-i)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i+l,k,j)*a(i+l,k,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3i

SUBROUTINE hbnrf3j(a,nol,lnf,bnf,                                           &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 3-dimensional, active index j
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k,j-l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j+l)*a(i,k,j+l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3j


SUBROUTINE vbnrf1k(a,nol,lnf,bnf,                                           &
       kds,kde,                                                             &
       kms,kme,                                                             &
       kts,kte                                                              )
!============================================================================
! Vertical bounded grid inhomogeneous recursive filter, 
! 1-dimensional, active index k
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: kds,kde
  INTEGER, INTENT(IN   ) :: kms,kme
  INTEGER, INTENT(IN   ) :: kts,kte

  REAL, DIMENSION(kms:kme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(kms:kme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, kms:kme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k)*a(k-l)
   ENDDO
ENDDO
DO k=kts,kte
   a(k)=bnf(k)*a(k)
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k+l)*a(k+l)
   ENDDO
ENDDO
END SUBROUTINE vbnrf1k

SUBROUTINE vbnrf2k(a,nol,lnf,bnf,                                           &
       ids,ide, kds,kde,                                                    &
       ims,ime, kms,kme,                                                    &
       its,ite, kts,kte                                                     )
!============================================================================
! Vertical bounded grid inhomogeneous recursive filter, 
! 2-dimensional, active index k
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k)*a(i,k-l)
      ENDDO
   ENDDO
ENDDO
DO k=kts,kte
   DO i=its,ite
      a(i,k)=bnf(i,k)*a(i,k)
   ENDDO
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k+l)*a(i,k+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf2k

SUBROUTINE vbnrf3k(a,nol,lnf,bnf,                                           &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
! Vertical bounded grid inhomogeneous recursive filter, 
! 3-dimensional, active index k
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts+1,kte
      nola=MIN(nol,k-kts)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k-l,j)
         ENDDO
      ENDDO
   ENDDO
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
   DO k=kte-1,kts,-1
      nola=MIN(nol,kte-k)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k+l,j)*a(i,k+l,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf3k

SUBROUTINE hbncij(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER                :: i,j
!============================================================================
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ids,ide, jds,jde,                    &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ids,ide, jds,jde,                    &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
END SUBROUTINE hbncij

SUBROUTINE hbncji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ids,ide, jds,jde,                                                      &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte

  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, jms:jme),                       &
           INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL, DIMENSION(nol, ims:ime, jms:jme),                  &
           INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER                :: i,j
!============================================================================
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ids,ide, jds,jde,                    &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ids,ide, jds,jde,                    &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!---------------
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
END SUBROUTINE hbncji

SUBROUTINE hbncijk(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k
!============================================================================
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
END SUBROUTINE hbncijk

SUBROUTINE hbnckji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ids,ide, jds,jde, kds,kde,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde 
  INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
           INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL, DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
           INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER                :: i,j,k
!============================================================================
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ids,ide, jds,jde, kds,kde,         &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!---------------
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
END SUBROUTINE hbnckji


!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						********************
!						* hgnrf.f90        *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

!						********************
!						* rfit.f90         *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************

!============================================================================
subroutine rfit(ng,sig,nu, ns,nw,ssig,snu,ins1,wts)  
!============================================================================
! R. J. Purser, NCEP 2001
!----------------------------------------------------------------------------
use vkind
use module_fitcons
implicit none
integer,                      intent(IN   ):: ng
real(vp),dimension(ng),       intent(IN   ):: sig,nu
integer,                      intent(OUT  ):: ns,nw
real(vp),dimension(ng),       intent(OUT  ):: ssig,snu
integer, dimension(ng),       intent(INOUT):: ins1
real(vp),dimension(no,ng),    intent(INOUT):: wts
!----------------------------------------------------------------------------
integer                                    :: i,i1,im,k,l,is,is0,is1,isn
real(vp)                                   :: t
real(vp),dimension(-nohm:ng+noh)           :: dcdg
real(vp),dimension(-noh:ng+noh)            :: cofg,cofs
real(vp),dimension(ng)                     :: dsdg,dhdg,rsnu
real(vp)                                   :: rnu
!============================================================================
nw=0
do i=1,ng
   dcdg(i)=1./sig(i)
   if(sig(i) <= sigb)then
!----------------------------------------------------------------------------
! sig(i) below threshold; cleave to original grid spacing with ds/dg and 
! dh/dg set accordingly:
!----------------------------------------------------------------------------
      dsdg(i)=1.     ;      dhdg(i)=0.
   else
!----------------------------------------------------------------------------
! sig(i) exceeds basic threshold sigb, allowing working grid with coordinate
! s to differ from original grid with coordinate g. The formula for ds/dg
! is now <1 but tends smoothly to 1 again at the threshold value, sig=sigb.
! [The function for log(ds/dg) is based on the "hyper-hyperbola":
!    y= (1+x**4)**(-4)-1, which rises very gradually from its base at x=y=0]
! Likewise, the perturbative component, dh/dg, is now < 0, but tends
! smoothly to 0 again at the threshold.
!----------------------------------------------------------------------------
      t=ldsig-sqrt(sqrt(ldsig4+(ldsig-log(sigc*dcdg(i)))**4))
      dsdg(i)=exp(t) ;      dhdg(i)=dsdg(i)*t
   endif
enddo

!----------------------------------------------------------------------------
! Apply mirror-symmetry to extrapolate beyond ends:
!----------------------------------------------------------------------------
do l=1,noh
   dcdg(1-l)=dcdg(l); dcdg(ng+l)=dcdg(ng+1-l)
enddo

!----------------------------------------------------------------------------
! Integrate dc/dg wrt g to get c(g) at each of the points of the g-grid
! which is NOT staggered relative to the boundary
!----------------------------------------------------------------------------
cofg(0)=0.
do i=1,ng
   cofg(i)=cofg(i-1)+dot_product(qco,dcdg(i-noh:i+noh))
enddo
do l=1,noh
   cofg(  -l)=-cofg(   l)
   cofg(ng+l)=-cofg(ng-l)+2*cofg(ng)
enddo

im=0
ns=0
!----------------------------------------------------------------------------
! loop over noncontiguous segments where it is numerically beneficial
! to employ a grid of relatively coarse resolution. The adoption of each
! alternative grid segment is subject to some conditions:
! 1) Each coarse-grid segment must span at least 5 points of the original grid
! 2) Each segment must shorten the tally of working grid points by at least 3.
!     Subject to the above conditions, the coarse grid is blended smoothly
! with the original grid at internal thresholds and is designed to provide
! a resolution such that the smoothing scale, sigma, never exceeds the 
! product, sigc*dg/ds, where sigc is a dimensionless parameter (e.g. sigc=3.)
! and dg/ds is the local working grid (s) spacing in units of the original
! grid (g) spacing. 
!
! Each segment found is defined by its end points in the original grid,
! i1 and im. k is the counter for segments along this line.
! ns keeps count of the number of working grid (s-grid) points found so far.
!----------------------------------------------------------------------------
cofs(0)=0.
do k=1,ng 
   do i1=im+1,ng
      if(i1< ng-3 .and. dhdg(i1) /= 0)exit
!----------------------------------------------------------------------------
! working s-grid continues to track the original g-grid; Set indices and 
! weight for the trivial "interpolation" between these coincident grids:
!----------------------------------------------------------------------------
      ns=ns+1
      ins1(i1)=-ns
      cofs(ns)=cofg(i1)
   enddo
   if(i1 > ng)exit
!----------------------------------------------------------------------------
! Having met the basic conditions for the start of a new segment in which
! the s-grid and g-grids may part company, seek the other end, im, of this
! possible segment:
!----------------------------------------------------------------------------
   do im=i1+1,ng
      if(dhdg(im) == 0)exit
   enddo
   im=im-1
   if(im < i1+4)then
!----------------------------------------------------------------------------
! Segment too short to be viable; keep s-grid and g-grids synchronized:
!----------------------------------------------------------------------------
      do i=i1,im
         ns=ns+1
         ins1(i)=-ns
         cofs(ns)=cofg(i)
      enddo
   else
!----------------------------------------------------------------------------
! Segment long enough to be potentially viable. Call jfit to determine if 
! the final condition is met, namely that the number of s-grid points 
! in this segment is smaller than the g-grid tally by at least 3. If so,
! Fit an exact integer number of s-points into this segment and compute
! the indices and weights for the associated nontrivial interpolation
! from these (and neighboring) s-points back to the g-points of the segment:
!----------------------------------------------------------------------------
      call jfit(ng,i1,im,ns,nw,cofg,dsdg,dhdg,cofs,ins1,wts)
   endif
enddo
if(ns<no .and. nw>0)then ! <- s-grid too short; use copy of g-grid instead
   wts(:,1:nw)=0
   nw=0
   do i=1,ng
      ins1(i)=-i
      cofs(i)=cofg(i)
   enddo
   ns=ng
endif

do l=1,noh
   cofs(  -l)=-cofs(   l)
   cofs(ns+l)=-cofs(ns-l)+2*cofs(ns)
enddo
do is=1,ns
   ssig(is)=1./dot_product(dco,cofs(is-nohp:is+noh))
enddo

!----------------------------------------------------------------------------
! By applying adjoint-interpolation to the g-grid metric terms, obtain
! the corresponding metric terms for the new s-grid:
!----------------------------------------------------------------------------
call stogt(ns,ng,ins1,wts, snu,nu)

end subroutine rfit

!============================================================================
subroutine jfit(ng,ig1,igm,ns,iw,cofg,dsdg,dhdg,cofs,ins1,wts)
!============================================================================
! R. J. Purser, NCEP 2001
!----------------------------------------------------------------------------
use vkind
use module_fitcons
implicit none
integer,                         intent(IN   ):: ng,ig1,igm
integer,                         intent(INOUT):: ns,iw
real(vp),dimension(ng),          intent(IN   ):: dsdg,dhdg
real(vp),dimension(-noh:ng+noh), intent(IN   ):: cofg
real(vp),dimension(-noh:ng+noh), intent(INOUT):: cofs
integer, dimension(ng),          intent(INOUT):: ins1
real(vp),dimension(no,ng),       intent(INOUT):: wts
!----------------------------------------------------------------------------
real(vp),dimension(-noh:ng+noh) :: sofg,dsdgt
real(vp)                        :: et,estar,destar,r,dr,sm,hm
integer                         :: i,l,ie,iep,ie0,ie1,ien,ig0,is0,ism,init
!============================================================================

!----------------------------------------------------------------------------
! Form the definite integral sm, of ds/dg, within this segment:
!----------------------------------------------------------------------------
sm=sum(dsdg(ig1:igm)) 

!---------------------------------------------------------------------------
! Test whether it is worthwhile to allow s-grid to deviate from the original
! g-grid within this segment on the basis of the number of grid points that
! could potentially be eliminated (we require a saving > 3 per segment):
!---------------------------------------------------------------------------
if(sm > igm-ig1-2)then
!----------------------------------------------------------------------------
! This putative s-grid segment reduces the total number of grid points by an
! insufficient amount to justify the additional interpolations. Therefore,
! keep the original g-grid instead for this segment, and return:
!---------------------------------------------------------------------------
   do i=ig1,igm
      ns=ns+1
      ins1(i)=-ns
      cofs(ns)=cofg(i)
   enddo
   return
endif
!----------------------------------------------------------------------------
! s-grid segment achieves a worthwhile reduction of the number of points
! of the working grid. The tasks of the rest of this routine are to:
! (1) adjust the segment length in the s-metric to make it an integer;
! (2) find the s-coordinates of each g-grid points in this segment
!     and hence the nontrivial interpolation indices and weights required 
!     to go from the s-grid to the g-grid (or adjoints going the other way);
! (3) use Newton iterations to find the accurate interpolation formulae
!     that enable c(s) to be interpolated from the given c(g).
!----------------------------------------------------------------------------
ig0=ig1-1
is0=ns; ism=sm
!----------------------------------------------------------------------------
! Fractional remainder of sm, divided by the definite integral of dh/dg
! provides the adjustment factor that scales the perturbative component,
! dhdg, by exactly the amount that will make the segment integral of the 
! perturbed grid-to-grid jacobian, dsdgt, the exact integer, ism:
!----------------------------------------------------------------------------
r=(sm-ism)/sum(dhdg(ig1:igm))
do i=ig1,igm
   dsdgt(i)=dsdg(i)-r*dhdg(i)
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate adjusted ds/dg as an even-symmetry function at the 
! ends of this segment. Note that the grid on which derivatives such as
! ds/dg reside is the one staggered wrt domain boundaries and segment
! end points. The indices of this grid go from ig1 to igm inside the
! segment. (The convention for the companion grid, NOT staggered wrt 
! boundaries, is such that the two segment ends are denoted by indices,
! ig0=ig1-1 and igm.)
!----------------------------------------------------------------------------
do l=1,noh
   dsdgt(ig1-l)=dsdgt(ig0  +l)
   dsdgt(igm+l)=dsdgt(igm+1-l)
enddo
ism=is0+ism ! This integer also becomes (within round-off) the value, sofg(igm)
!----------------------------------------------------------------------------
! Set s(g) at both ends of the segment to be the appropriate integers:
!----------------------------------------------------------------------------
sofg(ig0)=is0; sofg(igm)=ism
!----------------------------------------------------------------------------
! Get s(g) inside the segment by performing a numerical quadrature of dsdgt:
!----------------------------------------------------------------------------
do i=ig1,igm
   sofg(i)=sofg(i-1)+dot_product(qco,dsdgt(i-noh:i+noh))
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate s(g) as an odd-symmetry function at segment end points.
! Note that, being an inegral, s(g) resides on the grid NOT staggered wrt
! boundaries and segment end points.
!----------------------------------------------------------------------------
do l=1,noh
   sofg(ig0-l)=2*is0-sofg(ig0+l)
   sofg(igm+l)=2*ism-sofg(igm-l)
enddo
do i=ig1,igm
   iw=iw+1 ; wts(:,iw)=0
   r=dot_product(ico,sofg(i-nohp:i+noh))+.5_vp
   ie=r            ! Take integer part...
   ins1(i)=ie-nohm ! ...hence the index of the first point in the stencil...
   r=r-ie          ! ...then the fractional part to find interpolation weights:
   call lagw(hunit1,r,q1,wt1,dwt1,nom)   ! weights for left-biased stencil
   wts(:nom,iw) =              (1-r)*wt1 !   bias weight, 1-r
   call lagw(hunit2,r,q1,wt1,dwt1,nom)   ! weights for right-biased stencil
   wts(2:   ,iw) = wts(2:   ,iw)  +r*wt1 !   bias weight, r.
!----------------------------------------------------------------------------
! Exploit the mirror symmetries to confine the weight stencil to the 
! domain interior, even though this may entail padding innermost end of
! the stencil with useless zeroes:
!----------------------------------------------------------------------------
   L=1-INS1(I)
   IF(L > 0)THEN ! FOLD LEFT OVERLAP OF L ELEMENTS BACK INSIDE: 
      WTS(1:L,IW)      =WTS(L:1:-1,IW)+WTS(L+1:L*2,IW) ! FOLD INTO 1ST L
      WTS(L+1:NO-L,IW) =WTS(L*2+1:NO,IW)               ! SHIFT THE REST LEFT
      WTS(NOP-L:NO,IW)=0 ! SET TRAILING L ELEMENTS TO ZERO
      INS1(I)=1          ! RESET INDEX OF FIRST POINT OF STENCIL
   ENDIF
   l=ins1(i)+nom-ism
   if(l > 0)then ! Fold right overlap of L elements back inside:
      wts(nop-l:no,iw)=wts(no:nop-l:-1,iw)+wts(nop-l*2:no-l,iw) ! Fold last L
      wts(l+1:no-l,iw)=wts(1:no-l*2,iw)                         ! Shift right
      wts(1:l,iw)=0      ! Set first L elements to zero
      ins1(i)=ism-nom    ! reset index of first point of stencil
   endif
enddo
ns=ism

!----------------------------------------------------------------------------
! Use Newton-Raphson iterations to locate the g-coordinates of all this
! segment's s-grid points. Then interpolate the function c to each of
! these s-grid points. (Note that, in the present context, the
! s- and g-grids are the ones NOT staggered wrt the domain boundaries.)
!----------------------------------------------------------------------------
ie=ig0
do i=is0+1,ism-1 ! Loop over s-grid target points interior to this segment
   et=i
!----------------------------------------------------------------------------
! Find the g-grid interval containing this target: 
!----------------------------------------------------------------------------
   do iep=ie+1,igm-1;  if(sofg(iep) > et)exit; enddo
   do ie=iep-1,ig1,-1; if(sofg(ie) <= et)exit; enddo

   ie1=ie-nohm;   ien=ie+noh   ! <-- Set the interpolation stencil range:

   r=(et-sofg(ie))/(sofg(ie+1)-sofg(ie)) ! Linearly estimate interval fraction

!----------------------------------------------------------------------------
! Perform Newton-Raphson iterations to refine interval fraction, r:
!----------------------------------------------------------------------------
   do init=1,nnit
      call lagw(hunit,r,q,wt,dwt,no) ! Get Lagrange weights, wt and d(wt)/dg
      estar =dot_product(wt, sofg(ie1:ien))-et ! <- Residual error, estar.
      destar=dot_product(dwt,sofg(ie1:ien))    ! <- d(estar)/dg.
      dr=-estar/destar                         ! <- Newton correction to r
      r=r+dr                                   ! <- Refined estimate, r
      if(abs(dr) <= rcrit)goto 1               ! <- Converged enough yet?
   enddo
   stop 'Too many Newton iterations'           ! <- It never convergenced! 
1  wt=wt+dr*dwt                                ! <- Final refinement to wt
   cofs(i)=dot_product(wt, cofg(ie1:ien))      ! <- Interpolate c(s)
enddo
cofs(ism)=cofg(igm)                            ! <- End value directly
end subroutine jfit

!============================================================================
subroutine stog(ns,ng,ins1,wts, as,ag) 
!============================================================================
! R. J. Purser NCEP 2001
! Forward interpolation from s-grid to g-grid
! --> ns,ng: sizes of s and g grids
! --> ins1 : array of 1st stencil indices (s-grid) for each target (g) point.
! --> wts  : interpolation weights for each target (g-grid point).
! --> as   : s-grid array of source data.
! <-- ag   : g-grid array of interpolated target data.
!============================================================================
use vkind
implicit none
integer, parameter                      :: noh=3,no=noh*2,nom=no-1
integer,                  intent(IN   ) :: ns,ng
integer, dimension(ng),   intent(IN   ) :: ins1
real(vp),dimension(no,ng),intent(IN   ) :: wts
real(vp),dimension(ns),   intent(IN   ) :: as
real(vp),dimension(ng),   intent(OUT  ) :: ag
!----------------------------------------------------------------------------
integer                                 :: i,is,iw
!============================================================================
iw=0
ag=0
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      ag(i)=dot_product(wts(:,iw),as(is:is+nom))
   else
      ag(i)=as(-is)
   endif
enddo
end subroutine stog

!============================================================================
subroutine stogt(ns,ng,ins1,wts, as,ag) 
!============================================================================
! Perform the transpose of the operation defined by stog
! R. J. Purser NCEP 2001
!============================================================================
use vkind
implicit none
integer, parameter                      :: noh=3,no=noh*2,nom=no-1
integer,                  intent(IN   ) :: ns,ng
integer, dimension(ng),   intent(IN   ) :: ins1
real(vp),dimension(no,ng),intent(IN   ) :: wts
real(vp),dimension(ns),   intent(OUT  ) :: as
real(vp),dimension(ng),   intent(IN   ) :: ag
!----------------------------------------------------------------------------
integer                                 :: i,is,iw
!============================================================================
iw=0
as=0
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      as(is:is+nom)=as(is:is+nom)+wts(:,iw)*ag(i)
   else
      as(-is)=as(-is)+ag(i)
   endif
enddo
end subroutine stogt

!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND
!						********************
!						* rfit.f90         *
!						* PURSER 1994/1999 *
!                                               *   FUJITA 1999    *
!						********************
!ENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDENDEND

SUBROUTINE ad_raf(g,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  2nd half of recursive anisotropic self-adjoint filter (full-strings version)

  IMPLICIT NONE

  include 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(14)            ! structure defining recursive filter

  real(4) work(min(ims,jms,kms):max(ime,jme,kme))

  integer(4) i,icolor,icolor2,ierr,im,ip,ipass,ipep1,ipsm1,ismooth,j,jm
  integer(4) jp,jpass,jpep1,jpsm1,k,km,kp,kpep1,kpsm1
  integer(4) im3,ip3,ipep3,ipsm3,jm3,jp3,jpep3,jpsm3,km3,kp3,kpep3,kpsm3


  if(filter(1)%npass.gt.0) then
   do ipass=filter(1)%npass,1,-1
    jpass=min(ipass,filter(1)%mpass)
    do icolor2=8,14

     icolor=icolor2
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color_loc(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
     icolor=icolor2-7
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!      following barrier is required because there is no communication for icolor>=8--the call
!          to one_color_loc, and all work must end for a color, before moving to the next one

                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
     call mpi_barrier(my_comm,ierr)     !!!! DO NOT REMOVE THIS BARRIER !!!!!
                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
    end do

   end do
  end if

!      apply 1-2-1 smoother in each direction

  if(filter(1)%nsmooth.gt.0) then
   ipsm1=max(ids,ims,ips-1) ; ipep1=min(ide,ime,ipe+1)
   jpsm1=max(jds,jms,jps-1) ; jpep1=min(jde,jme,jpe+1)
   kpsm1=max(kds,kms,kps-1) ; kpep1=min(kde,kme,kpe+1)
   do ismooth=1,filter(1)%nsmooth
    call refresh_halo3x(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do j=jps,jpe
      work(ipsm1:ipep1)=g(ipsm1:ipep1,j,k)
      do i=ips,ipe
       ip=min(i+1,ipep1) ; im=max(ipsm1,i-1)
       g(i,j,k)=.25*(work(ip)+work(im))+.5*work(i)
      end do
     end do
    end do
    call refresh_halo3y(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do i=ips,ipe
      work(jpsm1:jpep1)=g(i,jpsm1:jpep1,k)
      do j=jps,jpe
       jp=min(j+1,jpep1) ; jm=max(jpsm1,j-1)
       g(i,j,k)=.25*(work(jp)+work(jm))+.5*work(j)
      end do
     end do
    end do
    do j=jps,jpe
     do i=ips,ipe
      work(kpsm1:kpep1)=g(i,j,kpsm1:kpep1)
      do k=kps,kpe
       kp=min(k+1,kpep1) ; km=max(kpsm1,k-1)
       g(i,j,k)=.25*(work(kp)+work(km))+.5*work(k)
      end do
     end do
    end do

   end do
  end if

!      and/or apply Shapiro smoother in each direction (2nd moment preserving)

  if(filter(1)%nsmooth_shapiro.gt.0) then
   ipsm3=max(ids,ims,ips-3) ; ipep3=min(ide,ime,ipe+3)
   jpsm3=max(jds,jms,jps-3) ; jpep3=min(jde,jme,jpe+3)
   kpsm3=max(kds,kms,kps-3) ; kpep3=min(kde,kme,kpe+3)
   do ismooth=1,filter(1)%nsmooth_shapiro
    call refresh_halo3x(g,3, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do j=jps,jpe
      work(ipsm3:ipep3)=g(ipsm3:ipep3,j,k)
      do i=ips,ipe
       ip=min(i+1,ipep3) ; im=max(ipsm3,i-1)
       ip3=min(i+3,ipep3) ; im3=max(ipsm3,i-3)
       g(i,j,k)=.28125*(work(ip)+work(im))+.5*work(i)-.03125*(work(ip3)+work(im3))
      end do
     end do
    end do
    call refresh_halo3y(g,3, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do i=ips,ipe
      work(jpsm3:jpep3)=g(i,jpsm3:jpep3,k)
      do j=jps,jpe
       jp=min(j+1,jpep3) ; jm=max(jpsm3,j-1)
       jp3=min(j+3,jpep3) ; jm3=max(jpsm3,j-3)
       g(i,j,k)=.28125*(work(jp)+work(jm))+.5*work(j)-.03125*(work(jp3)+work(jm3))
      end do
     end do
    end do
    do j=jps,jpe
     do i=ips,ipe
      work(kpsm3:kpep3)=g(i,j,kpsm3:kpep3)
      do k=kps,kpe
       kp=min(k+1,kpep3) ; km=max(kpsm3,k-1)
       kp3=min(k+3,kpep3) ; km3=max(kpsm3,k-3)
       g(i,j,k)=.28125*(work(kp)+work(km))+.5*work(k)-.03125*(work(kp3)+work(km3))
      end do
     end do
    end do

   end do
  end if


return
end subroutine ad_raf
subroutine alpha_beta(info_string,aspect_full,xyzvol_full, &
                     lensstr,ins1,wts,rsnui,lnf,bnf, &
                     istart_out,npoints_mype,binomial,npass,mpass,no_interp,int_ord,ifilt_ord, &
                  lenbar,lenmax,lenmin,npoints1,ratio_lens_min,oldf, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  !   compute recursion constants alpha and beta along unbroken strings

  IMPLICIT NONE

  logical oldf
  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: npoints_mype,npass,mpass,no_interp,int_ord,ifilt_ord

  REAL(8), DIMENSION( 20, 20 ), INTENT(IN) :: &
            binomial

  INTEGER(2), DIMENSION( 7, npoints_mype ), INTENT(IN) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7-- jumpx,jumpy,jumpz for this string
  REAL(4), DIMENSION( npoints_mype ) , INTENT(IN) :: &
            aspect_full,xyzvol_full
  integer(4) ins1(npoints_mype,mpass)
  real(4) wts(int_ord,npoints_mype,mpass),rsnui(npoints_mype,mpass)
  real(4) lnf(ifilt_ord,npoints_mype,mpass),bnf(npoints_mype,mpass)
  integer(4) istart_out(*),lensstr(mpass,*)
  integer(4)  lenmax,lenmin,npoints1 !  diagnostic output--to look at string
  real(4) lenbar,ratio_lens_min

  integer(4) i,iend,ierr,ipass,istart,j,jm,jp,jpass,jstart
  integer(4) mstrings,nstrings
  real(4) this_ratio

  nstrings=0

  istart=1
  if(npoints_mype.gt.1) then
   do i=2,npoints_mype
    if(info_string(1,i).ne.info_string(1,i-1)+1.or. &
           info_string(2,i).ne.info_string(2,i-1).or. &
               info_string(3,i).ne.info_string(3,i-1).or. &
                   info_string(4,i).ne.info_string(4,i-1).or. &
                       info_string(5,i).ne.info_string(5,i-1).or. &
                           info_string(6,i).ne.info_string(6,i-1).or. &
                               info_string(7,i).ne.info_string(7,i-1)) then
     iend=i-1
          lenbar=lenbar+(iend-istart+1)
          lenmax=max(iend-istart+1,lenmax)
          lenmin=min(iend-istart+1,lenmin)
          nstrings=nstrings+1
          if(iend.eq.istart) npoints1=npoints1+1
     istart_out(nstrings)=istart
     istart_out(nstrings+1)=iend+1

     do ipass=1,mpass
          if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
           mstrings=nstrings
           jstart=istart
           jpass=ipass
          else
           mstrings=1
           jstart=1
           jpass=1
          end if
      call alpha_betaa(aspect_full(istart),iend-istart+1,no_interp,binomial(ipass,npass), &
                       lensstr(jpass,mstrings),ins1(jstart,jpass),wts(1,jstart,jpass),xyzvol_full(istart), &
                rsnui(istart,ipass),lnf(1,istart,ipass),bnf(istart,ipass),int_ord,ifilt_ord,nstrings,oldf)
      if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
       this_ratio=float(lensstr(ipass,nstrings))/float(iend-istart+1)
      else
       this_ratio=1.
      end if
      ratio_lens_min=min(this_ratio,ratio_lens_min)
     end do
     
     istart=iend+1
    end if
   end do
  end if
  iend=npoints_mype
          lenbar=lenbar+(iend-istart+1)
          lenmax=max(iend-istart+1,lenmax)
          lenmin=min(iend-istart+1,lenmin)
          nstrings=nstrings+1
          if(iend.eq.istart) npoints1=npoints1+1
  istart_out(nstrings)=istart
  istart_out(nstrings+1)=iend+1
  do ipass=1,mpass
          if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
           mstrings=nstrings
           jstart=istart
           jpass=ipass
          else
           mstrings=1
           jstart=1
           jpass=1
          end if
   call alpha_betaa(aspect_full(istart),iend-istart+1,no_interp,binomial(ipass,npass), &
                  lensstr(jpass,mstrings),ins1(jstart,jpass),wts(1,jstart,jpass),xyzvol_full(istart), &
                  rsnui(istart,ipass),lnf(1,istart,ipass),bnf(istart,ipass),int_ord,ifilt_ord,nstrings,oldf)
   if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
    this_ratio=float(lensstr(ipass,nstrings))/float(iend-istart+1)
   else
    this_ratio=1.
   end if
   ratio_lens_min=min(this_ratio,ratio_lens_min)
  end do

return
end subroutine alpha_beta

subroutine alpha_betaa(aspect,ng,no_interp,binomial,ns,ins1,wts,nu,rsnui,lnf,bnf,int_ord,m,nstrings,oldf)

  !  compute various constants for Purser 1-d high-order filter

  !   --> aspect:   correlation scale (squared, i think), grid units
  !   --> ng:       length of string
  !   --> binomial: weighting factors (perhaps not needed with high-order filter)
  !  <--  ns:       computational string length
  !  <--  ins1:     interpolation indices
  !  <--  wts:      interpolation weights
  !   --> nu:       physical grid volume
  !  <--  rsnui:    1/sqrt(computational grid length)
  !  <--  lnf,bnf:  filter parameters
  !   --> int_ord:  interpolation order
  !   --> m:        filter order
  !   

  use vkind
  use module_fitcons
  IMPLICIT NONE

  logical oldf
  INTEGER(4), INTENT(IN) :: ng,no_interp,int_ord,m
  integer(4), intent(out):: ns
         integer(4) nstrings

  REAL(8), INTENT(IN) :: binomial

  REAL(4), DIMENSION( ng ), INTENT(IN) :: aspect
  integer(4) ins1(ng)
  real(4) wts(int_ord,ng),nu(ng),rsnui(ng)
  real(4) lnf(m,ng),bnf(ng)

  real(4) sig(ng),ssig(ng),snu(ng)
  integer(4) nss,nw
  
  sig(1:ng)=sqrt(aspect(1:ng)*binomial)
  if(ng.le.no_interp) then
   nss=ng
   ssig(1:ng)=sig(1:ng)
   snu(1:ng)=nu(1:ng)
  else
   call rfit(ng,sig,nu,ns,nw,ssig,snu,ins1,wts)
   nss=ns
  end if
                if(minval(ssig(1:nss)).lt.0.) then
                  write(0,*)' WARNING, SSIG<0 ENCOUNTERED,ng,ns,ivalmin,valmin=', &
                          ng,nss,minloc(ssig(1:nss)), minval(ssig(1:nss))
!                 write(0,*)'  sig follows: ',sig(1:ng)
!                 write(0,*)'  ssig follows: ',ssig(1:nss)
                  nss=ng
                  ssig(1:ng)=sig(1:ng)
                  snu(1:ng)=nu(1:ng)
                end if
  if(oldf) then
   call coefrf_out(aspect(1:nss),binomial,nss,bnf(1:nss),lnf(:,1:nss))
  else
   call coefrf(ssig(1:nss),snu(1:nss),nss,m,bnf(1:nss),lnf(:,1:nss))
  end if
  rsnui(1:nss)=1./sqrt(snu(1:nss))

return
end subroutine alpha_betaa
subroutine count_strings(info_string,nstrings,npoints_mype)

  !   compute recursion constants alpha and beta along unbroken strings

  IMPLICIT NONE

  INTEGER(4), INTENT(IN) :: npoints_mype

  INTEGER(2), DIMENSION( 7, npoints_mype ), INTENT(IN) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7-- jumpx,jumpy,jumpz for this string
  integer(4) nstrings

  integer(4) i,iend,istart

  nstrings=0

  istart=1
  if(npoints_mype.gt.1) then
   do i=2,npoints_mype
    if(info_string(1,i).ne.info_string(1,i-1)+1.or. &
           info_string(2,i).ne.info_string(2,i-1).or. &
               info_string(3,i).ne.info_string(3,i-1).or. &
                   info_string(4,i).ne.info_string(4,i-1).or. &
                       info_string(5,i).ne.info_string(5,i-1).or. &
                           info_string(6,i).ne.info_string(6,i-1).or. &
                               info_string(7,i).ne.info_string(7,i-1)) then
     iend=i-1
     nstrings=nstrings+1
     istart=iend+1
    end if
   end do
   iend=npoints_mype
   nstrings=nstrings+1
  end if

return
end subroutine count_strings
subroutine gather_grid(f,g, &
                     ifs,ife,jfs,jfe, &                   !  indices of input array f
                     ids, ide, jds, jde, &                     ! domain indices
                     ips, ipe, jps, jpe, &                     ! patch indices
                     ims, ime, jms, jme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  ! obtain filtering constants for 2-d recursive anisotropic filter

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, &   ! domain indices
                            ips, ipe, jps, jpe, &   ! patch indices
                            ims, ime, jms, jme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: ifs,ife,jfs,jfe

  REAL(4), INTENT(IN) :: f(ifs:ife,jfs:jfe)
  REAL(4), INTENT(OUT) :: g(ids:ide,jds:jde)

  real(4),allocatable::sendbuf(:,:)
  real(4),allocatable::recvbuf(:,:)
  integer(4) isendbounds(4),irecvbounds(4)
  integer(4) istat1(mpi_status_size)
  integer(4) istat2(mpi_status_size)

  integer(4) i,irecv1,irecv2,isend1,isend2,j,mpe,nrecv,nsend


  allocate(sendbuf(ips:ipe,jps:jpe))
  if(mype.ne.0) then
   isendbounds(1)=ips
   isendbounds(2)=ipe
   isendbounds(3)=jps
   isendbounds(4)=jpe
   call mpi_send(isendbounds,4,mpi_integer4,0,npes+mype,my_comm,isend1)
       if(isend1.ne.0) then
         print *,' mype,ierr for mpi_send in gather_grid=',mype,isend1
         call mpi_finalize(isend1)
         stop
       end if
   nsend=(isendbounds(2)-isendbounds(1)+1)*(isendbounds(4)-isendbounds(3)+1)
   do j=jps,jpe
    do i=ips,ipe
     sendbuf(i,j)=f(i,j)
    end do
   end do
   call mpi_send(sendbuf,nsend,mpi_real4,0,mype,my_comm,isend2)
       if(isend2.ne.0) then
         print *,' mype,ierr for mpi_send in gather_grid=',mype,isend2
         call mpi_finalize(isend2)
         stop
       end if

  else

   do j=jps,jpe
    do i=ips,ipe
     g(i,j)=f(i,j)
    end do
   end do

   do mpe=1,npes-1
    call mpi_recv(irecvbounds,4,mpi_integer4,mpe,npes+mpe,my_comm,istat1,irecv1)
       if(irecv1.ne.0) then
         print *,' mype,ierr for mpi_recv in gather_grid=',mype,irecv1
         call mpi_finalize(irecv1)
         stop
       end if
    nrecv=(irecvbounds(2)-irecvbounds(1)+1)*(irecvbounds(4)-irecvbounds(3)+1)
    allocate(recvbuf(irecvbounds(1):irecvbounds(2),irecvbounds(3):irecvbounds(4)))
    call mpi_recv(recvbuf,nrecv,mpi_real4,mpe,mpe,my_comm,istat2,irecv2)
       if(irecv2.ne.0) then
         print *,' mype,ierr for mpi_recv in gather_grid=',mype,irecv2
         call mpi_finalize(irecv2)
         stop
       end if
    do j=irecvbounds(3),irecvbounds(4)
     do i=irecvbounds(1),irecvbounds(2)
      g(i,j)=recvbuf(i,j)
     end do
    end do
    deallocate(recvbuf)
   end do
  end if
  deallocate(sendbuf)

return
end subroutine gather_grid
!------------------------------------------------------------------------------
!                             SUBROUTINE GETHEX                  Purser 1997
! PURPOSE:
!    Apply implicit lattice transformations until the target tensor UTARGET
!  can be represented as a positive combination of the components of a
! "canonical hexad", with coefficients which may each be interpreted as the
! grid-unit "spread" component in the associated generalized grid direction.
! The target tensor is given in basic grid units. If "LBASIS" is
! a triplet of integer 3-vectors, collectively of determinant = +4, and
! with the all Nth component of these vectors either odd or even, the convex
! hull of the six vectors, {LBASIS and -LBASIS}, forms an octahedron. The
! midpoints of the 12 edges form the 2 diametricaly opposite pairs of sets,
! each consisting of 6 distinct integer vectors, LHEXAD, which are the
! hexad of generalized grid steps along which the application of appropriately
! weighted smoothers will result in the target spread. The requisite weights,
! WHEXAD, are the spreads in the individual hexad directions when expressed
! in the natural grid-units of each of these 6 directions. These weights are
! such that the matrix LU multiplied by WHEXAD gives UTARGET, where the Jth
! column of LU is 6-vector representation of the degenerate tensor describing
! a spread in the direction of hexad-J of one unit of the grid spacing in this
! direction. matrix LUI is the transpose of the inverse of LU.
!
! HOW IT WORKS:
!   For the given target tensor, the hexad and weights are defined iteratively.
! A valid "guess" for LHEXAD, LU, LUI, must first be provided. If these are
! not provided by the user, just set the flag LGUESS to 0, and the routine
! will provide feasible default values; otherwise set LGUESS > 0.
!      First let us suppose the given hexad and accompanying LU, LUI are valid.
! Then we may compute the corresponding weights:
!                WHEXAD = (LUI-transpose)*UTARGET
! If the hexad really IS valid, these weights will all be non-negative and
! the task is done. But if some weight is negative, then the hexad is invalid
! and an alternative valid hexad must be sought. In this situation, the
! algorithm first determines the MOST negative weight and its associated
! grid direction and, keeping the other five directions the same, replaces
! the offending one with the unique (up to sign change) alternative such
! that the NEW hexad are also the midpoints of some grid-octahedron formed from
! an integer-vector basis of determinant = 4 and of the required form.
! To preserve algorithmic symmetry, the enumeration of the new grid directions
! undergoes a permutation. Since only one column of LU changes, the work needed
! to update LUI is less that the work needed to compute the new inverse from
! scratch (cf the "simplex method" of linear programming). We can also exploit
! the special property of this problem that the determinant of LU changes from
! +1 to -1 when the offending column is replaced by its alternative. The
! change in the enumeration of the vectors LHEXAD and of the columns of LU and
! LUI can be regarded as a way to preserve the pattern of implied geometrical
! relationships among these vectors, so that the algorithm is relatively simple.
!    By repeating this step, the algorithm eventually homes in on the
! uniquely valid hexad.
!
!   The "cuboctahedron" associated with the default guess basis is shown in 3
!   orthogonal views below, hexad vector shown thus, (); basis vectors, [].
!
!
!      (6)--------(3)
!       |          |  \         (In each view, nearest facet is speckled)
!       |   [1]    |    \[3]
!       |          |      \          <===== top view
!       |          |        \
!      (-4)-------(5)--------(1)                    south view
!         \::::::::|          |                    //
!           \::::::|          |                   //
!         [-3]\::::|     [2]  |                  //         east view
!               \::|          |                 //              ||
!                 (2)--------(-6)              //               ||
!                                             //                \/
!      (-4)------ (2)                        //      (2)------- (5)
!       |          |::\                     //        |          |  \
!       |          |::::\[2]               //         |          |    \
!       |          |::::::\         <=====//          |          |      \
!       |          |::::::::\                         |          |        \
!      (-1)------(-3)--------(-6)                   (-6)--------(1)--------(3)
!         \        |          |                         \::::::::|          |
!           \      |          |                           \::::::|          |
!             \    |          |                             \::::|          |
!               \  |          |                               \::|          |
!                 (-5)-------(4)                                (4)-------(-2)
!
!
!
!
!
! --> UTARGET: 6-vector comprising components of the target aspect tensor
! --> LGUESS:  Code to tell whether input LHEXAD are feasible (LGUESS.NE.0)
! <-> LHEXAD:  6 integer basis vectors giving the canonical grid steps
! <-> LUI:     6 6-vectors dual to those of LU: [LUI]^t*[LU]=[I]
! <-- WHEXAD:  6 real "spread" components in generalized grid-step units
! <-- KT:      The number of iterations it required to find the valid hexad
!------------------------------------------------------------------------------
      SUBROUTINE GETHEX(UTARGET,LGUESS,LHEXAD,LUI,WHEXAD,KT)
!     IMPLICIT DOUBLE PRECISION(A-H,O-Z)
          implicit real(8) (a-h,o-z)
      DIMENSION UTARGET(6),LHEXAD(3,6),LUI(6,6),WHEXAD(6) &
      ,IHEXAD(3,6),ILUI(6,6) &    ! defaults
      ,NEWLHEX(3,2:6),NEWLUI(6,2:6),LUI1(6),WT(6) &
      ,KP(6,6),KSG(6,6)
      DATA KP /1,2,6,3,4,5, 2,1,4,6,5,3 & ! Permutation code.
              ,3,4,2,5,6,1, 4,3,6,2,1,5 & ! This line is previous + 2 (modulo 6)
              ,5,6,4,1,2,3, 6,5,2,4,3,1/  ! This line is previous + 2 (modulo 6)
      DATA KSG/1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
              ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
              ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1/
      DATA IHEXAD/ 1, 0, 0,  0,-1, 1 &
                 , 0, 1, 0,  1, 0,-1 &
                 , 0, 0, 1, -1, 1, 0/

      DATA ILUI/1, 0, 0,  0, 1, 1,    0, 0, 0, -1, 0, 0 &
               ,0, 1, 0,  1, 0, 1,    0, 0, 0,  0,-1, 0 &
               ,0, 0, 1,  1, 1, 0,    0, 0, 0,  0, 0,-1/
!     DATA BCMINS/-1.D-14/  !  a criterion slightly < 0 avoids roundoff worries

  bcmins=-epsilon(utarget)

      IF(LGUESS.EQ.0)THEN
       DO J=1,6
        DO I=1,3
         LHEXAD(I,J)=IHEXAD(I,J)
        ENDDO
        DO I=1,6
         LUI(I,J)=ILUI(I,J)
        ENDDO
       ENDDO
      ENDIF

! Use initial estimate of hexad to compute implied weights directly.
! (Subsequent updates of these weights are done perturbatively to save time).
      DO I=1,6
        WHEXAD(I)=0.
      ENDDO
      DO I=1,6
        U=UTARGET(I)
        DO J=1,6
         WHEXAD(J)=WHEXAD(J)+LUI(I,J)*U
        ENDDO
      ENDDO
      K1OR3=1              !  At iteration 1, WHEXAD(1) and (2) might be < 0.

      DO IT=1,100          !  this should be ample
       KT=IT               !  report back how many iterations were needed
       L=0
       BCMIN=BCMINS
       DO K=K1OR3,6
        IF(WHEXAD(K).LT.BCMIN)THEN
         L=K
         BCMIN=WHEXAD(L)
        ENDIF
       ENDDO
       IF(L.EQ.0)RETURN ! If there are no negetive weights to offend, return
!  Permute the columns of LHEXAD and of LUI according to the permutation
!  scheme encoded by KP(J,L):
       DO J=2,6    ! J=1 corresponds to the NEW direction. (Treat separately).
        K=KP(J,L)
        KSIGN=KSG(J,L)
        WT(J)=WHEXAD(K)
        DO I=1,3
         NEWLHEX(I,J)=KSIGN*LHEXAD(I,K)
        ENDDO
        DO I=1,6
         NEWLUI(I,J)=LUI(I,K)
        ENDDO
       ENDDO

!  Set a temporary vector to what becomes the new column J=1 of LUI
       DO I=1,6
        LUI1(I)=-LUI(I,L)
       ENDDO

!  Replace the first hexad member, J=1, in this new arrangement:
       DO I=1,3
        LHEXAD(I,1)=NEWLHEX(I,4)+NEWLHEX(I,5) ! [  = NEWLHEX(I,3)-NEWLHEX(6) ]
       ENDDO

!  ..and make the corresponding update to the inverse-transpose of the
!  aspect-tensor basis LUI implied by the hexad:
       W1=-WHEXAD(L) ! new weight for J=1
       DO J=3,6
        WHEXAD(J)=WT(J)-W1 ! These weights become more negative than before..
        DO I=1,6
         LUI(I,J)=NEWLUI(I,J)-LUI1(I)
        ENDDO
       ENDDO
       WHEXAD(2)=WT(2)+W1  ! ..this one becomes more positive than before..
       WHEXAD(1)=W1        ! ..and this one simply switches sign to "positive"
       DO I=1,6
        LUI(I,2)=NEWLUI(I,2)+LUI1(I)
        LUI(I,1)=LUI1(I)
       ENDDO

!  copy the remaining new hexad of grid-steps back to array LHEXAD:
       DO J=2,6      !  (data for J=1 are already in place)
        DO I=1,3
         LHEXAD(I,J)=NEWLHEX(I,J)
        ENDDO
       ENDDO
       KFIRST=3 ! After iteration 1, WHEXAD(1) and (2) are always > 0.
      ENDDO
      STOP ' ALL ITERATIONS USED UP'        ! This should never happen !!!!!
      END
subroutine indexxi4(n,arrin4,indx)

  !-------- indexes an array arrin of length n, i.e. outputs the array indx
  !-------- such that arrin(indx(j)) is in ascending order for j=1,2,...,n.  The
  !-------- input quantities n and arrin are not changed.

  integer*4 arrin4(n)
  integer indx(n)
  integer*4 q4

  do j=1,n
   indx(j)=j
  end do
  if(n.eq.1) return

  l=n/2+1
  ir=n

  10 continue

    if(l.gt.1) then
     l=l-1
     indxt=indx(l)
     q4=arrin4(indxt)
    else
     indxt=indx(ir)
     q4=arrin4(indxt)
     indx(ir)=indx(1)
     ir=ir-1
     if(ir.eq.1) then
      indx(1)=indxt
      return
     end if
    end if

    i=l
    j=l+l

    20 continue

      if(j.le.ir) then
       if(j.lt.ir) then
        if(arrin4(indx(j)).lt.arrin4(indx(j+1)))j=j+1
       end if
       if(q4.lt.arrin4(indx(j))) then
        indx(i)=indx(j)
        i=j
        j=j+j
       else
        j=ir+1
       end if
       go to 20

      end if

      indx(i)=indxt
      go to 10

end subroutine indexxi4
subroutine indexxi8(n,arrin8,indx)

  !-------- indexes an array arrin of length n, i.e. outputs the array indx
  !-------- such that arrin(indx(j)) is in ascending order for j=1,2,...,n.  The
  !-------- input quantities n and arrin are not changed.

  integer*8 arrin8(n)
  integer indx(n)
  integer*8 q8

  do j=1,n
   indx(j)=j
  end do
  if(n.eq.1) return

  l=n/2+1
  ir=n

  10 continue

    if(l.gt.1) then
     l=l-1
     indxt=indx(l)
     q8=arrin8(indxt)
    else
     indxt=indx(ir)
     q8=arrin8(indxt)
     indx(ir)=indx(1)
     ir=ir-1
     if(ir.eq.1) then
      indx(1)=indxt
      return
     end if
    end if

    i=l
    j=l+l

    20 continue

      if(j.le.ir) then
       if(j.lt.ir) then
        if(arrin8(indx(j)).lt.arrin8(indx(j+1)))j=j+1
       end if
       if(q8.lt.arrin8(indx(j))) then
        indx(i)=indx(j)
        i=j
        j=j+j
       else
        j=ir+1
       end if
       go to 20

      end if

      indx(i)=indxt
      go to 10

end subroutine indexxi8
SUBROUTINE init_raf(aspect,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord,filter,xyzvol, &
                  anormal,oldf, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  ! Obtain filtering constants for recursive anisotropic filter.
  !     This form is based on assembling full strings, distributed evenly over processors.
  !     No attempt is made in this version to treat any points specially when gathering 
  !     the full strings required for each stage of the filter.  This is the simplest and probably
  !     least efficient parallel version of the recursive anisotropic filter.

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)


  TYPE(filter_cons), DIMENSION(14), INTENT(OUT) :: &
                       filter         !  structure which contains everything necessary to
                                      !     apply recursive anisotropic filter based on input
                                      !     aspect tensor

  INTEGER(4), INTENT(IN) :: npass     ! 1/2 num of binomial weighted filter apps--npass <= 10
  integer(4), intent(in) :: no_interp !  min length of string before interpolation takes place
  INTEGER(4), INTENT(IN) :: nsmooth   ! number of 1-2-1 smoothings to apply at beginning and
                                      !  end of filter
  integer(4), intent(in) :: nsmooth_shapiro,ifilt_ord
  logical, intent(in) :: binom        !   .false., then uniform factors, 
                                      !   .true., then binomial weighted factors

  REAL(4), DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ), INTENT(INOUT) :: &
            aspect                 ! aspect tensor for each point (destroyed)
                                   !    (1-xx,2--yy,3-zz,4-yz,5-xz,6-xy)
  real(4) xyzvol(ips:ipe,jps:jpe,kps:kpe)

  logical anormal                      ! .true., then do generalized normalization
  logical oldf                         ! .true., then compute old recursion coefficients

  INTEGER(1), allocatable::i1filter(:,:)    !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(2), allocatable::i2filter(:,:)    !  i2filter(1-4,.)=beginx,beginy,beginz,lenstring

  INTEGER(4) nstrings

  REAL(8) binomial0(40,39),sumbin(39)
  REAL(8) binomial(20,20)
  real(8) factor_binom
  INTEGER(1),allocatable::lhexadx(:,:,:,:)
  INTEGER(1),allocatable::lhexady(:,:,:,:)
  INTEGER(1),allocatable::lhexadz(:,:,:,:)
  INTEGER(4) lhexadlast(3,6),lui(6,6)
  INTEGER(2),allocatable::label_string(:,:)
  REAL(8) aspect8(6),whexad8(6)

  integer(4) npoints_recv(0:npes-1)
  integer(2),allocatable:: info_string(:,:)
  real(4),allocatable:: aspect_full(:)

  integer(4) i,ibeginpe,icolor,icolor2,iendpe,ierr,im,int_ord,itest,ixend,ixinc,ixstart,ixtemp,iyend, &
             iyinc,iystart,iytemp,j,jm,jtest,jumpx,jumpy,jumpz,k, &
             kk,km,kt,ktest,len,lentest,lguess,m,m_in,m_out,mpass,npoints_send,nstringsall,nstringsall2

  integer(4) lenmax,lenmin,npoints1
  integer(4) lenmaxdum,lenmindum,npoints1dum
  integer(4) lenmaxall,lenminall,totalpoints,totalpoints1
  integer(4) jumpxmax,jumpxmaxall
  integer(4) jumpymax,jumpymaxall
  integer(4) jumpzmax,jumpzmaxall
  real(4) lenbar,lenbarall,ratio_lens_min
  real(4) lenbardum,ratio_lens_mindum
  real(4) aspectmin,biga1,biga5,biga6,epstest,ratio_lens_minall
            integer(4) iabort

  call p_infit(int_ord)
        if(mype.eq.1) write(0,*)' after p_infit in init_raf, int_ord,mype=',int_ord,mype

  filter(1)%nsmooth=nsmooth
  filter(1)%nsmooth_shapiro=nsmooth_shapiro
  mpass=npass
  if(.not.binom) mpass=1
  filter(1)%npass=npass
  filter(1)%mpass=mpass
  filter(1)%no_interp=no_interp
  filter(1)%ifilt_ord=ifilt_ord
  filter(1)%int_ord=int_ord

!  compute binomial coefficients

  factor_binom=1._8
  if(.not.binom) factor_binom=0._8
  binomial0=0._8
  binomial0(1,1)=1._8
  binomial0(2,1)=1._8
  sumbin(1)=2._8
  do k=2,39
   binomial0(1,k)=1._8
   binomial0(k+1,k)=1._8
   do i=2,k
    binomial0(i,k)=binomial0(i-1,k-1)+binomial0(i,k-1)*factor_binom
   end do
   sumbin(k)=0._8
   do i=1,k+1
    sumbin(k)=sumbin(k)+binomial0(i,k)
   end do
  end do
  do k=1,39
   binomial0(:,k)=binomial0(:,k)/sumbin(k)
  end do

  kk=0
  binomial=0._8
  do k=1,39,2
   kk=kk+1
   binomial(1:kk,kk)=binomial0(1:kk,k)
  end do
        if(mype.eq.0) print *,' binom,binomial weightings used:',binom,binomial(1:npass,npass)

!  get normalization

  allocate(filter(1)%amp(ips:ipe,jps:jpe,kps:kpe))
  aspectmin=huge(aspectmin)
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     biga1=aspect(2,i,j,k)*aspect(3,i,j,k)-aspect(4,i,j,k)**2
     biga6=aspect(4,i,j,k)*aspect(5,i,j,k)-aspect(6,i,j,k)*aspect(3,i,j,k)
     biga5=aspect(6,i,j,k)*aspect(4,i,j,k)-aspect(5,i,j,k)*aspect(2,i,j,k)
     filter(1)%amp(i,j,k)=aspect(1,i,j,k)*biga1+aspect(6,i,j,k)*biga6+aspect(5,i,j,k)*biga5  ! det(aspect)
     aspectmin=min(aspect(1,i,j,k),aspect(2,i,j,k),aspect(3,i,j,k),aspectmin)
    end do
   end do
  end do

!  get all directions and smoothing coefficients

  epstest=3.*epsilon(epstest)*aspectmin
  ixstart=ipe ; ixend=ips ; ixinc=-1
  iystart=jpe ; iyend=jps ; iyinc=-1
  lguess=0
  allocate(lhexadx(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7))
  allocate(lhexady(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7))
  allocate(lhexadz(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7))
  lhexadx=0 ; lhexady=0 ; lhexadz=0
  do k=kps,kpe
   iytemp=iystart ; iystart=iyend ; iyend=iytemp ; iyinc=-iyinc
   do j=iystart,iyend,iyinc
    ixtemp=ixstart ; ixstart=ixend ; ixend=ixtemp ; ixinc=-ixinc
    do i=ixstart,ixend,ixinc
     aspect8(1:6)=aspect(1:6,i,j,k)
     call gethex(aspect8,lguess,lhexadlast,lui,whexad8,kt)
     aspect(1:7,i,j,k)=0.
     do kk=1,6
      if(whexad8(kk).gt.epstest) then
       jumpx=lhexadlast(1,kk)       !  make all directions positive and
       jumpy=lhexadlast(2,kk)       !  assign color
       jumpz=lhexadlast(3,kk)
       if(jumpz.ne.0.and.kds.eq.kde) go to 980   !  if 2-d, then all strings out of x-y surface are length 1
       if(jumpz.lt.0) then
        jumpx=-jumpx ; jumpy=-jumpy ; jumpz=-jumpz
       end if
       if(jumpz.eq.0) then
        if(jumpy.lt.0) then
         jumpx=-jumpx ; jumpy=-jumpy
        end if
        if(jumpy.eq.0.and.jumpx.lt.0) jumpx=-jumpx
       end if
       call what_color_is(jumpx,jumpy,jumpz,icolor)
       lhexadx(i,j,k,icolor)=jumpx ; lhexady(i,j,k,icolor)=jumpy ; lhexadz(i,j,k,icolor)=jumpz
       aspect(icolor,i,j,k)=whexad8(kk)
      end if
980   continue
     end do
     lguess=1
    end do
   end do
  end do

!     for isotropic filters, detect color number for each filtering direction

  filter(1)%icolorx=0
  filter(1)%icolory=0
  filter(1)%icolorz=0
  do icolor=1,7
   filter(icolor)%oldf=oldf
   filter(icolor+7)%oldf=oldf
   if(lhexadx(ips,jps,kps,icolor).ne.0.and.lhexady(ips,jps,kps,icolor).eq.0.and. &
           lhexadz(ips,jps,kps,icolor).eq.0) filter(1)%icolorx=icolor
   if(lhexadx(ips,jps,kps,icolor).eq.0.and.lhexady(ips,jps,kps,icolor).ne.0.and. &
           lhexadz(ips,jps,kps,icolor).eq.0) filter(1)%icolory=icolor
   if(lhexadx(ips,jps,kps,icolor).eq.0.and.lhexady(ips,jps,kps,icolor).eq.0.and. &
           lhexadz(ips,jps,kps,icolor).ne.0) filter(1)%icolorz=icolor
  end do

!                 big loop over all colors

  do icolor2=1,14
   icolor=icolor2
   if(icolor2.gt.7) icolor=icolor2-7

              if(mype.eq.0) write(0,*)' at 1 in init_raf, icolor,icolor2,mype=',icolor,icolor2,mype

!  get all string starting addresses and lengths

   allocate(i1filter(3,(ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1)))
   allocate(i2filter(4,(ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1)))
   m=0
   do k=kps,kpe
    do j=jps,jpe
     do i=ips,ipe
      jumpx=lhexadx(i,j,k,icolor) ; jumpy=lhexady(i,j,k,icolor) ; jumpz=lhexadz(i,j,k,icolor)
      if(jumpx.ne.0.or.jumpy.ne.0.or.jumpz.ne.0) then
       im=max(ips-1,min(i-jumpx,ipe+1))
       jm=max(jps-1,min(j-jumpy,jpe+1))
       km=max(kps-1,min(k-jumpz,kpe+1))
       if(lhexadx(im,jm,km,icolor).ne.jumpx.or.lhexady(im,jm,km,icolor).ne.jumpy.or. &
             lhexadz(im,jm,km,icolor).ne.jumpz) then
        m=m+1
        i1filter(1,m)=jumpx ; i1filter(2,m)=jumpy ; i1filter(3,m)=jumpz
        i2filter(1,m)=i ; i2filter(2,m)=j ; i2filter(3,m)=k
       end if
      end if
     end do
    end do
   end do
   nstrings=m
   npoints_send=0
   call mpi_allreduce(nstrings,nstringsall,1,mpi_integer4,mpi_sum,my_comm,ierr)
   m_in=0
   m_out=0
   if(nstringsall.gt.0) then
    if(nstrings.gt.0) then
     do m=1,nstrings
      len=1
      jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
      i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m)
      do
       lentest=len+1
       itest=max(ips-1,min(i+jumpx,ipe+1))
       jtest=max(jps-1,min(j+jumpy,jpe+1))
       ktest=max(kps-1,min(k+jumpz,kpe+1))
       if(jumpx.ne.lhexadx(itest,jtest,ktest,icolor).or. &
              jumpy.ne.lhexady(itest,jtest,ktest,icolor).or. &
                 jumpz.ne.lhexadz(itest,jtest,ktest,icolor)) then
        i2filter(4,m)=len
        npoints_send=npoints_send+len
        exit
       end if
       len=lentest
       i=itest ; j=jtest ; k=ktest
      end do
     end do

!             get external strings only (icolor2<=7) or internal strings only (icolor2>=8)

     do m=1,nstrings
      len=i2filter(4,m) ; jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
      i=i2filter(1,m)-jumpx ; j=i2filter(2,m)-jumpy ; k=i2filter(3,m)-jumpz
      ibeginpe=-1
      if(i.ge.ids.and.i.le.ide.and.j.ge.jds.and.j.le.jde.and.k.ge.kds.and.k.le.kde) &
          ibeginpe=pe_of_injn(in_of_i(i),jn_of_j(j))
      i=i2filter(1,m)+jumpx*len ; j=i2filter(2,m)+jumpy*len ; k=i2filter(3,m)+jumpz*len
      iendpe=-1
      if(i.ge.ids.and.i.le.ide.and.j.ge.jds.and.j.le.jde.and.k.ge.kds.and.k.le.kde) &
          iendpe=pe_of_injn(in_of_i(i),jn_of_j(j))
      if((ibeginpe.eq.mype.or.ibeginpe.eq.-1).and. &
        (iendpe.eq.mype.or.iendpe.eq.-1)) then
       m_in=m_in+1
       if(icolor2.ge.8) then
        i1filter(:,m_in)=i1filter(:,m)
        i2filter(:,m_in)=i2filter(:,m)
       end if
      else
       m_out=m_out+1
       if(icolor2.le.7) then
        i1filter(:,m_out)=i1filter(:,m)
        i2filter(:,m_out)=i2filter(:,m)
       end if
      end if
     end do

    end if

   end if

   if(icolor2.le.7) nstrings=m_out
   if(icolor2.ge.8) nstrings=m_in

   npoints_send=0
   npoints_recv=0
   filter(icolor2)%nstrings=0
   filter(icolor2)%npoints_send=0
   filter(icolor2)%npoints_recv=0
   filter(icolor2)%npointsmax=0
   filter(icolor2)%npointsmaxall=0
   lenbar=0.
   lenmax=-huge(lenmax)
   lenmin=huge(lenmin)
   npoints1=0
   ratio_lens_min=huge(ratio_lens_min)
   jumpxmax=0 ; jumpymax=0 ; jumpzmax=0
   lenbardum=0.
   lenmaxdum=-huge(lenmaxdum)
   lenmindum=huge(lenmindum)
   npoints1dum=0
   ratio_lens_mindum=huge(ratio_lens_mindum)
   if(icolor2.le.7) then
    call mpi_allreduce(nstrings,nstringsall,1,mpi_integer4,mpi_sum,my_comm,ierr)
   else
    nstringsall=nstrings
   end if
   if(nstringsall.gt.0) then
    if(nstrings.gt.0) then
     do m=1,nstrings
      len=i2filter(4,m) ; jumpx=abs(i1filter(1,m)) ; jumpy=abs(i1filter(2,m)) ; jumpz=abs(i1filter(3,m))
      jumpxmax=max(jumpx,jumpxmax)
      jumpymax=max(jumpy,jumpymax)
      jumpzmax=max(jumpz,jumpzmax)
      npoints_send=npoints_send+len
     end do
    end if
   

!         Begin computation of alpha, beta, the recursive filter coefficients.
!           It is necessary to have contiguous strings for this process, so first
!             must label strings and assign them to processors so the load is evenly distributed
!             when string pieces are gathered together into full strings


!         assign global label, origin and destination pe number to each string piece
!            (global label is starting i,j,k closest to edge of global domain)

    allocate(label_string(5,(ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1)))
    call string_label(icolor2,i1filter,i2filter,nstrings,label_string,npoints_recv, &
                     ids, ide, jds, jde, kds, kde, &         ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                     ims, ime, jms, jme, kms, kme,mype,npes )          ! memory indices

    filter(icolor2)%npointsmax=max(npoints_recv(mype),npoints_send)
    if(icolor2.le.7) then
     call mpi_allreduce(filter(icolor2)%npointsmax, &
                       filter(icolor2)%npointsmaxall,1,mpi_integer4,mpi_max,my_comm,ierr)
    else
     filter(icolor2)%npointsmaxall=filter(icolor2)%npointsmax
    end if
    filter(icolor2)%npoints_send=npoints_send
    filter(icolor2)%npoints_recv=npoints_recv(mype)
    allocate(info_string(7,max(1,npoints_recv(mype))))
    allocate(aspect_full(max(1,npoints_recv(mype))))
    allocate(filter(icolor2)%nu(max(1,npoints_recv(mype))))

!       assemble full strings

    allocate(filter(icolor2)%nsend(0:npes-1))
    allocate(filter(icolor2)%ndsend(0:npes))
    allocate(filter(icolor2)%nrecv(0:npes-1))
    allocate(filter(icolor2)%ndrecv(0:npes))
    allocate(filter(icolor2)%ia(max(1,npoints_send)))
    allocate(filter(icolor2)%ja(max(1,npoints_send)))
    allocate(filter(icolor2)%ka(max(1,npoints_send)))
    call string_assemble(icolor2,i1filter,i2filter,nstrings,label_string, &
                         npoints_send,npoints_recv(mype),aspect,icolor,xyzvol, &
                         info_string,aspect_full,filter(icolor2)%nu, &
                         filter(icolor2)%nsend,filter(icolor2)%ndsend, &
                         filter(icolor2)%nrecv,filter(icolor2)%ndrecv, &
                     filter(icolor2)%ia,filter(icolor2)%ja,filter(icolor2)%ka, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
    deallocate(label_string)

!       organize full strings for processing

    if(icolor2.le.7) then
     allocate(filter(icolor2)%ib(max(1,npoints_recv(mype))))
     if(npoints_recv(mype).gt.0) &
      call sort_strings(info_string,aspect_full,filter(icolor2)%nu, &
                     npoints_recv(mype),filter(icolor2)%ib, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
    end if

!      count number of strings 

    call count_strings(info_string,filter(icolor2)%nstrings,npoints_recv(mype))

!      compute desired alpha and beta for final filter

    allocate(filter(icolor2)%istart(filter(icolor2)%nstrings+1))

    if(anormal) then
     if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
      allocate(filter(icolor2)%lensstrsave(mpass,max(1,filter(icolor2)%nstrings)))
      allocate(filter(icolor2)%ins1save(max(1,npoints_recv(mype)),mpass))
      allocate(filter(icolor2)%wtssave(int_ord,max(1,npoints_recv(mype)),mpass))
     else
      allocate(filter(icolor2)%lensstrsave(1,1))
      allocate(filter(icolor2)%ins1save(1,1))
      allocate(filter(icolor2)%wtssave(1,1,1))
     end if
     allocate(filter(icolor2)%rsnuisave(max(1,npoints_recv(mype)),mpass))
     allocate(filter(icolor2)%lnfsave(ifilt_ord,max(1,npoints_recv(mype)),mpass))
     allocate(filter(icolor2)%bnfsave(max(1,npoints_recv(mype)),mpass))
 
     if(npoints_recv(mype).gt.0) &
       call alpha_beta(info_string,aspect_full,filter(icolor2)%nu, &
                      filter(icolor2)%lensstrsave,filter(icolor2)%ins1save, &
                      filter(icolor2)%wtssave, &
                      filter(icolor2)%rsnuisave,filter(icolor2)%lnfsave,filter(icolor2)%bnfsave, &
                      filter(icolor2)%istart,npoints_recv(mype),binomial,npass,mpass, &
                      no_interp,int_ord,ifilt_ord, &
                      lenbardum,lenmaxdum,lenmindum,npoints1dum, &
                      ratio_lens_mindum,oldf, &
                      ids, ide, jds, jde, kds, kde, &                          ! domain indices
                      ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                      ims, ime, jms, jme, kms, kme, &                          ! memory indices
                      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
    end if


!       compute "1/2" filter alpha and beta, needed one time only to get filter normalization

    if(no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
     allocate(filter(icolor2)%lensstr(mpass,max(1,filter(icolor2)%nstrings)))
     allocate(filter(icolor2)%ins1(max(1,npoints_recv(mype)),mpass))
     allocate(filter(icolor2)%wts(int_ord,max(1,npoints_recv(mype)),mpass))
    else
     allocate(filter(icolor2)%lensstr(1,1))
     allocate(filter(icolor2)%ins1(1,1))
     allocate(filter(icolor2)%wts(1,1,1))
    end if
    allocate(filter(icolor2)%rsnui(max(1,npoints_recv(mype)),mpass))
    allocate(filter(icolor2)%lnf(ifilt_ord,max(1,npoints_recv(mype)),mpass))
    allocate(filter(icolor2)%bnf(max(1,npoints_recv(mype)),mpass))
    if(anormal) aspect_full=.5*aspect_full
    if(npoints_recv(mype).gt.0) &
     call alpha_beta(info_string,aspect_full,filter(icolor2)%nu, &
                     filter(icolor2)%lensstr,filter(icolor2)%ins1, &
                     filter(icolor2)%wts, &
                     filter(icolor2)%rsnui,filter(icolor2)%lnf,filter(icolor2)%bnf, &
                     filter(icolor2)%istart,npoints_recv(mype),binomial,npass,mpass, &
                     no_interp,int_ord,ifilt_ord, &
                     lenbar,lenmax,lenmin,npoints1, &
                     ratio_lens_min,oldf, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

    deallocate(info_string)
    deallocate(aspect_full)

   end if

   deallocate(i1filter)
   deallocate(i2filter)
         call mpi_reduce(lenbar,lenbarall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
         call mpi_reduce(lenmax,lenmaxall,1,mpi_integer4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(lenmin,lenminall,1,mpi_integer4,mpi_min,0,my_comm,ierr)
         call mpi_reduce(npoints1,totalpoints1,1,mpi_integer4,mpi_sum,0,my_comm,ierr)
         call mpi_reduce(ratio_lens_min,ratio_lens_minall,1,mpi_integer4,mpi_min,0,my_comm,ierr)
         call mpi_reduce(jumpxmax,jumpxmaxall,1,mpi_integer4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(jumpymax,jumpymaxall,1,mpi_integer4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(jumpzmax,jumpzmaxall,1,mpi_integer4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(filter(icolor2)%nstrings,nstringsall2,1,mpi_integer4,mpi_sum,0,my_comm,ierr)
         if(mype.eq.0.and.nstringsall2.gt.0) then
          totalpoints=nint(lenbarall)
          lenbarall=lenbarall/max(1.,float(nstringsall2))
            if(icolor2.le.7) then
             print *,'  non-local string stats for icolor=',icolor
             print ('(''        ave non-local string length='',f8.2)'),lenbarall
             print ('(''        max non-local string length='',i8)'),lenmaxall
             print ('(''        min non-local string length='',i8)'),lenminall
             print ('(''        non-local ratio_lens_min='',f8.2)'),ratio_lens_minall
             print ('(''        total num non-local strings='',i9)'),nstringsall2
             print ('(''         total num non-local points='',i9)'),totalpoints
             print ('(''        num non-local len 1 strings='',i9)'),totalpoints1
             print ('(''                 non-local jumpxmax='',i9)'),jumpxmaxall
             print ('(''                 non-local jumpymax='',i9)'),jumpymaxall
             print ('(''                 non-local jumpzmax='',i9)'),jumpzmaxall
            else
             print *,'  local string stats for icolor=',icolor
             print ('(''        ave local string length='',f8.2)'),lenbarall
             print ('(''        max local string length='',i8)'),lenmaxall
             print ('(''        min local string length='',i8)'),lenminall
             print ('(''        local ratio_lens_min='',f8.2)'),ratio_lens_minall
             print ('(''        total num local strings='',i9)'),nstringsall2
             print ('(''         total num local points='',i9)'),totalpoints
             print ('(''        num local len 1 strings='',i9)'),totalpoints1
             print ('(''                 local jumpxmax='',i9)'),jumpxmaxall
             print ('(''                 local jumpymax='',i9)'),jumpymaxall
             print ('(''                 local jumpzmax='',i9)'),jumpzmaxall
            end if
            
            
         end if

  end do         !     end big loop over all colors

  deallocate(lhexadx)
  deallocate(lhexady)
  deallocate(lhexadz)

  if(anormal) call normalize_raf(filter, &
                 ids, ide, jds, jde, kds, kde, &                          ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                 ims, ime, jms, jme, kms, kme, &                          ! memory indices
                 inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

return
end subroutine init_raf
subroutine normalize_raf(filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   use "1/2" filter to compute proper amplitude normalization for filter

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  TYPE(filter_cons) filter(14)

  real(4),allocatable::amp(:,:,:)

  integer(4) i,icolor,ierr,ii,iter,itest,j,jtest,k,ktest,mpe
  real(4) amp_adjust,amp_adjust0

  allocate(amp(ims:ime,jms:jme,kms:kme))
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     amp(i,j,k)=filter(1)%amp(i,j,k)
     filter(1)%amp(i,j,k)=1.
    end do
   end do
  end do

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     amp(i,j,k)=filter(1)%amp(i,j,k)*amp(i,j,k)
    end do
   end do
  end do
  call raf(amp,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  call ad_raf(amp,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     amp(i,j,k)=filter(1)%amp(i,j,k)*amp(i,j,k)
    end do
   end do
  end do

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     filter(1)%amp(i,j,k)=sqrt(amp(i,j,k))
    end do
   end do
  end do

!   now restore alpha, beta to desired filter values

  if(filter(1)%npass.gt.0) then
   do icolor=1,14
    if(filter(icolor)%npoints_recv.gt.0) then
     if(filter(1)%no_interp.le.max(ide-ids,jde-jds,kde-kds)) then
      do i=1,filter(icolor)%nstrings
       do j=1,filter(1)%mpass
        filter(icolor)%lensstr(j,i)=filter(icolor)%lensstrsave(j,i)
       end do
      end do
      do j=1,filter(1)%mpass
       do i=1,filter(icolor)%npoints_recv
        filter(icolor)%ins1(i,j)=filter(icolor)%ins1save(i,j)
       end do
       do i=1,filter(icolor)%npoints_recv
        do ii=1,filter(1)%int_ord
         filter(icolor)%wts(ii,i,j)=filter(icolor)%wtssave(ii,i,j)
        end do
       end do
      end do
     end if
     do j=1,filter(1)%mpass
      do i=1,filter(icolor)%npoints_recv
       filter(icolor)%rsnui(i,j)=filter(icolor)%rsnuisave(i,j)
      end do
      do i=1,filter(icolor)%npoints_recv
       do ii=1,filter(1)%ifilt_ord
        filter(icolor)%lnf(ii,i,j)=filter(icolor)%lnfsave(ii,i,j)
       end do
      end do
      do i=1,filter(icolor)%npoints_recv
       filter(icolor)%bnf(i,j)=filter(icolor)%bnfsave(i,j)
      end do
     end do
     deallocate(filter(icolor)%lensstrsave)
     deallocate(filter(icolor)%ins1save)
     deallocate(filter(icolor)%wtssave)
     deallocate(filter(icolor)%rsnuisave)
     deallocate(filter(icolor)%lnfsave)
     deallocate(filter(icolor)%bnfsave)
    end if
   end do
  end if

!     figure out what the multiplying constant is

  itest=ids+(ide-ids)/2
  jtest=jds+(jde-jds)/2
  ktest=kds+(kde-kds)/2
  itest=max(ids,min(itest,ide))
  jtest=max(jds,min(jtest,jde))
  ktest=max(kds,min(ktest,kde))
  amp=0.
  if(itest.ge.ips.and.itest.le.ipe.and. &
     jtest.ge.jps.and.jtest.le.jpe.and. &
     ktest.ge.kps.and.ktest.le.kpe) amp(itest,jtest,ktest)=1.

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     amp(i,j,k)=filter(1)%amp(i,j,k)*amp(i,j,k)
    end do
   end do
  end do
  call raf(amp,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  call ad_raf(amp,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     amp(i,j,k)=filter(1)%amp(i,j,k)*amp(i,j,k)
    end do
   end do
  end do
  amp_adjust0=-1.
  if(itest.ge.ips.and.itest.le.ipe.and. &
     jtest.ge.jps.and.jtest.le.jpe.and. &
     ktest.ge.kps.and.ktest.le.kpe) amp_adjust0=1./sqrt(amp(itest,jtest,ktest))
  call mpi_allreduce(amp_adjust0,amp_adjust,1,mpi_real4,mpi_max,my_comm,ierr)
  filter(1)%amp=amp_adjust*filter(1)%amp
  deallocate(amp)
  
return
end subroutine normalize_raf
subroutine one_color(g,filter,ipass,no_interp,ifilt_ord, &
             nstrings,istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   apply one forward-backward recursive filter for one color

  use vkind
  use module_fitcons
  IMPLICIT NONE

  include 'filtertype.h'
  include 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: &
            ipass          !  total number of contiguous string points
  integer(4) no_interp
  integer(4),intent(in):: ifilt_ord

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid


  integer(4),intent(in):: nstrings
  integer(4),intent(in):: istart(nstrings+1)
  type(filter_cons) filter

  real(4),allocatable::work(:,:)

  integer(4) i,ierr,ishort_end,j

!-- gather up strings

  allocate(work(max(1,filter%npointsmax),2))
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    work(i,1)=g(filter%ia(i),filter%ja(i),filter%ka(i))
   end do
  end if
  call mpi_alltoallv(work(1,1),filter%nsend,filter%ndsend,mpi_real4, &
                   work(1,2),filter%nrecv,filter%ndrecv,mpi_real4,my_comm,ierr)
  if(filter%npoints_recv.gt.0) then
   do i=1,filter%npoints_recv
    work(i,1)=work(filter%ib(i),2)
   end do

   do j=1,nstrings
    do i=istart(j),istart(j+1)-1
     work(i,1)=work(i,1)*filter%nu(i)
    end do
    if(istart(j+1)-istart(j).le.no_interp) then
     do i=istart(j),istart(j+1)-1
      work(i,2)=work(i,1)
     end do
    else
     call stogt(filter%lensstr(ipass,j),istart(j+1)-istart(j), &
               filter%ins1(istart(j),ipass),filter%wts(1,istart(j),ipass), &
               work(istart(j),2),work(istart(j),1))
    end if
    if(istart(j+1)-istart(j).le.no_interp) then
     ishort_end=istart(j+1)-1
    else
     ishort_end=istart(j)+filter%lensstr(ipass,j)-1
    end if
    do i=istart(j),ishort_end
     work(i,2)=work(i,2)*filter%rsnui(i,ipass)
    end do
    if(filter%oldf) then
     call hbnrf1i_out(work(istart(j),2),ifilt_ord,filter%lnf(1,istart(j),ipass), &
                      filter%bnf(istart(j),ipass), &
              1,ishort_end-istart(j)+1,1,istart(j+1)-istart(j),1,ishort_end-istart(j)+1)
    else
     call hbnrf1i(work(istart(j),2),ifilt_ord,filter%lnf(1,istart(j),ipass), &
                      filter%bnf(istart(j),ipass), &
              1,ishort_end-istart(j)+1,1,istart(j+1)-istart(j),1,ishort_end-istart(j)+1)
    end if
    do i=istart(j),ishort_end
     work(i,2)=work(i,2)*filter%rsnui(i,ipass)
    end do
    if(istart(j+1)-istart(j).le.no_interp) then
     do i=istart(j),istart(j+1)-1
      work(i,1)=work(i,2)
     end do
    else
     call stog(filter%lensstr(ipass,j),istart(j+1)-istart(j), &
               filter%ins1(istart(j),ipass),filter%wts(1,istart(j),ipass), &
               work(istart(j),2),work(istart(j),1))
    end if
   end do

!-- send strings back

   do i=1,filter%npoints_recv
    work(filter%ib(i),2)=work(i,1)
   end do
  end if
  call mpi_alltoallv(work(1,2),filter%nrecv,filter%ndrecv,mpi_real4, &
                     work(1,1),filter%nsend,filter%ndsend,mpi_real4,my_comm,ierr)
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    g(filter%ia(i),filter%ja(i),filter%ka(i))=work(i,1)
   end do
  end if

  deallocate(work)

return
end subroutine one_color
subroutine one_color_loc(g,filter,ipass,no_interp,ifilt_ord, &
             nstrings,istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   apply one forward-backward recursive filter for one color

  use vkind
  use module_fitcons
  IMPLICIT NONE

  include 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: &
            ipass          !  total number of contiguous string points
  integer(4) no_interp
  integer(4),intent(in):: ifilt_ord

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid


  integer(4),intent(in):: nstrings
  integer(4),intent(in):: istart(nstrings+1)
  type(filter_cons) filter

  real(4),allocatable::work(:,:)

  integer(4) i,ierr,ishort_end,j

!-- gather up strings

  allocate(work(max(1,filter%npointsmax),2))
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    work(i,1)=g(filter%ia(i),filter%ja(i),filter%ka(i))
   end do
  end if
  if(filter%npoints_recv.gt.0) then

   do j=1,nstrings
    do i=istart(j),istart(j+1)-1
     work(i,1)=work(i,1)*filter%nu(i)
    end do
    if(istart(j+1)-istart(j).le.no_interp) then
     do i=istart(j),istart(j+1)-1
      work(i,2)=work(i,1)
     end do
    else
     call stogt(filter%lensstr(ipass,j),istart(j+1)-istart(j), &
               filter%ins1(istart(j),ipass),filter%wts(1,istart(j),ipass), &
               work(istart(j),2),work(istart(j),1))
    end if
    if(istart(j+1)-istart(j).le.no_interp) then
     ishort_end=istart(j+1)-1
    else
     ishort_end=istart(j)+filter%lensstr(ipass,j)-1
    end if
    do i=istart(j),ishort_end
     work(i,2)=work(i,2)*filter%rsnui(i,ipass)
    end do
    if(filter%oldf) then
     call hbnrf1i_out(work(istart(j),2),ifilt_ord,filter%lnf(1,istart(j),ipass), &
                      filter%bnf(istart(j),ipass), &
              1,ishort_end-istart(j)+1,1,istart(j+1)-istart(j),1,ishort_end-istart(j)+1)
    else
     call hbnrf1i(work(istart(j),2),ifilt_ord,filter%lnf(1,istart(j),ipass), &
                      filter%bnf(istart(j),ipass), &
              1,ishort_end-istart(j)+1,1,istart(j+1)-istart(j),1,ishort_end-istart(j)+1)
    end if
    do i=istart(j),ishort_end
     work(i,2)=work(i,2)*filter%rsnui(i,ipass)
    end do
    if(istart(j+1)-istart(j).le.no_interp) then
     do i=istart(j),istart(j+1)-1
      work(i,1)=work(i,2)
     end do
    else
     call stog(filter%lensstr(ipass,j),istart(j+1)-istart(j), &
               filter%ins1(istart(j),ipass),filter%wts(1,istart(j),ipass), &
               work(istart(j),2),work(istart(j),1))
    end if
   end do

!-- send strings back

  end if
  if(filter%npoints_send.gt.0) then
   do i=1,filter%npoints_send
    g(filter%ia(i),filter%ja(i),filter%ka(i))=work(i,1)
   end do
  end if

  deallocate(work)

return
end subroutine one_color_loc
subroutine p_infit(int_ord)

  use vkind
  use module_fitcons
  implicit none

  integer(4) int_ord

  call infit
  
  int_ord=no

return
end subroutine p_infit
SUBROUTINE raf(g,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  1st half of recursive anisotropic self-adjoint filter (full-strings version)

  IMPLICIT NONE

  include 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(14)             !  structure defining recursive filter

  real(4) work(min(ims,jms,kms):max(ime,jme,kme))

  integer(4) i,icolor,icolor2,ierr,im,ip,ipass,ipep1,ipsm1,ismooth,j,jm
  integer(4) jp,jpass,jpep1,jpsm1,k,km,kp,kpep1,kpsm1
  integer(4) im3,ip3,ipep3,ipsm3,jm3,jp3,jpep3,jpsm3,km3,kp3,kpep3,kpsm3


!      apply 1-2-1 smoother in each direction

  if(filter(1)%nsmooth.gt.0) then
   ipsm1=max(ids,ims,ips-1) ; ipep1=min(ide,ime,ipe+1)
   jpsm1=max(jds,jms,jps-1) ; jpep1=min(jde,jme,jpe+1)
   kpsm1=max(kds,kms,kps-1) ; kpep1=min(kde,kme,kpe+1)
   do ismooth=1,filter(1)%nsmooth
    call refresh_halo3x(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do j=jps,jpe
      work(ipsm1:ipep1)=g(ipsm1:ipep1,j,k)
      do i=ips,ipe
       ip=min(i+1,ipep1) ; im=max(ipsm1,i-1)
       g(i,j,k)=.25*(work(ip)+work(im))+.5*work(i)
      end do
     end do
    end do
    call refresh_halo3y(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do i=ips,ipe
      work(jpsm1:jpep1)=g(i,jpsm1:jpep1,k)
      do j=jps,jpe
       jp=min(j+1,jpep1) ; jm=max(jpsm1,j-1)
       g(i,j,k)=.25*(work(jp)+work(jm))+.5*work(j)
      end do
     end do
    end do
    do j=jps,jpe
     do i=ips,ipe
      work(kpsm1:kpep1)=g(i,j,kpsm1:kpep1)
      do k=kps,kpe
       kp=min(k+1,kpep1) ; km=max(kpsm1,k-1)
       g(i,j,k)=.25*(work(kp)+work(km))+.5*work(k)
      end do
     end do
    end do

   end do
  end if

!      and/or apply Shapiro smoother in each direction (2nd moment preserving)

  if(filter(1)%nsmooth_shapiro.gt.0) then
   ipsm3=max(ids,ims,ips-3) ; ipep3=min(ide,ime,ipe+3)
   jpsm3=max(jds,jms,jps-3) ; jpep3=min(jde,jme,jpe+3)
   kpsm3=max(kds,kms,kps-3) ; kpep3=min(kde,kme,kpe+3)
   do ismooth=1,filter(1)%nsmooth_shapiro
    call refresh_halo3x(g,3, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do j=jps,jpe
      work(ipsm3:ipep3)=g(ipsm3:ipep3,j,k)
      do i=ips,ipe
       ip=min(i+1,ipep3) ; im=max(ipsm3,i-1)
       ip3=min(i+3,ipep3) ; im3=max(ipsm3,i-3)
       g(i,j,k)=.28125*(work(ip)+work(im))+.5*work(i)-.03125*(work(ip3)+work(im3))
      end do
     end do
    end do
    call refresh_halo3y(g,3, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do i=ips,ipe
      work(jpsm3:jpep3)=g(i,jpsm3:jpep3,k)
      do j=jps,jpe
       jp=min(j+1,jpep3) ; jm=max(jpsm3,j-1)
       jp3=min(j+3,jpep3) ; jm3=max(jpsm3,j-3)
       g(i,j,k)=.28125*(work(jp)+work(jm))+.5*work(j)-.03125*(work(jp3)+work(jm3))
      end do
     end do
    end do
    do j=jps,jpe
     do i=ips,ipe
      work(kpsm3:kpep3)=g(i,j,kpsm3:kpep3)
      do k=kps,kpe
       kp=min(k+1,kpep3) ; km=max(kpsm3,k-1)
       kp3=min(k+3,kpep3) ; km3=max(kpsm3,k-3)
       g(i,j,k)=.28125*(work(kp)+work(km))+.5*work(k)-.03125*(work(kp3)+work(km3))
      end do
     end do
    end do

   end do
  end if

  if(filter(1)%npass.gt.0) then
   do ipass=1,filter(1)%npass
    jpass=min(ipass,filter(1)%mpass)
    do icolor2=14,8,-1

     icolor=icolor2
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color_loc(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
     icolor=icolor2-7
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!      following barrier is required because there is no communication for icolor>=8--the call
!          to one_color_loc, and all work must end for a color, before moving to the next one

                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
     call mpi_barrier(my_comm,ierr)     !!!! DO NOT REMOVE THIS BARRIER !!!!!
                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
   
    end do

   end do
  end if

return
end subroutine raf
subroutine refresh_halo3x(f,nhalo, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info

!   refresh rows of f in x direction 

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: nhalo

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            f                      !  input field to have halo updated in x direction


  real(4),allocatable::eastin(:,:,:)
  real(4),allocatable::westin(:,:,:)
  real(4),allocatable::eastout(:,:,:)
  real(4),allocatable::westout(:,:,:)

  integer(4) status(mpi_status_size)
  integer(4) eastpe,i,ierr,j,k,nbuf,westpe

!  to get processor number of point with coordinates i,j, use

              !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!        pe_of_ij=pe_of_injn(in_of_i(i),jn_of_j(j))  !!!!!!!!!!!!
              !!!!!!!!!!!!!!!!!!!!!!!!!!!

  nbuf=nhalo*(jpe-jps+1)*(kpe-kps+1)
  eastpe=mpi_proc_null
  if(ipe+nhalo.le.ide) eastpe=pe_of_injn(in_of_i(ipe+1),jn_of_j(jps))
  westpe=mpi_proc_null
  if(ips-nhalo.ge.ids) westpe=pe_of_injn(in_of_i(ips-1),jn_of_j(jps))

!  send east rows to west halo points first

  allocate(eastin(ipe-nhalo+1:ipe,jps:jpe,kps:kpe))
  do k=kps,kpe
   do j=jps,jpe
    do i=ipe-nhalo+1,ipe
     eastin(i,j,k)=f(i,j,k)
    end do
   end do
  end do

  allocate(westout(ips-nhalo:ips-1,jps:jpe,kps:kpe))
  call mpi_sendrecv(eastin,nbuf,mpi_real4,eastpe,mype, &
                    westout,nbuf,mpi_real4,westpe,mpi_any_tag, &
                    my_comm,status,ierr)
  deallocate(eastin)
  if(ips-nhalo.ge.ids) then
   do k=kps,kpe
    do j=jps,jpe
     do i=ips-nhalo,ips-1
      f(i,j,k)=westout(i,j,k)
     end do
    end do
   end do
  end if
  deallocate(westout)

!  now send west rows to east halo points

  allocate(westin(ips:ips+nhalo-1,jps:jpe,kps:kpe))
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ips+nhalo-1
     westin(i,j,k)=f(i,j,k)
    end do
   end do
  end do
  
  allocate(eastout(ipe+1:ipe+nhalo,jps:jpe,kps:kpe))
  call mpi_sendrecv(westin,nbuf,mpi_real4,westpe,mype, &
                    eastout,nbuf,mpi_real4,eastpe,mpi_any_tag, &
                    my_comm,status,ierr)
  deallocate(westin)
  if(ipe+nhalo.le.ide) then
   do k=kps,kpe
    do j=jps,jpe
     do i=ipe+1,ipe+nhalo
      f(i,j,k)=eastout(i,j,k)
     end do
    end do
   end do
  end if
  deallocate(eastout)

return
end

subroutine refresh_halo3y(f,nhalo, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info

!   refresh rows of f in y direction 

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: nhalo

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            f                      !  input field to have halo updated in x direction


  real(4),allocatable::northin(:,:,:)
  real(4),allocatable::southin(:,:,:)
  real(4),allocatable::northout(:,:,:)
  real(4),allocatable::southout(:,:,:)

  integer(4) status(mpi_status_size)
  integer(4) southpe,i,ierr,j,k,nbuf,northpe

!  to get processor number of point with coordinates i,j, use

              !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!        pe_of_ij=pe_of_injn(in_of_i(i),jn_of_j(j))  !!!!!!!!!!!!
              !!!!!!!!!!!!!!!!!!!!!!!!!!!

  nbuf=nhalo*(ipe-ips+1)*(kpe-kps+1)
  northpe=mpi_proc_null
  if(jpe+nhalo.le.jde) northpe=pe_of_injn(in_of_i(ips),jn_of_j(jpe+1))
  southpe=mpi_proc_null
  if(jps-nhalo.ge.jds) southpe=pe_of_injn(in_of_i(ips),jn_of_j(jps-1))

!  send north rows to south halo points first

  allocate(northin(ips:ipe,jpe-nhalo+1:jpe,kps:kpe))
  do k=kps,kpe
   do j=jpe-nhalo+1,jpe
    do i=ips,ipe
     northin(i,j,k)=f(i,j,k)
    end do
   end do
  end do

  allocate(southout(ips:ipe,jps-nhalo:jps-1,kps:kpe))
  call mpi_sendrecv(northin,nbuf,mpi_real4,northpe,mype, &
                    southout,nbuf,mpi_real4,southpe,mpi_any_tag, &
                    my_comm,status,ierr)
  deallocate(northin)
  if(jps-nhalo.ge.jds) then
   do k=kps,kpe
    do j=jps-nhalo,jps-1
     do i=ips,ipe
      f(i,j,k)=southout(i,j,k)
     end do
    end do
   end do
  end if
  deallocate(southout)

!  now send south rows to north halo points

  allocate(southin(ips:ipe,jps:jps+nhalo-1,kps:kpe))
  do k=kps,kpe
   do j=jps,jps+nhalo-1
    do i=ips,ipe
     southin(i,j,k)=f(i,j,k)
    end do
   end do
  end do
  
  allocate(northout(ips:ipe,jpe+1:jpe+nhalo,kps:kpe))
  call mpi_sendrecv(southin,nbuf,mpi_real4,southpe,mype, &
                    northout,nbuf,mpi_real4,northpe,mpi_any_tag, &
                    my_comm,status,ierr)
  deallocate(southin)
  if(jpe+nhalo.le.jde) then
   do k=kps,kpe
    do j=jpe+1,jpe+nhalo
     do i=ips,ipe
      f(i,j,k)=northout(i,j,k)
     end do
    end do
   end do
  end if
  deallocate(northout)

return
end
subroutine regular_ad_raf(f,filter)

  INCLUDE 'filtertype.h'

  real(4) f(*)
  type(filter_cons) filter(7)

  call ad_raf(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)

!    multiply by amp

  call regular_amp(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)

return
end subroutine regular_ad_raf
subroutine regular_init_filt(filter,nhalo, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)
  integer(4), intent(in):: nhalo

  TYPE(filter_cons), DIMENSION(7), INTENT(INOUT) :: &
                       filter         !  structure which contains everything necessary to
                                      !     apply recursive anisotropic filter based

  filter(1)%ids =ids  ; filter(1)%ide =ide
  filter(1)%jds =jds  ; filter(1)%jde =jde
  filter(1)%kds =kds  ; filter(1)%kde =kde
  filter(1)%ips =ips  ; filter(1)%ipe =ipe
  filter(1)%jps =jps  ; filter(1)%jpe =jpe
  filter(1)%kps =kps  ; filter(1)%kpe =kpe
  filter(1)%ims =ims  ; filter(1)%ime =ime
  filter(1)%jms =jms  ; filter(1)%jme =jme
  filter(1)%kms =kms  ; filter(1)%kme =kme
  filter(1)%ids2=ids ; filter(1)%ide2=ide
  filter(1)%jds2=jds ; filter(1)%jde2=jde
  filter(1)%kds2=kds ; filter(1)%kde2=kde
  filter(1)%ips2=ips ; filter(1)%ipe2=ipe
  filter(1)%jps2=jps ; filter(1)%jpe2=jpe
  filter(1)%kps2=kps ; filter(1)%kpe2=kpe
  filter(1)%ims2=ims ; filter(1)%ime2=ime
  filter(1)%jms2=jms ; filter(1)%jme2=jme
  filter(1)%kms2=kms ; filter(1)%kme2=kme
  filter(1)%inpes=inpes ; filter(1)%jnpes=jnpes
  filter(1)%nhalo=nhalo ; filter(1)%mype=mype ; filter(1)%npes=npes
  allocate(filter(1)%pe_of_injn(inpes,jnpes))
  allocate(filter(1)%pe_of_injn2(inpes,jnpes))
  filter(1)%pe_of_injn=pe_of_injn
  filter(1)%pe_of_injn2=pe_of_injn
  allocate(filter(1)%in_of_i(ids:ide))
  allocate(filter(1)%jn_of_j(jds:jde))
  allocate(filter(1)%in_of_i2(ids:ide))
  allocate(filter(1)%jn_of_j2(jds:jde))
  filter(1)%in_of_i=in_of_i
  filter(1)%jn_of_j=jn_of_j
  filter(1)%in_of_i2=in_of_i
  filter(1)%jn_of_j2=jn_of_j

return
end subroutine regular_init_filt
subroutine regular_raf(f,filter)

  INCLUDE 'filtertype.h'

  real(4) f(*)
  type(filter_cons) filter(7)

!    multiply by amp

  call regular_amp(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)

  call raf(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)

return
end subroutine regular_raf

SUBROUTINE regular_amp(f,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  1st half of recursive anisotropic self-adjoint filter (full-strings version)

  IMPLICIT NONE

  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            f                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)             !  structure defining recursive filter

  integer(4) i,j,k

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     f(i,j,k)=filter(1)%amp(i,j,k)*f(i,j,k)
    end do
   end do
  end do

return
end subroutine regular_amp
subroutine sort_strings(info_string,aspect_full,xyzvol_full, &
                     npoints_recv,ib, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
                     inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  !   sort strings by string id and distance

  IMPLICIT NONE

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  INTEGER(4), INTENT(IN) :: npoints_recv


  INTEGER(2), DIMENSION( 7, npoints_recv ), INTENT(INOUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7-- jumpx,jumpy,jumpz for this string
  REAL(4), DIMENSION( npoints_recv ) , INTENT(INOUT) :: &
            aspect_full,xyzvol_full

  integer(4) ib(npoints_recv)

  integer(4),allocatable::ij_origin(:)
  integer(2),allocatable::iwork(:)
  real(4),allocatable::work(:)

  integer(4) i,idist,idistlen,idistmax,idistmin,idjxlen,idjxylen,idjxyzlen,idjxyzx0len,idjxyzxy0len
  integer(4) ix0,ix0len,ix0max,ix0min,iy0,iy0len,iy0max,iy0min,iz0,iz0max
  integer(4) iz0min,j,jumpx,jumpxlen,jumpxmax,jumpxmin,jumpy,jumpylen,jumpymax,jumpymin
  integer(4) jumpz,jumpzlen,jumpzmax,jumpzmin

!   obtain range of jumpx,jumpy,originx,originy

  jumpxmin=huge(jumpxmin) ; jumpxmax=-jumpxmin
  jumpymin=jumpxmin ; jumpymax=jumpxmax
  jumpzmin=jumpxmin ; jumpzmax=jumpxmax
  ix0min=jumpxmin ; ix0max=jumpxmax
  iy0min=jumpxmin ; iy0max=jumpxmax
  iz0min=jumpxmin
  idistmin=jumpxmin ; idistmax=jumpxmax
  do i=1,npoints_recv
   jumpx=info_string(5,i) ; jumpy=info_string(6,i) ; jumpz=info_string(7,i)
   ix0=info_string(2,i) ; iy0=info_string(3,i) ; iz0=info_string(4,i)
   idist=info_string(1,i)
   jumpxmin=min(jumpx,jumpxmin) ; jumpxmax=max(jumpx,jumpxmax)
   jumpymin=min(jumpy,jumpymin) ; jumpymax=max(jumpy,jumpymax)
   jumpzmin=min(jumpz,jumpzmin) ; jumpzmax=max(jumpz,jumpzmax)
   ix0min=min(ix0,ix0min) ; ix0max=max(ix0,ix0max)
   iy0min=min(iy0,iy0min) ; iy0max=max(iy0,iy0max)
   iz0min=min(iz0,iz0min)
   idistmin=min(idist,idistmin) ; idistmax=max(idist,idistmax)
  end do
  jumpxlen=jumpxmax-jumpxmin+1 ; jumpylen=jumpymax-jumpymin+1 ; jumpzlen=jumpzmax-jumpzmin+1
  idistlen=idistmax-idistmin+1
  ix0len=ix0max-ix0min+1 ; iy0len=iy0max-iy0min+1 ; idjxlen=idistlen*jumpxlen
  idjxylen=idjxlen*jumpylen
  idjxyzlen=idjxylen*jumpzlen
  idjxyzx0len=idjxyzlen*ix0len
  idjxyzxy0len=idjxyzx0len*iy0len

  allocate(ij_origin(npoints_recv))
  do i=1,npoints_recv
   jumpx=info_string(5,i) ; jumpy=info_string(6,i) ; jumpz=info_string(7,i)
   ix0=info_string(2,i) ; iy0=info_string(3,i) ; iz0=info_string(4,i)
   idist=info_string(1,i)
   ij_origin(i)=idist-idistmin+idistlen*(jumpx-jumpxmin)+idjxlen*(jumpy-jumpymin) &
                            +idjxylen*(jumpz-jumpzmin)+ &
                                  idjxyzlen*(ix0-ix0min)+idjxyzx0len*(iy0-iy0min)+ &
                                       idjxyzxy0len*(iz0-iz0min)
  end do
  call indexxi4(npoints_recv,ij_origin,ib)
  deallocate(ij_origin)

  allocate(iwork(npoints_recv))
  do j=1,7
   do i=1,npoints_recv
    iwork(i)=info_string(j,ib(i))
   end do
   do i=1,npoints_recv
    info_string(j,i)=iwork(i)
   end do
  end do
  deallocate(iwork)
  allocate(work(npoints_recv))
  do i=1,npoints_recv
   work(i)=aspect_full(ib(i))
  end do
  do i=1,npoints_recv
   aspect_full(i)=work(i)
  end do
  do i=1,npoints_recv
   work(i)=xyzvol_full(ib(i))
  end do
  do i=1,npoints_recv
   xyzvol_full(i)=work(i)
  end do
  deallocate(work)

return
end subroutine sort_strings
SUBROUTINE string_assemble(icolor2,i1filter,i2filter,nstrings,label_string, &
                     npoints_send,npoints_recv,aspect,icolor,xyzvol, &
                     info_string,aspect_full,xyzvol_full,nsend,ndsend,nrecv,ndrecv,ia,ja,ka, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, &                          ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   assemble groups of unbroken strings approximately evenly distributed over all processors

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  integer(4), intent(in) :: icolor2  !     <=7, then external strings (communication required)
                                     !     >=8, then internal strings (communication not required)

  INTEGER(4), INTENT(IN) :: nstrings,icolor

  INTEGER(1), DIMENSION( 3, * ), INTENT(IN) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(2), DIMENSION( 4, * ), INTENT(IN) :: &
            i2filter                       !  i2filter(1-4,.)=beginx,beginy,beginz,lenstring

  INTEGER(2), DIMENSION( 5, * ), INTENT(IN) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5,.)=dest pe

  INTEGER(4), INTENT(IN) :: &
            npoints_send, &         !  number of points to send for assembling strings
            npoints_recv            !  number of points for assembled strings

  REAL(4), DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ), INTENT(IN) :: &
            aspect                   !  aspect tensor numbers (recursive filter parameters derived
  real(4) xyzvol(ips:ipe,jps:jpe,kps:kpe)
                                     !            from these)
  INTEGER(2), DIMENSION( 7, max(1,npoints_recv) ), INTENT(OUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7-- jumpx,jumpy,jumpz for this string
  REAL(4), DIMENSION( max(1,npoints_recv) ) , INTENT(OUT) :: &
            aspect_full,xyzvol_full
  integer(4) nsend(0:npes-1),ndsend(0:npes),nrecv(0:npes-1),ndrecv(0:npes)
  integer(2) ia(npoints_send),ja(npoints_send),ka(npoints_send)

  integer(4),allocatable::idest(:)
  integer(4),allocatable::indx(:)
  integer(2),allocatable::iwork(:)
  integer(2),allocatable::string_info(:,:)
  real(4),allocatable::full_aspect(:),full_xyzvol(:)
  real(4),allocatable::work(:)

  integer(4) i,i0,idestpe,idist,ierr,j,j0,jumpx,jumpy,jumpz,k,k0,kk,len,m,mbuf,mpe,mpi_string1

  mbuf=0

!       setup string_info array

  allocate(idest(npoints_send))
  allocate(string_info(7,npoints_send))
  allocate(full_aspect(npoints_send))
  allocate(full_xyzvol(npoints_send))
  nsend=0
  if(nstrings.gt.0) then
   do m=1,nstrings
    len=i2filter(4,m)
    jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
    i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m)
    i0=label_string(1,m) ; j0=label_string(2,m) ; k0=label_string(3,m)
    idist=label_string(4,m)
    idestpe=label_string(5,m)
    do kk=1,len
     mbuf=mbuf+1
     string_info(1,mbuf)=idist
     string_info(2,mbuf)=i0 ; string_info(3,mbuf)=j0 ; string_info(4,mbuf)=k0
     string_info(5,mbuf)=jumpx ; string_info(6,mbuf)=jumpy ; string_info(7,mbuf)=jumpz
     ia(mbuf)=i ; ja(mbuf)=j ; ka(mbuf)=k
     idest(mbuf)=idestpe
     nsend(idestpe)=nsend(idestpe)+1
     full_aspect(mbuf)=aspect(icolor,i,j,k)
     full_xyzvol(mbuf)=xyzvol(i,j,k)
     i=i+jumpx ; j=j+jumpy ; k=k+jumpz
     if(idist.ge.0) idist=idist+1
    end do
   end do
  end if
        if(mbuf.ne.npoints_send) then
         print *,' problem in string_assemble--mbuf ne npoints_send, mype,mbuf,npoints_send=', &
                    mype,mbuf,npoints_send
         stop
        end if

!    sort destination pe numbers from smallest to largest

  if(icolor2.le.7) then
   allocate(indx(npoints_send))
   if(mbuf.gt.0) then
    call indexxi4(mbuf,idest,indx)
   end if

   allocate(iwork(npoints_send))
   allocate(work(npoints_send))
   if(mbuf.gt.0) then

!     use sort index to reorder everything

    do j=1,7
     do i=1,mbuf
      iwork(i)=string_info(j,indx(i))
     end do
     do i=1,mbuf
      string_info(j,i)=iwork(i)
     end do
    end do
    do i=1,mbuf
     iwork(i)=ia(indx(i))
    end do
    do i=1,mbuf
     ia(i)=iwork(i)
    end do
    do i=1,mbuf
     iwork(i)=ja(indx(i))
    end do
    do i=1,mbuf
     ja(i)=iwork(i)
    end do
    do i=1,mbuf
     iwork(i)=ka(indx(i))
    end do
    do i=1,mbuf
     ka(i)=iwork(i)
    end do

    do i=1,mbuf
     work(i)=full_aspect(indx(i))
    end do
    do i=1,mbuf
     full_aspect(i)=work(i)
    end do
    do i=1,mbuf
     work(i)=full_xyzvol(indx(i))
    end do
    do i=1,mbuf
     full_xyzvol(i)=work(i)
    end do
   end if
   deallocate(indx)
   deallocate(iwork)
   deallocate(work)
  end if
  deallocate(idest)

!!  now get remaining info necessary for using alltoall command

  ndsend(0)=0
  do mpe=1,npes
   ndsend(mpe)=ndsend(mpe-1)+nsend(mpe-1)
  end do
  if(icolor2.le.7) then
   
   call mpi_alltoall(nsend,1,mpi_integer, &
       nrecv,1,mpi_integer,my_comm,ierr)
   ndrecv(0)=0
   do mpe=1,npes
    ndrecv(mpe)=ndrecv(mpe-1)+nrecv(mpe-1)
   end do
   call mpi_type_contiguous(7,mpi_integer2,mpi_string1,ierr)
   call mpi_type_commit(mpi_string1,ierr)
   call mpi_alltoallv(string_info,nsend,ndsend,mpi_string1, &
                     info_string,nrecv,ndrecv,mpi_string1,my_comm,ierr)
   call mpi_type_free(mpi_string1,ierr)
   call mpi_alltoallv(full_aspect,nsend,ndsend,mpi_real4, &
                     aspect_full,nrecv,ndrecv,mpi_real4,my_comm,ierr)
   call mpi_alltoallv(full_xyzvol,nsend,ndsend,mpi_real4, &
                     xyzvol_full,nrecv,ndrecv,mpi_real4,my_comm,ierr)
  else
   nrecv=nsend
   ndrecv=ndsend
   aspect_full=full_aspect
   xyzvol_full=full_xyzvol
   info_string=string_info
  end if
  deallocate(string_info)
  deallocate(full_aspect)
  deallocate(full_xyzvol)

return
end subroutine string_assemble
SUBROUTINE string_label(icolor2,i1filter,i2filter,nstrings,label_string,npoints_recv, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     ims, ime, jms, jme, kms, kme, mype, npes )               ! memory indices

  ! assign global string labels to each string
  !   (global label is first i,j,k inside global domain)

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  integer(4),intent(in):: icolor2    !    <= 7, then external strings (communication required) 
                                     !    >= 8, then internal strings (communication not required) 
  INTEGER(4) nstrings

  INTEGER(1), DIMENSION( 3, * ), INTENT(IN) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(2), DIMENSION( 4, * ), INTENT(IN) :: &
            i2filter                       !  i2filter(1-4,.)=beginx,beginy,beginz,lenstring

  INTEGER(2), DIMENSION( 5, * ), INTENT(OUT) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5,.)=destination pe for string piece

  integer(4), intent(out):: npoints_recv(0:npes-1)

  integer(8) labelijk(max(1,nstrings))
  integer(4) nrecv(0:npes-1),ndrecv(0:npes)
  integer(8),allocatable::labelijk0(:)
  integer(4),allocatable::index(:)

  integer(4) i,idist,idisttest,ierr,istring_pe,itest,j,jtest,jumpx,jumpy
  integer(4) jumpz,k,ktest,mpe,mype,n,npes,nstrings0
  integer(8) lastlabel

  if(nstrings.gt.0) then
   do n=1,nstrings

    jumpx=i1filter(1,n) ; jumpy=i1filter(2,n) ; jumpz=i1filter(3,n)
    i=i2filter(1,n) ; j=i2filter(2,n) ; k=i2filter(3,n)
    idist=0
    do 
     idisttest=idist+1
     itest=i-jumpx
     if(itest.lt.ids.or.itest.gt.ide) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist
      exit
     end if
     jtest=j-jumpy
     if(jtest.lt.jds.or.jtest.gt.jde) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist
      exit
     end if
     ktest=k-jumpz
     if(ktest.lt.kds.or.ktest.gt.kde) then
      label_string(1,n)=i ; label_string(2,n)=j ; label_string(3,n)=k
      label_string(4,n)=idist
      exit
     end if
     i=itest ; j=jtest ; k=ktest
     idist=idisttest
    end do
    labelijk(n)=label_string(1,n)+(ide-ids+1)*((label_string(2,n)-1)+(jde-jds+1)*(label_string(3,n)-1))

   end do
  end if

!--  assemble all string labels to pe 0, for assignment of pe numbers

  if(icolor2.le.7) then
   nrecv=0
   call mpi_gather(nstrings,1,mpi_integer4,nrecv,1,mpi_integer4,0,my_comm,ierr)
   if(mype.eq.0) then
    ndrecv(0)=0
    do i=1,npes
     ndrecv(i)=ndrecv(i-1)+nrecv(i-1)
    end do
    nstrings0=ndrecv(npes)
    allocate(labelijk0(nstrings0))
   end if
   call mpi_gatherv(labelijk,nstrings,mpi_integer8,labelijk0,nrecv,ndrecv,mpi_integer8,0,my_comm,ierr)

!------ sort strings so strings with same labels are adjacent, then assign adjacent strings to same pe.
!------   then when we assemble all pieces, it is guaranteed that all pieces of every contiguous string
!------   will end up on the same processor.

   if(mype.eq.0) then
    allocate(index(nstrings0))
    call indexxi8(nstrings0,labelijk0,index)
    lastlabel=-huge(lastlabel)
    istring_pe=0
    do i=1,nstrings0
     j=index(i)
     if(labelijk0(j).ne.lastlabel) then
      lastlabel=labelijk0(j)
      istring_pe=mod(istring_pe+1,npes)
     end if
     labelijk0(j)=istring_pe
    end do
    deallocate(index)
   end if

!---- now scatter pe destination numbers back
 
   call mpi_scatterv(labelijk0,nrecv,ndrecv,mpi_integer8,labelijk,nstrings,mpi_integer8,0,my_comm,ierr)
   if(mype.eq.0) deallocate(labelijk0)

  else
   labelijk=mype
  end if

!---- assign destination pe numbers and count up number of points at each destination pe

  nrecv=0
  if(nstrings.gt.0) then
   do i=1,nstrings
    mpe=labelijk(i)
    nrecv(mpe)=nrecv(mpe)+i2filter(4,i)
    label_string(5,i)=mpe
   end do
  end if
  if(icolor2.le.7) then
   call mpi_allreduce(nrecv,npoints_recv,npes,mpi_integer4,mpi_sum,my_comm,ierr)
  else
   npoints_recv=nrecv
  end if

return
end subroutine string_label
SUBROUTINE subdomain_def(nx,ny,nz,nhalo, &
      ids, ide, jds, jde, kds, kde, &         ! domain indices
      ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
      ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )           ! processor info

  ! define domain, patch, and memory starting and ending values for each processor

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: nx, ny, nz           ! domain dimensions

  INTEGER(4), INTENT(IN) :: inpes, jnpes         ! number of processors in each directionh

  INTEGER(4), INTENT(IN) :: nhalo               ! number of halo rows around each subdomain

  INTEGER(4), INTENT(OUT) :: &
                  ids, ide, jds, jde, kds, kde, &         ! domain indices
                  ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                  ims, ime, jms, jme, kms, kme            ! memory indices
  INTEGER(4), INTENT(IN) :: &
                  mype, npes                    ! current processor, total number of processors

  INTEGER(4), DIMENSION( inpes, jnpes ), INTENT(OUT) :: &
                  pe_of_injn                    !  absolute processor address in terms of inpe, jnpe

  INTEGER(4), DIMENSION( nx ), INTENT(OUT) :: &
                  in_of_i                       !  x processor coordinate for each x grid index

  INTEGER(4), DIMENSION( ny ), INTENT(OUT) :: &
                  jn_of_j                       !  y processor coordinate for each y grid index
                  

  INTEGER(4) in,ichunk,ichunk_calc,ierr,ipe_calc,ips_calc,itail,jn,jchunk,jchunk_calc, &
             jnchunks,jpe_calc,jps_calc,jtail,mpe,nchunks,nx_loc,nx_min,ny_loc,ny_min


  if(inpes*jnpes.ne.npes) then
   if(mype.eq.0) then
    print *,' NUMBER OF PE''S AVAILABLE NOT EQUAL TO NUMBER REQUIRED'
    print *,' NUMBER OF PE''S AVAILABLE NOT EQUAL TO NUMBER REQUIRED'
    print *,' NUMBER OF PE''S AVAILABLE NOT EQUAL TO NUMBER REQUIRED'
    print *,' NUMBER OF PE''S AVAILABLE NOT EQUAL TO NUMBER REQUIRED'
   end if
   call mpi_finalize(ierr)
   stop
  end if

  ids=1
  ide=nx
  jds=1
  jde=ny

  kds=1 ; kde=nz ; kps=1 ; kpe=nz ; kms=1 ; kme=nz


  ichunk=nx/inpes
  jchunk=ny/jnpes
  itail=nx-(inpes*(nx/inpes))
  jtail=ny-(jnpes*(ny/jnpes))

  mpe=0
  jps_calc=jds
  jnchunks=jps_calc-1

  do jn=1,jnpes
   jchunk_calc=jchunk
   if(jn.le.jtail) jchunk_calc=jchunk+1
   jnchunks=jnchunks+jchunk_calc
   jpe_calc=jnchunks
   jn_of_j(jps_calc-jds+1:jpe_calc-jds+1)=jn
   ips_calc=ids
   nchunks=ips_calc-1
   do in=1,inpes
    pe_of_injn(in,jn)=mpe
    ichunk_calc=ichunk
    if(in.le.itail) ichunk_calc=ichunk+1
    nchunks=nchunks+ichunk_calc
    ipe_calc=nchunks
    in_of_i(ips_calc-ids+1:ipe_calc-ids+1)=in
    if(mype.eq.mpe) then
     ips=ips_calc
     ipe=ipe_calc
     nx_loc=ipe-ips+1
     jps=jps_calc
     jpe=jpe_calc
     ny_loc=jpe-jps+1
    end if
    ips_calc=ipe_calc+1
    mpe=mpe+1
   end do
   jps_calc=jpe_calc+1
  end do
  call mpi_allreduce(nx_loc,nx_min,1,mpi_integer4,mpi_min,my_comm,ierr)
  call mpi_allreduce(ny_loc,ny_min,1,mpi_integer4,mpi_min,my_comm,ierr)
  if(nx_min.le.1) then
      if(mype.eq.0) then
    print *,' INPES TOO LARGE FOR GRID SIZE, inpes,nx,nx_locmin=',inpes,nx,nx_min
    print *,' INPES TOO LARGE FOR GRID SIZE, inpes,nx,nx_locmin=',inpes,nx,nx_min
    print *,' INPES TOO LARGE FOR GRID SIZE, inpes,nx,nx_locmin=',inpes,nx,nx_min
    print *,' INPES TOO LARGE FOR GRID SIZE, inpes,nx,nx_locmin=',inpes,nx,nx_min
   end if
   call mpi_finalize(ierr)
   stop
  end if
  if(ny_min.le.1) then
      if(mype.eq.0) then
    print *,' JNPES TOO LARGE FOR GRID SIZE, jnpes,ny,ny_locmin=',jnpes,ny,ny_min
    print *,' JNPES TOO LARGE FOR GRID SIZE, jnpes,ny,ny_locmin=',jnpes,ny,ny_min
    print *,' JNPES TOO LARGE FOR GRID SIZE, jnpes,ny,ny_locmin=',jnpes,ny,ny_min
    print *,' JNPES TOO LARGE FOR GRID SIZE, jnpes,ny,ny_locmin=',jnpes,ny,ny_min
   end if
   call mpi_finalize(ierr)
   stop
  end if

  ims=max(ids,ips-nhalo)
  ime=min(ide,ipe+nhalo)
  jms=max(jds,jps-nhalo)
  jme=min(jde,jpe+nhalo)


return
end subroutine subdomain_def
subroutine regular2super(f,g,filter, &
                 ifs, ife, jfs, jfe, kfs, kfe, &
                 ids, ide, jds, jde, kds, kde, &
                 ips, ipe, jps, jpe, kps, kpe, &
                 ims, ime, jms, jme, kms, kme, &
                 igs, ige, jgs, jge, kgs, kge, &
                 ids2,ide2,jds2,jde2,kds2,kde2, &
                 ips2,ipe2,jps2,jpe2,kps2,kpe2, &
                 ims2,ime2,jms2,jme2,kms2,kme2)

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  real(4) f(ifs:ife,jfs:jfe,kfs:kfe)
  real(4) g(igs:ige,jgs:jge,kgs:kge)
  type(filter_cons) filter(7)
  integer(4)  ifs, ife, jfs, jfe, kfs, kfe, &
              ids, ide, jds, jde, kds, kde, &
              ips, ipe, jps, jpe, kps, kpe, &
              ims, ime, jms, jme, kms, kme, &
              igs, ige, jgs, jge, kgs, kge, &
              ids2,ide2,jds2,jde2,kds2,kde2, &
              ips2,ipe2,jps2,jpe2,kps2,kpe2, &
              ims2,ime2,jms2,jme2,kms2,kme2

  integer(4) npes,nsendthis,i,k,nrecvthis,mpi_vstack,ierr
  real(4),allocatable::sendbuf(:,:),recvbuf(:,:)

  npes=filter(1)%npes
  nsendthis=filter(1)%ndsendsup(npes)
  allocate(sendbuf(kps:kpe,nsendthis))
  do i=1,nsendthis
   do k=kps,kpe
    sendbuf(k,i)=f(filter(1)%iasup(i),filter(1)%jasup(i),k)
   end do
  end do
  nrecvthis=filter(1)%ndrecvsup(npes)
  allocate(recvbuf(kps:kpe,nrecvthis))
  call mpi_type_contiguous(kpe-kps+1,mpi_real4,mpi_vstack,ierr)
  call mpi_type_commit(mpi_vstack,ierr)
  call mpi_alltoallv(sendbuf,filter(1)%nsendsup,filter(1)%ndsendsup,mpi_vstack, &
                     recvbuf,filter(1)%nrecvsup,filter(1)%ndrecvsup,mpi_vstack,my_comm,ierr)

  call mpi_type_free(mpi_vstack,ierr)
  deallocate(sendbuf)
  g=0.
  do i=1,nrecvthis
   do k=kps,kpe
    g(filter(1)%ibsup(i),filter(1)%jbsup(i),k)=recvbuf(k,i)
   end do
  end do
  deallocate(recvbuf)

return
end subroutine regular2super

subroutine super2regular(f,g,filter, &
                 ifs, ife, jfs, jfe, kfs, kfe, &
                 ids, ide, jds, jde, kds, kde, &
                 ips, ipe, jps, jpe, kps, kpe, &
                 ims, ime, jms, jme, kms, kme, &
                 igs, ige, jgs, jge, kgs, kge, &
                 ids2,ide2,jds2,jde2,kds2,kde2, &
                 ips2,ipe2,jps2,jpe2,kps2,kpe2, &
                 ims2,ime2,jms2,jme2,kms2,kme2)

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  real(4) f(ifs:ife,jfs:jfe,kfs:kfe)
  real(4) g(igs:ige,jgs:jge,kgs:kge)
  type(filter_cons) filter(7)
  integer(4)  ifs, ife, jfs, jfe, kfs, kfe, &
              ids, ide, jds, jde, kds, kde, &
              ips, ipe, jps, jpe, kps, kpe, &
              ims, ime, jms, jme, kms, kme, &
              igs, ige, jgs, jge, kgs, kge, &
              ids2,ide2,jds2,jde2,kds2,kde2, &
              ips2,ipe2,jps2,jpe2,kps2,kpe2, &
              ims2,ime2,jms2,jme2,kms2,kme2

  integer(4) npes,nsendthis,i,k,nrecvthis,mpi_vstack,ierr
  real(4),allocatable::sendbuf(:,:),recvbuf(:,:)

  npes=filter(1)%npes
  nsendthis=filter(1)%ndsendsup(npes)
  nrecvthis=filter(1)%ndrecvsup(npes)
  allocate(recvbuf(kps:kpe,nrecvthis))
  do i=1,nrecvthis
   do k=kps,kpe
    recvbuf(k,i)=g(filter(1)%ibsup(i),filter(1)%jbsup(i),k)
   end do
  end do
  allocate(sendbuf(kps:kpe,nsendthis))
  call mpi_type_contiguous(kpe-kps+1,mpi_real4,mpi_vstack,ierr)
  call mpi_type_commit(mpi_vstack,ierr)
  call mpi_alltoallv(recvbuf,filter(1)%nrecvsup,filter(1)%ndrecvsup,mpi_vstack, &
                     sendbuf,filter(1)%nsendsup,filter(1)%ndsendsup,mpi_vstack,my_comm,ierr)
  deallocate(recvbuf)
  call mpi_type_free(mpi_vstack,ierr)
  f=0.
  do i=1,nsendthis
   do k=kps,kpe
    f(filter(1)%iasup(i),filter(1)%jasup(i),k)=sendbuf(k,i)
   end do
  end do
  deallocate(sendbuf)

return
end subroutine super2regular

subroutine super_ad_raf(f,g,filter)

  INCLUDE 'filtertype.h'

  real(4) f(*)
  type(super_grid) g
  type(filter_cons) filter(7)

  call ad_raf(g%f,filter, &
        filter(1)%ids2,filter(1)%ide2, &
        filter(1)%jds2,filter(1)%jde2, &
        filter(1)%kds2,filter(1)%kde2, &
        filter(1)%ips2,filter(1)%ipe2, &
        filter(1)%jps2,filter(1)%jpe2, &
        filter(1)%kps2,filter(1)%kpe2, &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn2,filter(1)%in_of_i2,filter(1)%jn_of_j2)

  call super2regular(f,g%f,filter, &
        filter(1)%ims ,filter(1)%ime , &
        filter(1)%jms ,filter(1)%jme , &
        filter(1)%kms ,filter(1)%kme , &
        filter(1)%ids ,filter(1)%ide , &
        filter(1)%jds ,filter(1)%jde , &
        filter(1)%kds ,filter(1)%kde , &
        filter(1)%ips ,filter(1)%ipe , &
        filter(1)%jps ,filter(1)%jpe , &
        filter(1)%kps ,filter(1)%kpe , &
        filter(1)%ims ,filter(1)%ime , &
        filter(1)%jms ,filter(1)%jme , &
        filter(1)%kms ,filter(1)%kme , &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2, &
        filter(1)%ids2,filter(1)%ide2, &
        filter(1)%jds2,filter(1)%jde2, &
        filter(1)%kds2,filter(1)%kde2, &
        filter(1)%ips2,filter(1)%ipe2, &
        filter(1)%jps2,filter(1)%jpe2, &
        filter(1)%kps2,filter(1)%kpe2, &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2)
  call super_amp(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)

return
end subroutine super_ad_raf

subroutine super_allocate(f,filter)

!   allocate extended grid variable

  IMPLICIT NONE

  INCLUDE 'filtertype.h'

  type(super_grid) f
  type(filter_cons) filter(7)

  allocate(f%f(filter(1)%ims2:filter(1)%ime2, &
             filter(1)%jms2:filter(1)%jme2, &
             filter(1)%kms2:filter(1)%kme2))

return
end subroutine super_allocate

SUBROUTINE super_amp(g,filter, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  1st half of recursive anisotropic self-adjoint filter (full-strings version)

  IMPLICIT NONE

  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(7)             !  structure defining recursive filter

  integer(4) i,j,k

!   multiply by amp 

  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     g(i,j,k)=filter(1)%amp(i,j,k)*g(i,j,k)
    end do
   end do
  end do

return
end subroutine super_amp

SUBROUTINE super_domain_def(nx,ny,nz,nhalo,nx2,ny2,nz2, &
          add_west,add_east,add_south,add_north,add_bottom,add_top, &
      ids, ide, jds, jde, kds, kde, &         ! domain indices
      ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
      ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j, &          ! processor info
      ids2, ide2, jds2, jde2, kds2, kde2, &         ! domain indices
      ips2, ipe2, jps2, jpe2, kps2, kpe2,  &         ! patch indices
      ims2, ime2, jms2, jme2, kms2, kme2, &         ! memory indices
      pe_of_injn2, in_of_i2, jn_of_j2 )           ! processor info

  ! define extended domain, patch, and memory starting and ending values for each processor

  !  original domain indices already defined

  IMPLICIT NONE

  INTEGER(4), INTENT(IN) :: nx, ny, nz           ! domain dimensions
  integer(4), intent(in) :: nx2,ny2,nz2          !  size of internal super grid
  integer(4), intent(in) :: add_west,add_east        !  margins added to domain in x
  integer(4), intent(in) :: add_south,add_north      !  margins added to domain in y
  integer(4), intent(in) :: add_bottom,add_top       !  margins added to domain in z

  INTEGER(4), INTENT(IN) :: inpes, jnpes         ! number of processors in each direction

  INTEGER(4), INTENT(IN) :: nhalo               ! number of halo rows around each subdomain

  INTEGER(4), INTENT(IN) :: &
                  ids, ide, jds, jde, kds, kde, &         ! domain indices
                  ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                  ims, ime, jms, jme, kms, kme, &         ! memory indices
                  mype, npes                    ! current processor, total number of processors

  INTEGER(4), DIMENSION( inpes, jnpes ), INTENT(IN) :: &
                  pe_of_injn                    !  absolute processor address in terms of inpe, jnpe

  INTEGER(4), DIMENSION( nx ), INTENT(IN) :: &
                  in_of_i                       !  x processor coordinate for each x grid index

  INTEGER(4), DIMENSION( ny ), INTENT(IN) :: &
                  jn_of_j                       !  y processor coordinate for each y grid index

!-------------------------------super internal grid indices:

  INTEGER(4), INTENT(OUT) :: &
                  ids2, ide2, jds2, jde2, kds2, kde2, &         ! domain indices
                  ips2, ipe2, jps2, jpe2, kps2, kpe2, &         ! patch indices
                  ims2, ime2, jms2, jme2, kms2, kme2            ! memory indices

  INTEGER(4), DIMENSION( inpes, jnpes ), INTENT(OUT) :: &
                  pe_of_injn2                    !  absolute processor address in terms of inpe, jnpe

  INTEGER(4), DIMENSION( nx2 ), INTENT(OUT) :: &
                  in_of_i2                       !  x processor coordinate for each x grid index

  INTEGER(4), DIMENSION( ny2 ), INTENT(OUT) :: &
                  jn_of_j2                       !  y processor coordinate for each y grid index
                  

  INTEGER(4) in,ichunk,ichunk_calc,ipe_calc,ips_calc,itail,jn,jchunk,jchunk_calc, &
             jnchunks,jpe_calc,jps_calc,jtail,mpe,nchunks


  ids2=ids-add_west
  ide2=ide+add_east
  jds2=jds-add_south
  jde2=jde+add_north
  kds2=kds-add_bottom
  kde2=kde+add_top

  kps2=kds2 ; kpe2=kde2 ; kms2=kds2 ; kme2=kde2

  ichunk=nx2/inpes
  jchunk=ny2/jnpes
  itail=nx2-(inpes*(nx2/inpes))
  jtail=ny2-(jnpes*(ny2/jnpes))

  mpe=0
  jps_calc=jds2
  jnchunks=jps_calc-1

  do jn=1,jnpes
   jchunk_calc=jchunk
   if(jn.le.jtail) jchunk_calc=jchunk+1
   jnchunks=jnchunks+jchunk_calc
   jpe_calc=jnchunks
   jn_of_j2(jps_calc-jds2+1:jpe_calc-jds2+1)=jn
   ips_calc=ids2
   nchunks=ips_calc-1
   do in=1,inpes
    pe_of_injn2(in,jn)=mpe
    ichunk_calc=ichunk
    if(in.le.itail) ichunk_calc=ichunk+1
    nchunks=nchunks+ichunk_calc
    ipe_calc=nchunks
    in_of_i2(ips_calc-ids2+1:ipe_calc-ids2+1)=in
    if(mype.eq.mpe) then
     ips2=ips_calc
     ipe2=ipe_calc
     jps2=jps_calc
     jpe2=jpe_calc
    end if
    ips_calc=ipe_calc+1
    mpe=mpe+1
   end do
   jps_calc=jpe_calc+1
  end do

  ims2=max(ids2,ips2-nhalo)
  ime2=min(ide2,ipe2+nhalo)
  jms2=max(jds2,jps2-nhalo)
  jme2=min(jde2,jpe2+nhalo)

return
end subroutine super_domain_def

subroutine super_init_raf(aspect,super_factor,nhalo,npass,no_interp, &
                 binom,nsmooth,nsmooth_shapiro,ifilt_ord,filter,anormal,oldf, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


!   this is wrapper program around init_raf which allows for super domain so when filter
!   is applied, ugly boundary problems can be eliminated.

  IMPLICIT NONE

  INCLUDE 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  TYPE(filter_cons), DIMENSION(7), INTENT(OUT) :: &
                       filter         !  structure which contains everything necessary to
                                      !     apply recursive anisotropic filter based on input
                                      !     aspect tensor

  logical anormal                     !   .true., then do general normalization
  logical oldf                        !   .true., then use old recursion coefficients

  INTEGER(4), INTENT(IN) :: npass     ! 1/2 num of binomial weighted filter apps--npass <= 10
  integer(4) no_interp
  INTEGER(4), INTENT(IN) :: nsmooth   ! number of 1-2-1 smoothings to apply at beginning and
                                      !  end of filter
  integer(4), intent(in) :: nsmooth_shapiro,ifilt_ord
  logical,intent(in)::binom

  REAL(4), DIMENSION( 6, ips:ipe, jps:jpe, kps:kpe ), INTENT(IN) :: &
            aspect                 ! aspect tensor for each point
                                   !    (1-xx,2--yy,3-zz,4-yz,5-xz,6-xy)
  real(4), intent(in)::super_factor   !  helps determine width of expansion zone (=0, then no expansion)
  integer(4) nhalo                   !  number of halo rows required for subdomains

  INTEGER(4) lhexadlast(3,6),lui(6,6)
  REAL(8) aspect8(6),whexad8(6)
  real(4) steps
  integer(4) ixstart,ixend,ixinc,iystart,iyend,iyinc,lguess,imax,jmax,kmax,imin,jmin,kmin
  integer(4) i,isteps,j,jumpx,jumpy,jumpz,k,kk,kt,iytemp,ixtemp

  integer(4) ids2,ide2,jds2,jde2,kds2,kde2, &   ! domain indices
             ips2, ipe2, jps2, jpe2, kps2, kpe2, &   ! patch indices
             ims2, ime2, jms2, jme2, kms2, kme2      ! memory indices
  integer(4) pe_of_injn2(inpes,jnpes)
  integer(4),allocatable::in_of_i2(:),jn_of_j2(:)
  integer(4) nxglb,nyglb,nzglb,nx2glb,ny2glb,nz2glb
  integer(4) add_west,add_east,add_south,add_north,add_bottom,add_top
  integer(4),allocatable::ija(:,:),idest(:),indx(:),iwork(:)
  integer(4) nsend(0:npes-1),ndsend(0:npes)
  integer(4) nrecv(0:npes-1),ndrecv(0:npes)
  integer(4) idestpe,mbuf
  integer(4),allocatable::ijb(:,:)
  real(4),allocatable::aspectrbuf(:,:,:),aspectsbuf(:,:,:)
  integer(4) mpi_ij,mpi_vstack,nsendthis,nrecvthis
  real(4),allocatable::aspectex0(:,:,:,:),aspectex(:,:,:,:)
  integer(4) ip,im,jp,jm,idsnext,idenext,jdsnext,jdenext,loop,loopmax
  integer(4) ierr,mpe
  integer(4) ibad,jbad
  real(4),allocatable::amp(:,:,:)
  real(4),allocatable::xyzvolex(:,:,:)

           if(mype.eq.0) write(0,*)' at 1 in super_init_raf'
!  first use hexad to find global super domain indices

  ixstart=ipe ; ixend=ips ; ixinc=-1
  iystart=jpe ; iyend=jps ; iyinc=-1
  lguess=0
  imax=-huge(imax)
  jmax=-huge(imax)
  kmax=-huge(imax)
  imin=huge(imax)
  jmin=huge(imax)
  kmin=huge(imax)
  do k=kps,kpe
   iytemp=iystart ; iystart=iyend ; iyend=iytemp ; iyinc=-iyinc
   do j=iystart,iyend,iyinc
    ixtemp=ixstart ; ixstart=ixend ; ixend=ixtemp ; ixinc=-ixinc
    do i=ixstart,ixend,ixinc
     aspect8(1:6)=aspect(1:6,i,j,k)
     call gethex(aspect8,lguess,lhexadlast,lui,whexad8,kt)
     do kk=1,6
      steps=super_factor*sqrt(whexad8(kk))
      isteps=ceiling(steps)
      jumpx=isteps*lhexadlast(1,kk)
      jumpy=isteps*lhexadlast(2,kk)       
      jumpz=isteps*lhexadlast(3,kk)
      imax=max(imax,i+jumpx,i-jumpx)
      jmax=max(jmax,j+jumpy,j-jumpy)
      kmax=max(kmax,k+jumpz,k-jumpz)
      imin=min(imin,i+jumpx,i-jumpx)
      jmin=min(jmin,j+jumpy,j-jumpy)
      kmin=min(kmin,k+jumpz,k-jumpz)
     end do
     lguess=1
    end do
   end do
  end do
  call mpi_allreduce(imin,ids2,1,mpi_integer4,mpi_min,my_comm,ierr)
  call mpi_allreduce(imax,ide2,1,mpi_integer4,mpi_max,my_comm,ierr)
  ids2=min(ids2,ids) ; ide2=max(ide2,ide)
  call mpi_allreduce(jmin,jds2,1,mpi_integer4,mpi_min,my_comm,ierr)
  call mpi_allreduce(jmax,jde2,1,mpi_integer4,mpi_max,my_comm,ierr)
  jds2=min(jds2,jds) ; jde2=max(jde2,jde)
  if(kds.eq.kde) then
   kds2=kds ; kde2=kde
  else
   call mpi_allreduce(kmax,kde2,1,mpi_integer4,mpi_max,my_comm,ierr)
   call mpi_allreduce(kmin,kds2,1,mpi_integer4,mpi_min,my_comm,ierr)
   kds2=min(kds2,kds) ; kde2=max(kde2,kde)
  end if
  
  nxglb=ide-ids+1
  nyglb=jde-jds+1
  nzglb=kde-kds+1
  nx2glb=ide2-ids2+1
  ny2glb=jde2-jds2+1
  nz2glb=kde2-kds2+1
     if(mype.eq.0) then
       print *,' in super_init_raf, nx,y,zglb=',nxglb,nyglb,nzglb
       print *,' in super_init_raf, nx,y,z2glb=',nx2glb,ny2glb,nz2glb
       print *,' original number of points=',nxglb*nyglb*nzglb
       print *,' superset number of points=',nx2glb*ny2glb*nz2glb
       print *,' ratio superset/original = ',float(nx2glb*ny2glb*nz2glb)/float(nxglb*nyglb*nzglb)
     end if
  add_west=ids-ids2
  add_east=ide2-ide
  add_south=jds-jds2
  add_north=jde2-jde
  add_bottom=kds-kds2
  add_top=kde2-kde
  allocate(in_of_i2(ids2:ide2))
  allocate(jn_of_j2(jds2:jde2))
           if(mype.eq.0) write(0,*)' at 2 in super_init_raf'
  call super_domain_def(nxglb,nyglb,nzglb,nhalo,nx2glb,ny2glb,nz2glb, &
          add_west,add_east,add_south,add_north,add_bottom,add_top, &
      ids, ide, jds, jde, kds, kde, &         ! domain indices
      ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
      ims, ime, jms, jme, kms, kme, &         ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j, &          ! processor info
      ids2, ide2, jds2, jde2, kds2, kde2, &         ! domain indices
      ips2, ipe2, jps2, jpe2, kps2, kpe2,  &         ! patch indices
      ims2, ime2, jms2, jme2, kms2, kme2, &         ! memory indices
      pe_of_injn2, in_of_i2, jn_of_j2 )           ! processor info

  filter(1)%ids =ids  ; filter(1)%ide =ide 
  filter(1)%jds =jds  ; filter(1)%jde =jde
  filter(1)%kds =kds  ; filter(1)%kde =kde
  filter(1)%ips =ips  ; filter(1)%ipe =ipe
  filter(1)%jps =jps  ; filter(1)%jpe =jpe
  filter(1)%kps =kps  ; filter(1)%kpe =kpe
  filter(1)%ims =ims  ; filter(1)%ime =ime
  filter(1)%jms =jms  ; filter(1)%jme =jme
  filter(1)%kms =kms  ; filter(1)%kme =kme
  filter(1)%ids2=ids2 ; filter(1)%ide2=ide2
  filter(1)%jds2=jds2 ; filter(1)%jde2=jde2
  filter(1)%kds2=kds2 ; filter(1)%kde2=kde2
  filter(1)%ips2=ips2 ; filter(1)%ipe2=ipe2
  filter(1)%jps2=jps2 ; filter(1)%jpe2=jpe2
  filter(1)%kps2=kps2 ; filter(1)%kpe2=kpe2
  filter(1)%ims2=ims2 ; filter(1)%ime2=ime2
  filter(1)%jms2=jms2 ; filter(1)%jme2=jme2
  filter(1)%kms2=kms2 ; filter(1)%kme2=kme2
  filter(1)%inpes=inpes ; filter(1)%jnpes=jnpes 
  filter(1)%nhalo=nhalo ; filter(1)%mype=mype ; filter(1)%npes=npes
  allocate(filter(1)%pe_of_injn(inpes,jnpes))
  allocate(filter(1)%pe_of_injn2(inpes,jnpes))
  filter(1)%pe_of_injn=pe_of_injn
  filter(1)%pe_of_injn2=pe_of_injn2
  allocate(filter(1)%in_of_i(ids:ide))
  allocate(filter(1)%jn_of_j(jds:jde))
  allocate(filter(1)%in_of_i2(ids2:ide2))
  allocate(filter(1)%jn_of_j2(jds2:jde2))
  filter(1)%in_of_i=in_of_i
  filter(1)%jn_of_j=jn_of_j
  filter(1)%in_of_i2=in_of_i2
  filter(1)%jn_of_j2=jn_of_j2

!   set up send and recieve arrays for moving from original grid to super grid and vice-versa

  nsendthis=(ipe-ips+1)*(jpe-jps+1)
  allocate(ija(2,nsendthis))
  allocate(idest(nsendthis))
  allocate(indx(nsendthis))
  allocate(iwork(nsendthis))
  mbuf=0
  nsend=0
  do j=jps,jpe
   do i=ips,ipe
    mbuf=mbuf+1
    idestpe=pe_of_injn2(in_of_i2(i),jn_of_j2(j))
    nsend(idestpe)=nsend(idestpe)+1
    ija(1,mbuf)=i
    ija(2,mbuf)=j
    idest(mbuf)=idestpe
   end do
  end do
     
!  sort destination pe numbers from smallest to largest

  call indexxi4(mbuf,idest,indx)

!   use sort index to reorder everything

  do k=1,2
   do i=1,mbuf
    iwork(i)=ija(k,indx(i))
   end do
   do i=1,mbuf
    ija(k,i)=iwork(i)
   end do
  end do
  deallocate(idest)
  deallocate(indx)
  deallocate(iwork)

  allocate(aspectsbuf(6,kps:kpe,mbuf))
  do i=1,mbuf
   do k=kps,kpe
    do kk=1,6
     aspectsbuf(kk,k,i)=aspect(kk,ija(1,i),ija(2,i),k)
    end do
   end do
  end do

!  now get remaining all_to_all info

  ndsend(0)=0
  do mpe=1,npes
   ndsend(mpe)=ndsend(mpe-1)+nsend(mpe-1)
  end do

  call mpi_alltoall(nsend,1,mpi_integer,nrecv,1,mpi_integer,my_comm,ierr)
  ndrecv(0)=0
  do mpe=1,npes
   ndrecv(mpe)=ndrecv(mpe-1)+nrecv(mpe-1)
  end do
  nrecvthis=ndrecv(npes)

!    send ij indices first

  allocate(ijb(2,nrecvthis))
  call mpi_type_contiguous(2,mpi_integer4,mpi_ij,ierr)
  call mpi_type_commit(mpi_ij,ierr)
  call mpi_alltoallv(ija,nsend,ndsend,mpi_ij,ijb,nrecv,ndrecv,mpi_ij,my_comm,ierr)
  call mpi_type_free(mpi_ij,ierr)

      !   check to see that ijb indices are legal

        ibad=0
        jbad=0
        do i=1,nrecvthis
         if(ijb(1,i).lt.ips2.or.ijb(1,i).gt.ipe2) ibad=ibad+1
         if(ijb(2,i).lt.jps2.or.ijb(2,i).gt.jpe2) jbad=jbad+1
        end do

!    now send aspect tensors and put on new super grid

  allocate(aspectrbuf(6,kps:kpe,nrecvthis))
  call mpi_type_contiguous(6*(kpe-kps+1),mpi_real4,mpi_vstack,ierr)
  call mpi_type_commit(mpi_vstack,ierr)
  call mpi_alltoallv(aspectsbuf,nsend,ndsend,mpi_vstack, &
                     aspectrbuf,nrecv,ndrecv,mpi_vstack,my_comm,ierr)
  call mpi_type_free(mpi_vstack,ierr)
  deallocate(aspectsbuf)

!    save alltoall information in filter

  allocate(filter(1)%nrecvsup(0:npes-1))
  allocate(filter(1)%ndrecvsup(0:npes))
  allocate(filter(1)%nsendsup(0:npes-1))
  allocate(filter(1)%ndsendsup(0:npes))
  filter(1)%nrecvsup=nrecv
  filter(1)%ndrecvsup=ndrecv
  filter(1)%nsendsup=nsend
  filter(1)%ndsendsup=ndsend
  allocate(filter(1)%iasup(nsendthis))
  allocate(filter(1)%jasup(nsendthis))
  filter(1)%iasup(1:nsendthis)=ija(1,1:nsendthis)
  filter(1)%jasup(1:nsendthis)=ija(2,1:nsendthis)
  deallocate(ija)
  allocate(filter(1)%ibsup(nrecvthis))
  allocate(filter(1)%jbsup(nrecvthis))
  filter(1)%ibsup(1:nrecvthis)=ijb(1,1:nrecvthis)
  filter(1)%jbsup(1:nrecvthis)=ijb(2,1:nrecvthis)

  allocate(aspectex0(ims2:ime2,jms2:jme2,kms2:kme2,6))
  aspectex0=0.
  do i=1,nrecvthis
   do k=kps,kpe
    do kk=1,6
     aspectex0(ijb(1,i),ijb(2,i),k,kk)=aspectrbuf(kk,k,i)
    end do
   end do
  end do
  deallocate(aspectrbuf)
  deallocate(ijb)

!   extend aspect tensors in vertical first

  if(kde2.gt.kde) then
   do kk=1,6
    do k=kde+1,kde2
     do j=jps2,jpe2
      do i=ips2,ipe2
       aspectex0(i,j,k,kk)=aspectex0(i,j,kde,kk)
      end do
     end do
    end do
   end do
  end if
  if(kds2.lt.kds) then
   do kk=1,6
    do k=kds2,kds-1
     do j=jps2,jpe2
      do i=ips2,ipe2
       aspectex0(i,j,k,kk)=aspectex0(i,j,kds,kk)
      end do
     end do
    end do
   end do
  end if

!  extend in y one row at a time

  jdsnext=jds ; jdenext=jde ; loopmax=max(add_south,add_north)
  if(loopmax.gt.0) then
   do loop=1,loopmax
    call refresh_halo3y(aspectex0,1, &
                  ids2,ide2,jds2,jde2,1,(kme2-kms2)*6, &
                  ips2,ipe2,jps2,jpe2,1,(kme2-kms2)*6, & 
                  ims2,ime2,jms2,jme2,1,(kme2-kms2)*6, & 
                  inpes,jnpes,mype,npes,pe_of_injn2,in_of_i2,jn_of_j2)
    jdsnext=max(jdsnext-1,jds2)
    jdenext=min(jdenext+1,jde2)
    if(jdsnext.ge.jps2.and.jdsnext.le.jpe2) then
     j=jdsnext ; jp=jdsnext+1
     do kk=1,6
      do k=kms2,kme2
       do i=ips2,ipe2
        aspectex0(i,j,k,kk)=aspectex0(i,jp,k,kk)
       end do
      end do
     end do
    end if
    if(jdenext.ge.jps2.and.jdenext.le.jpe2) then
     j=jdenext ; jm=jdenext-1
     do kk=1,6
      do k=kms2,kme2
       do i=ips2,ipe2
        aspectex0(i,j,k,kk)=aspectex0(i,jm,k,kk)
       end do
      end do
     end do
    end if
   end do
  end if

!  extend in x one row at a time

  idsnext=ids ; idenext=ide ; loopmax=max(add_east,add_west)
  if(loopmax.gt.0) then
   do loop=1,loopmax
    call refresh_halo3x(aspectex0,1, &
                  ids2,ide2,jds2,jde2,1,(kme2-kms2)*6, &
                  ips2,ipe2,jps2,jpe2,1,(kme2-kms2)*6, & 
                  ims2,ime2,jms2,jme2,1,(kme2-kms2)*6, & 
                  inpes,jnpes,mype,npes,pe_of_injn2,in_of_i2,jn_of_j2)
    idsnext=max(idsnext-1,ids2)
    idenext=min(idenext+1,ide2)
    if(idsnext.ge.ips2.and.idsnext.le.ipe2) then
     i=idsnext ; ip=idsnext+1
     do kk=1,6
      do k=kms2,kme2
       do j=jps2,jpe2
        aspectex0(i,j,k,kk)=aspectex0(ip,j,k,kk)
       end do
      end do
     end do
    end if
    if(idenext.ge.ips2.and.idenext.le.ipe2) then
     i=idenext ; im=idenext-1
     do kk=1,6
      do k=kms2,kme2
       do j=jps2,jpe2
        aspectex0(i,j,k,kk)=aspectex0(im,j,k,kk)
       end do
      end do
     end do
    end if
   end do
  end if

!   finally copy into final array compatable with init_raf

  allocate(aspectex(7,ips2:ipe2,jps2:jpe2,kps2:kpe2))
  do kk=1,6
   do k=kps2,kpe2
    do j=jps2,jpe2
     do i=ips2,ipe2
      aspectex(kk,i,j,k)=aspectex0(i,j,k,kk)
     end do
    end do
   end do
  end do
  deallocate(aspectex0)
  do k=kps2,kpe2
   do j=jps2,jpe2
    do i=ips2,ipe2
     aspectex(7,i,j,k)=0.
    end do
   end do
  end do

!  now call init_raf on super grid

!???????following is temporary--if all works, then need to pass in true xyzvol
!????????  from calling program, and extrapolate them accordingly 

  allocate(xyzvolex(ips2:ipe2,jps2:jpe2,kps2:kpe2))
  do k=kps2,kpe2
   do j=jps2,jpe2
    do i=ips2,ipe2
     xyzvolex(i,j,k)=1.
    end do
   end do
  end do

           if(mype.eq.0) write(0,*)' at 3 in super_init_raf'
  call init_raf(aspectex,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord,filter,xyzvolex, &
             anormal,oldf, &
             ids2,ide2,jds2,jde2,kds2,kde2, &
             ips2,ipe2,jps2,jpe2,kps2,kpe2, &
             ims2,ime2,jms2,jme2,kms2,kme2, &
             inpes,jnpes,mype,npes,pe_of_injn2,in_of_i2,jn_of_j2)
           if(mype.eq.0) write(0,*)' at 4 in super_init_raf'
  deallocate(aspectex)
  deallocate(xyzvolex)
  deallocate(in_of_i2)
  deallocate(jn_of_j2)

!  transfer amp from supergrid to regular grid

  allocate(amp(ips:ipe,jps:jpe,kps:kpe))
  call super2regular(amp,filter(1)%amp,filter, &
             ips,ipe,jps,jpe,kps,kpe, &
             ids,ide,jds,jde,kds,kde, &
             ips,ipe,jps,jpe,kps,kpe, &
             ims,ime,jms,jme,kms,kme, &
             ips2,ipe2,jps2,jpe2,kps2,kpe2, &
             ids2,ide2,jds2,jde2,kds2,kde2, &
             ips2,ipe2,jps2,jpe2,kps2,kpe2, &
             ims2,ime2,jms2,jme2,kms2,kme2)
  deallocate(filter(1)%amp)
  allocate(filter(1)%amp(ips:ipe,jps:jpe,kps:kpe))
  filter(1)%amp=amp
  deallocate(amp)
           if(mype.eq.0) write(0,*)' at 5 in super_init_raf'

return
end subroutine super_init_raf

subroutine super_product(prod_8,f,g,filter)

  implicit none

  include 'filtertype.h'

  type(super_grid) f,g
  type(filter_cons) filter(7)

  real(8) prod_8,sup_prod8


  prod_8=sup_prod8(f%f,g%f, &
                   filter(1)%ims2,filter(1)%ime2, &
                   filter(1)%jms2,filter(1)%jme2, &
                   filter(1)%kms2,filter(1)%kme2, &
                   filter(1)%ips2,filter(1)%ipe2, &
                   filter(1)%jps2,filter(1)%jpe2, &
                   filter(1)%kps2,filter(1)%kpe2)

return
end subroutine super_product
function sup_prod8(f,g,ims,ime,jms,jme,kms,kme, &
                      ips,ipe,jps,jpe,kps,kpe)

  include 'mpif.h'
      include "my_comm.h"

  real(4) f(ims:ime,jms:jme,kms:kme)
  real(4) g(ims:ime,jms:jme,kms:kme)

  real(8) sumv_8(ips:ipe,jps:jpe)
  real(8) sum_8,sup_prod8

  sumv_8=0._8
  do k=kps,kpe
   do j=jps,jpe
    do i=ips,ipe
     sumv_8(i,j)=sumv_8(i,j)+f(i,j,k)*g(i,j,k)
    end do
   end do
  end do
  sum_8=0._8
  do j=jps,jpe
   do i=ips,ipe
    sum_8=sum_8+sumv_8(i,j)
   end do
  end do
  call mpi_allreduce(sum_8,sup_prod8,1,mpi_real8,mpi_sum,my_comm,ierr)

return
end function sup_prod8

subroutine super_raf(f,g,filter)

  INCLUDE 'filtertype.h'

  real(4) f(*)
  type(super_grid) g
  type(filter_cons) filter(7)

  call super_amp(f,filter, &
        filter(1)%ids,filter(1)%ide, &
        filter(1)%jds,filter(1)%jde, &
        filter(1)%kds,filter(1)%kde, &
        filter(1)%ips,filter(1)%ipe, &
        filter(1)%jps,filter(1)%jpe, &
        filter(1)%kps,filter(1)%kpe, &
        filter(1)%ims,filter(1)%ime, &
        filter(1)%jms,filter(1)%jme, &
        filter(1)%kms,filter(1)%kme, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn,filter(1)%in_of_i,filter(1)%jn_of_j)
  call regular2super(f,g%f,filter, &
        filter(1)%ims ,filter(1)%ime , &
        filter(1)%jms ,filter(1)%jme , &
        filter(1)%kms ,filter(1)%kme , &
        filter(1)%ids ,filter(1)%ide , &
        filter(1)%jds ,filter(1)%jde , &
        filter(1)%kds ,filter(1)%kde , &
        filter(1)%ips ,filter(1)%ipe , &
        filter(1)%jps ,filter(1)%jpe , &
        filter(1)%kps ,filter(1)%kpe , &
        filter(1)%ims ,filter(1)%ime , &
        filter(1)%jms ,filter(1)%jme , &
        filter(1)%kms ,filter(1)%kme , &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2, &
        filter(1)%ids2,filter(1)%ide2, &
        filter(1)%jds2,filter(1)%jde2, &
        filter(1)%kds2,filter(1)%kde2, &
        filter(1)%ips2,filter(1)%ipe2, &
        filter(1)%jps2,filter(1)%jpe2, &
        filter(1)%kps2,filter(1)%kpe2, &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2)

  call raf(g%f,filter, &
        filter(1)%ids2,filter(1)%ide2, &
        filter(1)%jds2,filter(1)%jde2, &
        filter(1)%kds2,filter(1)%kde2, &
        filter(1)%ips2,filter(1)%ipe2, &
        filter(1)%jps2,filter(1)%jpe2, &
        filter(1)%kps2,filter(1)%kpe2, &
        filter(1)%ims2,filter(1)%ime2, &
        filter(1)%jms2,filter(1)%jme2, &
        filter(1)%kms2,filter(1)%kme2, &
        filter(1)%inpes,filter(1)%jnpes,filter(1)%mype,filter(1)%npes, &
        filter(1)%pe_of_injn2,filter(1)%in_of_i2,filter(1)%jn_of_j2)

return
end subroutine super_raf
subroutine what_color_is(i1,i2,i3,color)
implicit none
integer,intent(IN):: i1,i2,i3
integer,intent(OUT):: color
integer,dimension(3):: v,vh,vh2,b124
logical same
integer:: itest
data b124/1,2,4/
!----------------------------------------------------------------
vh(1)=i1; vh(2)=i2; vh(3)=i3
do itest=1,20
   v=vh; vh=v/2; vh2=vh*2; if(.NOT.same(vh2,v,3))exit
enddo
v=modulo(v,2)
color=dot_product(v,b124)
end subroutine what_color_is
 
function same(v1,v2,n)
logical same
integer,intent(IN):: n
integer,dimension(n),intent(IN):: v1,v2
integer i
same=.true.
do i=1,n; if(v1(i) /= v2(i))same=.false.; enddo
end function same

SUBROUTINE hbnrf1i_out(a,nol,lnf,bnf,                                       &
       ids,ide,                                                             &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
! Horizontal basic inhomogeneous recursive filter, 
! 1-dimensional, active index i
!============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN   ) :: nol
  INTEGER, INTENT(IN   ) :: ids,ide
  INTEGER, INTENT(IN   ) :: ims,ime
  INTEGER, INTENT(IN   ) :: its,ite

  REAL, DIMENSION(ims:ime),                       &
           INTENT(INOUT) :: a
  REAL, DIMENSION(ims:ime),                       &
           INTENT(IN   ) :: bnf
  REAL, DIMENSION(nol, ims:ime),                  &
           INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER                :: i,l,nola
!============================================================================
DO i=its,ite
   a(i)=bnf(i)*a(i)
ENDDO
DO i=its+1,ite
   nola=MIN(nol,i-its)
   DO l=1,nola
      a(i)=a(i)+lnf(l,i)*a(i-l)
   ENDDO
ENDDO
DO i=ite-1,its,-1
   nola=MIN(nol,ite-i)
   DO l=1,nola
      a(i)=a(i)+lnf(l,i+l)*a(i+l)
   ENDDO
ENDDO
DO i=its,ite
   a(i)=bnf(i)*a(i)
ENDDO
END SUBROUTINE hbnrf1i_out

subroutine coefrf_out(aspect,binomial,n,beta,alpha)

  !  compute recursion constants for one string of length n

  IMPLICIT NONE

  INTEGER(4), INTENT(IN) :: n

  REAL(8), INTENT(IN) :: &
            binomial

  REAL(4), DIMENSION( n ), INTENT(IN) :: &
            aspect
  REAL(4), DIMENSION( n ), INTENT(OUT) :: &
            alpha,beta


  REAL(8) alphathis,athis,betathis,bthis,cthis,dlast,dthis

  integer(4) kk


  dlast=0._8
  athis=1._8+.5_8*(aspect(1)+aspect(2))*binomial
  bthis=-.5_8*(aspect(1)+aspect(2))*binomial
  cthis=sqrt(athis)
  dthis=bthis/cthis
  betathis=1._8/cthis
  alphathis=-dlast*betathis
  alpha(1)=alphathis
  beta(1)=betathis
  dlast=dthis
  if(n.gt.2) then
   do kk=2,n-1
    athis=1._8+.5_8*(aspect(kk-1)+2._8*aspect(kk)+aspect(kk+1))*binomial
    bthis=-.5_8*(aspect(kk)+aspect(kk+1))*binomial
    cthis=sqrt(athis-dlast**2)
    dthis=bthis/cthis
    betathis=1._8/cthis
    alphathis=-dlast*betathis
    alpha(kk)=alphathis
    beta(kk)=betathis
    dlast=dthis
   end do
  end if
  athis=1._8+.5_8*(aspect(n-1)+aspect(n))*binomial
  cthis=sqrt(athis-dlast**2)
  betathis=1._8/cthis
  alphathis=-dlast*betathis
  alpha(n)=alphathis
  beta(n)=betathis

return
end subroutine coefrf_out
