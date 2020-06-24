program main
  USE SF_OPTIMIZE, ONLY: fmin_cg
  USE SF_LINALG,   ONLY: eye
  implicit none
interface
    function fpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8)  :: fpolym
    end function
    function dfpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8),dimension(size(x))  :: dfpolym
    end function
end interface
  real(kind=8) :: x0, y0
  real(kind=8) :: param0(2),a(3,3)
  integer      :: iter
  real(kind=8) :: fret
  x0=0.0;y0=0.0
  param0(1) = x0
  param0(2) = y0
  print *,"I pop here 1."
  ! test for eye in module SF_LINALG
  a=eye(3)
  ! test for fmin_cg in module SF_OPTIMIZE
  call fmin_cg(param0,fpolym,dfpolym,iter,fret) 
  print *,"minimum point:",param0

end program main

function fpolym(x)
    real(kind=8),dimension(:)  :: x
    real(kind=8)  :: fpolym
    fpolym = 2.0*x(1)*x(1) + 3.0*x(1)*x(2) + 7.0*x(2)*x(2) + 8.0*x(1) + 9.0*x(2) + 10.0 
    !
    return
end function
function dfpolym(x)
    real(kind=8),dimension(:)  :: x
    real(kind=8),dimension(size(x))  :: dfpolym
    dfpolym(1) = 4.0*x(1)+3.0*x(2)+8.0
    dfpolym(2) = 3.0*x(1)+14.0*x(2)+9.0
    !
    return
end function
