program main
    USE SF_OPTIMIZE, ONLY: fmin_bfgs
    implicit none
interface
    function fpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8)  :: fpolym
    end function fpolym
    function dfpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8),dimension(size(x))  :: dfpolym
    end function dfpolym
end interface
  real(kind=8),allocatable :: param0(:)
  integer,allocatable      :: nbd0(:)
  real(kind=8),allocatable :: l0(:),u0(:)
  allocate(nbd0(2),param0(2),l0(2),u0(2))
  param0 = [0d0,0d0]
  nbd0   = [2,2]
  l0     = [-10d0,-10d0]
  u0     = [10d0,10d0]
  print *,"I pop here 1."
! call fmin_bfgs(fpolym,dfpolym,param0,nbd=nbd0,l=l0,u=u0)
  call fmin_bfgs(fpolym,dfpolym,x=param0)!,l=l0,u=u0,nbd=nbd0)
  print *,"the minimum point:",param0
  deallocate(l0,u0,param0)
end program main

function fpolym(x)
    real(kind=8),dimension(:) :: x
    real(kind=8)              :: fpolym
    fpolym = 2.0*x(1)*x(1) + 3.0*x(1)*x(2) + 7.0*x(2)*x(2) + 8.0*x(1) + 9.0*x(2) + 10.0 
    !
    return
end function
function dfpolym(x)
    real(kind=8),dimension(:)       :: x
    real(kind=8),dimension(size(x)) :: dfpolym
    dfpolym(1) = 4.0*x(1)+3.0*x(2)+8.0
    dfpolym(2) = 3.0*x(1)+14.0*x(2)+9.0
    !
    return
end function
