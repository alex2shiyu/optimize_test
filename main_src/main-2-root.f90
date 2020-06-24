program main
!   USE SF_OPTIMIZE, ONLY: fmin_bfgs
    use SF_OPTIMIZ,  ONLY: fsolve
!   use OPTIMIZE_ROOT_FINDING, only : fsolve
    implicit none
interface
    function fpolym(x)
        real(kind=8),dimension(:),intent(in)  :: x
!       real(kind=8),dimension(:)             :: x
        real(kind=8),dimension(size(x))  :: fpolym
    end function fpolym
    function dfpolym(x)
        real(kind=8),dimension(:),intent(in)  :: x
!       real(kind=8),dimension(:)             :: x
        real(kind=8),dimension(size(x),size(x))  :: dfpolym
    end function dfpolym
end interface
  real(kind=8),allocatable :: param0(:)
!!!!!!(kind=8)             :: param0(2)
  integer,allocatable      :: nbd0(:)
  real(kind=8),allocatable :: l0(:),u0(:)
  allocate(nbd0(2),l0(2),u0(2),param0(2))
! allocate(nbd0(2),l0(2),u0(2))
  param0 = [0d0,0.5d0]
  nbd0   = [2,2]
  l0     = [-10d0,-10d0]
  u0     = [10d0,10d0]
  print *,"I pop here 1."
! call fmin_bfgs(fpolym,dfpolym,param0,nbd=nbd0,l=l0,u=u0)
! call fmin_bfgs(fpolym,dfpolym,x=param0)!,l=l0,u=u0,nbd=nbd0)
  call fsolve(fpolym,dfpolym,x=param0,tol=1.d-10)
  print *,"the minimum point:",param0
  deallocate(l0,u0,param0)
! deallocate(l0,u0)
end program main

function fpolym(x)
    real(kind=8),dimension(:),intent(in) :: x
!   real(kind=8),dimension(:)            :: x
    real(kind=8),dimension(size(x))              :: fpolym
!   fpolym(1) = (x(1)-1) * (x(1)-1) + (x(2)-2) * (x(2)-2)     
!   fpolym(2) = (x(2)-2) * (x(1)-1) + (x(2)-2) * (x(2)-2)     
    fpolym(1) = (x(1)-1) * (x(1)-1) 
    fpolym(2) = (x(2)-2) * (x(2)-2)     
    !
    return
end function fpolym
function dfpolym(x)
    real(kind=8),dimension(:),intent(in)       :: x
!   real(kind=8),dimension(:)                  :: x
    real(kind=8),dimension(size(x),size(x)) :: dfpolym
    dfpolym(1,1) = 2*(x(1)-1)
    dfpolym(1,2) = 0
    dfpolym(2,1) = 0
    dfpolym(2,2) = 2*(x(2)-2)
    !
    return
end function dfpolym
