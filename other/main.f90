program main
    USE SF_OPTIMIZE, ONLY: fmin_bfgs
    implicit none
! interface 
!     function fxy2(x,y)
!         real(kind=8) :: fxy2
!         real(kind=8) :: x,y
!     end function
! end interface
! interface 
!     function fx2(x)
!         real(kind=8),dimension(:) :: x
!         real(kind=8) :: fx2
!     end function
!     function dfx2(x)
!         real(kind=8),dimension(:) :: x
!         real(kind=8),dimension(size(x)) :: dfx2
!     end function
! end interface
interface
    function fpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8)  :: fpolym
    end function fpolym
    function dfpolym(x)
        real(kind=8),dimension(:)  :: x
        real(kind=8),dimension(size(x))  :: dfpolym
    end function dfpolym
!   subroutine fmin_bfgs(func,grad,x,l,u,nbd,factr,pgtol,iprint,nloop)
!     interface
!        function func(y)
!          real(8),dimension(:) :: y
!          real(8) :: func
!        end function func
!     end interface
!     interface
!        function grad(y)
!          real(8),dimension(:) :: y
!          real(8),dimension(size(y)) :: grad
!        end function grad
!     end interface
!     integer,optional                          :: iprint,nloop
!     real(8),dimension(:),intent(inout)        :: x
!     real(8),optional                          :: factr, pgtol
!     integer,dimension(:),intent(in), optional :: nbd
!     real(8),dimension(:),intent(in), optional :: l,u
!   end subroutine fmin_bfgs
end interface
  real(kind=8) :: x0, y0
  real(kind=8) :: param0(2)
  integer      :: iter,nbd0(2)
  real(kind=8) :: l0(2),u0(2)
  real(kind=8) :: fret
  print *,"I pop here 1."
  x0=0.0;y0=0.0
  param0(1) = x0
  param0(2) = y0
  nbd0 = (/2,2/)
  l0   = (/-10.0,-10.0/)
  u0   = (/10.0 , 10.0/)
  print *,"I pop here 1."
! call fmin_cg_df(param0,fpolym,dfpolym,iter,fret) 
! call bfgs_with_grad(fpolym,dfpolym,param0,nbd=nbd0,l=l0,u=u0)
! call fmin_bfgs(fpolym,dfpolym,param0,nbd=nbd0,l=l0,u=u0)
  call fmin_bfgs(fpolym,dfpolym,param0,nbd=nbd0,l=l0,u=u0)
  print *,' '
  print *,' The minimum points are:'
  print *,' '
  write(*,10) param0
  print *,' '
  print *,' Corresponding function values:'
  print *,' '
  write(*,20) 
  print *,' '
  print *,' '
  print *,' Iter Number Is:'
  print *,' '
  write(*,30) 
  print *,' '
  print *,' '
  print *,' '
  stop
  10 format('  param0=',2(F10.6,2x))
  20 format('  F=',F10.6)
  30 format('  Iter=',I5)
end program main

function fxy2(x,y)
    real(kind=8)  :: fxy2
    real(kind=8)  :: x,y
    fxy2 = x*x + y*y
    return
end function
function dfxy2(x,y)
    real(kind=8)  :: dfxy2(2)
    real(kind=8)  :: x,y
    dfxy2(1) = 2*x
    dfxy2(2) = 2*y
    !
    return
end function
function fx2(x)
    real(kind=8),dimension(:)  :: x
    real(kind=8)  :: fx2
    fx2 = sum(x*x) 
    !
    return
end function
function dfx2(x)
    real(kind=8),dimension(:)  :: x
    real(kind=8),dimension(size(x))  :: dfx2
    dfx2 = 2.0*x
    !
    return
end function
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
