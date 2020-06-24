!HYBRD INTERFACE:
!solve N nonlinear equations in N unknowns
!numerical jacobian 


!HYBRJ INTERFACE:
!solve N nonlinear equations in N unknowns
!user supplied analytic jacobian 
subroutine fsolve_hybrj_func(func,dfunc,x,tol,info,check)
  interface
     function func(x)
       real(8),dimension(:),intent(in) :: x
!      real(8),dimension(:)            :: x
       real(8),dimension(size(x))      :: func
     end function func
     !
     function dfunc(x)
       real(8),dimension(:),intent(in)    :: x
!      real(8),dimension(:)               :: x
       real(8),dimension(size(x),size(x)) :: dfunc
     end function dfunc
  end interface
  real(8),dimension(:)               :: x      
  real(8),optional                   :: tol
  integer,optional                   :: info
  real(8)                            :: tol_
  integer                            :: info_
  logical,optional                   :: check
  logical                            :: check_
  integer                            :: n
  real(8),dimension(size(x))         :: fvec
  real(8),dimension(size(x),size(x)) :: fjac
  tol_ = 1.d-15;if(present(tol))tol_=tol
  check_=.true.;if(present(check))check_=check
  n=size(x)
  call hybrj1(fsolve_hybrj1_func2sub,n,x,fvec,fjac,n,tol_,info_)
  if(present(info))info=info_
  if(check_)then
     include "fsolve_error.h90"
  endif
contains
  subroutine fsolve_hybrj1_func2sub(n,x,fvec,fjac,ldfjac,iflag)
    integer ::  n
    real(8) ::  x(n)
    real(8) ::  fvec(n)
    integer ::  ldfjac
    real(8) ::  fjac(ldfjac,n)
    integer ::  iflag
    if(iflag==1)then
       fvec = func(x)
    elseif(iflag==2)then
       fjac = dfunc(x)
    endif
    if(iflag<0)stop "FSOLVE_HYBRJ1_func2sub ERROR: iflag < 0 "
  end subroutine fsolve_hybrj1_func2sub
end subroutine fsolve_hybrj_func

