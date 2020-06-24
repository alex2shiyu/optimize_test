MODULE SF_OPTIMIZ
  private

  interface fsolve
     module procedure :: fsolve_hybrj_func
  end interface fsolve


  !Multidimensional
  !General nonlinear solvers:
  public :: fsolve              !
  
  real(8)                         :: df_eps=tiny(1d0)
  
contains

    
! Interface to MINPACK hybrd/hybrj: FSOLVE
  include "fsolve.f90"

END MODULE SF_OPTIMIZ
