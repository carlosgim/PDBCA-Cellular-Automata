module mod_functions
!================================================================================!
! Initializacion of all variables 
!================================================================================!
!
!   This module was develop by: Carlos A. Gimenez
!
!                                  Natural and Exact Science Faculty
!                                  Northeastern University of Argentina,
!                                  Corrientes, Argentina
!
! 
!   31 of may 2016
!
!================================================================================!

  implicit none

  public  :: test


  type :: tg_variables
    integer :: i
    character(len=50) :: title
  end type tg_variables

contains

  subroutine test

  !==============================================================================!
  !           Initialize module                                                  !
  !==============================================================================!

!  ntest=21
  write(*,*) 'ntest'
  end subroutine test


end module mod_functions
