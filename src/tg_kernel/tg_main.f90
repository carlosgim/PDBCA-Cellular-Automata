!--------------------------------------------------------------------------
!REFERENCIA
! 
!---------------------------------------------------------------------------
!DATOS
!---------------------------------------------------------------------------
program tg_kernel
  use tg_functions; use constants

implicit none
integer, parameter :: dp = selected_real_kind(15,307)
!real(kind=dp), allocatable :: x(:), x_old(:)
integer, allocatable :: x(:), x_old(:)
integer :: i, j, step_num, n

! Set initial parameters
n = 80; step_num = 2

! Allocate the size of the array
allocate( x(0:n+1)); allocate( x_old(0:n+1))

x(0:n+1) = 0
x(40)    = 1

step_numbers: do i = 1, step_num

    x_old(0:n+1) = x(0:n+1)

    size_array: do j = 1, n

        if (( x_old(j-1) == 0 .and. &
              x_old(j)   == 0 .and. &
              x_old(j+1) == 1 )     &
              .or. &
            ( x_old(j-1) == 0 .and.  &
              x_old(j)   == 1 .and.  &
              x_old(j+1) == 0 )      &
               .or. &
            ( x_old(j-1) == 0 .and.  &
              x_old(j)   == 1 .and.  &
              x_old(j+1) == 0 )      &
                .or. &
            ( x_old(j-1) == 1 .and.  &
              x_old(j)  == 0 .and.   &
              x_old(j+1) == 1 ) ) then
              x(j) = 1
        else
              x(j) = 0
        end if

    end do size_array

! Boundary
    
    x(0) = x(n); x(n+1) = x(1)
    
! Print
    write(*,*) 'step', step_num
    write(*,'(2I3)') x(1:n)              
    
end do step_numbers

      stop
end program tg_kernel
     
  
