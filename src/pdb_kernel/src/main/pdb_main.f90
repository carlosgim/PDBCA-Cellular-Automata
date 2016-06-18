!--------------------------------------------------------------------------
!REFERENCIA
! 
!---------------------------------------------------------------------------
!DATOS
!---------------------------------------------------------------------------
program pdb_main
  use mod_functions
  use mod_constants
  use mod_readinp

implicit none
integer, parameter :: dp = selected_real_kind(15,307)
!real(kind=dp), allocatable :: x(:), x_old(:)
character, allocatable :: x(:,:), x_old(:,:)
integer :: i, j, k, step_num, n

! Set initial parameters
n = 80; step_num = 50

! Allocate the size of the array
allocate( x(0:n+1, 1)); allocate( x_old(0:n+1, 1))

x(0:n+1, 1) = ' '
x(40, 1)    = '*'

step_numbers: do i = 1, step_num

    x_old(0:n+1, 1) = x(0:n+1, 1)

    width_array: do j = 1, n

        long_array: do k = 1, 1

            if (( x_old(j-1, k) == ' ' .and. &
                  x_old(j, k)   == ' ' .and. &
                  x_old(j+1, k) == '*' )     &
                  .or. &
                ( x_old(j-1, k) == '*' .and.  &
                  x_old(j, k)   == ' ' .and.  &
                  x_old(j+1, k) == ' ' )      &
                  .or. &
                ( x_old(j-1, k) == ' ' .and.  &
                 x_old(j, k)   == '*' .and.  &
                 x_old(j+1, k) == ' ' )      &
                  .or. &
               ( x_old(j-1, k) == '*' .and.  &
                 x_old(j, k)  ==  ' ' .and.   &
                 x_old(j+1, k) == ' ' ) ) then
                 x(j, k) = '*'
            else
                 x(j, k) = ' '
            end if

        end do long_array

    end do width_array

! Boundary
    
    x(0, k) = x(n, k); x(n+1, k) = x(1, k)
    
! Print
    write(*,*) 'step', step_num
    write(*,'(80a)') x(1:n, k)              
    
end do step_numbers

      stop
end program pdb_main
     
  
