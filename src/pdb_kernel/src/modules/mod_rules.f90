!=============================================================================
!This module computes a set of rules for the cellular automata. Till the moment
!it's just a sketch.
! 
!=============================================================================
! Name: Santiago Walterio, Wiedmann
! Date: 
!=============================================================================
module mod_rules

implicit none
public  :: rules_init

contains

!Set initial parameters
subroutine rules_init

implicit none
real :: prob_m, prob_ij, prob_0, prob_e, acid_0, acid_c, acid_ij, grwth_0
real :: grwth_c, grwth_ij, x_rnd, y_rnd
integer :: i, j, k, l, m, n, t, time, step_num
real, allocatable, dimension(:,:,:) :: state
real, allocatable, dimension(:,:) :: temp

print*, "introduce the state matrix dimension, please"
read*, n
print*, "introduce the step number, please "
read*, step_num

allocate(state(n,n,4))
allocate(temp(n,n))

fill_row: do l = 1,n

    fill_columm: do m = 1,n

                     temp(l,m) = 0

     end do fill_columm

end do fill_row

!Set the rules
step_numbers: do t = 1, step_num

    k = 0

    k_reach: do while (k <= n**2) 

        x_rnd = rand()*n
        y_rnd = rand()*n
        i = x_rnd
        j = y_rnd

        if ( temp(i,j) == 0 ) then
            temp(i,j) = 1
            k = k + 1
!========================================================================================
!                                      LIVING CELLS
!========================================================================================

            if      ( state(i,j,4) == 2       .and. &
                     acid_ij <= acid_0 )      then
                      state(i,j,4) = 1
                      state(i,j,3) = grwth_0

            else if ( state(i,j,4) == 2       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_0 )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 3
                      state(i,j,3) = grwth_0

            else if ( state(i,j,4) == 3       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_0 )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 4
                      state(i,j,3) = grwth_0

            else if ( state(i,j,4) == 4       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_0 )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 5
                      state(i,j,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i-1,j-1,4) == 1 )  then
                      state(i,j,4) = 2
                      state(i-1,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i-1,j-1,3) = grwth_0 
  
            else if ( state(i,j,4) == 5       .and. &
                      state(i,j-1,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i,j-1,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i+1,j-1,4) == 1 )   then
                      state(i,j,4) = 2
                      state(i+1,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i+1,j-1,3) = grwth_0 

            else if ( state(i,j,4) == 5       .and. &
                      state(i-1,j,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i-1,j,4) = 2
                      state(i,j,3) = grwth_0
                      state(i-1,j,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i+1,j,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i+1,j,4) = 2
                      state(i,j,3) = grwth_0
                      state(i+1,j,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i-1,j+1,4) == 1 )   then
                      state(i,j,4) = 2
                      state(i-1,j+1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i-1,j+1,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i,j+1,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i,j+1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i,j+1,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i+1,j+1,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i+1,j+1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i+1,j+1,3) = grwth_0

                 
            else 
                      state(i,j,4) = 2
                      state(i,j,3) = grwth_0

            end if
!========================================================================================
!                                      CANCER CELLS
!========================================================================================
            if      ( state(i,j,4) == 6       .and. &
       	              acid_ij <= acid_c )      then
                      state(i,j,4) = 0
                      state(i,j,3) = grwth_ij

            else if ( state(i,j,4) == 6       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_c )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 7
                      state(i,j,3) = grwth_c

            else if ( state(i,j,4) == 7       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_c )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 8
                      state(i,j,3) = grwth_c

            else if ( state(i,j,4) == 8       .and. &
                    ( state(i-1,j-1,4) == 1    .or. &
                      state(i,j-1,4) == 1      .or. &
                      state(i+1,j-1,4) == 1    .or. &
                      state(i-1,j,4) == 1      .or. &
                      state(i+1,j,4) == 1      .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i-1,j+1,4) == 1    .or. &
                      state(i,j+1,4) == 1      .or. &
                      state(i+1,j+1,4) == 1 ) .and. &
                    ( acid_ij >= acid_c )     .and. &
                    ( prob_0 <= prob_ij ))      then
                      state(i,j,4) = 9
                      state(i,j,3) = grwth_c

            else if ( state(i,j,4) == 5       .and. &
                      state(i-1,j-1,4) == 1 )  then
                      state(i,j,4) = 2
                      state(i-1,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i-1,j-1,3) = grwth_0 
  
            else if ( state(i,j,4) == 5       .and. &
                      state(i,j-1,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i,j-1,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i+1,j-1,4) == 1 )   then
                      state(i,j,4) = 2
                      state(i+1,j-1,4) = 2
                      state(i,j,3) = grwth_0
                      state(i+1,j-1,3) = grwth_0 

            else if ( state(i,j,4) == 5       .and. &
                      state(i-1,j,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i-1,j,4) = 2
                      state(i,j,3) = grwth_0
                      state(i-1,j,3) = grwth_0

            else if ( state(i,j,4) == 5       .and. &
                      state(i+1,j,4) == 1 )     then
                      state(i,j,4) = 2
                      state(i+1,j,4) = 2
                      state(i,j,3) = grwth_0
                      state(i+1,j,3) = grwth_0

            else if ( state(i,j,4) == 9       .and. &
                      state(i-1,j+1,4) == 1 )   then
                      state(i,j,4) = 6
                      state(i-1,j+1,4) = 6
                      state(i,j,3) = grwth_c
                      state(i-1,j+1,3) = grwth_c

            else if ( state(i,j,4) == 9       .and. &
                      state(i,j+1,4) == 1 )     then
                      state(i,j,4) = 6
                      state(i,j+1,4) = 26
                      state(i,j,3) = grwth_c
                      state(i,j+1,3) = grwth_c

            else if ( state(i,j,4) == 9       .and. &
                      state(i+1,j+1,4) == 1 )     then
                      state(i,j,4) = 6
                      state(i+1,j+1,4) = 6
                      state(i,j,3) = grwth_c
                      state(i+1,j+1,3) = grwth_c
            end if 
!========================================================================================
!                                      NECTROTIC CELLS
!========================================================================================
            if ( state(i,j,4) == 0       .and. &
               ( state(i-1,j-1,4) == 1    .or. &
                 state(i,j-1,4) == 1      .or. &
                 state(i+1,j-1,4) == 1    .or. &
                 state(i-1,j,4) == 1      .or. &
                 state(i+1,j,4) == 1      .or. &
                 state(i,j+1,4) == 1      .or. &
                 state(i-1,j+1,4) == 1    .or. &
                 state(i,j+1,4) == 1      .or. &
                 state(i+1,j+1,4) == 1 ) .and. &
               ( prob_0 <= prob_e ))      then
                 state(i,j,4) = 1
                 state(i,j,3) = grwth_ij
             end if



!========================================================================================
!                                      
!========================================================================================
        else 

            k = k

        end if

    end do k_reach      

end do step_numbers

end subroutine rules_init 
end module mod_rules 
