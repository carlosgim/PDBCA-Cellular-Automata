!=============================================================================
!This module computes a set of rules for the cellular automata. Till the moment
!is just an sketch
! 
!=============================================================================
! Name: Santiago Walterio, Wiedmann
! Date: 
!=============================================================================
program mod_rules

implicit none
real :: prob_m, prob_ij, prob_0, acid_0, acid_ij, grwth_0, grwth_ij, 
integer :: i, j, l, m, k, t, n, step_num,
real, dimension(:,:,4) :: state
real, dimension(:,:) :: temp

!Set initial parameters
print*, "introduce the state matrix dimension, please "
read*, n
print*, "introduce the step number, please "
read*, step_num
allocate (state(n,n,4))
allocate (temp(n,n))

do l = 1,n

    do m = 1,n

        temp(l,m) = 0

    end do

end do

!Set the rules
step_numbers: do t = 1, step_num

    k = 0

    while k <= n**2 then

        x_rnd = random(1:n)
        y_rnd = random(1:n)
        x_rnd = i
        y_rnd = j

        if ( temp(i,j) == 0 ) then
            temp(i,j) = 1
            k = k + 1
!========================================================================================
!                                      LIVING CELLS
!========================================================================================

	    if      ( state(i,j,4) == 2       .and. &
       	              acid_ij <= acid_0 )      then
                      state(i,j,4) = 0
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
                    ( prob_0 <= prob_ij )      then
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
                     ( prob_0 <= prob_ij )      then
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
                      ( prob_0 <= prob_ij )      then
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
!========================================================================================
!                                      CANCER CELLS
!========================================================================================





!========================================================================================
!                                      
!========================================================================================
              else 

                  k = k

              end if



end do
     
