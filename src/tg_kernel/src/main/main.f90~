! (c) Carlos A. Gimenez
! licensed under the GNU General Public License V3
! include 'oprad.f90'
program main

use readinput
!================================================================================!
! Interface program
!================================================================================!
!
!   This module was develop by: Carlos A. Gimenez
!
!                                  Natural and Exact Science Faculty
!                                  Northeastern University of Argentina,
!                                  Corrientes, Argentina
!
! 
!   01 of jan 2016
!
!================================================================================!


implicit none
 integer :: i

! Call to read input module
write(*,*) 'Please check the INPUT.inp file'
write(*,*) 'Then push enter'
read(*,*)

call readeig_mod


if(kindofwork.eq.'**OPRAD') then
 write(*,*) 'Running OpenRadiac Program'
 call openradiac    
elseif(kindofwork.eq.'**STOPPW') then
 write(*,*) 'Stopping Power Program - Check the cgimsoft.out file'
 call stopping_power
  
else
 write(*,*) 'Something goes wrong in the INPUT file'
endif


  stop
end program main
     
  
