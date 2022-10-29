subroutine sub_target_pr()
!use mod_OCT,only:alloc,int_t,m_s_target,ct_,t_end,t,dt
use mod_OCT
implicit none
integer::i0	,j0				  

print*, ' '
print*, ' -----------------------------------------------'
print*, ' '

! -----------------' Processing target operator'
print*,' '
print*,' Processing target operator, please wait'


do i0=0,alloc    ! making m_f_weight
		t=t_end*(real(i0)/real(alloc))
						
		call sub_target_f()
		do j0=1,n
		m_s_target(j0,i0)=ct_(j0)
		end do
							 
		if (mod(i0,1000)==0) print*,i0,' of ',alloc
end do 

print*, ' Target operator construction is done.'
print*,' '

print*, ' '
print*, ' -----------------------------------------------'
print*, ' '

end