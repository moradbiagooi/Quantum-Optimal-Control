subroutine sub_pr_b0()
!use mod_OCT,only:alloc ,f_weight,int_t,exact,sai_,khy_,khy1_,khy0_,sai_t,khy_t,m_s_target,m_f_weight,ct_,mu_,energy,t,t_end,dt
use mod_OCT
implicit none      
integer::i0,j0 ,k0

!-----------------------Zeroth step propagation-START
! eq :(?)

	
	full_Yield=0.d0
	call sub_BV_F() 
		 
	do j0=alloc,0,-1 ! Backward propagation-zeroth step
		
		f_weight= m_f_weight(j0)
			
		call sub_pr_b0_cn()
		
	 									   
		khy_=khy1_
		do i0=1,n
        khy_t(i0,j0-1)=khy_(i0)
	    end do 
    	
		
		full_Yield=full_Yield+real(sum((m_s_target(:,j0)*m_s_target(:,j0))*conjg(m_s_target(:,j0)*m_s_target(:,j0))))
end	do
print*, ' Zeroth propagation is done.'
print*,' '
end