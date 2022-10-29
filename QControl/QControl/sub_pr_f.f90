subroutine sub_pr_f() 
!use mod_OCT,only:alloc ,exact,sai_,khy_,sai1_,sai0_,sai_t,khy_t,mu_,energy,t,t_end,dt,alpha_k,e_k
use mod_OCT
implicit none
integer::i0,j0 



		 
call sub_BV_I()
	 

Yield=0.d0 
do j0=0,alloc ! Forward propagation
		       
		do i0=1,n
		
		if (exact_TR==0) then
		!sai0_(i0)=sai_t(i0,j0)	   ! doubt1 ! not trick
		sai0_(i0)=sai_(i0)			  !	 doubt2 ! not trick
        else
		sai0_(i0)=m_s_target(i0,j0) ! My trick
		endif
    	
		
		
		khy_(i0)=khy_t(i0,j0)
		Yield=Yield+real((m_s_target(i0,j0)*sai_t(i0,j0))*conjg(sai_t(i0,j0)*m_s_target(i0,j0)))
		
		end do 


		call sub_pr_f_cn()
		     
		sai_=sai1_
		do i0=1,n										        
        sai_t(i0,j0+1)=sai_(i0)
		m_field(j0+1)=real(e_c)
		end do 


        
        
	end do  !do while(t<t_end)
	
end