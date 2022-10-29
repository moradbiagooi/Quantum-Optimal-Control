SUBROUTINE sub_pr_b() 
!use mod_OCT,only:alloc ,int_t,alpha_k,exact,sai_,khy_,khy1_,khy0_,sai_t,khy_t,m_s_target,ct_,mu_,energy,t,t_end,dt
use mod_OCT
implicit none
      
integer::i0 ,j0


	
	 
	call sub_BV_F()
		 	  
	 
	do j0=alloc,0,-1 ! Backward propagation-zeroth step
	
		
		do i0=1,n
		
		if (exact_TR==0) then
		sai_(i0)=sai_t(i0,j0) ! not trick
		else
		sai_(i0)=m_s_target(i0,j0) ! My trick
		endif
    			
		khy0_(i0)=khy_(i0)
        ct_(i0)=m_s_target(i0,j0)
		end do 
		f_weight= m_f_weight(j0)

		
		call sub_pr_b_cn()
        	 									   
	
		khy_=khy1_
		do i0=1,n
        khy_t(i0,j0-1)=khy_(i0)
	    end do 
		

    	
		
    end do  !do while(t=>0)
  end