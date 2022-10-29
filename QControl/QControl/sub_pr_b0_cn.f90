subroutine sub_pr_b0_cn()
!use mod_OCT,only:sai_,khy_,khy1_,ct_,H,H_0,H1,H2,H2_inv,H_t,Id_,mu_,t_end,dt,f_weight,alpha_k ,n,errorflag,i,e_c,t_sum
use mod_OCT
implicit none
integer::i0,i1
        




		e_c=0.d0

H=H_0-mu_*e_c

call sub_hermit_maker()
	
H1=Id_+(i*dt/2.)*H	! Backward
H2=Id_-(i*dt/2.)*H  ! Backward


CALL sub_invert_matrix(H2, H2_inv, n, ErrorFlag)	  

H_t=matmul(H2_inv,H1)
	
!-----------------------  ! Crank-Nicolson
do i0=1,n
t_sum=0.d0
do i1=1,n
!t_sum=t_sum+(real(H_t(i0,i1))*khy_(i1))
t_sum=t_sum+(H_t(i0,i1)*khy_(i1))
end do
khy1_(i0)=t_sum
end do


!-----------------------

do i0=1,n
t_sum=0.d0
do i1=1,n
t_sum=t_sum+ct_(i0)*ct_(i1)*sai_(i1)
end do
if (exact_TR==0) then
khy1_(i0)=khy1_(i0)+(dt/t_end)*t_sum*f_weight
else
khy1_(i0)=khy1_(i0)+(dt/t_end)*ct_(i0)*f_weight  !my trick
endif

end do


	

    	

end 
