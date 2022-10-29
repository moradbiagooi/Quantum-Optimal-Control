subroutine sub_pr_f_cn()
!use mod_OCT,only:sai_,khy_,sai1_,sai0_,H,H_0,H1,H2,H2_inv,H_t,Id_,errorflag,dt,mu_,alpha_k,e_k,n,i,e_c,t_sum
use mod_OCT
implicit none
integer::i0,i1




			   
		        
		e_c=0.d0
        do i0=1,n
         	do i1=1,n
 			e_c=e_c+conjg(khy_(i0))*mu_(i0,i1)*sai0_(i1) !eq (91,92,93) , G(e_k(t))=e_k(t)
        	end do
        end do
        
		e_c=-imag(e_c) ! -Im(<khy|mu_|sai>)
		
		
		e_c=e_c/alpha_k
        
		


       
H=H_0-mu_*e_c
!H=H_0


call sub_hermit_maker()

	
H1=Id_-(i*dt/2.)*H
H2=Id_+(i*dt/2.)*H

CALL sub_invert_matrix(H2, H2_inv, n, ErrorFlag)	  


H_t=matmul(H2_inv,H1)
	
!-----------------------  ! Crank-Nicolson
do i0=1,n
t_sum=0.
do i1=1,n
!t_sum=t_sum+(real(H_t(i0,i1))*sai_(i1))
t_sum=t_sum+(H_t(i0,i1)*sai_(i1))
end do
sai1_(i0)=t_sum
end do
sai_=sai1_


        

end 