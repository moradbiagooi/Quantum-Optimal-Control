! Optimal control of time-dependent occupation numbers
! in a asymmetric double well model.
! V(x)=(w_0^4/64B)x^4-(w_0^2/4)x^2 +Beta*x^3


program OCT_TD_ADW ! Optimal control theory _ time-dependent _ Asymmetric double well.
!use mod_OCT,only:sai_t,khy_t,m_s_target,m_f_weight,mu_,energy,t,t_end,dt,alloc,exact,i,alpha_k
use mod_OCT
implicit none
integer::k ,k_end
!=========================================================
!--------------- To check-------
! Genetic algorithm
! Filtering . Is it in weight function?
! Making the target accurate and the boundary values approximate.
! Hermit Maker
! Doubt 1&2 - It doesn't show any difference in graphs. But I choose the second!
!=========================================================
! --------------Data-----------
n=6 ! number of states of the system. You have to change it in the module ,too.
!allocate(sai_(n),sai1_(n),sai0_(n),khy0_(n),khy1_(n),khy_(n),ct_(n))
exact_BVI=0 ! Initial Boundary values. 0:Perfect OCT , 1:Approximate OCT  
exact_BVF=1 ! Final Boundary values. 0:Perfect OCT , 1:Approximate OCT ,5:2-level system
exact_H0=3 ! Hamiltony. 0:Perfect OCT, 1:Random , 2:non-diagonal Random, 3:Numerov, 5:2-level system
exact_MU=3 ! Dipole moment. 0:Perfect OCT, 1:Random , 2:Random+1.d0 , 3:Numerov, 5:2-level system
exact_CT=0 ! Target operator .0:Perfect OCT, 1:Approximate OCT, 2:sin , 3:Linear , 4: Perfect Linear , 5:Zero, 5:2-level system
exact_WE=3 ! Weight function propagation . 0:Perfect OCT, 1:OCT/20 , 2:0.1 , 3:1.d0 
exact_TR=0 !0:Trick is off , 1:Trick is on

!khy_t=0.001d0 !khy_t=0.d0 It was like this before
khy_t=0.d0

sai_t=0.d0
sai_t(1,:)=1.d0


dt=.01d0		!time differential
t_end=800.d0 	! t_end: T	finish time  , 800.d0 at OCT
k_end=1000
alloc=int(t_end/dt)	 
t=0.d0 ! t:t   time in propagations
!alpha_k=0.2d0	! penalty factor _ time independent
open(106,file='Yeild.txt')	
!=========================================================
 call sub_dipole_moment()
 call sub_energylevels()
! -----------------' Processing weight function and target operator'
call sub_weight_pr()
call sub_target_pr()
!-----------------------Zeroth step propagation-START
print*,' '
print*,' Zeroth Propagation :'
call sub_pr_b0()	! eq :(?)
!-----------------------Zeroth step propagation-ENDs
!=========================================================
print*,' '
print*,' Iteration process :'
do k=1,k_end ! Iteration process
	call sub_pr_f() 
	call sub_pr_b() 
	print*,'Itr:',k ,' Yield(in percent)=',(Yield/full_yield)*100.d0
	write(106,*)k,(Yield/full_yield)*100.d0
end do !do k=0,200
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
call sub_writaData()
!=========================================================
    
end program OCT_TD_ADW

