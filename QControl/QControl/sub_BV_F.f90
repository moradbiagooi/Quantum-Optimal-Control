subroutine sub_BV_F()
!use mod_OCT,only:exact,alloc,khy_,khy1_,khy_t
use mod_OCT
implicit none
integer::i0
	khy_=0.d0 
	khy1_=0.d0

if (exact_BVF==0) then
	khy_=0.d0 
	khy1_=khy_

	do i0=1,n
	khy_t(i0,alloc)=khy_(i0)
	end do

else if(exact_BVF==1) then
	khy_=0.001d0 
	khy1_=khy_

	do i0=1,n
	khy_t(i0,alloc)=khy_(i0)
	end do

	
else if(exact_BVF==2) then
	khy_=.9d0  
	khy1_=khy_

	do i0=1,n
	khy_t(i0,alloc)=khy_(i0)
	end do

	else if(exact_BVF==5) then

	khy_(1)=0.01d0
	khy_(2)=0.99d0 
	khy1_=khy_
		
	khy_t(1,alloc)=khy_(1)
	khy_t(2,alloc)=khy_(2)

endif



end