subroutine	 sub_BV_I()
!use mod_OCT,only:nornmm,sai_,sai1_,sai_t,exact
use mod_OCT
implicit none
integer::i0

sai_=0.d0
sai1_=0.d0
if (exact_BVI==0) then

	sai_(1)=1.d0
	sai1_(1)=sai_(1)
	sai_t(1,0)=sai_(1)
	
	do i0=2,n
	sai_t(i0,0)=0.d0
	end do
	
	
else if(exact_BVI==1)  then

	sai_(1)=0.98d0
	sai1_(1)=sai_(1)
	sai_t(1,0)=sai_(1)
	
	do i0=2,n
	sai_t(i0,0)=0.001d0
	end do
	
	

endif


normm=0.d0
        do i0=1,n
		normm=normm+real(sai_t(i0,0)*conjg(sai_t(i0,0)))
        end do
        normm=sqrt(normm)
        do i0=1,n
		if (normm>0) sai_t(i0,0)=sai_t(i0,0)/normm
        end do
        if (normm>0) sai_=sai_/normm
		if (normm>0) sai1_=sai1_/normm

end