subroutine sub_weight_pr()
!use mod_OCT,only:alloc,int_t,t_end,t,dt,f_weight
use mod_OCT
implicit none
integer::i0
integer::a1,a2,a3
real*8::test1

print*, ' '
print*, ' -----------------------------------------------'
print*, ' '


! -----------------' Processing weight function'
print*,' Processing weight function  please wait'


test1=(400./800.)*real(alloc)
a1=int(test1)
test1=(600./800.)*real(alloc)
a2=int(test1)
test1=(750./800.)*real(alloc)
a3=int(test1)

if (exact_WE==0) then

	m_f_weight=1.0
	do i0=0,alloc
		if (a1<i0 .and. i0<a2 ) then
		m_f_weight(i0)=1.-sin(pi*(real(i0-a1)/real(a2-a1)))
		end if
		if (a3<i0 ) then
		m_f_weight(i0)=1.+sin((pi/2.5)*(real(i0-a3)/real(alloc-a3)))
		end if
	end do

else if (exact_WE==1) then

	m_f_weight=0.1d0! this workd with 0.05 at t_end=50 for linear target
	do i0=0,alloc
		if (a1<i0 .and. i0<a2 ) then
		m_f_weight(i0)=(1.d0-sin(pi*(real(i0-a1)/real(a2-a1))))/10.d0
		end if
		if (a3<i0 ) then
		m_f_weight(i0)=(1.d0+sin((pi/2.5d0)*(real(i0-a3)/real(alloc-a3))))/10.d0
		end if
	end do

else if (exact_WE==2) then

	m_f_weight=0.1d0

else if (exact_WE==3) then

	m_f_weight=1.d0

else if (exact_WE==5) then

	m_f_weight=0.d0

endif


print*, ' Weight function construction is done.'
print*,' '

end
