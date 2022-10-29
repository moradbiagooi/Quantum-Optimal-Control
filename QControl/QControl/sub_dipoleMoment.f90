!Dipole matrix elements (table3. page R188)
subroutine sub_dipole_moment()
!use mod_OCT,only:mu_
use mod_OCT
implicit none
integer::i0,j0
real*8::s1,s2

print*, ' '
print*, ' -----------------------------------------------'
print*, ' '

mu_=0.d0

if(exact_MU==0) then 
mu_(1,1)=-2.5676
mu_(2,1)=0.3921
mu_(3,1)=0.6382
mu_(4,1)=-0.3865
mu_(5,1)=-0.1414
mu_(6,1)=0.01921	!***************8


mu_(2,2)=2.3242
mu_(3,2)=-0.7037
mu_(4,2)=-0.4630
mu_(5,2)=0.2118
mu_(6,2)=0.06118	 !******************8

mu_(3,3)=-0.5988
mu_(4,3)=1.7051
mu_(5,3)=0.1593
mu_(6,3)=-0.1593


mu_(4,4)=0.1958
mu_(5,4)=-1.7862
mu_(6,4)=-0.0022   !***********88


mu_(5,5)=-0.0939
mu_(6,5)=1.9939		!***********8 maybe a small number

mu_(6,6)=0.0099		!*************

else if (exact_MU==1) then

do i0=1,n
do j0=1,n
CALL RANDOM_NUMBER(s1)
CALL RANDOM_NUMBER(s2)
mu_(i0,j0)=(s1+s2)*2.d0
end do
end do

else if (exact_MU==2) then

do i0=1,n
do j0=1,n
CALL RANDOM_NUMBER(s1)
CALL RANDOM_NUMBER(s2)
mu_(i0,j0)=1.d0+(s1+s2)*2.d0
end do
end do



else if (exact_MU==3) then

open(2000,file='Dipole.txt')
do i0=1,n
read(2000,*) mu_(i0,:)
end do
close(2000)

else if (exact_MU==5) then

mu_(2,1)=-.51d0
mu_(1,2)=mu_(2,1)
endif

if (exact_MU<3) then
! If dipole matrix is symmetric, It doesn't matter if inputted data is triangular or complete
	do j0=1,n
	do i0=1,n

	if (abs(mu_(i0,j0))>abs(mu_(j0,i0))) then
	mu_(j0,i0)=mu_(i0,j0)
	else
	mu_(i0,j0)=mu_(j0,i0)
	endif
	
	end do
	end do
endif
end subroutine sub_dipole_moment