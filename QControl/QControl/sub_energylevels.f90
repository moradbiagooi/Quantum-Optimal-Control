! Excitation energys in atomic units (table2. page R187)
subroutine sub_energylevels()
!use mod_OCT,only:energy_,H_0,Id_,energy,i
use mod_OCT
implicit none
integer::i0,j0
real*8::s1,s2

print*, ' '
print*, ' -----------------------------------------------'
print*, ' '

energy=0.d0
energy_=0.d0


Id_=0.d0
Do i0=1,n
id_(i0,i0)=1.d0
end do
H_0=Id_

energy_(1,1)=0.0
energy_(2,1)=0.1568
energy_(3,1)=0.7022
energy_(4,1)=1.0147
energy_(5,1)=1.5294
energy_(6,1)=1.9294	! *******8

energy_(2,2)=0.0
energy_(3,2)=0.5454
energy_(4,2)=0.8580
energy_(5,2)=1.3726
energy_(6,2)=1.6726	!************88

energy_(3,3)=0.0
energy_(4,3)=0.3125
energy_(5,3)=0.8273
energy_(6,3)=1.2273	!*************8

energy_(4,4)=0.0
energy_(5,4)=0.5147
energy_(6,4)=0.9147	!**************

energy_(5,5)=0.0
energy_(6,5)=0.3  !****************

energy_(6,6)=0.0

! I doubt about negative symbol
if (1==0) then
    do j0=1,n
	do i0=j0,n
	energy_(j0,i0)=-energy_(i0,j0)
	end do
	end do
else
    do j0=1,n
	do i0=j0,n
	energy_(j0,i0)=+energy_(i0,j0)
	end do
	end do
endif

energy(1)=0.132! just a guess
do i0=2,n
energy(i0)=energy_(i0,i0-1)+energy(i0-1)
end do

H_0=0.d0
if (exact_H0==0) then

do i0=1,n
H_0(i0,i0)=energy(i0)
end do

else if (exact_H0==1)then

do i0=1,n
CALL RANDOM_NUMBER(s1)
CALL RANDOM_NUMBER(s2)
H_0(i0,i0)=(s1+s2)*10.
end do


else if (exact_H0==2)then

do i0=1,n
do j0=1,n
CALL RANDOM_NUMBER(s1)
CALL RANDOM_NUMBER(s2)
H_0(i0,j0)=(s1+i*s2)*10.
end do
end do

else if (exact_H0==3)then

open(2000,file='Energy_levels.txt')
do i0=1,n
read(2000,*) H_0(i0,i0)
H_0(i0,i0)=H_0(i0,i0)+0.1d0
end do
close(2000)

else if (exact_H0==5)then
H_0(1,1)=1.1d0
H_0(2,2)=2.1d0
endif



end subroutine sub_energylevels