program Im_t_p
implicit none
real*8,allocatable,dimension(:,:)::sai1,mu_	
real*8,allocatable,dimension(:)::x_ ,V_ ,e_ ,sai0,saiN_
real*8,allocatable,dimension(:)::energy
integer*8::n ,i0 ,j0,k0,flag ,iE ,states  ,int_E
real*8::x_a,x_b,dx ,L,t_end,pi,dE	,norm ,E
real*8::v	,sai_min,sai_min_n  ,test,e_mean
real*8::E_a,E_b

t_end=.9
pi=3.141592d0

X_a=-5.5d0
X_b=5.5d0
dx=0.01d0
L=X_b-X_a
n=int(L/dx)

sai_min=0.000001d0
sai_min_n=0.00001d0
states=15

dE=0.00001d0
E_a=-1.5d0
E_b=25.d0
int_E=int((E_b-E_a)/dE)


allocate(sai0(0:n))
allocate(sai1(states,0:n))

allocate(x_(0:n),V_(0:n),mu_(states,states))
allocate(energy(states),e_(0:int_E),saiN_(0:int_E))

do i0=0,n
x_(i0)=(real(i0)*dx)+X_a
V_(i0)=v(x_(i0))
end do

do i0=0,int_E
E_(i0)=(real(i0)*dE)+E_a
end do


do iE=1,states
sai0(0)=sai_min/2.7d0
sai0(1)=sai_min

sai1(iE,0)=0.d0
sai1(iE,1)=sai_min

end do

!------------------------------------------------
print*,'Step 1'
!open(400,file='energy_diagram.txt')
	
do j0=0,int_e
do i0=2,n
sai0(i0)=-sai0(i0-2)+(2.d0-(dx**2)*(E_(j0)-V_(i0-1)))*sai0(i0-1)
end do
saiN_(j0)=sai0(n)
!print*,E_(j0) 
!write(400,*)E_(j0),sai0(n)
if (mod(j0,1000)==0) print*,j0 , ' of ' , int_e

end do
!--------------------------------------------------
print*,'Step 2'
flag=-1
iE=1
do j0=0,int_e

if (abs(saiN_(j0))<sai_min_n) then
	test=saiN_(j0+1)/saiN_(j0)
	if (test<0.d0 .and. flag==-1) then
	flag =1
	e_mean=E_(j0)
	end if

	if (test>0.d0 .and. flag==1) then
	flag =-1
	energy(iE)=(E_(j0)+e_mean)/2.d0
	!energy(iE)=(E_(j0))
	iE=iE+1
	if (iE>states) exit
	end if

end if

end do
!--------------------------------------------------
print*,'Step 3'


do iE=1,states
E=energy(iE)
do i0=2,n
sai1(iE,i0)=-sai1(iE,i0-2)+(2.d0-(dx**2)*(E-V_(i0-1)))*sai1(iE,i0-1)
end do

end do

!--------------------------------------------------

print*,'Step 4'
do iE=1,states
norm=0.d0
do i0=0,n
norm=norm+ (sai1(iE,i0))**2
end do
norm=sqrt(norm)
if (norm==0) cycle
sai1(iE,:)=sai1(iE,:)/norm
end do


!-------------------------------------------------
open(100,file='result.txt')
open(1,file='sai1.txt')
open(2,file='sai2.txt')
open(3,file='sai3.txt')
open(4,file='sai4.txt')
open(5,file='sai5.txt')
open(6,file='sai6.txt')
open(7,file='x.txt')
open(200,file='energy_levels.txt')
open(300,file='potential.txt')
open(400,file='Dipole.txt')
!sai1=(sai1)**2
sai1=sai1*10
do i0=0,n
!write(100,*) sai1(0,i0),-sai1(1,i0)+1.d0,sai1(2,i0)+2.d0,-sai1(3,i0) +3.d0
write(1,*) sai1(1,i0)
write(2,*) -sai1(2,i0)+1.d0
write(3,*) sai1(3,i0)+2.d0
write(4,*) -sai1(4,i0)+3.d0
write(5,*) sai1(5,i0)+4.d0
write(6,*) -sai1(6,i0)+5.d0
write(7,*) x_(i0)
write(300,*) V_(i0)
end do

do iE=1,states
write(200,*)  Energy(iE)-Energy(1)
end do


do j0=1,states
do k0=1,states
test=0.
do i0=0,n
test=test+sai1(j0,i0)*x_(i0)*sai1(k0,i0)/100
end do
mu_(j0,k0)=test
end do
end do

do j0=1,states
write(400,*)  mu_(j0,:)
end do


end program

!+++++++++++++++++++++++++++++++
function V(x)
implicit none
real*8::V,x,B
B=0.003906d0 
V=((X**4)/64)-((X**2)/4)+(x**3)*B
end
