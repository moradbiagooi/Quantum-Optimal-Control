subroutine sub_writaData()
!use mod_OCT,only:alloc,dt,t,f_rp,sai_t,khy_t
use mod_OCT
implicit none
integer::k0,i0,j0,m0,n0
real*8::f_rp,rr0,rt0

print*,' '
print*,'Writing results in files:'

open(600,file="sai.txt")
!open(700,file="khy.txt")
!open(800,file="normSai.txt")
!open(900,file="normKhy.txt")
!open(1000,file="weight.txt")
open(1100,file="target.txt")
!open(1200,file="field.txt")
open(1300,file="field_full.txt")


m0=2000
do i0=0,m0

		rt0=(real(i0)/real(m0))
		n0=int(rt0)
		rr0=(real(i0)/real(m0))*alloc
		k0=int(rr0)     
		
		do j0=1,n
		sai_r(j0)=f_rp(sai_t(j0,k0))
		khy_r(j0)=f_rp(khy_t(j0,k0))
		ct_r(j0)=f_rp(m_s_target(j0,k0))
		end do
		

write(600,'(1X,40(1X,F8.5))')rt0,sai_r
!write(700,'(1X,40(1X,F8.5))')rt0,khy_r

!write(800,*)rt0,sum(sai_r)
!write(900,*)rt0,sum(khy_r)

!write(1000,*)rt0,m_f_weight(k0)
write(1100,'(1X,40(1X,F8.5))')rt0,ct_r
!write(1200,*)rt0,m_field(k0)
end do  

do i0=0,alloc
write(1300,*)i0*dt,m_field(i0)
end do

close(600)
!close(700)
!close(800)
!close(900)
!close(1000)
close(1100)
!close(1200)
close(1300)
close(106)

print*, ' Finish. '
print*, ' '
print*, ' -----------------------------------------------'
print*, ' '
print*, ' ========================='
print*, ' || Moraad Biagooi      ||'
print*, ' || M.Biagooi@gmail.com || '
print*, ' || 2012-2013           ||   '
print*, ' ========================='
print*, ' '
end