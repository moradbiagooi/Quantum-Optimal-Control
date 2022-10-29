subroutine sub_weight_f()
!use mod_OCT,only:t,t_end,f_weight,f_ft
use mod_OCT
implicit none
integer ::j,nnn
real*8::s,hhh,a,b
real*8::f_ft

a=0.0
b=t_end
nnn=t_end		 ! this was	 n=100*t_end
s=0.0
hhh=(b-a)/nnn
do j=1,nnn/2 ! integral by Sympson method   !**** check do j=0,nnn/2
  s=s+(hhh/3.)*(f_ft((2.*j-2.)*hhh+a,t_end)+4.*f_ft((2.*j-1.)*hhh+a,t_end)+f_ft((2.*j)*hhh+a,t_end))
end do
 
f_weight=t_end*f_ft(t,t_end)/s

end



