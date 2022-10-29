!ct_(0): c_0(t) Target state probability amplitudes
subroutine sub_target_f()
!use mod_OCT,only:t_end,t,t_end,f_teta,pi,ct_
use mod_OCT
implicit none
real*8::f_teta			

if (exact_CT==0) then
ct_=0.d0
ct_(1)=f_teta(t_end/2.,t)*cos(pi*t/t_end)   !eq(96)
ct_(2)=f_teta(t,t_end*3./4.)*sin((2.*pi*t/t_end)-3.*pi/2.)	!eq(97)
ct_(3)=0.d0	!eq(98)
ct_(4)=sqrt(real((1.-ct_(1)*conjg(ct_(1))-ct_(2)*conjg(ct_(2))-ct_(5)*conjg(ct_(5)))))
ct_(4)=ct_(4)*f_teta(t,t_end/2.)
ct_(5)=f_teta(t_end/2.,t)*sin(pi*t/t_end)!+f_teta(t,t_end/2)*f_teta(5*t_end/8,t)	!eq(100)
!ct_(7)=0.

else if (exact_CT==1) then
ct_=0.005d0
ct_(1)=abs(f_teta(t_end/2.,t)*cos(pi*t/t_end))   !eq(96)
ct_(2)=abs(f_teta(t,t_end*3./4.)*sin((2.*pi*t/t_end)-3.*pi/2.))	!eq(97)
ct_(3)=0.005d0	!eq(98)
ct_(4)=sqrt(real((1.-ct_(1)*conjg(ct_(1))-ct_(2)*conjg(ct_(2))-ct_(5)*conjg(ct_(5)))))
ct_(4)=abs(ct_(4)*f_teta(t,t_end/2.))
ct_(5)=abs(f_teta(t_end/2.,t)*sin(pi*t/t_end))+f_teta(t,t_end/2)*f_teta(5*t_end/8,t)	!eq(100)


else if(exact_CT==2) then
ct_=0.00545
!ct_(1)=cos(3.d0*pi*t/(t_end*2.d0))   
!ct_(2)=sqrt(real( ( 1.d0 - ct_(1) * conjg(ct_(1)) ) ) )

!ct_(4)=0.0113	
ct_(4)=0.99d0
!ct_(3)=0.0115
!ct_(6)=0.0321
else if (exact_CT==3) then
ct_(1)=0.012d0
ct_(2)=0.016d0
ct_(5)=0.013d0	
ct_(4)=0.014d0
ct_(3)=0.95d0
ct_(6)=0.015d0
else if (exact_CT==4) then
ct_=0.d0
ct_(3)=1.d0

else if (exact_CT==5) then
!ct_=0.9d0
!ct_(2)=0.09d0

ct_(1)=cos(3.*pi*t/(t_end*2.))   !eq(96)
ct_(2)=sqrt(real( ( 1. - ct_(1) * conjg(ct_(1)) ) ) )

endif


end subroutine sub_target_f