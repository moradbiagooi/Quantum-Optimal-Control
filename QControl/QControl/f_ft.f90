
function f_ft(t,t_end)
!use mod_OCT,only:t_end
use mod_OCT
implicit none
real*8::f_ft,t,t_end
f_ft=1-exp(-((t-5.*t_end/8.)**2.)/1600.)+exp(-((t-t_end)**2.)/64.)
end