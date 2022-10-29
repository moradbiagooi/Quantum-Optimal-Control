!-----------------------------
function f_rp(c) !real Part
!use mod_OCT,only:
implicit none
complex*16::c
real*8::f_rp
f_rp=real(c)**2+imag(c)**2
end function