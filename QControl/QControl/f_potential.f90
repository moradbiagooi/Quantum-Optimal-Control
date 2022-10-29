! Asymmetric double well model.
!V(x)=(w_0^4/64B)x^4-(w_0^2/4)x^2 +Beta*x^3
function f_potential(x)
!use mod_OCT,only:
implicit none
real*8::f_potential,x
real*8::Beta,B,w_0
w_0=1.0
B=w_0
Beta=1/256
f_potential=((w_0**4)/(64*B))*x**4-((w_0**2)/4)*x**2 +Beta*x**3
end function f_potential