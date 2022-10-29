! Step function
function f_teta(t1,t2)
!use mod_OCT,only:

  !teta(t1,t2)=teta(t-T), t1=t,t2=T
  implicit none
real*8::t1,t2,f_teta
  if (t1>t2) then 
    f_teta=1.0
    else
      f_teta=0.0
      end if


end function f_teta