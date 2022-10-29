subroutine sub_hermit_maker()
!use mod_OCT,only:n,H
use mod_OCT
implicit none
integer::i0,j0
do i0=1,n
do j0=i0,n
h(i0,j0)=(h(i0,j0)+conjg(h(j0,i0)))/2.
h(j0,i0)=conjg(h(i0,j0))
end do
end do

end