program abc
implicit none
integer,parameter :: rk=selected_real_kind(33)
real(kind=rk) :: x=1.0_rk,y=1.0_rk,z
integer ::i
do i=1,100000000
  z=x*(y+i)
end do
end program abc
