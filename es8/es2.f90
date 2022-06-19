Module A
implicit none
contains
elemental function raddoppia(x) result(dop)
real,intent(in) ::x
real            ::dop
dop= 2*x
end function raddoppia
end module A

program es2
use A
implicit none
real, dimension(:), allocatable :: x,risultato
real :: res,r
integer :: m
print*, "Dimesione dell'array:"
read*, m
allocate (x(m))
print*, "Immettere i valori dell'array:"
read*, x
print*, "Immettere il valore dello scalare:"
read*, r
risultato=raddoppia(x)
res=raddoppia(r)
print*, "Il primo risultato è:",risultato
print*, "Il secondo risultato è:",res
end program es2
