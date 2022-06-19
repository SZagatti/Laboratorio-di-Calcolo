program espsommeparziali
implicit none
integer, parameter :: rk=selected_real_kind(33)
real(kind=rk) :: k
real :: x,fatt
integer :: N,i,q
print*, "Inserire il valore di x: x="
read*, x
print*, "Inserire il valore di N: N="
read*, N
k=1.0
do i=1,N
fatt=i
do q=1,i-1
   fatt = fatt*(i-q)
   end do
k= k + (((-x)**i)/fatt)
print*,i,fatt,k
end do
print*, "Il valore di e^(-x) calcolato con le somme parziali fino a",N,"in",x,"e'",k
print*, "Il valore di e^(-x) calcolato con la funzione in",x,"e'", exp(-x)
end program espsommeparziali
