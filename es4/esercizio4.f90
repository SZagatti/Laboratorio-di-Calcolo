program esercizio4
implicit none
integer, parameter :: rk=selected_real_kind(15)
real(kind=rk) :: a=0.0 , b=0.001 , x , y , pi=acos(-1.0)
integer :: N , i
print*,'Inserire il valore degli N in cui suddividere gli intervalli'
print*,'N?'
read*,N
Do i =0, N , 1
x=a+i*(b-a)/N
y=sqrt(2/pi/x)*((3/x**2-1)*sin(x)-3/x*cos(x))
print*, i,x,y
write(unit=1,fmt=*)x,y
end do
end program esercizio4

