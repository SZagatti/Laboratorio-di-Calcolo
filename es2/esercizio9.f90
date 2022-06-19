program esercizio9
implicit none
real :: a=-1.5 , b=1.5 , x , y
integer :: N , i
print*,'Inserire il valore degli N in cui suddividere gli intervalli'
print*,'N?'
read*,N
print*,N
Do i =0, N , 1
x=a+i*(b-a)/N
print*, i,x
y=(exp(-4x**2))*sin(10x)
print*, i,y
end do
end program esercizio9

