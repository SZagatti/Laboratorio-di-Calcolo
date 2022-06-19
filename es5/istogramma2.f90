program istogramma2
 implicit none
integer, dimension (-5:5) :: A
integer :: n,j,i,s
A=0
print*, "Numero Totale di Dati:"
read*, n
print*, "Inserire i",n,"Dati:"
do i=1,n
read*, j
if (j<-5) cycle 
if (j>5) cycle
A(j)=A(j)+1
end do
do s=-5,5
print*, s, A(s)
end do
end program istogramma2


