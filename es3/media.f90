program media
implicit none
real :: aver,x
integer :: i,N
   
print*," N= ?"
read*,N    
print*," immetti i dati uno alla volta"

aver=0.0
do i=1,N
   read*,x
   aver = aver + x
end do
aver = aver/N
print*," media di ",N," dati : ",aver
end program media
