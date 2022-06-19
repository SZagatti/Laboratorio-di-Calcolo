program media
implicit none
real :: aver,x, mquad
integer :: i,N
   
print*," N= ?"
read*,N    
print*," immetti i dati uno alla volta"

aver=0.0
mquad=0.0
do i=1,N
   read*,x
   aver = aver + x
   mquad = mquad + (x)**2
end do
aver = aver/N
mquad = mquad/N
print*," media di ",N," dati : ",aver
print*,"media dei quadrati di ",N,"dati :",mquad
end program media
