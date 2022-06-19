program somma_N
implicit none
integer :: N , somma , j
print*,'Inserire il valore degli N interi la cui somma dei quadrati si vuole calcolare'
print*,'N?'
read*,N
print*,N
	somma = 0
	do j = 1,N,1
	somma = somma + j**2
end do
print*,'La somma degli N quadrati è:'
print*,somma
end program somma_N
