program uno
implicit none
integer,parameter                         :: rk=selected_real_kind(15)
integer,parameter                         :: N=3                   !  dimensione del sistema  lineare
integer                                   :: i,info,nrhs,lwork,lda=N,ldb=N,j 
integer,dimension(N)                      :: ipiv
real(kind=rk),dimension(N,N)              :: a,aorig,e    ! a contiene la matrice dei coefficienti del 
real(kind=rk),dimension(N)                :: x            ! sistema. Viene copiata su aorig all' inizio
real(kind=rk),dimension(N)                :: b,borig,work ! b e' il vettore dei termini noti; copiato
real(kind=rk)                             :: fac          ! all' inizio su borig

lwork=n
do i=1,N
do,j=1,N
a(i,j)=1/(i+j-1)
end do 
end do
aorig=a
b = [ (1.0_rk*i,i=1,N) ]
borig = b
nrhs = 1
call dgesv(N,nrhs,a,lda,ipiv,b,ldb,info)   ! il nome iniziante con d implica real a precisione doppia
a=aorig                                    ! in uscita da dgesv a e b sono stati modificati
x=b                                        ! in particolare b contiene il vettore soluzione
b=borig
print*
print*,"  soluzione x: "
print*,x
print*
print*,"  check diretto su soluzione: Ax - b = 0 (entro il roundoff) "
do i=1,N
   print*,sum(a(i,:)*x)-b(i)
end do

call dgetrf(N,N,a,lda,ipiv,info)                  ! fattorizzazione di a
call dgetri(N,a,lda,ipiv,work,lwork,b,ldb,info)   ! calcolo della matrice inversa di a
print*                                            ! in uscita a contiene l' inversa
print*," matrice inversa "
do i = 1,N
   print*,a(i,:)
end do
e = matmul(a,aorig)
print*
print*," check  sull'  inversa ( a*a^(-1) ) :"
do i = 1,N
   print*,e(i,:)
end do
end program uno
