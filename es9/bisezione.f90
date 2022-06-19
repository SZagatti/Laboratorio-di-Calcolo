module prec
integer, parameter :: rk = selected_real_kind(6)
end module prec

module A
use prec
implicit none
contains

function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
y=(x**2)-4
end function fun

subroutine bisezione (fun,xl,xu,eps,xm)
real(kind=rk) :: xl,xu,eps
real(kind=rk), intent(out) :: xm
integer :: i

interface
function fun(x) result(y)
use prec
real(kind=rk), intent(in) :: x
real(kind=rk) :: y
end function fun
end interface
xm=0
if (fun(xl)<0 .and. fun(xu)<0) print*, "ERRORE,valori incorretti degli estremi"
if (fun(xl)>0 .and. fun(xu)>0) print*, "ERRORE,valori incorretti degli estremi"
if (fun(xl)==0) print*, "La funzione ha valore zero nel punto x=",xl
if (fun(xu)==0) print*, "La funzione ha valore zero nel punto x=",xu

do i=1,10000
xm=(xl+xu)/2
if (fun(xm)==0 .or. abs(fun(xm))<eps) exit 
if (fun(xm)>0 .and. fun(xu)<0) xl=xm
if (fun(xm)>0 .and. fun(xl)<0) xu=xm
if (fun(xm)<0 .and. fun(xu)>0) xl=xm
if (fun(xm)<0 .and. fun(xl)>0) xu=xm
end do
end subroutine
end module

program es4
use prec
use A
implicit none
real(kind=rk) :: xl,xu,eps,xm
print*,"Inserire il valore di xl:"
read*, xl
print*,"Inserire il valore di xu:"
read*, xu
print*,"Inserire il valore della precisione epsilon:"
read*, eps
call bisezione(fun,xl,xu,eps,xm)
print*, "La funzione ha valore zero nel punto x=",xm
print*, "La funzione ha valore analitico zero nel punto x=", sqrt(4.0_rk)
end program
