module prec
integer, parameter :: rk = selected_real_kind(6)
end module prec

module metodo
use prec
implicit none
contains

function fun(x,r,s,q) result(y)
use prec
real(kind=rk), intent(in) :: x,r,s,q
real(kind=rk) :: y
y=((x**2)*r)+(x*s)+q
end function fun

function derivfun(x,r,s) result(y)
use prec
real(kind=rk), intent(in) :: x,r,s
real(kind=rk) :: y
y=(((2.0_rk)*x)*r)+s
end function derivfun

subroutine newton (fun,derivfun,p,primo,secondo,terzo,eps,x1)
real(kind=rk), intent(in) :: eps,primo,secondo,terzo
real(kind=rk) :: p
real(kind=rk), intent(out) :: x1
integer :: i

interface
function fun(x,r,s,q) result(y)
use prec
real(kind=rk), intent(in) :: x,r,s,q
real(kind=rk) :: y
end function fun

function derivfun(x,r,s) result(y)
use prec
real(kind=rk), intent(in) :: x,r,s
real(kind=rk) :: y
end function derivfun
end interface

do
x1=p - fun(p,primo,secondo,terzo)/derivfun(p,primo,secondo)
if(x1==0 .or. abs(x1-p)<eps) exit
p=x1
end do
end subroutine
end module

module exam
use prec
implicit none
contains

function posx (xo,velx,t) result(rx)
real (kind=rk), intent(in) :: xo,velx,t
real (kind=rk) :: rx
rx=xo+velx*t
end function

function posy (yo,vely,t) result(ry)
real (kind=rk) :: yo,vely,t
real (kind=rk) :: ry
ry=yo+vely*t
end function
end module

program esame
use prec
use exam
use metodo
implicit none
real, parameter :: pi=acos(-1.0)
real (kind=rk),dimension(2) :: vel,n
real (kind=rk) :: a,b,xo,yo,controllo,alpha,rad,primo,secondo,terzo,t,eps,x1,p,z
integer :: i,K

print*,"Inserire i valori a e b dei semiassi dell'ellisse"
print*,"a:"
read*, a
print*,"b:"
read*, b
print*,"Inserire le coordinete x(0) e y(0) della posizione iniziale della particella all'interno della scatola ellittica"
print*,"x(0):" 
read*, xo
print*,"y(0):" 
read*, yo
controllo=((xo/a)**2)+((yo/b)**2)
if (controllo >= 1.0) STOP "ERRORE: i valori di posizione della particella si trovano all' esterno dell' ellisse o sul suo bordo"
print*,"Inserire il valore dell'angolo alpha tra il vettore velocità e l'asse delle x"
print*,"Alpha (in gradi):"
read*,alpha
print*,"Inserire il numero di collisioni che si vuole considerare"
print*,"N:"
read*,K
print*, "Valore epsilon:"
read*, eps
rad=(alpha*pi)/(180.0_rk)
vel(1)=cos(rad)
vel(2)=sin(rad)
do i=1,K
x1=0
primo=((b**2)*(vel(1)**2)+(a**2)*(vel(2)**2))
secondo=(2.0_rk)*((b**2)*xo*vel(1)+(a**2)*yo*vel(2))
terzo=-(((a**2)*(b**2))-((a**2)*(yo**2))-((b**2)*(xo**2)))
p=(-secondo)/((2.0_rk)*primo)
z=p+1
call newton (fun,derivfun,z,primo,secondo,terzo,eps,x1)
t=x1
xo=posx (xo,vel(1),t)
yo=posy (yo,vel(2),t)
n(1)=xo/(a*a)/sqrt((xo/(a*a))**2+(yo/(b*b))**2)
n(2)=yo/(b*b)/sqrt((xo/(a*a))**2+(yo/(b*b))**2)
vel=vel-(2.0_rk)*(dot_product(vel,n))*n
write(unit=1,fmt=*)xo,yo
end do

end program