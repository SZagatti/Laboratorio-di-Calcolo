module gravitazione
implicit none
integer,parameter :: kr=selected_real_kind(15), nbody=2
real(kind=kr),parameter :: G=6.673E-11_kr
real(kind=kr),dimension(nbody) :: massa

contains
  subroutine interazione(pos,f,epot)
   real(kind=kr), intent(in), dimension(:,:) :: pos
   real(kind=kr), intent(out) :: epot
   real(kind=kr), intent(out), dimension(:,:) :: f
   real(kind=kr), dimension(size(pos,1)) :: posij
   real(kind=kr) :: rij
   integer ::i,j
   epot = 0
   f    = 0
   do i=1,nbody
      do j=1,nbody
         if( i==j ) cycle
         posij  = pos(:,j)-pos(:,i)
         rij=sqrt( dot_product(posij,posij) )
         epot   = epot   -(G*massa(i)*massa(j))/rij
         f(:,i) = f(:,i) +(G*massa(i)*massa(j)) * posij/rij**3
      end do
   end do
   epot = epot/2    ! divisione per 2 perche' tutti i contributi di
                    ! ciascuna coppia sono stati contati due volte: (i,j) e (j,i)                                                            

  end subroutine interazione
end module gravitazione



program harm
use gravitazione

 real (kind=kr) :: dt,ekin,epot
 real (kind=kr),dimension(3,nbody)    :: pos,vel,vel_parziale,f
 real (kind=kr),dimension(3) :: momang,qmoto
 integer :: nstep,it

 massa(1)= 1.989E30_kr ! Massa del Sole
 massa(2)= 5.98E24_kr ! Massa della Terra
 pos(1,1)=0
 pos(2,1)=0
 pos(3,1)=0
 pos(1,2)=152.1E9_kr
 pos(2,2)=0
 pos(3,2)=0
 vel(1,1)=0
 vel(2,1)=0
 vel(3,1)=0
 vel(1,2)=0
 vel(2,2)=29.29E3_kr
 vel(3,2)=0
 write(unit=*,fmt="(a)",advance="no")"delta t : "   ! il formato (a) chiede che il dato sia trattato come 
 read*,dt                                           ! caratteri e advance="no"  sopprime l'  inserimento del 
 write(unit=*,fmt="(a)",advance="no")"n.step: "     ! caratter "a capo"  alla fine della linea per cui 
 read*,nstep                                        ! la prossima operazione di lettura/scrittura inziera'    ! sula stessa riga di schermo di quella corrente
 it=0        ! step 0 : valori iniziali
 write(unit=1,fmt=*)it,it*dt,pos,vel
call interazione(pos,f,epot)
 ekin =  sum(spread(massa,1,3) * vel**2) * 0.5
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot
 do it = 1,nstep
    pos = pos + vel * dt + 0.5* f/spread(massa,1,3) * dt**2
    vel_parziale = vel + 0.5 * dt * f/spread(massa,1,3)                !  prima parte della formula per le velocita'
   call interazione(pos,f,epot)
 vel = vel_parziale + 0.5 * dt * f/spread(massa,1,3)                !  la formula per le velocita' viene completata qui
    write(unit=1,fmt=*)it,it*dt,pos,vel
    ekin =  sum(spread(massa,1,3) * vel**2) * 0.5
qmoto(1)=massa(2)*vel(1,2)
qmoto(2)=massa(2)*vel(2,2)
qmoto(3)=massa(2)*vel(3,2)
momang(1)=pos(2,2)*qmoto(3)-pos(3,2)*qmoto(2)
momang(1)=pos(1,2)*qmoto(3)-pos(3,2)*qmoto(1)
momang(3)=pos(1,2)*qmoto(2)-pos(2,2)*qmoto(1)
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot,momang
 end do
 end program harm
 
