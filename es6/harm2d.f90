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

 real    :: massa=1.0 ! valori di default per massa e cost. elastica
 real    :: dt,ekin,epot,Lz
 real (kind=kr),dimension(3,nbody)    :: pos,vel,vel_parziale,f
 real,dimension(2)    :: kappa=1.0
 integer :: nstep,it
 write(unit=*,fmt="(a)",advance="no")"delta t : "   ! il formato (a) chiede che il dato sia trattato come 
 read*,dt                                           ! caratteri e advance="no"  sopprime l'  inserimento del 
 write(unit=*,fmt="(a)",advance="no")"n.step: "     ! caratter "a capo"  alla fine della linea per cui 
 read*,nstep                                        ! la prossima operazione di lettura/scrittura inziera'
 do i=1,nbody
write(unit=*,fmt="(a)",advance="no")"massa",1,":"      ! sula stessa riga di schermo di quella corrente
 read*,massa(i)
end do
 write(unit=*,fmt="(a)",advance="no")"kappa: "
 read*,kappa
 write(unit=*,fmt="(a)",advance="no")"posizione iniziale x: "
 read*,pos(1)
 write(unit=*,fmt="(a)",advance="no")"posizione iniziale y: "
 read*,pos(2)
 write(unit=*,fmt="(a)",advance="no")"velocita' iniziale x: "
 read*,vel(1)
 write(unit=*,fmt="(a)",advance="no")"velocita' iniziale y: "
 read*,vel(2)

 it=0        ! step 0 : valori iniziali
 write(unit=1,fmt=*)it,it*dt,pos,vel
 ekin =  sum(spread(massa,1,3) * vel**2) * 0.5
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 do it = 1,nstep
    pos = pos + vel * dt + 0.5* f/spread(massa,1,3) * dt**2
    vel_parziale = vel + 0.5 * dt * f/spread(massa,1,3)                !  prima parte della formula per le velocita'
    vel = vel_parziale + 0.5 * dt * f/massa                !  la formula per le velocita' viene completata qui
    Lz = massa * (pos(1) * vel(1) - pos(2) * vel(2))
    write(unit=1,fmt=*)it,it*dt,pos,vel
    ekin =  sum(spread(massa,1,3) * vel**2) * 0.5
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot,Lz
 end do
 end program harm
 
