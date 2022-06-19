program pendolo
 implicit none

 real    :: l=1.0,g=9.81 ! valori di default la lunghezza e acc. di gravità
 real    :: dt,ekin,epot,m
 real    :: alpha,vel,vel_parziale, f
 integer :: nstep,it
 write(unit=*,fmt="(a)",advance="no")"delta t:"   ! il formato (a) chiede che il dato sia trattato come 
 read*,dt                                           ! caratteri e advance="no"  sopprime l'  inserimento del 
 write(unit=*,fmt="(a)",advance="no")"n.step:"     ! caratter "a capo"  alla fine della linea per cui 
 read*,nstep                                        ! la prossima operazione di lettura/scrittura inziera'
 write(unit=*,fmt="(a)",advance="no")"lunghezza pendolo:"      ! sula stessa riga di schermo di quella corrente
 read*,l
 write(unit=*,fmt="(a)",advance="no")"massa fisica:"
 read*,m
 write(unit=*,fmt="(a)",advance="no")"accelerazione di gravità g:"
 read*,g
 write(unit=*,fmt="(a)",advance="no")"angolo di partenza alpha(0):"
 read*,alpha
 write(unit=*,fmt="(a)",advance="no")"velocità di partenza vel(0):"
 read*,vel

 it=0        ! step 0 : valori iniziali
 write(unit=1,fmt=*)it,it*dt,alpha,vel
 epot =  m*g*l*(1-cos(alpha))
 f    = -g*sin(alpha)
 ekin =  0.5 * m * l**2 *vel**2
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 do it = 1,nstep
    alpha = alpha + vel * dt + 0.5* f/l * dt**2
    vel_parziale = vel + 0.5 * dt * f/l                !  prima parte della formula per le velocita'
    f    = -g*sin(alpha)
    epot = m*g*l*(1-cos(alpha))
    vel = vel_parziale + 0.5 * dt * f/l               !  la formula per le velocita' viene completata qui
    write(unit=1,fmt=*)it,it*dt,alpha,vel
    ekin = 0.5 * m * l**2 *vel**2
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot
 end do
 end program pendolo
 
