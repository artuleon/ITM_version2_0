! This file is part of the ITM model.
!
! Copyright 2009 University of Illinois at Urbana-Champaign
! Copyright 2011 Oregon State University, Corvallis
!
! Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
!
! ITM is a free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published
! by the Free Software Foundation; either version 2.0 of the
! License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
! 02110-1301, USA.

      Subroutine CFL_time_step (t,Dt,Dt_Courant)
	!This routine computes the time step according with the Courant criteria.	
      use common_module
	implicit none	    
      integer i,j,k,R
	double precision t,YY,Dt,Dt_Courant,Lambda,lambda1,lambda2
	double precision Ts,Area,Area1,Area2,RH,Phi,Cr,Crini,Qinf_new
	double precision Cr_open,Cr_mixed,Cr_pressur,Cr_dry,temcel
	double precision vel_max,vel
      
 5	Dt = Tmax-t
      !Courant numbers
      Cr_pressur = 0.50 !Courant number for pressurized flow
      Cr_open = 0.50 !Courant number for open channel flow
	Cr_mixed = 0.50 !Courant number for mixed flow				
	Crini = 0.05 !Courant number for initial time steps
	
10    do j = 1,NR		
          if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)    
			do i = 3,Nx(j)-2
				Area = A0(j,i)
				vel = dabs(Q0(j,i)/A0(j,i))
				vel_max = vel			
				!Cr to be used			
				if(Idflow(j,i) == 0)then  !Free surface flow
					yy = h0(j,i)
					call Area_from_H(j,YY,Area,Ts,RH,Idflow(j,i))					
					if(h0(j,i) > 0.90*yref(j))then					
						Cr = Cr_mixed  !mixed flow						
						lambda = pc
					else
						Cr = Cr_open  !open channel flow											
						lambda = vel_max + sqrt(g*Area/Ts)
					endif
					! The first steps must have a low courant number because high
					! courant numbers can produce unreliable
					!estimates of the wave speed. 
					!For example use CFL = 0.2 for 20 time steps 
					if(number_steps < 200)then
						Cr = Crini
						if(number_steps == 1)then
                              temcel = 0.3*Dx(j)/pc
							Dt = Min(Dt,temcel)
						endif		
					endif
     					Dt = Min(Dt,Cr*Dx(j)/lambda)		
				elseif(Idflow(j,i) == 1)then  !Pressurized flow
					Cr = Cr_mixed  !Press. side of mixed flow
					lambda = pc !This is a good approximation given that Cr_pressur doesn't exceed 0.5 						
					Dt = Min(Dt,Cr*Dx(j)/lambda)
				else						
					write(98,*),'Subr. CFL_time_step. Idflow.ne.0,1'
                      write(99,*),'Subr. CFL_time_step. Idflow.ne.0,1'
					!write(98,*),'Idflow(j,i)',Idflow(j,i)
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif					
              enddo
          endif
	enddo
20	Dt_Courant = Dt
      end subroutine
