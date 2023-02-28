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

	Subroutine itm_conser_volume (current_time,delta_time)
	!This routine computes the water entered to the system and the water stored in the system. 
	use common_module
      use itm_accessors
	implicit none	
	double precision Stora_new
	double precision tf,dt,current_time,delta_time
	double precision sum_temp1,sum_temp2,sum_temp3,sum_temp4
	double precision Qinf_old,Qinf_new,Vol_enter_node,Qb
	integer i,j,R
      character(IDLEN) temp_id
      
      double precision inflow_volume, outflow_volume
      
	tf = current_time
	dt = delta_time
      vol_reservoirs_time_step = 0d0
      vol_rating_time_step = 0d0
      vol_dropshafts_time_step = 0d0
      vol_junctions_time_step = 0d0
      vol_pipes_time_step = 0d0
      vol_inflows_time_step = 0d0
      vol_const_bound_time_step = 0d0      
      vol_lost_time_step = 0d0
      vol_reserv_outflow_time_step = 0d0
      
	!Volume stored in tunnels
	sum_temp1 = 0d0
      do j = 1,NR
          if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)    
              do i = 3,Nx(j)-2
                  if (ISNAN(A0(j,i)))then 	
                      call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                      write(98,*),'NaN is found in itm_conser_volume'
                      write(99,*),'NaN is found in itm_conser_volume'
				    !write(98,*),'Area',A0(j,i),'pipe No',temp_id,'cell',i
				    call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif              
                  sum_temp1 = sum_temp1 + A0(j,i)*dx(j)
                  vol_pipes_time_step =vol_pipes_time_step+A0(j,i)*dx(j)
              enddo
          endif
      enddo	
      
      !Volume stored in dropshafts, junctions and reservoirs
	sum_temp2 = 0d0; sum_temp3 = 0d0; sum_temp4 = 0d0      
	do R=1,Nnodes
          
          If(BCnode(R)==7 .or. BCnode(R)==4 .or. BCnode(R)==20)then 
              if (ISNAN(yres_jun_old(R)))then              
                  write(98,*),'NaN is found in subr. itm_conser_volume'
                  write(99,*),'NaN is found in subr. itm_conser_volume'
             !         call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
             !         write(98,*),'yres_jun_old(R)',yres_jun_old(R),
             !&				    'Node No',temp_id
                  call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          endif
		
          !Volume at dropshafts and junctions
          If(BCnode(R) == 7)then !junctions
			sum_temp2 = sum_temp2 + Ares_junct(R)*yres_jun_old(R)	
          endif
          
          If(BCnode(R) == 4)then !dropshafts			
			sum_temp3 = sum_temp3 + Ares_junct(R)*yres_jun_old(R)	
          endif          
          
		!Volume at reservoirs
		If(BCnode(R) == 20)then
			call itm_get_storage(R,yres_jun_old(R),Stora_new)
			sum_temp4 = sum_temp4 + Stora_new
		endif	
      enddo	
          
      Volume_stored_current_step =    sum_temp1 + sum_temp2 + 
     &                                sum_temp3 + sum_temp4
	
      
      vol_pipes_time_step = sum_temp1
      vol_junctions_time_step = sum_temp2
      vol_dropshafts_time_step = sum_temp3
      vol_reservoirs_time_step = sum_temp4
      
	!Vomume that entered/exited the system at 
	!vol = initial_volume + 
	!Vol_entered_system = 0d0
      
	do R=1,Nnodes
		!Volume entered at dropshafts and junctions				
          If(BCnode(R)==7 .or. BCnode(R)==4 .or. BCnode(R)==20)then !dropshafts, junctions and reservoirs may have inflows
			!call get_curve_area(R, tf+Dt, Vol_enter_node)
			!Vol_entered_system = Vol_entered_system + Vol_enter_node
			if (itm_has_inflow(R) == 1)then
                  call itm_get_inflow(R,tf,Qinf_old)
			    call itm_get_inflow(R,tf+Dt,Qinf_new)
              else 
                  Qinf_old = 0d0; Qinf_new = 0d0   
              endif
              
              
	!		Vol_inflows = Vol_inflows + 
      !&			(Qinf_new+Qinf_old)*Dt/2d0
              
              Vol_inflows = Vol_inflows + Qinf_old*Dt
              vol_inflows_time_step = 
     &        vol_inflows_time_step + Qinf_old*Dt
      !&        (Qinf_new+Qinf_old)*Dt/2d0
              
      !         vol_inflows_time_step = 
      !&        vol_inflows_time_step +
      !&        (Qinf_new+Qinf_old)*Dt/2d0
          endif
         
				
	    !Volume lost in the boundaries of the system
	    !Volume lost in a Rating curve BC
	    If(BCnode(R) == 30)then !Rating curve BC
	        j = NodeID(R,1)
              if (Nodetype(R,1) == 2)then !outflowing
                  Qb = abs(Qbound(j,1))
	        elseif (Nodetype(R,1) == 1)then !inflowing
		        Qb = abs(Qbound(j,2))
	        else
	            write(98,*),'itm_conser_volume. Nodetype .ne. 1,2'
		        write(99,*),'itm_conser_volume. Nodetype .ne. 1,2'
		        call endprog; GLOBAL_STATUS_FLAG = 1; return
              Endif
	        Vol_lost_system = Vol_lost_system + Qb*dt
              vol_rating_time_step = 
     &        vol_rating_time_step + Qb*dt
          endif            
		
          !Volume lost in a Q and h constant BC     
          !10: Discharge Q constant'
          !11: Water depth constant'
		If(BCnode(R) == 10 .or. BCnode(R) == 11)then  
          !If(BCnode(R) == 10)then     
	        j = NodeID(R,1)                                 		
              if (Nodetype(R,1) == 2)then !outflowing
		        Qb = -Qbound(j,1)
	        elseif (Nodetype(R,1) == 1)then !inflowing
		        Qb = Qbound(j,2)
	        else
	            write(98,*),'itm_conser_volume. Nodetype .ne. 1,2'
                  write(99,*),'itm_conser_volume. Nodetype .ne. 1,2'
		        call endprog; GLOBAL_STATUS_FLAG = 1; return
              Endif
	        Vol_lost_system = Vol_lost_system + Qb*dt
              vol_const_bound_time_step = 
     &        vol_const_bound_time_step + Qb*dt
          endif  
          
         !         If(BCnode(R) == 11)then 
         !             Qb = 0d0
         !             Vol_lost_system = Vol_lost_system + Qb*dt
         !             vol_const_bound_time_step = 
         !&            vol_const_bound_time_step + Qb*dt 
         !         endif	
          
		!Volume lost at reservoirs	!This needs to be updated as in many instances there is no outflow wnen the system is empty
		If(BCnode(R) == 20)then
			Vol_lost_system = Vol_lost_system + Outflow_limited(R)*dt    
              vol_reserv_outflow_time_step=vol_reserv_outflow_time_step
     &            + Outflow_limited(R)*dt
		endif	
      enddo
           
	
      vol_lost_time_step = vol_rating_time_step + 
     &        vol_const_bound_time_step + vol_reserv_outflow_time_step
          
              !Vol_entered_system = Initial_volume_stored + Vol_entered_system 	
	!Volume error of 5% may be acceptable	
      
!      Error_volume = (Initial_volume_stored+Vol_inflows-Vol_lost_system
!     & - Volume_stored_current_step)/
!     &  (Volume_stored_current_step + Vol_inflows)
      
      ! Corrected by LR
      inflow_volume = Initial_volume_stored + Vol_inflows
      outflow_volume = Vol_lost_system + Volume_stored_current_step
      Error_volume = inflow_volume - outflow_volume
      if (Error_Volume .ne. 0d0) then
          if (abs(outflow_volume) > 0d0) then
              Error_volume = Error_volume / outflow_volume
          else
              Error_volume = Error_volume / inflow_volume
          end if
      end if
      
      Error_volume = 100d0*Error_volume
	!Error_volume = Vol_entered_system - Volume_stored_current_step -
      !&	 Vol_lost_system
	!Error_volume = 100d0*Error_volume/(0.5*(Vol_entered_system +
      !&	Volume_stored_current_step + Vol_lost_system))
      


	!To balance conservation of volume problems
	balance_volume = Initial_volume_stored + Vol_inflows
     & - Vol_lost_system - Volume_stored_current_step
      ! abs(Vol_entered_system - 
      ! &	Volume_stored_current_step - Vol_lost_system)
      
      balance_volume_time_step=Vol_stored_old_time+vol_inflows_time_step
     & -vol_lost_time_step-Volume_stored_current_step
      
      
      
      !if (current_time > 30250d0)then
      !    write(98,*),'time, Error_volume ',current_time,Error_volume
      !write(98,*),'Volume_storedCurrent_step',Volume_stored_current_step
      !write(98,*),'vol_rating_time_step',vol_rating_time_step
      !write(98,*),'vol_const_bound_time_step',vol_const_bound_time_step
      !write(98,*),'vol_reserv_outflow_time',vol_reserv_outflow_time_step
      !write(98,*),' Out_limR)*dt',Outflow_limited(R)*dt
      !write(98,*),'Volume_stored_curr_step',Volume_stored_current_step
      !write(98,*),'vol_pipes_time_step ',vol_pipes_time_step
      !write(98,*),'vol_junctions_time_step ',vol_junctions_time_step
      !write(98,*),'vol_dropshafts_time_step ',vol_dropshafts_time_step
      !write(98,*),'vol_reservoirs_time_step ',vol_reservoirs_time_step
          
     
      
      !   write(98, '(5 F18.3)'), current_time, vol_pipes_time_step, 
      !& vol_junctions_time_step, vol_dropshafts_time_step, 
      !&  vol_reservoirs_time_step
       
          
      !!!       write(98, '(6 F12.2)'), Vol_inflows, Vol_lost_system,
      !!!& Volume_stored_current_step, Vol_stored_old_time, 
      !!!&  Volume_stored_current_step,vol_lost_time_step
      !!  
      !endif       
	end subroutine
