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

      subroutine boundariesROE(time,Dt)
	!This routine is the main module for computing fluxes at boundaries
      use common_module
	implicit none 
	integer R 
      double precision time,Dt
	      
	do R=1,Nnodes	
		If(BCnode(R) == 7)then !junction for pipes >= 2
			call junction_general(time,Dt,R)
          elseIf(BCnode(R) == 24)then  !Junction 2 pipes with same diameter (without inflows) 
			!we assign internally the ID of 24.
              call junct2pipes_same_diam(R)                  
		elseIf(BCnode(R) == 20)then  !Reservoirs				
			call Reservoirs(Dt,R)				 					    
		elseIf(BCnode(R) == 4)then  !dropshafts				    		    	
			call Dropshaft_general(time,Dt,R)
          elseIf(BCnode(R) == 10 .or. BCnode(R) == 11)then  !constant boundary
              call const_bound(R)
		elseIf(BCnode(R) == 30)then !Rating curve boundary
			call Rating_curve(R)								
		elseIf(BCnode(R) == 40)then !Gate Boundary condition (Two pipes)
			!It supports both open-channel and pressurized flow			
			call Gate_two_pipes(time,Dt,R)
		elseIf(BCnode(R) == 41)then !Gate Boundary condition (one pipe)
			!It supports both open-channel and pressurized flow			
			call Gate_one_pipe(time,Dt,R)				
		else
			write(98,*),'Subr. boundaries. Type not supported'
              write(99,*),'Subr. boundaries. Type not supported'
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      enddo
      
	!For overflows
	sum_temp = sum_temp +1 
      end subroutine 