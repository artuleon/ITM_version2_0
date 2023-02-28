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

      subroutine Gate_two_pipes(tf,Dt,R)
	!Purpose: To compute the fluxes at a looped junction boundary 
	use common_module
      use itm_accessors
	implicit none 	
	integer j,R,k,p,IH,RR
	integer n  
	double precision fvec(10), x(10)
	double precision RHIA(10)
	double precision P_phoIA,phibound,hbound,Ab,uaver      
	double precision tf,tim,Dt,t_days
	double precision AA,RH,TsIA,hCentL,hSurL	
	double precision h0b1,A0b1,Q0b1	
	double precision tol_local	
	double precision con1,Av,gate_open_height_meters
	double precision Q_gate,coeff_k_gate
	double precision Q_max,Q_max1,Q_max2, vmax,Q_gate_temp       
	double precision ctrl_depth,initOpening
	character(IDLEN) temp_id
	integer conver_result,info
	external gate_solver 
	integer ctrl_node	  
	integer ctrlNode_id 
      
	tim = tf
	k = NodeID(R,1) 
      
      !new gate opening = itm_get_gate_opening(node_id, time, Dt, Old gateOpening)
      ! node_id is the id of the Gate node 
      t_days = tim   !/(24d0*3600d0)
            
c     Find gate opening.
c     This function in itm_accessors automatically identifies time series control, depth control or no control. 
c     The function also adjusts for time of gate opening/closing
c     Hgate_open(R) on teh right is the old gate opening and on the left is the new one. 
c     itm_get_gate_opening() function determines how much actual change should occur in the gate setting based on the opening rate 
c     and current time step. (E.g., if the time step were only 10% of the opening rate, then only 10% of the difference between the current 
c     opening and target opening would be used to adjust the current opening.)


      Hgate_open(R) = itm_get_gate_opening(R, tim, Dt, Hgate_open(R)) 
	
	!Gate height in meters
	h_gate_m(R) = Hgate_open(R)*d(k)/100d0
		
	!Head loss coefficient (Use gate opening in % because K is given as a function of it)
	call itm_get_gate_loss_coeff(R,Hgate_open(R),coeff_k_gate)	
	Cd_gate(R) = 1d0/sqrt(coeff_k_gate)	
	gate_open_height_meters = h_gate_m(R)
	
	If (ISNAN(gate_open_height_meters))then 
          temp_id = ''; call itm_get_swmm_id(0,R,temp_id) ! 0 for nodes 
          write(98,*),'Gate_two_pipes. Undef. open. Node ',trim(temp_id)
          write(99,*),'Gate_two_pipes. Undef. open. Node ',trim(temp_id)
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
	do j = 1, NodeNS(R)
 		    k = NodeID(R,j)              
              if (Nodetype(R,j) == 1)then !inflowing				
                  IH = Nx(k)-2; y1(j) = h0(k,IH); A1(j) = A0(k,IH)
                  Q1(j) = Q0(k,IH); IDf1(j) = IDFlow(k,IH) 	
              elseif (Nodetype(R,j) == 2)then !outflowing
                  IH = 3; y1(j) = h0(k,IH); A1(j) = A0(k,IH)
                  Q1(j) = Q0(k,IH); IDf1(j) = IDFlow(k,IH)     
		    else
			    write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
                  write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
              
		    u1(j) = Q1(j)/A1(j)		    
		    if (IDf1(j) ==0)then
                  call Area_from_H(k,y1(j),AA,TsIA,RHIA(j),IDf1(j))
                  call Pressure_Pho(k,y1(j),P_pho1(j),IDf1(j))
		        c1(j) = sqrt(g*A1(j)/TsIA)
              elseif(IDf1(j) ==1)then
                  call CentroidHeight_from_H(k,y1(j),hCentL,hSurL,1) 	
                  P_pho1(j) = Aref(k)*g*(hCentL + hSurL)
		        c1(j) = pc	
		    else	    
		        write(98,*),'IDf1(j) .ne. 0,1, Subrout. Gate_two_pipes'
			    write(99,*),'IDf1(j) .ne. 0,1, Subrout. Gate_two_pipes'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
		    endif    
      enddo	              
	
	!To avoid error in computations when the flow depth is zero
	do j = 1, NodeNS(R)
          k = NodeID(R,j)
	    if(IDf1(j) == 0)then
			if (y1(j) < ydry(k))then
				y1(j) = ydry(k); A1(j) = Adry(k)
				P_pho1(j) = P_pho_dry(k); Q1(j) = 0d0; u1(j)=0d0	
			endif 
		endif		
      enddo
	
	!If gate invert is above the crown of the pipe, gate equations are NOT used. 
	!If flow is in free surface conditions but gate invert elevation is above 
	!the free surface elevation, gate equations are also NOT used. 
	!Instead we use the routine junct2pipes_same_diam
	
	!When gate is closed or almost closed	
	k = NodeID(R,1)  !This k is necessary
	If (h_gate_m(R) <= 2d0*ydry(k))then 	
          do j = 1, NodeNS(R)
		    k = NodeID(R,j)
              if (IDf1(j) == 0)then !Open Channel
                  if (Nodetype(R,j) == 1)then !inflowing		(U+phi)boundary = 	(U+phi)internal	
                      call Phi1(k,y1(j),phi11(j))
                      phibound =  u1(j) + phi11(j)
                      call H_from_Phi(k,phibound,hbound)
					call Pressure_Pho(k,hbound,P_phoIA,IDf1(j))
					Fdownst(k,1) = 0d0;	Fdownst(k,2) = 0d0 + P_phoIA
                  elseif (Nodetype(R,j) == 2)then !outflowing   (U-phi)boundary = 	(U-phi)internal	
				    call Phi1(k,y1(j),phi11(j))
                      phibound =  phi11(j) - u1(j)  
                      if (phibound < phi_dry(k))then
                          phibound = phi_dry(k)
                      endif                       
                      call H_from_Phi(k,phibound,hbound)
					call Pressure_Pho(k,hbound,P_phoIA,IDf1(j))					
                      Fupst(k,1) = 0d0; Fupst(k,2) = 0d0 + P_phoIA	
				else
					write(98,*),'Pipe not inf./outf. Gate_two_pipes'
                      write(99,*),'Pipe not inf./outf. Gate_two_pipes'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
              elseif (IDf1(j) == 1)then !Pressurized flow
                  if (Nodetype(R,j) == 1)then !inflowing	 Qb - Q1 = (uaver - a)(Ab-A1)	
                      uaver = u1(j)/2d0
                      Ab =  A1(j) - Q1(j)/(uaver - c1(j))
                      call H_from_Area(k,Ab,hbound,902,IDf1(j))
                      call CentroidHeight_from_H(k,hbound,
     &                   hCentL,hSurL,IDf1(j)) 	
                      P_phoIA = Aref(k)*g*(hCentL + hSurL)
					Fdownst(k,1) = 0d0; Fdownst(k,2) = 0d0 + P_phoIA
                  elseif (Nodetype(R,j) == 2)then !outflowing   Qb - Q1 = (uaver + a)(Ab-A1)	
				    uaver = u1(j)/2d0
                      Ab =  A1(j) - Q1(j)/(uaver + c1(j))
                      call H_from_Area(k,Ab,hbound,903,IDf1(j))
                      call CentroidHeight_from_H(k,hbound,
     &                   hCentL,hSurL,IDf1(j)) 	
                      P_phoIA = Aref(k)*g*(hCentL + hSurL)
                      Fupst(k,1) = 0d0; Fupst(k,2) = 0d0 + P_phoIA	
				else
					write(98,*),'Pipe not inf./outf. Gate_two_pipes'
                      write(99,*),'Pipe not inf./outf. Gate_two_pipes'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif                  
                  !call endprog; GLOBAL_STATUS_FLAG = 1; return
              else
                  write(98,*),'IDf1(j) .ne. 0,1. Gate_two_pipes'
                  write(99,*),'IDf1(j) .ne. 0,1. Gate_two_pipes'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return  
              endif
		enddo
		goto 20  
	endif	
	
	k = NodeID(R,1) !This k is necessary
	if (h_gate_m(R) >= (1d0-0.01)*Yref(k))then
          call junct2pipes_same_diam(R)
          goto 20
	elseIf(y1(1)<= (1d0+0.01)*h_gate_m(R) .and. 
     &	    y1(2)<= (1d0+0.01)*h_gate_m(R))then 
          call junct2pipes_same_diam(R)
          goto 20
      endif	
      
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	!solving equations: free surface, pressurized and combined	
	!Pure free surface flow or combined	
	parint1000 = 0 
	param14 = tim	
	parint4 = R				
	n = 2*NodeNS(R)
	do j = 1, NodeNS(R)
		    !To start interation use y1 and Q1. 
		    p=2*j-1
		    x(p) = y1(j)
		    x(p+1) = Q1(j)
	enddo 

	!Appropiate tolerance for solving equations	
	tol_local = Tol_int_10_6
	!Solving equation in routine gate_solver
	call hybrd1 (gate_solver,n,x,fvec,tol_local,info)
	call converg (conver_result,info)
      If(conver_result == 1 .or. parint1000 == 1)then
		temp_id = ''
		call itm_get_swmm_id(0,R,temp_id) ! 0 for nodes      		
          write (99,*),'no converg. Gate boundary at node',trim(temp_id)
            						    
		!Flow across a gate
		k = NodeID(R,1)
		call Area_from_H(k,h_gate_m(R),Av,TsIA,RH,0)
		x(1) = y1(1); x(3) = y1(2)
		con1 = x(1)-x(3)
		Q_gate_temp = Cd_gate(R)*Av*sqrt(2d0*g*abs(con1))
		vmax = 20d0
		Q_max1 = vmax*Av                  
		Q_max2 = (1d0/Dt)*Aref(k)*dx(k)/2d0 
		Q_max = min(Q_max1,Q_max2)
		if (Q_gate_temp > Q_max)then
			Q_gate_temp = Q_max  
		endif             
              
		!Submerged and free condition
		if (con1 >= 0d0) then
			Q_gate = Q_gate_temp
		else
			Q_gate = -Q_gate_temp
		endif
			    			    
          !Continuity equation (This will be always conserved regardless of gate position	
          !First pipe
		if (Nodetype(R,1) == 1)then !inflowing
			x(2) = Q_gate
		elseif (Nodetype(R,1) == 2)then !outflowing
			x(2) = -Q_gate
		else
			write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
              write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif	

		!Second pipe 
		if (Nodetype(R,2) == 2)then !outflowing
			x(4) = Q_gate
		elseif (Nodetype(R,2) == 1)then !inflowing
			x(4) = -Q_gate
		else
			write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
              write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif		    
      endif      

	!Flux computation	
	do j = 1, NodeNS(R)
		k = NodeID(R,j)
		p=2*j-1
		If (ISNAN(x(p)) .or. ISNAN(x(p+1)))then 
              temp_id = ''
		    call itm_get_swmm_id(0,R,temp_id) ! 0 for nodes      		
              write (98,*),'NaN in Gate_two_pipes. Node ',trim(temp_id)
              write (99,*),'NaN in Gate_two_pipes. Node ',trim(temp_id)
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif

		if (Nodetype(R,j) == 1)then !inflowing
			call Area_from_H(k,x(p),AA,TsIA,RHIA(j),IDf1(j))
			call Pressure_Pho(k,x(p),P_phoIA,IDf1(j))
			Fdownst(k,1) = x(p+1)
			Fdownst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA
			Qbound(k,2) = x(p+1)
			!To avoid mixed flow problems in gate boundary			
			if (x(p) < yref(k) .and. Idf1(1) .ne.  Idf1(2))then
			    Idf1(j) = 0
			endif			
		elseif(Nodetype(R,j) == 2)then !outflowing			
			call Area_from_H(k,x(p),AA,TsIA,RHIA(j),IDf1(j))
			call Pressure_Pho(k,x(p),P_phoIA,IDf1(j))
			Fupst(k,1) = x(p+1)
			Fupst(k,2) = x(p+1)*x(p+1)/AA + P_phoIA	
			Qbound(k,1) = x(p+1)			
			!To avoid mixed flow problems in gate boundary			
			if (x(p) < yref(k) .and. Idf1(1) .ne.  Idf1(2))then
			    Idf1(j) = 0
			endif					
		else
			write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
              write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
	end do
      return	
 20   yres_jun_old(R) = min(h_gate_m(R),y1(1),y1(2))  !water level is specified the same as the gate invert. 
      continue
      end subroutine
		
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine gate_solver(n, x, fvec, iflag )
	use common_module
	implicit none
	integer n,iflag,R,j,k,p
	character*25 temp_id
	double precision fvec(n),x(n),AIA(10),ubA(10),cbA(10),RHIA(10)
	double precision TsIA,tim,RH,con1,Av,Q_gate
	parint1000 = 0
	tim = param14		
	R = parint4	
	!compute Area and velocity
	do j = 1, NodeNS(R)		
		k = NodeID(R,j)
		p=2*j-1
		    
		!To check that flow depth is not infinite
		If (ISNAN(x(p)))then 
			x(p) = y1(j); parint1000 = 1
		endif

		If (ISNAN(x(p+1)))then 
			x(p+1) = Q1(j); parint1000 = 1
		endif				
    		
		If (IDf1(j) == 0)then
              if(x(p) > yref(k)) x(p) = yref(k)
              if(x(p) < ydry(k)) x(p) = ydry(k)              
			call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),0)
			cbA(j) = sqrt(g*AIA(j)/TsIA)														
		elseif (IDf1(j) == 1)then		
			call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),1)				
		else
			write(98,*),'IDf1(j) .ne. 0,1. Gate_two_pipes'
			write(99,*),'IDf1(j) .ne. 0,1. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
		ubA(j) = x(p+1)/AIA(j)
 		continue
	end do
	
	!Flow across a gate
	k = NodeID(R,1)
	con1 = x(1)-x(3)	
	call Area_from_H(k,h_gate_m(R),Av,TsIA,RH,0)
	If (h_gate_m(R) <= (1d0+Tol_int_10_2)*ydry(k))then 
		Q_gate = 0d0; x(2) = 0d0; x(4) = 0d0
		fvec(2) = 0d0; fvec(4) = 0d0
		goto 107		    
	elseif (x(1) <= (1d0+Tol_int_10_2)*ydry(k) .and. 
     &	    x(3) <= (1d0+Tol_int_10_2)*ydry(k))then 
		!This condition corresponds to very shallow water depths regardless of gate position 
		Q_gate = 0d0; x(2) = 0d0; x(4) = 0d0
		fvec(2) = 0d0; fvec(4) = 0d0
		goto 107
      elseIf(x(1)<= (1d0+Tol_int_10_2)*h_gate_m(R) .and. 
     &    x(3)<= (1d0+Tol_int_10_2)*h_gate_m(R))then 
		fvec(4) = x(1)-x(3)   !Free surface flow. No influence of gate
		if (Nodetype(R,1) == Nodetype(R,2))then !inflowing
			fvec(2) = x(2) + x(4)  !Continuity equation
		elseif (Nodetype(R,1)+Nodetype(R,2) == 3)then !outflowing
			fvec(2) = x(2) - x(4)  !Continuity equation
		else
			write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
              write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
		goto 107
	elseIf(h_gate_m(R) >= (1d0-0.01)*Yref(k))then 
		!When the gate invert elevation is near the crown of the pipe, gate equations are not used.
		fvec(4) = x(1)-x(3)   !Free surface flow. No influence of gate
		if (Nodetype(R,1) == Nodetype(R,2))then !inflowing
			fvec(2) = x(2) + x(4)  !Continuity equation
		elseif (Nodetype(R,1)+Nodetype(R,2) == 3)then !outflowing
			fvec(2) = x(2) - x(4)  !Continuity equation
		else
			write(98,*),'Pipe not infl. or outfl. Gate_two_pipes'
              write(99,*),'Pipe not infl. or outfl. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
		goto 107
	else
		!Submerged condition or free condition
		if (con1 >= 0d0) then
			Q_gate = Cd_gate(R)*Av*sqrt(2d0*g*con1)
		else
			Q_gate = -Cd_gate(R)*Av*sqrt(2d0*g*abs(con1))
		endif
	endif	
	
	!Continuity equation (This will be always conserved regardless of gate position	
	!First pipe
	if (Nodetype(R,1) == 1)then !inflowing
		!x(2) = Q_gate
		fvec(2) = x(2)-Q_gate
	elseif (Nodetype(R,1) == 2)then !outflowing
		!x(2) = -Q_gate
		fvec(2) = x(2)+Q_gate
	else
		write(98,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
          write(99,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif	

	!Second pipe 
	if (Nodetype(R,2) == 2)then !outflowing
		!x(4) = Q_gate
		fvec(4) = x(4)-Q_gate
	elseif (Nodetype(R,2) == 1)then !inflowing
		!x(4) = -Q_gate
		fvec(4) = x(4)+Q_gate
	else
		write(98,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
          write(99,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
107	ubA(1) = x(2)/AIA(1)
	ubA(2) = x(4)/AIA(2)	
	!Riemann invariants and freefall/constant state conditions
	do j = 1, NodeNS(R)
		k = NodeID(R,j)
		p=2*j-1
		If (IDf1(j) == 0) then !Check this routine 
			If (x(p) < ydry(k))then !For dry beds
				fvec(p) = x(p) - 0.5*ydry(k)
				fvec(p+1) = x(p+1)-0d0
				goto 100
			endif
		endif
     
		If (IDf1(j) == 0) then						
			if (Nodetype(R,j) == 1)then !inflowing	
				fvec(p) = ubA(j) - u1(j) + (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))				
			elseif(Nodetype(R,j) == 2)then !outflowing
				fvec(p) = ubA(j) - u1(j) - (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))
			else
				write(98,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
				write(99,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		elseIf (IDf1(j) == 1) then	
		    if (Nodetype(R,j) == 1)then !inflowing	
				fvec(p) = ubA(j) + pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) + pc1(k)*LOG(A1(j)))				
			elseif(Nodetype(R,j) == 2)then !outflowing
				fvec(p) = ubA(j) - pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) - pc1(k)*LOG(A1(j)))
			else
				write(98,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
				write(99,*),'Pipe not inflowing or outflowing. Gate_two_pipes'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		else
			write(98,*),'SumIDFIA(j) .ne. 0, 1, and 2. Subr. Gate_two_pipes'
			write(99,*),'SumIDFIA(j) .ne. 0, 1, and 2. Subr. Gate_two_pipes'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
100		continue
	enddo
	end subroutine