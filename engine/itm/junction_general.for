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

	subroutine junction_general(tf,Dt,R)
	!Purpose: This routine computes fluxes at a junction boundary of $N$ connecting pipes. 
	use common_module
      use itm_accessors
	implicit none 
	double precision temp1,temp2,temp3,temp4	
	integer i,j,prod,r,k,p,pp,s,sum
	integer n, N_Equations_to_solve
	double precision fvec(10), x(10)
	double precision RHIA(10)
	double precision P_phoIA,Qsum
	double precision Qinflow,Qinf_new,Qinf_old,tf,tim,Dt
	double precision AA,TS,RH,Wpred,wtemp3,Area
	double precision y,A,TsIA,hIA_old,hIB_old,Ab,Qb
	double precision ytemp,Qtemp,yb,ub
	double precision tempIA,Ares,h0b1,A0b1,Q0b1
	double precision yL,YR,AL,AR,QL,QR
	double precision fun,Dfun,yres,dely,av_y_conv
	double precision dr,inflowtemp,Ref_level,tol_local 
	Integer node_pressur_temp1(10),sum_press
	double precision elev_temp,Q_temp,Error_y
	doubleprecision	temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	double precision fvec_sum,Min_depth_Posit_interf
      double precision delta_h,yc_11,Ac_temp,Q_allow,Qc_temp,con1
      double precision Ynormal,P_phob,FL2,FR2
      double precision E11_infl,E11_outf,Eres,delta_h_check
      double precision yres_Riemann,count,vmax,Q_max,sum_vec
      double precision FF1L,FF1R,FF2L,FF2R,FF1,FF2
      double precision Minimum_y1_elev,Y_solv_mixed
	character(IDLEN) temp_id
	integer	CODL,IDF0,int_mult
	integer conver_result,info,node_pressur	
      integer mm,balan_junc,solve_full_equation
      double precision iterat_depth, vol,water_level_converg,y_water_min 
      integer IH,IDL,IDR,IDfb_reser,codeconvergence,Posit_interf
      external junction_solver,junction_solver_implicit
      external posit_dropsh_to_pipe,posit_pipe_to_dropsh

      !NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
	!NodeID(r,j) = pipe ID(1, 2, 3, ...) 
	!Nodetype = inflowing or outflowing

	!To enforce that the minimum water depth at pond is ydropmin(R)
	if (yres_jun_old(R) < ydropmin(R))then !ydropmin(R))then
		yres_jun_old(R) = ydropmin(R)
      endif	
	
	tim = tf	
	Ares = Ares_junct(R)
      Posit_interf =3
      
      		
	!Inflow hydrographs
      if (itm_has_inflow(R) == 1)then
          call itm_get_inflow(R,tim,Qinf_old)
	    call itm_get_inflow(R,tim+Dt,Qinf_new)  
      else
          Qinf_old = 0d0; Qinf_new = 0d0 
      endif 
      
	Qinflow = (Qinf_old+Qinf_new)/2d0 
      temp3 = Qinflow + PumpFlowToNode(R) !We are adding pump flow to compute the water elevation in the node 
      
	IDf1(:) = -1
	do j = 1, NodeNS(r)
 		k = NodeID(r,j)
 		dropIA(j) = Drop(R,j)
          dr = dropIA(j)              
		if (Nodetype(r,j) == 1)then !inflowing			
			IH = Nx(k)-2 
			y1(j) = h0(k,IH); Q1(j) = Q0(k,IH); A1(j)=A0(k,IH)
			IDf1(j) = IDFlow(k,IH)
		elseif (Nodetype(r,j) == 2)then !outflowing			
			IH = 3
			y1(j) = h0(k,IH); Q1(j) = Q0(k,IH); A1(j) = A0(k,IH)
			IDf1(j) = IDFlow(k,IH)
		else
			write(98,*),'Pipe not infl/outflowing. Junction_general'
              write(99,*),'Pipe not infl/outflowing. Junction_general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
              
		if(IDf1(j) == 0)then !Open Channel
			call Area_from_H(k,y1(j),Area,Ts,RH,0); 
			c1(j) = sqrt(g*Area/Ts)
		elseif (IDf1(j) == 1)then !Pressurized Flow
			c1(j) = pc
		else
			write(98,*),'IDf1(j)  .ne. 0,1. Junction_general'
              write(99,*),'IDf1(j)  .ne. 0,1. Junction_general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
          
		if(IDf1(j) == 0)then
			if (y1(j) <= ydry(k))then
				y1(j) = ydry(k); A1(j) = Adry(k); Q1(j) = 0d0		
			endif 
		endif	
		u1(j) = Q1(j)/A1(j)
          call Pressure_Pho(k,y1(j),P_pho1(j),IDf1(j))
      enddo	
          
	sum = 0
	do j = 1, NodeNS(r)
 		k = NodeID(r,j); dr = dropIA(j) 
          call Freesurface_flowregime(R,k,dr,IDf1(j),IDfbIA(j),Q1(j),
     &    y1(j),A1(j), yres_jun_old(R),flow_regIA(j),flowcaseIA(j),
     &    Nodetype(r,j),cond_mixed1(j),SumIDFIA(j))
          sum = sum + SumIDFIA(j)
      end do
      
      N_Equations_to_solve = 0
      do j = 1, NodeNS(r)
 		k = NodeID(r,j); dr = dropIA(j) 
          if(SumIDFIA(j) == 2)then
              solve_full_eq(j) = 1
          elseif (SumIDFIA(j) == 1)then
              solve_full_eq(j) = 0  
          elseif (SumIDFIA(j) == 0)then
              if (max(yres_jun_old(r) - dr, y1(j)) > 0.05*yref(k))then
				solve_full_eq(j) = 1
			else
				solve_full_eq(j) = 0 
			endif
          else
              write(98,*),'SumIDFIA .ne. 0,1,2. Junction_general'
              write(99,*),'SumIDFIA .ne. 0,1,2. Junction_general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return  
          endif
          N_Equations_to_solve = N_Equations_to_solve + solve_full_eq(j)
      enddo        
      
c     Here we either solve the Riemann problem or the mixed flow conditions for each individual pipe      
      do j = 1, NodeNS(r)	
          k = NodeID(r,j)			
          if (solve_full_eq(j) == 0)then
              if (Nodetype(r,j) == 1)then !inflowing
                  yL = y1(j); AL = A1(j); QL = Q1(j)
                  if (SumIDFIA(j) == 0 .or. SumIDFIA(j) == 1)then
                      yR = max(yres_jun_old(R)-dropIA(j),ydry(k))
                      QR = 0d0
                  else     
                      yR = yres_jun_old(R)-dropIA(j);QR = 0d0    
                  endif
              
                  if (SumIDFIA(j) == 0)then
                      IDL=0; IDR=0; call Area_from_H(k,yR,AR,Ts,RH,0)
                  elseif (SumIDFIA(j) == 2)then
                      IDL=1; IDR=1; call Area_from_H(k,yR,AR,Ts,RH,1)
                  elseif (SumIDFIA(j) == 1)then
                      if  (IDf1(j) ==0)then
                          call Area_from_H(k,yR,AR,Ts,RH,1)
                          CODL = 0; IDL = 0; IDR = 1
                      else 
                          call Area_from_H(k,yR,AR,Ts,RH,0)
                          CODL = 1; IDL = 1; IDR = 0
                      endif
                  else
                      write(98,*),'SumIDFIA.ne.0,1,2. junction_general1'
                      write(99,*),'SumIDFIA.ne.0,1,2. junction_general1'
			        call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
		    elseif(Nodetype(r,j) == 2)then !outflowing              
                  if (SumIDFIA(j) == 0 .or. SumIDFIA(j) == 1)then
                      yL = max(yres_jun_old(R)-dropIA(j),ydry(k))
                      QL = 0d0
                  else     
                      yL = yres_jun_old(R)-dropIA(j);QL = 0d0
                  endif    
              
                  if (SumIDFIA(j) == 0)then
                      call Area_from_H(k,yL,AL,Ts,RH,0)
                      IDL = 0; IDR = 0
                  elseif (SumIDFIA(j) == 2)then
                      IDL = 1; IDR = 1
                      call Area_from_H(k,yL,AL,Ts,RH,1)
                  elseif (SumIDFIA(j) == 1)then
                      if  (IDf1(j) ==0)then
                          call Area_from_H(k,yL,AL,Ts,RH,1)
                          CODL = 1; IDL = 1; IDR = 0
                      else 
                          call Area_from_H(k,yL,AL,Ts,RH,0)
                          CODL = 0; IDL = 0; IDR = 1
                      endif                      
                  else
                      write(98,*),'SumIDFIA.ne.0,1,2. junction_general2'
                      write(99,*),'SumIDFIA.ne.0,1,2. junction_general2'
                      call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
                      yR = y1(j); AR = A1(j); QR = Q1(j)
              else
                  write(98,*),'Pipe not inf. or outf. junction_general'
                  write(99,*),'Pipe not inf. or outf. junction_general'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif               
              
c             !solving mixed flow conditions (sum = 1) 
              if (SumIDFIA(j) == 1)then                       
                  yb = max(yres_jun_old(R)-dropIA(j),ydry(k))
                  Qb = 0d0
                  call Area_from_H(k,yb,Ab,Ts,RH,IDfbIA(j))   
                  Min_depth_Posit_interf = max(yb,y1(j))
              
                  wtemp3 = (Q1(j)-Qb)/(A1(j)-Ab)	
                  Wpred = abs(wtemp3)*SIGN(1d0,wtemp3)                
                  !call Area_from_H(k,yb,Ab,Ts,RH,IDfbIA(j))  
                  !call Pressure_Pho(k,yb,P_phob,IDfbIA(j))
                  !FL2 = Q1(j)*Q1(j)/A1(j) + P_pho1(j)
                  !FR2 = Qb*Qb/Ab + P_phob
                  !wtemp3 = (FL2 - FR2)/(Q1(j) - Qb)
                  !Wpred = abs(wtemp3)*SIGN(1d0,wtemp3)
          
                  !cond_mixed  = 1 --> pipe is in open channel conditions and junction is pressurized. 
                  !cond_mixed  = 2 --> pipe is in pressurized conditions and junction is open channel.
                  Y_solv_mixed = 1.03*yref(k)  
                  if (Nodetype(r,j) == 1)then !inflowing
                      if (Wpred < 0d0 .and. cond_mixed1(j)  == 1 .and.
     &                    Min_depth_Posit_interf  > Y_solv_mixed)then
                          Posit_interf = 1 !Positive interface, flow is from dropshaft to pipe
                      elseif (Wpred > 0d0 .and.cond_mixed1(j) == 2 .and.
     &                    Min_depth_Posit_interf  > Y_solv_mixed)then
                          Posit_interf = 2 !Positive interface, flow is from pipe to dropshaft
                      else 
                          Posit_interf = 3  !Negative interface
                      endif
                  elseif(Nodetype(r,j) == 2)then !outflowing
                      if (Wpred >= 0d0 .and. cond_mixed1(j) == 1 .and.
     &                    Min_depth_Posit_interf  > Y_solv_mixed)then
                          Posit_interf = 1 !Positive interface, flow is from dropshaft to pipe
                      elseif (Wpred < 0d0 .and. cond_mixed1(j)==2 .and.
     &                    Min_depth_Posit_interf  > Y_solv_mixed)then
                          Posit_interf = 2 !Positive interface, flow is from pipe to dropshaft
                      else 
                          Posit_interf = 3  !Negative interface
                      endif
                  else
                      write(98,*),'Nodetype .ne. 1,2. Junction_general'
                      write(99,*),'Nodetype .ne. 1,2. Junction_general'
                      call endprog; GLOBAL_STATUS_FLAG = 1; return  
                  endif
          
                  if (Posit_interf == 3)then !Negative interface
                      call Riemann_Mixed_HLL_Leon(k,SumIDFIA(j),
     &                IDL,IDR,yL,yR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,
     &                Nodetype(r,j),Qb)
                      Qb_oldIA(j) = Qb
		            goto 84
                  endif              
                          
                  param1 = A1(j); param2 = Q1(j); param3 = c1(j)
                  param4 = y1(j); param5 = P_pho1(j); 
                  param6 = yres_jun_old(R); param12 = dropIA(j)
                  parint1 = k; parint9 = Nodetype(r,j)
                  tol_local = Tol_int_10_5
                  if(Posit_interf == 1)then !Positive interface, flow is from dropshaft to pipe     
                      n = 1; x(1) = 0d0
                      tol_local = Tol_int_10_5
                      call hybrd1 (posit_dropsh_to_pipe,n,x,
     &                fvec,tol_local,info)
	                call converg (conver_result,info)
                      If (conver_result == 0)then		
                          !yb = yres_jun_old(R) - dropIA(j) - x(1)*x(1)/(2d0*g)  
                          ub = x(1)
                          if (Nodetype(r,j) == 1)then !inflowing	
                              yb = yres_jun_old(R)-dropIA(j)-
     &                        ub*ub/(2d0*g) + Kloss*ub*dabs(ub)/(2d0*g)
                          elseif (Nodetype(r,j) == 2)then !outflowing	
                              yb = yres_jun_old(R)-dropIA(j)-
     &                        ub*ub/(2d0*g) - Kloss*ub*dabs(ub)/(2d0*g)
                          else
                              write(98,*),'Nodetype.ne.1,2.Junction_gen'
                              write(99,*),'Nodetype.ne.1,2.Junction_gen'
		                    call endprog; GLOBAL_STATUS_FLAG = 1; return  
                          endif                  
                  
                          IDfbIA(j) = 0
                          if (yb > yref(k))IDfbIA(j) = 1              
                          call Area_from_H(k,yb,Ab,Ts,RH,IDfbIA(j))
	                    call Pressure_Pho(k,yb,P_phoIA,IDfbIA(j)) 
                          Qb = x(1)*Ab
                          
                          
                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          FF1 = Qb; FF2 = Qb*Qb/Ab + P_phoIA
						call Fluxes_boundary_HLL_Leon(k,yb,IDfbIA(j),IDL,IDR,
     &                    yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
c                         
						if (Nodetype(r,j) == 1 )then !inflowing
							Qb_oldIA(j) = FF1L 
						elseif (Nodetype(r,j) == 2)then !outflowing	
							Qb_oldIA(j) = FF1R 
						else
							write(98,*),'Nodetype .ne. 1,2. junc_gen.'	
                              write(98,*),'Nodetype .ne. 1,2. junc_gen.'
						    call endprog; GLOBAL_STATUS_FLAG = 1; return  
						endif                          
                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          
                          
                          !FF1L = Qb; FF1R = FF1L
                          !FF2L = Qb*Qb/Ab + P_phoIA; FF2R = FF2L 
                          !Qb_oldIA(j) = (FF1L+FF1R)/2d0
                          goto 84
                      endif 
                  elseif(Posit_interf == 2)then !Positive interface, flow is from pipe to dropshaft
                      n = 2; x(1) = y1(j); x(2) = 0d0              
                      tol_local = Tol_int_10_5
                      call hybrd1 (posit_pipe_to_dropsh,n,x,fvec,
     &                tol_local,info)
                      call converg (conver_result,info)
                      If (conver_result == 0)then		
                          yb = x(1); ub = x(2)
                          IDfbIA(j) = 0
                          if (yb > yref(k))IDfbIA(j) = 1  
                          call Area_from_H(k,yb,Ab,Ts,RH,IDfbIA(j)) 
	                    call Pressure_Pho(k,yb,P_phoIA,IDfbIA(j)) 
                          Qb = ub*Ab
                          
                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          FF1 = Qb; FF2 = Qb*Qb/Ab + P_phoIA
						call Fluxes_boundary_HLL_Leon(k,yb,IDfbIA(j),IDL,IDR,
     &                    yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
c                         
						if (Nodetype(r,j) == 1 )then !inflowing
							Qb_oldIA(j) = FF1L 
						elseif (Nodetype(r,j) == 2)then !outflowing	
							Qb_oldIA(j) = FF1R 
						else
							write(98,*),'Nodetype .ne. 1,2. junction'	
                              write(99,*),'Nodetype .ne. 1,2. junction'	
						    call endprog; GLOBAL_STATUS_FLAG = 1; return  
						endif                          
                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          
                          !FF1L = Qb ; FF1R = FF1L
                          !FF2L = Qb*Qb/Ab + P_phoIA; FF2R = FF2L  
                          !Qb_oldIA(j) = (FF1L+FF1R)/2d0
                          goto 84
                      endif 
                  else
                      write(98,*),'Unkn. posit.interf. Junction_general'
                      write(99,*),'Unkn. posit.interf. Junction_general'
                      call endprog; GLOBAL_STATUS_FLAG = 1; return 
                  endif
              elseif (SumIDFIA(j) == 0)then !this is near dry bed conditions
                  call Riemann_Mixed_HLL_Leon(k,SumIDFIA(j),
     &            IDL,IDR,yL,yR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,
     &            Nodetype(r,j),Qb)
                  Qb_oldIA(j) = Qb
                  
                  If (ISNAN(Qb))then 
				    write(98,*),'Qb is NaN. junction_general' 
                      write(99,*),'Qb is NaN. junction_general' 
				    call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif                  
                  goto 84 
              else
                  write(98,*),'SumIDFIA .ne. 0,1 Junction_general'
                  write(99,*),'SumIDFIA .ne. 0,1 Junction_general'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return 
              endif                  
      
60            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		    write(99,*),'t,No conv.Junc',t_global,sum_no_converg,trim(temp_id)
              call Riemann_Mixed_HLL_Leon(k,SumIDFIA(j),IDL,IDR,yL,yR,
     &        AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(r,j),Qb)
              Qb_oldIA(j) = Qb
          
              !Flux computation	
84            if (Nodetype(r,j) == 1)then !inflowing
                  Fdownst(k,1)=FF1L;Fdownst(k,2)=FF2L
		    elseif(Nodetype(r,j) == 2)then !outflowing			
                  Fupst(k,1)=FF1R; Fupst(k,2)=FF2R
		    else
			    write(98,*),'Pipe is not infl/outfl. Junction_general'
                  write(99,*),'Pipe is not infl/outfl. Junction_general'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          endif
      enddo
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&      
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   
	!solving equations: free surface and pressurized
	!n = 2*NodeNS(r)+1
      if (N_Equations_to_solve >= 1)then
          n = 2*N_Equations_to_solve + 1  
      else
          goto 2000
      endif
      
      parint1000 = 0 
	param1 = Ares; param13 = dt ; param17 = Qinf_new; param18 = Qinf_old
	parint4 = R
          
      count = 0
	do j = 1, NodeNS(R)		
		!p=2*j-1
          count = solve_full_eq(j) + count
          p = 2*count-1          
		if (solve_full_eq(j) == 1)then
			!To start iteration use y1 and Q1. These values are very stable 
			!compared to those of yb_oldIA(j) and Qb_oldIA(j)
			x(p) = y1(j)  !yb_oldIA(j)		
			x(p+1) = Q1(j) !Qb_oldIA(j)
          
			if (flowcaseIA(j) == 2)then 
				!x(1) = yres - dr; x(2) = yres; x(3) = 0d0              
				x(p) = yres_jun_old(r) - dropIA(j)	
				x(p+1) = Q1(j)
				!x(p+1) = 0d0
			else 
				x(p) = y1(j)  !yb_oldIA(j)		
				x(p+1) = Q1(j) !Qb_oldIA(j)
              endif
          endif
	enddo 
	x(n) = yres_jun_old(r)

	!Appropiate tolerance for solving equations	
	!tol_local = Tol_int_10_4	
	
      if(sum == 2*NodeNS(R))then
          tol_local = Tol_int_10_6
      else
         tol_local = Tol_int_10_4
      endif
	Counter_printing = 0
	call hybrd1 (junction_solver_implicit,n,x,fvec,tol_local,info) !the name of this is implicit
	call converg (conver_result, info)
      
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          
      !Flux computation
      count = 0
      do j = 1, NodeNS(r)
		k = NodeID(r,j)
		!p=2*j-1              
		count = solve_full_eq(j) + count
		p = 2*count-1
              
          if (solve_full_eq(j) == 1)then
			If (ISNAN(x(p)) .or. ISNAN(x(p+1)))then 
				write(98,*),'NaN is found in junction_general' 
                  write(99,*),'NaN is found in junction_general' 
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              if (Nodetype(r,j) == 1)then !inflowing
				yL = y1(j); AL = A1(j); QL = Q1(j)
				yR = yres_jun_old(R)-dropIA(j);QR = 0d0  
				if (SumIDFIA(j) == 0)then
					IDL=0; IDR=0; call Area_from_H(k,yR,AR,Ts,RH,0)
				elseif (SumIDFIA(j) == 2)then
					IDL=1; IDR=1; call Area_from_H(k,yR,AR,Ts,RH,1)
				else
					write(98,*),'SumIDFIA.ne.0,2. junction_general3'
                      write(99,*),'SumIDFIA.ne.0,2. junction_general3'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif                  
			elseif(Nodetype(r,j) == 2)then !outflowing              
				yL = yres_jun_old(R)-dropIA(j);QL = 0d0
				if (SumIDFIA(j) == 0)then
					call Area_from_H(k,yL,AL,Ts,RH,0)
					IDL = 0; IDR = 0
				elseif (SumIDFIA(j) == 2)then
					IDL = 1; IDR = 1
					call Area_from_H(k,yL,AL,Ts,RH,1)
				else
					write(98,*),'SumIDFIA.ne.0,1,2. junction_general4'
                      write(99,*),'SumIDFIA.ne.0,1,2. junction_general4'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
				yR = y1(j); AR = A1(j); QR = Q1(j)
			else
				write(98,*),'Pipe not infl/outfl. junction_general'
                  write(99,*),'Pipe not infl/outfl. junction_general'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
                  
              If(conver_result == 0)then	
                  yres_jun_old(R) = x(n)	  
                  call Area_from_H(k,x(p),AA,TsIA,RH,IDfbIA(j))
                  call Pressure_Pho(k,x(p),P_phoIA,IDfbIA(j))     
                  FF1 = x(p+1); FF2 = x(p+1)*x(p+1)/AA + P_phoIA
                  call Fluxes_boundary_HLL_Leon(k,x(p),IDfbIA(j),
     &            IDL,IDR,yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
              elseif(	conver_result == 1)then
                  !No convergence. Riemann problem will be ved
				call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                  
                  ! Commenting out next statement - LR
				!write(98,*),'t, No convergence at Node',T_GLOBAL,temp_id
                  
                  call Riemann_Mixed_HLL_Leon(k,SumIDFIA(j),IDL,IDR,yL,
     &            yR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(r,j),Qb)
                  Qb_oldIA(j) = Qb                   
                  if (ISNAN(Qb))then
                      write(98,*),'Qb is NaN. junction_general'
                      write(99,*),'Qb is NaN. junction_general'
                      call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
              else 
			    write(98,*),'Unkown cond. in Convergence, junction'
                  write(99,*),'Unkown cond. in Convergence, junction'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
		    endif   
c             Flux computation	
			if (Nodetype(r,j) == 1)then !inflowing
                  Fdownst(k,1)=FF1L; Fdownst(k,2)=FF2L
			elseif(Nodetype(r,j) == 2)then !outflowing			
                  Fupst(k,1)=FF1R; Fupst(k,2)=FF2R
			else
				write(98,*),'Pipe not inf or outf. Junction_general'	
                  write(99,*),'Pipe not inf or outf. Junction_general'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          endif              
      end do 
              
      If(conver_result == 0)then	
          goto 3000  
      elseif(	conver_result == 1)then        
          goto 2000       
      else 
		write(98,*),'Unkown cond. in Convergence, junction_general'
          write(99,*),'Unkown cond. in Convergence, junction_general'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif 
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&          
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!2000 = 88               
2000 	Qsum = 0d0 !Flow discharge
	do j = 1, NodeNS(r)
		if (Nodetype(r,j) == 1)then !inflowing
			Qsum = Qsum + Qb_oldIA(j)
		elseif (Nodetype(r,j) == 2)then !outflowing
			Qsum = Qsum - Qb_oldIA(j)
		else 
			write(98,*),'Pipe not inf or outf. Junction_general'	
              write(99,*),'Pipe not inf or outf. Junction_general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
	end do
	yres = yres_jun_old(R) + (temp3+Qsum)*dt/Ares
      
      if (yres < 0d0)then
          write(98,*),'yres < 0d0',yres
          write(98,*),'t_global',t_global
          write(99,*),'yres < 0d0',yres
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif  
      
      if (ISNAN(yres))then
          write(98,*),'yres(Reserv. depth) is NaN. junction_general'
          write(99,*),'yres(Reserv. depth) is NaN. junction_general'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif        
      
      if (yres > huge(0.0d0) .or. yres < -huge(0.0d0))then
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
          write(98,*),'Junction. yres is infin2. Node = ',trim(temp_id)
          write(99,*),'Junction. yres is infin2. Node = ',trim(temp_id)
          write(98,*),'Qinfl,PumpFlowToNo= ', Qinflow,PumpFlowToNode(R) !We are adding pump flow to compute the water elevation in the node 
          write(98,*),'Subr. junction_general, Yres= ', yres
          write(98,*),'yres_jun_old(R), temp3 = ',yres_jun_old(R), temp3
          write(98,*),'Qsum, Ares = ',Qsum, Ares  
          write(98,*),'conver_result = ',conver_result
          call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif       
      yres_jun_old(R) = yres
      
3000  do j = 1, NodeNS(r)
              k = NodeID(r,j)	
		    dr = dropIA(j)                              
              call Freesurface_flowregime(R,k,dr,IDf1(j),IDfbIA(j),
     &		Q1(j),y1(j),A1(j), yres_jun_old(R),flow_regIA(j),flowcaseIA(j),
     &		Nodetype(r,j),cond_mixed1(j),SumIDFIA(j))              
			if (Nodetype(r,j) == 1)then !inflowing
				IH = Nx(k)-2 + 1 
			elseif (Nodetype(r,j) == 2)then !outflowing
				IH = 3 - 1
              else 
				write(98,*),'Pipe is not inflow. or outflow. junction'
                  write(99,*),'Pipe is not inflow. or outflow. junction'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
              IDFlow(k,IH) = IDfbIA(j)
	end do      
      end subroutine
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  
  
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine junction_solver(n, x, fvec, iflag )
	use common_module
	implicit none
	integer n,iflag,r,j,k,p,itm_has_inflow
	double precision fvec(n),x(n),yreser
	double precision temp1,temp2,temp4
	double precision k1
	double precision AIA(10),yIA(10),ubA(10),cbA(10),RHIA(10) 
	double precision TsIA
	double precision temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	doubleprecision	TS,Wpred
	double precision y,A,con1,con2
      integer :: seed
	double precision ran_number
	parint1000 = 0; yreser = param1; r = parint4; j = parint2
	!Riemann invariants and freefall/constant state conditions	
      
		k = NodeID(r,j)
          
          If (ISNAN(x(1)) .or. ISNAN(x(2)))then 			
			write(98,*),'x(1), x(2)',x(1), x(2)	
              write(99,*),'x(1), x(2)',x(1), x(2)	
			call endprog; GLOBAL_STATUS_FLAG = 1; return
	    endif
          
          if (x(1) > yref(k)) x(1) = yref(k)
          if (x(1) < ydry(k)) x(1) = ydry(k)
          
          If (SumIDFIA(j) == 0)then 
			call Area_from_H(k,x(1),AIA(j),TsIA,RHIA(j),0)
			cbA(j) = sqrt(g*AIA(j)/TsIA)	
          elseif (SumIDFIA(j) == 2)then		
		    call Area_from_H(k,x(1),AIA(j),TsIA,RHIA(j),1)
              cbA(j) = pc
          else
				write(98,*),'SumIDFIA(j) == 2 .ne. 0,2. junction_solver'
				write(99,*),'SumIDFIA(j) == 2 .ne. 0,2. junction_solver'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
          
          ubA(j) = x(2)/AIA(j)
          if (abs(ubA(j)) > 10d0)then
              ubA(j) = 10d0*SIGN(1d0,ubA(j))
              x(2) = ubA(j)*AIA(j)
          endif          
          
		If (SumIDFIA(j) == 0) then
              !con2 = 1d0/(2d0*g)*ubA(j)**2d0
              !con2 = 0d0
			if (Nodetype(r,j) == 1)then !inflowing	
				fvec(1) = ubA(j) - u1(j) + (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))
				If (flowcaseIA(j) == 1)then	
					!free outfall or normal flow 
					if(flow_regIA(j) == 1)then	!Subcritical flow                        
						fvec(2) = ubA(j) - cbA(j) 
                      elseif(flow_regIA(j) == 2)then !Supercritical flow
						fvec(2) = ubA(j) - 
     &				        1d0/nm(k)*RHIA(j)**(2d0/3d0)*sqrt(S0(k))
					else
						write(98,*),'flow_regIA(j) not supported. junction_solver'
						write(99,*),'flow_regIA(j) not supported. junction_solver'
						call endprog; GLOBAL_STATUS_FLAG = 1; return
					endif
                  elseIf (flowcaseIA(j) == 2)then
                      if (u1(j) + c1(j) <= 0d0)then !the characteristic doesn't connect the boundary 
                              fvec(1)  = ubA(j) + cbA(j)
                      endif  				
         !                 fvec(2) = x(1) + ubA(j)*ubA(j)/(2d0*g) - 
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j))
                      fvec(2) = x(1) - (yreser-dropIA(j))
                  else
				    write(98,*),'flowcaseIA1 undef. junction_solver'	
                      write(99,*),'flowcaseIA1 undef. junction_solver'	
                      !write(98,*),'flowcaseIA(j)',flowcaseIA(j)
					call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
                  
              elseif(Nodetype(r,j) == 2)then !outflowing
				fvec(1) = ubA(j) - u1(j) - (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))                       
				If(flowcaseIA(j) == 1)then				    
					If(flow_regIA(j) == 10)then !flow is critical at the node (negative flow). Flow is from pipe to junction						
						!negative sign for critical depth (negative flow)
         !                     fvec(1) =  x(1) + ubA(j)*ubA(j)/(2d0*g) +
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j))
                          
                          fvec(1) = x(1) - (yreser-dropIA(j))   
						fvec(2) = -ubA(j) - cbA(j)						
                      elseif(flow_regIA(j) == 11)then
						!Riemann invariants can not be used for outflowing 
						!pipes and supercritical flows
						!positive sign for critical depth (positive flow)
					    !	fvec(1) =  x(1) + ubA(j)*ubA(j)/(2d0*g) +
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j))
         !                     
                          fvec(1) = x(1) - (yreser-dropIA(j))
                          fvec(2) = ubA(j) - cbA(j)
					else
						write(98,*),'flow_regIA(j) not supported. junction_solver'
						write(99,*),'flow_regIA(j) not supported. junction_solver'
						call endprog; GLOBAL_STATUS_FLAG = 1; return
					endif
				elseIf (flowcaseIA(j) == 2)then !Constant head
                      if (u1(j) - c1(j) >= 0d0)then !the characteristic doesn't connect the boundary 
                          fvec(1) = ubA(j) - cbA(j)
                      endif
         !                 fvec(2) = x(1) + ubA(j)*ubA(j)/(2d0*g) +
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j))                      
					fvec(2) = x(1) - (yreser-dropIA(j))
                  else
				    write(98,*),'flowcaseIA1 undef. junction_solver'	
                      write(99,*),'flowcaseIA1 undef. junction_solver'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
			else
				write(98,*),'Pipe not infl. or outfl. junction_solver'
                  write(99,*),'Pipe not infl. or outfl. junction_solver'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		elseif (SumIDFIA(j) == 2) then	
			if (Nodetype(r,j) == 1)then !inflowing	
				fvec(1) = ubA(j) + pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) + pc1(k)*LOG(A1(j)))	
			elseif(Nodetype(r,j) == 2)then !outflowing
				fvec(1) = ubA(j) - pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) - pc1(k)*LOG(A1(j)))
			else
				write(98,*),'Pipe not infl. or outfl. junction_solver'
                  write(99,*),'Pipe not infl. or outfl. junction_solver'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
			
			If(flowcaseIA(j) == 2)then
				if (Nodetype(r,j) == 1)then !inflowing	
					!fvec(2) = dropIA(j)+x(1)+con1-k1*con2 - yreser	
					!fvec(2) = dropIA(j) + x(1) - yreser
         !                 fvec(2) = x(1) + ubA(j)*ubA(j)/(2d0*g) - 
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j))
                      fvec(2) = x(1) - (yreser-dropIA(j))
				elseif(Nodetype(r,j) == 2)then !outflowing							
					!fvec(2) = dropIA(j)+x(1)+con1+k1*con2 - yreser	
					!fvec(2) = dropIA(j) + x(1) - yreser
         !                 fvec(2) = x(1) + ubA(j)*ubA(j)/(2d0*g) +
         !&                     0.5*ubA(j)*dabs(ubA(j))/(2d0*g)
         !&                     - (yreser-dropIA(j)) 
                      fvec(2) = x(1) - (yreser-dropIA(j))
				else
					write(98,*),'Pipe not infl. or outfl. jun_solver'
                      write(99,*),'Pipe not infl. or outfl. jun_solver'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif				
              else
			    write(98,*),'flowcaseIA1 undef. in subr. junction'	
                  write(99,*),'flowcaseIA1 undef. in subr. junction'	
                  !write(98,*),'flowcaseIA(j)',flowcaseIA(j)
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		else
			write(98,*),'SumIDFIA(j) .ne. 0, 2. Junction general'
              write(99,*),'SumIDFIA(j) .ne. 0, 2. Junction general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
100       continue
          end subroutine
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
   
c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine junction_solver_implicit(n, x, fvec, iflag )
	use common_module
	implicit none
	integer n,iflag,r,j,k,p,itm_has_inflow,count
	double precision fvec(n),x(n)
	double precision temp1,temp2,temp4
	double precision Qinf_new,Qinf_old,dt,k1,Qsum
	double precision AIA(10),yIA(10),ubA(10),cbA(10),RHIA(10)
	double precision TsIA,Ares,Horif,Qorif
	double precision temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
	doubleprecision	TS,Wpred
	double precision y,A,con1,con2,delta_h
	double precision yL,YR,AL,AR,QL,QR
      double precision Q_max1,Q_max2, Q_max, vmax
      double precision y_temp,A_temp,T_temp,R_temp
      integer :: seed
	double precision ran_number
	parint1000 = 0      
	Ares = param1
	dt = param13
	Qinf_new = param17; Qinf_old = param18
	r = parint4
      
	If (ISNAN(x(n)))then !To make sure that water depth at reservoir is not a NAN
		x(n) = yres_jun_old(R)
      endif
           
      vmax = 30d0 !max velocity = 30 m/s
	!compute Area and velocity
      count = 0
	do j = 1, NodeNS(r)		
		k = NodeID(r,j)
		!p=2*j-1	      
          count = solve_full_eq(j) + count
          p = 2*count-1
          if (solve_full_eq(j) == 1)then
			If (ISNAN(x(p)))then 
				call random_seed(seed)  
				call random_number(ran_number)              
				if(SumIDFIA(j) == 0)then
					x(p) = ran_number*yref(k) 
				elseif(SumIDFIA(j) == 2)then
					x(p) = yref(k) + ran_number*yref(k)   
				else 
					write(98,*),'SumIDFIA.ne.0,2. Junction_implic.'
                      write(99,*),'SumIDFIA.ne.0,2. Junction_implic.'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif              
			    parint1000 = 1 
			endif

			If (ISNAN(x(p+1)))then           
				  x(p+1) = 0d0
              endif          
				
			if(SumIDFIA(j) == 0 .or. SumIDFIA(j) == 2)then
				If (SumIDFIA(j) == 0)then
					If (x(p) > yref(k))then			
						x(p) = yref(k)
                      endif                      
                      
					If (x(p) < ydry(k))then			
						x(p) = ydry(k); x(p+1) = 0d0
					endif
					call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),0)
					cbA(j) = sqrt(g*AIA(j)/TsIA)	
				elseif (SumIDFIA(j) == 2)then		
					call Area_from_H(k,x(p),AIA(j),TsIA,RHIA(j),1)				
				else
					write(98,*),'SumIDFIA.ne.0,2. Junction_implic.'
                      write(99,*),'SumIDFIA.ne.0,2. Junction_implic.'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
              
                  If (abs(x(p+1)/AIA(j)) > vmax)then		
					x(p+1) =  vmax*AIA(j)*SIGN(1d0,x(p+1))
				endif	 
              
				ubA(j) = x(p+1)/AIA(j)
              endif
          endif
 90		continue
      end do	      
		
	!Riemann invariants and freefall/constant state conditions
      count = 0
	do j = 1, NodeNS(r)
		k = NodeID(r,j)
		!p=2*j-1          
          count = solve_full_eq(j) + count
          p = 2*count-1
          
          if (solve_full_eq(j) == 1)then
              If (SumIDFIA(j) == 0 .or. SumIDFIA(j) == 2) then	
				con1 = 1d0/(2d0*g)*u1(j)**2d0
				if (Nodetype(r,j) == 1)then !inflowing	
					k1 =  Klocal(k,2)
				elseif(Nodetype(r,j) == 2)then !outflowing
					K1 = Klocal(k,1)
				else
					write(98,*),'Pipe not infl. or outfl. junction'
                      write(99,*),'Pipe not infl. or outfl. junction'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
			endif               
              
			If (SumIDFIA(j) == 0) then
					!con2 = 1d0/(2d0*g)*ubA(j)**2d0
					!con2 = 0d0
				if (Nodetype(r,j) == 1)then !inflowing	
					fvec(p) = ubA(j) - u1(j) + (cbA(j) + c1(j))*
     &				(AIA(j) - A1(j))/(AIA(j) + A1(j))
					If (flowcaseIA(j) == 1)then	
						!free outfall or normal flow 
						if(flow_regIA(j) == 1)then	!Subcritical flow                     
							fvec(p+1) = ubA(j) - cbA(j) 
                          elseif(flow_regIA(j) == 2)then !Supercritical flow
							fvec(p+1) = ubA(j) - 
     &				        1d0/nm(k)*RHIA(j)**(2d0/3d0)*sqrt(S0(k))
						else
							write(98,*),'flow_regIA not sup. Ju.impl.'
                              write(99,*),'flow_regIA not sup. Ju.impl.'
							call endprog; GLOBAL_STATUS_FLAG = 1; return
						endif
                      elseIf (flowcaseIA(j) == 2)then
                              if (u1(j) + c1(j) <= 0d0)then !the characteristic doesn't connect the boundary 
								fvec(p)  = ubA(j) + cbA(j)
							endif 				
							fvec(p+1) = x(p) - (x(n)-dropIA(j))
                      else
						write(98,*),'flowcaseIA1 undef. jun_solver'
                          write(99,*),'flowcaseIA1 undef. jun_solver'
                          !write(98,*),'flowcaseIA(j)',flowcaseIA(j)
						call endprog; GLOBAL_STATUS_FLAG = 1; return
                      endif                  
                  elseif(Nodetype(r,j) == 2)then !outflowing
					    fvec(p) = ubA(j) - u1(j) - (cbA(j) + c1(j))*
     &				    (AIA(j) - A1(j))/(AIA(j) + A1(j))                       
					If(flowcaseIA(j) == 1)then				    
						If(flow_regIA(j) == 10)then						
							!negative sign for critical depth (negative flow)
							fvec(p+1) = -ubA(j) - cbA(j)						
                          elseif(flow_regIA(j) == 11)then
							!Riemann invariants can not be used for outflowing pipes and supercritical flows
							!positive sign for critical depth (positive flow)
							fvec(p) = x(p) - (x(n)-dropIA(j))                          
                              fvec(p+1) = ubA(j) - cbA(j)
						else
							write(98,*),'flow_regIA unkn. Jun_implic.'
                              write(99,*),'flow_regIA unkn. Jun_implic.'
							call endprog; GLOBAL_STATUS_FLAG = 1; return
						endif
					elseIf (flowcaseIA(j) == 2)then !Constant head
						if (u1(j) - c1(j) >= 0d0)then !the characteristic doesn't connect the boundary 
							fvec(p) = ubA(j) - cbA(j)
						endif        
						fvec(p+1) = x(p) - (x(n)-dropIA(j))
					else
						write(98,*),'flowcaseIA1 undef. Jun_implicit'
                          write(99,*),'flowcaseIA1 undef. Jun_implicit'
						call endprog; GLOBAL_STATUS_FLAG = 1; return
					endif
				else
					write(98,*),'Pipe not infl/outfl. Junct_implicit'
                      write(99,*),'Pipe not infl/outfl. Junct_implicit'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
			elseif (SumIDFIA(j) == 2) then	
				if (Nodetype(r,j) == 1)then !inflowing	
					fvec(p) = ubA(j) + pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) + pc1(k)*LOG(A1(j)))	                 
				elseif(Nodetype(r,j) == 2)then !outflowing
					fvec(p) = ubA(j) - pc1(k)*LOG(AIA(j)) - 
     &				(u1(j) - pc1(k)*LOG(A1(j)))
				else
					write(98,*),'Pipe not inf./outf. Junct_implicit'
                      write(99,*),'Pipe not inf./outf. Junct_implicit'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
			
				If(flowcaseIA(j) == 2)then
					!fvec(p+1) = dropIA(j)+x(p)-x(n)				
					if (Nodetype(r,j) == 1)then !inflowing	
						!fvec(p+1) = dropIA(j)+x(p)+con1-k1*con2 - x(n)	
						fvec(p+1) = dropIA(j) + x(p) - x(n)
					elseif(Nodetype(r,j) == 2)then !outflowing							
						!fvec(p+1) = dropIA(j)+x(p)+con1+k1*con2 - x(n)	
						fvec(p+1) = dropIA(j) + x(p) - x(n)
					else
						write(98,*),'Pipe not inf./outf. Jun_implicit'
                          write(99,*),'Pipe not inf./outf. Jun_implicit'
						call endprog; GLOBAL_STATUS_FLAG = 1; return
					endif				
				else				
					write(98,*),'flowcaseIA1 undef. Jun_implicit'
                      write(99,*),'flowcaseIA1 undef. Jun_implicit'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif              
			else
				write(98,*),'SumIDFIA(j) .ne. 0, 2. Junct_implicit'
                  write(99,*),'SumIDFIA(j) .ne. 0, 2. Junct_implicit'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          endif          
100		continue
      enddo	
	
	!fvec(n) represents the storage relation (if pond is not present, 
	!the storage relation reduces to the continuity equation)
      count = 0       
	Qsum = 0d0
	do j = 1, NodeNS(r)
		!p=2*j-1           
          count = solve_full_eq(j) + count
          p = 2*count-1
          
          if (solve_full_eq(j) == 1)then
			if (Nodetype(r,j) == 1)then !inflowing
				Qsum = Qsum + x(p+1)			
			elseif(Nodetype(r,j) == 2)then !outflowing
				Qsum = Qsum - x(p+1)
			else
				write(98,*),'Nodetype .ne. 1,2. Junct_implicit'
                  write(99,*),'Nodetype .ne. 1,2. Junct_implicit'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          elseif (solve_full_eq(j) == 0)then
              if (Nodetype(r,j) == 1)then !inflowing
				Qsum = Qsum + Qb_oldIA(j)			
			elseif(Nodetype(r,j) == 2)then !outflowing
				Qsum = Qsum - Qb_oldIA(j)
			else
				write(98,*),'Nodetype .ne. 1,2. Junct_implicit'
                  write(99,*),'Nodetype .ne. 1,2. Junct_implicit'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif
          else  
              write(98,*),'solve_full_eq.ne.0,1. Junction_solver_impl.'
              write(99,*),'solve_full_eq.ne.0,1. Junction_solver_impl.'
              call endprog; GLOBAL_STATUS_FLAG = 1; return      
          endif 
	end do
	temp2 = 5d-1*(Qinf_new+Qinf_old) + PumpFlowToNode(R) 
	temp4 = (x(n)-yres_jun_old(R))
      fvec(n) = (temp2+Qsum)*dt - temp4*Ares
120	continue
      end