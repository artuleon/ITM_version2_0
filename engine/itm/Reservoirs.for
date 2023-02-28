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

	subroutine Reservoirs(Dt,R) 
	!Reservoir boundary condition		
	!Purpose: This routine is used for computing fluxes at a reservoir boundary.  
	!At this stage only one inflow/outflow pipe is considered. 
	!Later may be added "n" inflow/outflow pipes. 
	use common_module
      use itm_accessors
	implicit none	 
      integer k,sum,n,R,ID11,Idb,cond_mixed,CODL,Idf0
	double precision A,Ts,RH,dt,Stora_old
      double precision AA,u11,y11,A11,Q11,c11,T1,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob,temp4
	double precision yL,yR,AL,QL,AR,QR,h0b1,A0b1,Q0b1	
	double precision yres,dr,tol_const_state
	double precision hIA_old,hIA,hIC,ScIA,Ycrit
	double precision ytemp,TEMPIA,k1,Ares_aprox,y
	double precision x(10),fvec(10),tol_local
	double precision Stora_new,inflowtemp,Q_allow
	double precision fun,Dfun,av_y_conv,dely,Ref_level
	double precision temp_con1,temp_con2,temp_con3,temp_con4
      double precision Pw0,temp3     
      double precision delta_h, Q_flux_max, veloc_max, Qvalue
      double precision Q_magnitude, Y_temp_storage,y_water_min 
      double precision Stora_temp,tf,Storage_new
      double precision Hmax,yc,Ac,Qc,Qb_temp
      double precision FF1L,FF1R,FF2L,FF2R,F11,F12,FF1,FF2 
      double precision Wpred,wtemp3,Y_solv_mixed,Min_depth_Posit_interf
	character(IDLEN) temp_id
	integer i,conver_result,info
	integer flowcaseIA1,FLOW_REGIA1,drybed11,Posit_interf 
      integer IH, IDL,IDR,IDfb_reser
      external Reser 
      external posit_dropsh_to_pipe,posit_pipe_to_dropsh

       if (NodeNS(R) > 0)then  !NodeNS(R) = Number of pipes connected to each node     
          dr = Drop(R,1)	
	    k = NodeID(R,1)	
       endif       
      
      tf = T_GLOBAL  
      	      
      if (yres_jun_old(R) >= reser_maxdepth(R))then
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes   
          write(99,*),'Stage-storage is exceeded in Reservoir ',temp_id
          write(99,*),'yres,yresmax',yres_jun_old(R),reser_maxdepth(R)
          write(98,*),'Stage-storage is exceeded in Reservoir ',temp_id
          write(98,*),'yres,yresmax',yres_jun_old(R),reser_maxdepth(R)
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif  
            
      Outflow_limited(R) = Reser_outflow(R) !Reservoir outflow. This may be zero for shallow water depths
      if (NodeNS(R) == 0)then 
          if (yres_jun_old(R) <= 0d0)then
              yres_jun_old(R) = 0d0; Outflow_limited(R) = 0
          endif  
      else
      !    if (yres_jun_old(R) < ydropmin(R))then !ydropmin(R))then
		    !yres_jun_old(R) = ydropmin(R); Outflow_limited(R) = 0
      !    endif	
          
          if (yres_jun_old(R) < 0d0)then
              yres_jun_old(R) = 0d0; Outflow_limited(R) = 0
          endif  
      endif
      
      
      call itm_get_storage(R,yres_jun_old(R),Stora_old) 
      
      if (Stora_old < 0d0)then
          write(98,*),'Storage data must start with zero depth. Reserv.'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
      !If there are only pumps connected to the reservoir node      
      if (NodeNS(R) == 0)then  !NodeNS(R) = Number of pipes connected to each node     
          temp3 = PumpFlowToNode(R) - Outflow_limited(R)
          Storage_new = Stora_old + temp3*dt
          call itm_get_storage_depth(R,Storage_new, yres) 
          
          if (yres < 0d0)then
              call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes              
              write(98,*),'There is a pump at Node ',trim(temp_id)
              write(98,*),'causing negative water depth at the node. '
              write(98,*),'Increase the node area or change pump curve'
              write(98,*),'or the control curve. Subr. Reservoirs' 
              write(99,*),'There is a pump at Node ',trim(temp_id)
              write(99,*),'causing negative water depth at the node. '
              write(99,*),'Increase the node area or change pump curve' 
              write(99,*),'or the control curve. Subr. Reservoirs'              
              call endprog; GLOBAL_STATUS_FLAG = 1; return              
          endif  
          
          yres_jun_old(R) = yres
          goto 300 
      endif
      
      !To enforce that the minimum water depth at pond is ydropmin(R). This is not for reservoir with pumps     
      if (yres_jun_old(R) < 0d0)then
		yres_jun_old(R) = 0d0
          Outflow_limited(R) = 0d0
      endif
      
		
 	if (Nodetype(R,1) == 2)then !outflowing 
          IH = 3
          y11 = h0(k,IH); Q11 = Q0(k,IH); A11=A0(k,IH)
          ID11= IDFlow(k,IH)        	    
      Elseif (Nodetype(R,1) == 1)then !inflowing 
          IH = Nx(k)-2  
          y11 = h0(k,IH); Q11 = Q0(k,IH); A11=A0(k,IH)
          ID11= IDFlow(k,IH)
	Else
		write(98,*),'Nodetype .ne. 1,2. Subr. Reservoirs'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      Endif	         
      
      call Freesurface_flowregime(R,k,dr,ID11,IDfb_reser,Q11,y11,A11,
     & yres_jun_old(R),FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum)
      Idb = IDfb_reser      
      
      !Determining left and right states 
      if (Nodetype(R,1) == 1)then !inflowing          
          yL = y11; AL = A11; QL = Q11; IDL = ID11       
          if (sum == 0 .or. sum == 1)then
              yR = max(yres_jun_old(R)-Drop(R,1),ydry(k))
          else     
              yR = yres_jun_old(R)-Drop(R,1) !+ (temp3)*dt/Ares 	
          endif
          IDR = IDfb_reser; QR=0d0; call Area_from_H(k,yR,AR,Ts,RH,IDR)
      elseif (Nodetype(R,1) == 2)then  !outflowing          
          if (sum == 0 .or. sum == 1)then
              yL = max(yres_jun_old(R)-dr,ydry(k))
          else     
              yL = yres_jun_old(R)-dr !+ (temp3)*dt/Ares 	
          endif
          IDL=IDfb_reser; QL = 0d0; call Area_from_H(k,yL,AL,Ts,RH,IDL)
		yR = y11; AR = A11; QR = Q11; IDR = ID11
      else
          write(98,*),'Nodetype .ne. 1,2. Subr. Reservoirs'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif      
      
c     Calculating u11 and c11
      u11=Q11/A11
	call Pressure_Pho(k,y11,P_pho11,Id11)
	If (Id11==0)then
          call Area_from_H(k,y11,AA,Ts,RH,Id11); c11 = sqrt(g*A11/Ts)
      elseIf (Id11==1)then
          c11 = pc1(k)
      else
          write(98,*),'Id11 .ne. 0,1. Subr. Reservoirs'	    
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif      
      
      !To check if we need to solve the equations at the boundary or not
c     If depths are shallow, solve Riemann problem instead of junction equations	
      if (max(yL,yR) <= 0.05*yref(k))then           
          call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &        AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
          goto 24
      endif  
          
      if (sum == 0 .or. sum == 2)then
   		n = 3; yres =  yres_jun_old(R)	
		if (flowcaseIA1 == 2)then 
			x(1) = yres - dr; x(2) = yres; x(3) = Q11 !x(3) = 0d0
		else 
		    x(1:n) = (/y11,yres,Q11/)
		endif      	
		!Stora_old = volume of storage at y = yres_old (time t)
		!Stora_new = volume of storage at y = yres_new (time t+dt)	
		param1 = A11; param2 = Q11; param3 = c11; param4 = y11 
          param5 = Stora_old; param6 = P_pho11; param7 = dt; param8 = k1
		parint1 = k; parint2 = sum; parint3 = R; parint4 = Idb; 
          parint5 = flowcaseIA1; parint6 = FLOW_REGIA1

		!Choosing appropiate tolerance for solving equations
          if (sum == 0 )then
              tol_local = Tol_int_10_4    
          elseif (sum == 2)then
              tol_local = Tol_int_10_6
          endif
          
		call hybrd1 (Reser, n, x, fvec, tol_local, info)
		call converg (conver_result,info)
          if (conver_result == 0)then
              yb = x(1); yres = x(2); Qb = x(3)                   
              call Area_from_H(k,x(1),Ab,Ts,RH,Idb)
	        call Pressure_Pho(k,x(1),P_phob,Idb) 
              FF1 = x(3); FF2 =  x(3)*x(3)/Ab + P_phob  
              call Fluxes_boundary_HLL_Leon(k,x(1),Idb,IDL,IDR,
     &        yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
          !    if (Nodetype(R,1) == 1 )then !inflowing
          !        Qb = FF1L 
          !    elseif (Nodetype(R,1) == 2)then !outflowing	
          !        Qb = FF1R 
          !    else
          !        write(98,*),'Nodetype(R,1) .ne. 1,2. Reservoir'	
		        !call endprog; GLOBAL_STATUS_FLAG = 1; return  
          !    endif
              goto 44  
          elseIf(conver_result == 1)then	          
              call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &        AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
          else	
			write(98,*),'conver_result .ne. 0,1. Subr. reservoirs1'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif	
      elseif (sum == 1)then  
          Posit_interf = 3; yb = yres_jun_old(R) - dr; Qb = 0d0
          call Area_from_H(k,yb,Ab,Ts,RH,Idb) 
          Min_depth_Posit_interf = max(yb,y11)
          wtemp3 = (Q11-Qb)/(A11-Ab)	
          Wpred = abs(wtemp3)*SIGN(1d0,wtemp3)
          !call Area_from_H(k,yb,Ab,Ts,RH,Idb)          
          !call Pressure_Pho(k,y11,P_pho11,ID11)
          !call Pressure_Pho(k,yb,P_phob,Idb)
          !FL2 = Q11*Q11/A11 + P_pho11
          !FR2 = Qb*Qb/Ab + P_phob
          !wtemp3 = (FL2 - FR2)/(Q11 - Qb)
          !Wpred = abs(wtemp3)*SIGN(1d0,wtemp3)
          
          Y_solv_mixed = 1.10*yref(k)          
          !cond_mixed  = 1 --> pipe is in open channel conditions and junction is pressurized. 
          !cond_mixed  = 2 --> pipe is in pressurized conditions and junction is open channel.
          if (Nodetype(R,1) == 1)then !inflowing
              if (Wpred <= 0d0 .and. cond_mixed  == 1 .and. 
     &            Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 1 !Positive interface, flow is from dropshaft to pipe
              elseif (Wpred > 0d0 .and. cond_mixed  == 2 .and. 
     &            Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 2 !Positive interface, flow is from pipe to dropshaft
              else 
                  Posit_interf = 3  !Negative interface
              endif
          elseif(Nodetype(R,1) == 2)then !outflowing
              if (Wpred >= 0d0 .and. cond_mixed  == 1 .and. 
     &                Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 1 !Positive interface, flow is from dropshaft to pipe
              elseif (Wpred < 0d0 .and. cond_mixed  == 2 .and. 
     &                Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 2 !Positive interface, flow is from pipe to dropshaft
              else 
                  Posit_interf = 3  !Negative interface
              endif
          else
              write(98,*),'Nodetype .ne. 1,2. Reservoirs'
              call endprog; GLOBAL_STATUS_FLAG = 1; return  
          endif
          
          if (Posit_interf == 3)then !Negative interface
              call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &             AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
		    goto 24
          endif                  
          param1 = A11; param2 = Q11; param3 = c11; param4 = y11
	    param5 = P_pho11; param6 = yres_jun_old(R); param12 = dr   
          parint1 = k; parint9 = Nodetype(R,1)
          if(Posit_interf == 1)then !Positive interface, flow is from dropshaft to pipe 
              n = 1; x(1) = 0d0; tol_local = Tol_int_10_5
              call hybrd1 (posit_dropsh_to_pipe,n,x,fvec,tol_local,info)
	        call converg (conver_result,info)
              If (conver_result == 0)then	
                  ub = x(1)             
                  if (Nodetype(R,1) == 1)then !inflowing	
                      yb = yres_jun_old(R) - dr - ub*ub/(2d0*g) + 
     &                Kloss*ub*dabs(ub)/(2d0*g)    
                  elseif (Nodetype(R,1) == 2)then !outflowing	
                      yb = yres_jun_old(R) - dr - ub*ub/(2d0*g) - 
     &                Kloss*ub*dabs(ub)/(2d0*g)
                  else
                      write(98,*),'Nodetype(R,1) .ne. 1,2. Reserv.'
		            call endprog; GLOBAL_STATUS_FLAG = 1; return  
                  endif
              
                  Idb = 0; if (yb > yref(k))Idb = 1              
                  call Area_from_H(k,yb,Ab,Ts,RH,Idb)
	            call Pressure_Pho(k,yb,P_phob,Idb) 
                  Qb = x(1)*Ab; FF1 = Qb; FF2 = Qb*Qb/Ab + P_phob
                  call Fluxes_boundary_HLL_Leon(k,yb,Idb,IDL,IDR,
     &            yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
                  goto 24
              else
                  call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &            AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
		        goto 24
              endif 
          elseif(Posit_interf == 2)then !Positive interface, flow is from pipe to dropshaft
              n = 2; x(1) = y11; x(2) = 0d0; tol_local = Tol_int_10_5
              call hybrd1 (posit_pipe_to_dropsh,n,x,fvec,tol_local,info)
	        call converg (conver_result,info)
              If (conver_result == 0)then
                  yb = x(1); ub = x(2)
                  Idb = 0; if (yb > yref(k))Idb = 1
                  call Area_from_H(k,yb,Ab,Tb,RH,Idb) 
                  !Idb = 1
	            call Pressure_Pho(k,yb,P_phob,Idb) 
                  Qb = ub*Ab; FF1 = Qb; FF2 = Qb*Qb/Ab + P_phob
                  call Fluxes_boundary_HLL_Leon(k,yb,Idb,IDL,IDR,
     &            yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
                  goto 24
              else
                  call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &            AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
		        goto 24
              endif
          endif      
      else
          write(98,*),'sum .ne. 0,1,2. Subr. reservoirs3'
          call endprog; GLOBAL_STATUS_FLAG = 1; return        
      endif 
            
            
      !Storage equation		
24	if (Nodetype(R,1) == 2)then !outflowing
		!sign for Qb is negative for upstream reservoir
		inflowtemp = (PumpFlowToNode(R)-Qb - Outflow_limited(R))*dt
	Elseif (Nodetype(R,1) == 1)then !inflowing	
		inflowtemp = (PumpFlowToNode(R)+Qb - Outflow_limited(R))*dt 
	else
		write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. reservoirs'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      Endif      
      Storage_new = Stora_old + inflowtemp
      call itm_get_storage_depth(R,Storage_new, yres) 
      
      !Flux computation
44    if (Nodetype(R,1) == 1)then !inflowing		
		Fdownst(k,1) = FF1L; Fdownst(k,2) = FF2L
      elseif (Nodetype(R,1) == 2)then !outflowing		
		Fupst(k,1) = FF1R; Fupst(k,2) = FF2R
      else       
          write(98,*),'Unknown Nodetype(R,1). Subr. dropshaft'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif     
 	
      if (ISNAN(yres))then
          write(98,*),'yres (Reservoir depth) is NaN. Subr. Reservoirs'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
      if (yres > huge(0.0d0) .or. yres < -huge(0.0d0))then
          write(98,*),'yres is inf. Subr. Reservoirs. Yres = ', yres
          call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif 
      
       
      if (yres > 0.98*reser_maxdepth(R))then
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
          
          write(98,*),'R,yres,maxdepth',temp_id,yres, reser_maxdepth(R)
          write(98,*),'yres > reserv height(reser_maxdepth). Reservoirs'
          write(99,*),'yres > reserv height(reser_maxdepth). Reservoirs'
          call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif     
      
      
      yres_jun_old(R) = yres
      call Freesurface_flowregime(R,k,dr,ID11,IDfb_reser,Q11,y11,A11,
     & yres_jun_old(R),FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum)
      if (Nodetype(R,1) == 1)then !inflowing
          IH = Nx(k)-2 + 1 
      elseif (Nodetype(R,1) == 2)then !outflowing              
          IH = 3 - 1
	Else    
          write(98,*),'Unknown Nodetype(R,1). Subr. Reservoir'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
	Endif 
      IDFlow(k,IH) = IDfb_reser
300   return     
      end subroutine
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine Reser (n, x, fvec, iflag )
	use common_module
      use itm_accessors
	implicit none	
	integer n,p, iflag, i, k, R, sum,Idb
	integer flowcaseIA1,FLOW_REGIA1
	double precision fvec(n), x(n)	
	double precision A11, Q11, u11, c11, y11, P_pho11, Stora_old
	double precision Ab, ub, cb, P_phob, Tb, RH,temp8
	double precision dt,Stora_new,K1,y,A,Ts,Ares_aprox,con1,con2
	double precision temp_con1,temp_con2,temp_con3,temp_con4,res_temp
      integer :: seed
	double precision ran_number
      
	A11 = param1; Q11= param2; c11 = param3; y11 = param4
      Stora_old = param5; P_pho11 = param6; dt = param7
	k1 = param8
	
	k = parint1; sum = parint2; R = parint3; Idb = parint4
	flowcaseIA1 = parint5; FLOW_REGIA1 = parint6
	u11 = Q11/A11
          
      If (ISNAN(x(1)))then 
          call random_seed(seed)  
          call random_number(ran_number)
          if(sum == 0)then
              x(1) = ran_number*yref(k)   !y11  
          elseif(sum == 2)then
              x(1) = yref(k)  + ran_number*yref(k) 
          else 
              write(98,*),'sum n.e. 0,2. Reser'
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif	
      endif
      
      If (ISNAN(x(2)))then 		
		x(2) = yres_jun_old(R)
      endif      

	If (ISNAN(x(3)))then 	         
          call random_seed(seed)  
          call random_number(ran_number)              
          x(3) = ran_number*Qcrit_maxIA(k) 
      endif
          
      If (x(2) > reser_maxdepth(R))then 		
		x(2) = reser_maxdepth(R)
      endif

	!yb = x(1); yres = x(2); Qb = x(3)
	if(sum == 0 .or. sum == 2)then
          If (Idb == 0)then
			If (x(1) >= yref(k))then			
				x(1) = yref(k)
			endif
			If (x(1) < ydry(k))then	
				x(1) = ydry(k); x(3) = 0d0
              endif
			call Area_from_H(k,x(1),Ab,Tb,RH,0)
			cb = sqrt(g*Ab/Tb)
		elseif (Idb == 1)then
			call Area_from_H(k,x(1),Ab,Tb,RH,1)
		else
			write(98,*),'Idb undefined, Subr. Reservoirs'
			write(99,*),'Idb undefined, Subr. Reservoirs'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif			
		
		!If reservoir water depth is very small pumping outflow must be zero
          If (x(2) < 0d0)then 
			x(2) = 0d0; Outflow_limited(R) = 0d0
          endif              
		
		ub = x(3)/Ab	

		if (Nodetype(R,1) == 2)then !outflowing		
			if (sum == 2)then		
				fvec(1) = ub - pc1(k)*LOG(Ab) - (u11 - pc1(k)*LOG(A11))
			elseif (sum == 0)then			
				fvec(1) = ub - u11 - (cb + c11)*(Ab - A11)/(Ab + A11)
			else
				write(98,*),'sum .ne. 0,2. Subr. Reservoirs'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		Elseif (Nodetype(R,1) == 1)then !inflowing
			if (sum == 2)then
				fvec(1) = ub + pc1(k)*LOG(Ab) - (u11 + pc1(k)*LOG(A11))
			elseif (sum == 0)then 
				fvec(1) = ub - u11 + (cb + c11)*(Ab - A11)/(Ab + A11)
			else
				write(98,*),'sum .ne. 0,2. Subr. Reservoirs'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		else
			write(98,*),'Nodetype(R,1) .ne. 1,2, Subr. reservoir'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif	
	endif

	!Constant state, critical flow, normal flow,pressurized/freefall		
	if (sum == 0)then 
		if (Nodetype(R,1) == 2)then !outflowing			    
			If(flowcaseIA1 == 1)then !free outfall or normal flow  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXWORK HERE FLOW_REGIA1 == 10, 11
				If(FLOW_REGIA1 == 10)then	!flow is critical at the node (negative flow). Flow is from pipe to junction			
					!negative sign for critical depth (negative flow)					
					fvec(1) = -ub - cb					
                      !fvec(2) = Drop(R,1) + x(1) + con1 - x(2) !Energy equation neglecting losses
                      fvec(2) = x(1) + ub*ub/2d0 - (x(2) - Drop(R,1)) !Energy equation neglecting losses
					!Riemann invariants can not be used for outflowing pipes and supercritical flows	
                  elseif (FLOW_REGIA1 == 11)then !Flow is supercritical in the pipe, and critical at the outlet of the reservoir. Flow is from reservoir to pipe                      
					!positive sign for critical depth (positive flow)					
					fvec(1) = ub - cb
                      fvec(2) = x(1) + ub*ub/2d0 - (x(2) - Drop(R,1)) !Energy equation 
				else
					write(98,*),'FLOW_REGIA1 not supported. Subr. reservoir'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
				if (u11 - c11 >= 0d0)then !the characteristic doesn't connect the boundary 
                      fvec(1) = ub - cb
                      !fvec(1) = x(3) - Q_open
                  endif  
                  fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation      
			else
				!If the flow is supercritical in the outflowing pipe
				!we may have critical flow at the boundary (posit.) 
				!direction. We can add this later.
				write(98,*),'flowcaseIA1 is not supported, Subr. reservoir'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		elseIf(Nodetype(R,1) == 1)then !inflowing 
			If(flowcaseIA1 == 1)then	!free outfall or normal flow 
				!free outfall or normal flow 
				If(FLOW_REGIA1 == 1)then	!critical flow
					fvec(2) = ub - cb
				elseif(FLOW_REGIA1 == 2)then !normal flow
					fvec(2) = ub - 1d0/nm(k)*RH**(2d0/3d0)* sqrt(S0(k))
				else
					write(98,*),'FLOW_REGIA1 not supported. Subr. reservoir'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
				fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation
			    if (u11 + c11 <= 0d0)then !the characteristic doesn't connect the boundary 
					fvec(1) = ub + cb
				endif  
			else
				write(98,*),'flowcaseIA1 not supported. Subr. reservoir'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif	
		else
			write(98,*),'Nodetype(R,1) .ne. 1,2, Subr. reservoir'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
	elseif (sum == 2)then
			If(flowcaseIA1 == 2)then
				if (Nodetype(R,1) == 2)then !outflowing	
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation
				elseIf(Nodetype(R,1) == 1)then !inflowing 							
                      fvec(2) = Drop(R,1) + x(1) - x(2) !Energy equation       
				else
					write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. reservoir'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
			else 
				write(98,*),'flowcaseIA1 undef. in subr. Reserv'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif			
	else
		write(98,*),'sum .ne. 0,2. Subr. Reserv'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
	
	!Storage equation
	call itm_get_storage(R,x(2),Stora_new) 
	if (Nodetype(R,1) == 2)then !outflowing
		!sign for x(3) is negative for upstream reservoir
		res_temp = (-x(3) + PumpFlowToNode(R) - Outflow_limited(R))*dt
	Elseif (Nodetype(R,1) == 1)then !inflowing
		res_temp = (x(3)  + PumpFlowToNode(R) - Outflow_limited(R))*dt
	else
		write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. reservoir'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	Endif
	fvec(3) = res_temp - (Stora_new-Stora_old)
 400	return
      end