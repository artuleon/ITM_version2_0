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

      subroutine Dropshaft_general(tf,Dt,R)
      !This routine is used for computing fluxes at a dropshaft boundary.  
      use common_module
      use itm_accessors
      implicit none		       
      integer n,r,k,p,Idb,sum,i
      integer conver_result,info,codeconvergence
      integer flowcaseIA1,FLOW_REGIA1,cond_mixed
      double precision fvec(2),x(2),Y_solv_mixed
      double precision P_phob,AIA,TsIA,RHIA,Wpred
      double precision Ab,yb,Qb,ub !F11,F12,F13
      double precision u11,Q11,temp1,temp3,FF1L,FF1R,FF2L,FF2R,FF1,FF2
      double precision tf,Dt,test,der_P_phoIA,P_pho
      double precision c11,y11,A11,P_pho11,hdropsh	
      double precision AA,Ts,RH,AL,AR,Area,Tb
      double precision y,A,AStar,Qstar,h0b1,A0b1,Q0b1
      double precision Qinflow,Qinf_new,Qinf_old,Ares
      double precision con1,con2,con3
      double precision k1,dr,Ref_level,yres
      double precision temp_con1,temp_con2,temp_con3,temp_con4,temp_con5
      double precision Qsum,Error_y,tol_local
      double precision yL,YR,QL,QR,Min_depth_Posit_interf
      double precision delta_h, Q_flux_max, vmax, Qvalue
      double precision Q_max,K_head_Loss,wtemp3
      double precision Hmax,yc,Ac,Qc,Qb_temp,y_temp_pond,FL2,FR2
                      
      character(IDLEN) temp_id
      integer CODL, Idf0,Id11,IH,IDL,IDR,IDfb_reser,Posit_interf
      external dropsh,posit_dropsh_to_pipe,posit_pipe_to_dropsh
      
      if (NodeNS(R) > 0)then  !NodeNS(R) = Number of pipes connected to each node     
          k = NodeID(R,1)
          dr = Drop(R,1)	
      endif
      
        !Inflow hydrographs
      if (itm_has_inflow(R) == 1)then
          call itm_get_inflow(R,tf,Qinf_old)
          call itm_get_inflow(R,tf+Dt,Qinf_new)
          else
          Qinf_old = 0d0; Qinf_new = 0d0 
      endif
      Posit_interf = 3
		
      Qinflow = (Qinf_old+Qinf_new)/2d0
      Ares = Ares_junct(R)
      
      !To enforce that the minimum water depth at pond then is ydropmin(R) whene there is no pumps 
      if (NodeNS(R) == 0)then 
            if (yres_jun_old(R) < 0d0)then
                yres_jun_old(R) = 0d0; Outflow_limited(R) = 0
            endif  
      else
            if (yres_jun_old(R) < 0d0)then
                yres_jun_old(R) = 0d0; Outflow_limited(R) = 0
            endif  
      !      if (yres_jun_old(R) < ydropmin(R))then !ydropmin(R))then
		    !yres_jun_old(R) = ydropmin(R); Outflow_limited(R) = 0
      !      endif	         
      endif       
            
      !If there are only pumps connected to the node      
      if (NodeNS(R) == 0)then  !NodeNS(R) = Number of pipes connected to each node     
          temp3 = (Qinf_new+Qinf_old)/2d0 + PumpFlowToNode(R)
          yres = yres_jun_old(R) + temp3*dt/Ares
          
          if (yres < 0d0)then
              PumpFlowToNode(R) = 0
              temp3 = (Qinf_new+Qinf_old)/2d0 
              yres = yres_jun_old(R) + temp3*dt/Ares              
              call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes              
              write(98,*),'Dropshaft. Pump link at Node ',trim(temp_id)
              write(98,*),'causing negative water depth. Increase the '
              write(98,*),'node area or change pump/control curve'
              write(99,*),'Dropshaft. Pump link at Node ',trim(temp_id)
              write(99,*),'causing negative water depth. Increase the '
              write(99,*),'node area or change pump/control curve'
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif            
          yres_jun_old(R) = yres
          goto 300 
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
          write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
          write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      Endif	
      
      temp3 = Qinflow + PumpFlowToNode(R)  !This includes pump flow
      y_temp_pond = yres_jun_old(R)+temp3*dt/Ares
      call Freesurface_flowregime(R,k,dr,Id11,Idb,Q11,y11,A11,
     &y_temp_pond,FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum)
           
	if (Nodetype(R,1) == 1)then !inflowing
          yL = y11; AL = A11; QL = Q11; IDL = ID11
          if (sum == 0 .or. sum == 1)then
              !yR = max(yres_jun_old(R)-dr+temp3*dt/Ares, ydry(k))
              yR = max(yres_jun_old(R)-dr, ydry(k))
          else     
              yR = max(yres_jun_old(R)-dr, ydry(k))
          endif             
                  
		IDR = Idb; QR = 0d0 !-temp3/Ares 
		call Area_from_H(k,yR,AR,Ts,RH,Idb)
      elseif(Nodetype(R,1) == 2)then !outflowing
          if (sum == 0 .or. sum == 1)then
              yL = max(yres_jun_old(R)-dr, ydry(k))
          else     
              yL = max(yres_jun_old(R)-dr, ydry(k))
          endif   
		call Area_from_H(k,yL,AL,Ts,RH,Idb)          
		IDL = Idb; QL = 0d0 !temp3/Ares
		yR = y11; AR = A11; IDR = ID11; QR =  Q11
      endif
           
	u11=Q11/A11
	call Pressure_Pho(k,y11,P_pho11,Id11)
	If (Id11==0)then
          call Area_from_H(k,y11,AA,Ts,RH,Id11); c11 = sqrt(g*A11/Ts)
      elseIf (Id11==1)then
          c11 = pc1(k)
      else
          write(98,*),'Id11 .ne. 0,1. Subr. Dropshaft_general'		    
          write(99,*),'Id11 .ne. 0,1. Subr. Dropshaft_general'		    
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
		
	!To check if we need to solve the equations at the boundary or not
c     If depths are shallow, solve Riemann problem instead of junction equations	
      if (max(yL,yR) <= 0.10*yref(k))then !may change here          
          call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &        AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
          goto 24
      endif
      
	!solving equations: free surface, pressurized and combined
  	!Pure free surface flow or combined 
16	param1 = A11; param2 = Q11; param3 = c11; param4 = y11
	param7 = dt; param8 = k1
	param9 = Ares; param10 = Qinf_new; param11 = Qinf_old
	param15 = tf   

	parint1 = k; parint2 = sum; parint3 = R
	parint4 = Idb; parint5 = flowcaseIA1
	parint6 = FLOW_REGIA1	
	parint7 = cond_mixed	
	n = 2
	yres =  yres_jun_old(R)
      
      if (flowcaseIA1 == 2)then 
          x(1) = yres_jun_old(R)-dr; x(2) = Q11 
      else 
	    x(1:n) = (/y11,Q11/)
      endif	

	!Appropiate tolerance for solving equations	
      if (sum == 0 .or. sum == 2)then
		If(sum == 2)then	
              tol_local = Tol_int_10_6
          elseif(sum == 0)then     
              if (max(yL,yR) < 0.1*yref(k))then
                   call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &             AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb) 
                   goto 24
              endif
              tol_local = Tol_int_10_4
		endif
		
	    call hybrd1 (dropsh,n,x,fvec,tol_local,info)
	    call converg (conver_result,info)
          codeconvergence = 0 !0 there is convergence, otherwise no convergence
          if (sum == 0)then
              if(x(1)>yref(k) .or. x(1)<=ydry(k))codeconvergence=1
          endif
          
          If(conver_result == 0 .and. codeconvergence == 0)then		
              call Area_from_H(k,x(1),Ab,Ts,RH,Idb)
	        call Pressure_Pho(k,x(1),P_phob,Idb) 
              FF1 = x(2)
              FF2 =  x(2)*x(2)/Ab + P_phob  
              call Fluxes_boundary_HLL_Leon(k,x(1),Idb,IDL,IDR,
     &        yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
c             This is to calculate the flow discharge at the boundary. In internal cells, the value doesn't have any meaning.              
              if (Nodetype(R,1) == 1 )then !inflowing
                  Qb = FF1L 
              elseif (Nodetype(R,1) == 2)then !outflowing	
                  Qb = FF1R 
              else
                  write(98,*),'Nodetype(R,1) .ne. 1,2. dropshaft'	
                  write(99,*),'Nodetype(R,1) .ne. 1,2. dropshaft'	
		        call endprog; GLOBAL_STATUS_FLAG = 1; return  
              endif
              goto 24  
          endif
      elseif (sum == 1)then 
          yb = max(yres_jun_old(R)-dr, ydry(k))
          Qb = 0d0
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
          
          
          
          Y_solv_mixed = 1.03*yref(k)          
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
     &            Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 1 !Positive interface, flow is from dropshaft to pipe
              elseif (Wpred < 0d0 .and. cond_mixed  == 2 .and. 
     &            Min_depth_Posit_interf > Y_solv_mixed)then
                  Posit_interf = 2 !Positive interface, flow is from pipe to dropshaft
              else 
                  Posit_interf = 3  !Negative interface
              endif
          else
              write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
              write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
              call endprog; GLOBAL_STATUS_FLAG = 1; return  
          endif
          
          if (Posit_interf == 3)then !Negative interface
              call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     &        AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb)
		    goto 24
          endif
                  
          param1 = A11; param2 = Q11; param3 = c11; param4 = y11
	    param5 = P_pho11; param6 = yres_jun_old(R); param12 = dr
          
          parint1 = k; parint9 = Nodetype(R,1)
          if(Posit_interf == 1)then !Positive interface, flow is from dropshaft to pipe 
              n = 1; x(1) = 0d0
              tol_local = Tol_int_10_5
              call hybrd1 (posit_dropsh_to_pipe,n,x,fvec,tol_local,info)
	        call converg (conver_result,info)
              If (conver_result == 0)then	
                  ub = x(1)
                  !yb = yres_jun_old(R) - dr - x(1)*x(1)/(2d0*g)               
                  if (Nodetype(R,1) == 1)then !inflowing	
                      yb = yres_jun_old(R) - dr - ub*ub/(2d0*g) + 
     &                Kloss*ub*dabs(ub)/(2d0*g)    
                  elseif (Nodetype(R,1) == 2)then !outflowing	
                      yb = yres_jun_old(R) - dr - ub*ub/(2d0*g) - 
     &                Kloss*ub*dabs(ub)/(2d0*g)
                  else
                      write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
                      write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
		            call endprog; GLOBAL_STATUS_FLAG = 1; return  
                  endif
              
                  Idb = 0
                  if (yb > yref(k))Idb = 1              
                  call Area_from_H(k,yb,Ab,Ts,RH,Idb)
	            call Pressure_Pho(k,yb,P_phob,Idb) 
                  Qb = x(1)*Ab
                  FF1 = Qb; FF2 = Qb*Qb/Ab + P_phob
                  call Fluxes_boundary_HLL_Leon(k,yb,Idb,IDL,IDR,
     &            yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
                  goto 24
              else 
                  goto 20
              endif 
          elseif(Posit_interf == 2)then !Positive interface, flow is from pipe to dropshaft
              n = 2; x(1) = y11; x(2) = 0d0              
              tol_local = Tol_int_10_5
              call hybrd1 (posit_pipe_to_dropsh,n,x,fvec,tol_local,info)
	        call converg (conver_result,info)
              If (conver_result == 0)then
                  yb = x(1); ub = x(2)
                  Idb = 0
                  if (yb > yref(k))Idb = 1                 
                  call Area_from_H(k,yb,Ab,Tb,RH,Idb) 
	            call Pressure_Pho(k,yb,P_phob,Idb) 
                  Qb = ub*Ab                                    
                  FF1 = Qb; FF2 = Qb*Qb/Ab + P_phob
                  call Fluxes_boundary_HLL_Leon(k,yb,Idb,IDL,IDR,
     &            yL,yR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)
                  goto 24
              else 
                  goto 20
              endif
          endif 
      else
          write(98,*),'SumIDFIA .ne. 0, 1, 2. dropshaft_general'
          write(99,*),'SumIDFIA .ne. 0, 1, 2. dropshaft_general'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif      
      
20    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
      !write(99,*),'t, Conv. Drops',t_global,sum_no_converg,trim(temp_id)
      call Riemann_Mixed_HLL_Leon(k,sum,IDL,IDR,yL,yR,
     & AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,Nodetype(R,1),Qb) 
      
24    if (Nodetype(R,1) == 2)then !outflowing
		!sign for Qb is negative for upstream reservoir
		temp1 = -Qb
	Elseif (Nodetype(R,1) == 1)then !inflowing
		temp1 = Qb
      else 
		write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
          write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	Endif 					
	temp3 = 5d-1*(Qinf_new+Qinf_old) + PumpFlowToNode(R)
      yres = yres_jun_old(R) + (temp3+temp1)*dt/Ares

	!Flux computation
 40   if (Nodetype(R,1) == 1)then !inflowing		
		Fdownst(k,1) = FF1L; Fdownst(k,2) = FF2L
      elseif (Nodetype(R,1) == 2)then !outflowing		
		Fupst(k,1) = FF1R; Fupst(k,2) = FF2R
      else       
          write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
          write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      Endif
      
      
200   if (ISNAN(yres))then
          write(98,*),'yres (Reservoir depth) is NaN. Subr. dropshaft'
          write(99,*),'yres (Reservoir depth) is NaN. Subr. dropshaft'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif     
      
      if (yres > huge(0.0d0) .or. yres < -huge(0.0d0))then
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
          write(98,*),'yres is infinity. Dropshaft. Node ',trim(temp_id)
          write(99,*),'yres is infinity. Dropshaft. Node ',trim(temp_id)
          call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif 
           
      yres_jun_old(R) = yres  
      call Freesurface_flowregime(R,k,dr,ID11,IDfb_reser,Q11,y11,A11,
     & yres_jun_old(R),FLOW_REGIA1,flowcaseIA1,Nodetype(R,1),cond_mixed,
     & sum) !This is done with tne new reservoir depth
      if (Nodetype(R,1) == 1)then !inflowing
          IH = Nx(k)-2 + 1 
      elseif (Nodetype(R,1) == 2)then !outflowing              
          IH = 3 - 1
	Else    
          write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
          write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
	Endif 
      IDFlow(k,IH) = IDfb_reser
300   return    
      end
	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine dropsh(n,x,fvec,iflag)
	use common_module
	implicit none	
	integer n,iflag,r,k,Idb,Id11,flowcaseIA1,FLOW_REGIA1
	integer sum,cond_mixed
	double precision fvec(n),x(n)
	double precision AIA,TsIA,RHIA,u11,Q11,temp1,temp3,temp4
	double precision Qinf_new,Qinf_old,Ares,y11,A11,P_pho11,cb1,c11
	double precision Qb,Ab,ub,yb,ub1,dt,cb,Tb,RH,yres,dr
	double precision con1,con2,con3,con4,con5,Qs,k1,y,A,Ts,tf
	double precision temp_con1,temp_con2,temp_con3,temp_con4,Q_open
      integer :: seed
	double precision ran_number
	parint1000 = 0
	A11 = param1; Q11= param2; c11 = param3; y11 = param4; dt = param7 
	k1 = param8; Ares = param9; Qinf_new = param10; Qinf_old = param11
	tf = param15

	k = parint1; sum = parint2; R = parint3; Idb = parint4
	flowcaseIA1 = parint5; FLOW_REGIA1 = parint6; cond_mixed = parint7
	u11 = Q11/A11	
      dr = Drop(R,1); yres = yres_jun_old(R)

	if(sum == 0 .or. sum == 2)then
		If (Idb == 0)then
			If (x(1) >= b2_max(k))then			
				x(1) = b2_max(k)
			endif
			If (x(1) < 0.1*b2_min(k))then	
				x(1) = 0.1*b2_min(k)
			endif
			call Area_from_H(k,x(1),Ab,Tb,RH,0)
			cb = sqrt(g*Ab/Tb)
		elseif (Idb == 1)then
			call Area_from_H(k,x(1),Ab,Tb,RH,1)
		else
			write(98,*),'Idb .ne. 0,1. Dropshaft_general'
              write(99,*),'Idb .ne. 0,1. Dropshaft_general'
              !write(98,*),'node,pipe,Idb',R,k,Idb
			!write(98,*),'cond_mixed,flowcaseIA1,FLOW_REGIA1',
              !&			cond_mixed,flowcaseIA1,FLOW_REGIA1
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif		
		ub = x(2)/Ab	

		if (Nodetype(R,1) == 2)then !outflowing
			if (sum == 2)then		
				fvec(1) = ub - pc1(k)*LOG(Ab) - (u11 - pc1(k)*LOG(A11))
      !             fvec(1) = x(1) + 1d0/(2*g)*ub**2d0 -
      !&             (y11 + 1d0/(2*g)*u11**2d0)
			elseif (sum == 0)then			
				fvec(1) = ub - u11 - (cb + c11)*(Ab - A11)/(Ab + A11)
			else      
                  write(98,*),'Sum .ne, 0,2. Subr. dropshaft'
                  write(99,*),'Sum .ne, 0,2. Subr. dropshaft'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		Elseif (Nodetype(R,1) == 1)then !inflowing
			if (sum == 2)then
				fvec(1) = ub + pc1(k)*LOG(Ab) - (u11 + pc1(k)*LOG(A11))
      !            fvec(1) = x(1) + 1d0/(2*g)*ub**2d0 -
      !&             (y11 + 1d0/(2*g)*u11**2d0)
              elseif (sum == 0)then 
				fvec(1) = ub - u11 + (cb + c11)*(Ab - A11)/(Ab + A11)
			else      
                  write(98,*),'Sum .ne, 0,2. Subr. dropshaft'
                  write(99,*),'Sum .ne, 0,2. Subr. dropshaft'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		Endif
	endif
	
	!Constant state, critical flow, normal flow,pressurized/freefall	
	if (sum == 0)then 
		if (Nodetype(R,1) == 2)then !outflowing			    
			If(flowcaseIA1 == 1)then !free outfall or normal flow 
				If(FLOW_REGIA1 == 10)then				
					!negative sign for critical depth (negative flow)					
					fvec(1) = -ub - cb					
                      !fvec(2) = dr + x(1) + con1 - yres_jun_old(R) !Energy equation neglecting losses
                      fvec(2) = x(1) + ub*ub/(2d0*g) + 
     &                Kloss*ub*dabs(ub)/(2d0*g) - (yres-dr) 
				elseif(FLOW_REGIA1 == 11)then
					!Riemann invariants can not be used for outflowing pipes and supercritical flows					
					!fvec(1) = yres-dr-x(1)
					!positive sign for critical depth (positive flow)					
					fvec(1) = ub - cb
                      !fvec(2) = dr + x(1) - yres !Energy equation                       
                      fvec(2) = x(1) + ub*ub/(2d0*g) + 
     &                Kloss*ub*dabs(ub)/(2d0*g) - 
     &                (yres-dr) 
				else
					write(98,*),'FLOW_REGIA1 not supported. Dropshaft'
                      write(99,*),'FLOW_REGIA1 not supported. Dropshaft'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
		        if (u11 - c11 >= 0d0)then !the characteristic doesn't connect the boundary 
                      fvec(1) = ub - cb
                  endif
                  !fvec(2) = dr + x(1) - yres !Energy equation  
                  fvec(2) = x(1)+ub*ub/(2d0*g) + 
     &            Kloss*ub*dabs(ub)/(2d0*g) - (yres-dr) 
			else
				!If the flow is supercritical in the outflowing pipe
				!we may have critical flow at the boundary (posit.) 
				!direction. We can add this later.
				write(98,*),'flowcaseIA1 not supported. Dropshaft'
                  write(99,*),'flowcaseIA1 not supported. Dropshaft'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		elseIf(Nodetype(R,1) == 1)then !inflowing 
			If(flowcaseIA1 == 1)then	!free outfall or normal flow 
				!free outfall or normal flow 
				If(FLOW_REGIA1 == 1)then	!critical flow
					fvec(2) = ub - cb
				elseif(FLOW_REGIA1 == 2)then !normal flow
					fvec(2) = ub - 1d0/nm(k)*
     &			RH**(2d0/3d0)* sqrt(S0(k))
				else
					write(98,*),'FLOW_REGIA1 not supported. Dropshaft'
                      write(99,*),'FLOW_REGIA1 not supported. Dropshaft'
					call endprog; GLOBAL_STATUS_FLAG = 1; return
				endif
              elseIf (flowcaseIA1 == 2)then !Constant head
                  con1 = ub**2d0/(2d0*g)
                  con2 = u11**2d0/(2d0*g)                     
                  !fvec(1) = x(1)+con1 - (y11+con2) 
                  !fvec(1) = x(2) - Q_open    
		        if (u11 + c11 <= 0d0)then !the characteristic doesn't connect the boundary 
                        fvec(1) = ub + cb
                  endif                    
                  !fvec(2) = dr + x(1) - yres !Energy equation     
                  fvec(2) = x(1)+ub*ub/(2d0*g) - 
     &            Kloss*ub*dabs(ub)/(2d0*g) - (yres-dr) 
			else
				write(98,*),'flowcaseIA1 not supported. Dropshaft'
                  write(99,*),'flowcaseIA1 not supported. Dropshaft'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif	
		else
              write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
              write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif
	elseif (sum == 2)then
		If(flowcaseIA1 == 2)then
			!fvec(2) = yres-dr-x(1)
			if (Nodetype(R,1) == 2)then !outflowing	
                  !fvec(2) = dr + x(1) - yres !Energy equation
                  fvec(2) = x(1)+ub*ub/(2d0*g) + 
     &            Kloss*ub*dabs(ub)/(2d0*g) - (yres-dr) 
			elseIf(Nodetype(R,1) == 1)then !inflowing 							
                  !fvec(2) = dr + x(1) - yres !Energy equation
                  fvec(2) = x(1)+ub*ub/(2d0*g) - 
     &            Kloss*ub*dabs(ub)/(2d0*g) - (yres-dr) 
			else
                  write(98,*),'Nodetype .ne. 1,2. Dropshaft_general'
                  write(99,*),'Nodetype .ne. 1,2. Dropshaft_general'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			endif
		else			
			write(98,*),'flowcaseIA1 undef. Subr. Dropshaft_general'
              write(99,*),'flowcaseIA1 undef. Subr. Dropshaft_general'
              !write(98,*),'11,sum,flowcaseIA',sum,flowcaseIA1
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif 
	else
		write(98,*),'sum .ne. 0,1,2. Subr. Dropshaft_general'
          write(99,*),'sum .ne. 0,1,2. Subr. Dropshaft_general'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
400	return
      end

      
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posit_dropsh_to_pipe(n,x,fvec,iflag)      
	use common_module
	implicit none	
	integer n,iflag,r,k,Idb,Id11,Infl_Outf_node
	double precision fvec(n),x(n)
	double precision u11,Q11,y11,A11,P_pho11,P_phob
	double precision Qb,Ab,ub,yb,ub1,Tb,RH,yres
	double precision k1,y,A,Ts,Sw,dr
      integer :: seed
	double precision ran_number
      
	A11 = param1; Q11= param2; y11 = param4; P_pho11 = param5
      yres = param6; dr = param12
      k = parint1
      Infl_Outf_node = parint9
	u11 = Q11/A11	
      
      if (ISNAN(x(1)))then
          call random_seed(seed)  
          call random_number(ran_number)  
          x(1) = 0.5 - 1d0*ran_number !Velocity changes between -0.5 ad 0.5 m/s
          !write(99,*),'x(1) =',x(1)
      endif
      
      if (abs (x(1))> 50d0)then
          x(1) = 50d0*SIGN(1d0,x(1))
      endif
      
      
      ub = x(1) 
      
      !yb = yres - ub*ub/(2d0*g)   
      if (Infl_Outf_node == 1)then !inflowing	
          yb = yres - dr - ub*ub/(2d0*g) + Kloss*ub*dabs(ub)/(2d0*g)    
      elseif (Infl_Outf_node == 2)then !outflowing	
          yb = yres - dr - ub*ub/(2d0*g) - Kloss*ub*dabs(ub)/(2d0*g)
      else
          write(98,*),'Infl_Outf_node .ne. 1,2. Dropshaft_general'
          write(99,*),'Infl_Outf_node .ne. 1,2. Dropshaft_general'
		call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif
      
      if (yb >= yres-dr)then
          yb = yres - dr; x(1) = 0d0; ub = x(1)
      elseif  (yb <= ydry(k))then
          yb = ydry(k); x(1) = 0d0; ub = x(1) !sqrt(2d0*g*(yres - yb))   
      endif
      
      if (yb > yref(k))then
          Idb = 1        
      else
          Idb = 0 
      endif
      call Area_from_H(k,yb,Ab,Tb,RH,Idb) 
      
      Qb = ub*Ab
      Sw = (Ab*ub - A11*u11)/(Ab-A11)   
      call Pressure_Pho(k,yb,P_phob,Idb)
      fvec(1) = Sw*(Ab*ub - A11*u11) -
     &    (Qb*Qb/Ab + P_phob - Q11*Q11/A11 - P_pho11)
	return
      end

c     !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine posit_pipe_to_dropsh(n,x,fvec,iflag)      
	use common_module
	implicit none	
	integer n,iflag,r,k,Idb,Id11,Infl_Outf_node
	double precision fvec(n),x(n)
	double precision u11,Q11,c11,y11,A11,P_pho11,P_phob
	double precision Qb,Ab,ub,yb,ub1,Tb,RH,yres
	double precision k1,y,A,Ts,Sw,dr
      integer :: seed
	double precision ran_number
      
	A11 = param1; Q11= param2; c11 = param3
      y11 = param4; P_pho11 = param5
      yres = param6; dr = param12
      k = parint1
      Infl_Outf_node = parint9
	u11 = Q11/A11
           
      if (ISNAN(x(1)))then
          call random_seed(seed)  
          call random_number(ran_number)
          x(1) = y11 + d(k)*ran_number
          !write(99,*),'yb = ',x(1)
      endif
      
       if (ISNAN(x(2)))then
          call random_seed(seed)  
          call random_number(ran_number)
          x(2) = 0.5 - 1d0*ran_number !Velocity changes between -0.5 ad 0.5 m/s
          !write(99,*),'ub = ',x(2)
       endif
       
      if (abs (x(2))> 20d0)then
          x(2) = 20d0*SIGN(1d0,x(2))
      endif
       
      yb = x(1); ub = x(2)       
      
      if (yb > yref(k))then
          Idb = 1        
      else
          Idb = 0 
      endif
      call Area_from_H(k,yb,Ab,Tb,RH,Idb) 
      
      Qb = ub*Ab
      Sw = (Ab*ub - A11*u11)/(Ab-A11)   
      call Pressure_Pho(k,yb,P_phob,Idb)
      fvec(1) = Sw*(Ab*ub - A11*u11) -
     &    (Qb*Qb/Ab + P_phob - Q11*Q11/A11 - P_pho11)
      
      
      if (Infl_Outf_node == 1)then !inflowing	
          fvec(2) = Qb - Q11 - (Ab - A11)*(u11 - c11)   
      elseif (Infl_Outf_node == 2)then !outflowing	
          fvec(2) = Qb - Q11 - (Ab - A11)*(u11 + c11)     
      else
          write(98,*),'Infl_Outf_node .ne. 1,2. Dropshaft_general'
          write(99,*),'Infl_Outf_node .ne. 1,2. Dropshaft_general'
		call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif
	return
	end
