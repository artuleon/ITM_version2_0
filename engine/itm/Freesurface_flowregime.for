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

	subroutine Freesurface_flowregime(R,k,dr,IDf11,IDfb_reser,Q11,y11,A11,
     & Yreser,flow_regIA1,flowcaseIA1,nodetype1,cond_mixed,sum)
      !&flow_regIA1,flowcaseIA1,nodetype1,cond_mixed,sum,yw0,Aw0,Qw0,Idf0)
     
	!This routine is used for determining if a junction/reservoir/dropshaft boundary is surcharged or not. 
	use common_module 
	implicit none	
	integer k,i,R,sum,flow_regIA1,flowcaseIA1,nodetype1
	integer IDf11,IDfb_old,cond_mixed,node_pressur,IDfb_reser
	double precision dr,tol_const_state,tempIA
	double precision ScIA,yb,A,Q,Ab,dh,Ts,RH,Ycrit,Q_allow
	double precision Ref_level,tol_level
	double precision hIA,hIC,vIA,Q11,y11,A11,hIA_old,ytemp,Slo
	double precision F1,d1,A1_temp,Ynormal,Yconjugate,area,y
	double precision Ener_super,E1,Wpred,Pw0
	double precision FF1,FF2,yL,YR,QL,QR
      double precision Eres, yc_res,yc_11,yc_11_hager,Ac_temp,Qc_temp 
      double precision E11_infl,E11_outf,con1,temp1,Yreser
	integer CODL,IDfb_reser_old
            
	!#######################################################
	!flowcase = 1 --> free outfall or normal flow
	!flowcase = 2 --> constant state	
      !cond_mixed  = 1 --> pipe is in open channel conditions and junction is pressurized. Here we need to use the two equations of rankine Hugoniot conditions + energy equation
      !cond_mixed  = 2 --> pipe is in pressurized conditions and junction is open channel. Use Riemann Mixed solver
      !flow_regIA1 = 1 (flow is subcritical), flow_regIA1 = 2 (flow is supercritical)
      
	IDfb_reser = 1; 
      cond_mixed  = -100; flowcaseIA1 = -100; flow_regIA1 = -100 !to initialize      
	sum = -100  !to initialize		
	!#######################################################
      !tol_const_state = 0.01*d(k)
      tol_const_state = 0d0
      !tol_const_state = Tol_int_10_8*d(k)	
      Ref_level = Yreser - (dr + yref(k))
      If(Ref_level > tol_const_state) then
          IDfb_reser = 1
      elseIf(Ref_level <= tol_const_state) then
          IDfb_reser = 0	
      else
          write(98,*),'tol_const_state unknown. Freesurface_flowreg'
          write(99,*),'tol_const_state unknown. Freesurface_flowreg'
          call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif	
      
 5    sum = IDf11+IDfb_reser	
      
 10	if (sum == 2)then		
		IDfb_reser = 1; flowcaseIA1 = 2; goto 20	
      elseIf (sum == 0)then
          IDfb_reser = 0
          !if (BCnode(R) .ne. 20 .and. BCnode(R) .ne. 4)then  !Reservoirs and dropshafts don't need to have this condition
          if (BCnode(R) == 4 .or. BCnode(R) == 7)then  !Reservoirs don't need to have this condition
              if (abs(dr) <= 0.02*d(k))then
                  flowcaseIA1 = 2; goto 20  !constant state
              endif
          endif 
          !WORK HERE Yc = 2/3*(Y11+v^2/(2g)). Use this as a metric.
          con1 = Q11*Q11/(2d0*g*A11*A11)
          E11_infl = y11  + con1 !+ 0.5*abs(s0(k)*dx(k))
          E11_outf = y11  + con1 !- 0.5*abs(s0(k)*dx(k))
          Eres = Yreser - dr
		!Determining if the inflows are subcrit. or supercrit or pressurized	
		
          if (nodetype1 == 1)then !inflowing 
              !Determining critical slope to determine if the flow at the incoming pipe is free outfall [normal flow or 
			!critical flow (Free Surface)],  or Constant state (free and pressurized flow)
              if (E11_infl > Eres)then !Flow is from pipe to pond      
                  yc_11 = 2d0/3d0*E11_infl                      
                  if (Q11 > 0d0)then
                      yc_11_hager = (Q11/sqrt(g*d(k)))**0.5  !Hagger's formula Page 150
                  else
                       yc_11_hager = 0d0
                  endif
                  
                  yc_11 = max(yc_11, yc_11_hager)                      
                  if (yc_11 > (ycrit_max(k)+d(k))/2d0)then
                      yc_11 = (ycrit_max(k)+d(k))/2d0
                  endif                      
                  if (E11_infl > Ecrit_max(k))then
                      yc_11 = ycrit_max(k); E11_infl = Ecrit_max(k)
                  endif
                  call Area_from_H(k,yc_11,Ac_temp,Ts,RH,0)         
                  Qc_temp = Ac_temp*sqrt(g*Ac_temp/Ts)          
			    Q_allow = Qc_temp
                  if (Q_allow <= Qmin(k))then
                      Q_allow = Qmin(k)
			    elseif (Q_allow > Qcrit_maxIA(k))then
				    Q_allow = Qcrit_maxIA(k)
                  endif
                  
                  ScIA = ((Q_allow*nm(k))/(Ac_temp*RH**(2d0/3d0)))**2d0
			    
                  if (S0(k) <= ScIA)then !flow is subcritical	
				    flow_regIA1 = 1
                      ytemp = y11
			    elseif (S0(k) > ScIA)then !flow is supercritical
				    flow_regIA1 = 2
                  else
                      write(98,*),'S0 >=<than ScIA. Freesurface_flowreg'
                      write(99,*),'S0 >=<than ScIA. Freesurface_flowreg'
			        call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif                                           
                      
                  if(flow_regIA1 == 2)then !flow is supercritical
                      !This is done only for supercritical flows because 
			        !for subcritical flow already was made before
				    if (Q_allow <= Qmin(k))then	
					    ytemp = d2_min(k)		
				    else 					        
                          if (Q_allow > Qnor_maxIA(k))then		
                              ytemp = (ycrit_max(k)+d(k))/2d0
					    else
                              d1 = Ynormal(k,Q_allow)
                              call Area_from_H(k,d1,A1_temp,Ts,RH,0)  
                              !according to Hager (Wastewater Hydraulics, page 182, d2/d1 = 1.16*F1^0.85, where F1 = Froude Number) 
                              F1 = Q_allow/A1_temp
                              F1 = F1/sqrt(g*A1_temp/Ts)                
                              If (ISNAN(F1))then 		
                                  write(98,*),'F1 NaN. Freesurface_flow'
                                  write(99,*),'F1 NaN. Freesurface_flow'
                                  call endprog; GLOBAL_STATUS_FLAG = 1
                                  return
                              endif                                  
                              if (F1 <= 1.0)then
                                  write(98,*),'Free_flowreg, Sup.& F1<1'
                                  write(99,*),'Free_flowreg, Sup.& F1<1'
			                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                              endif
                                  
                              ytemp = d1*(1.16*F1**0.85)  !Conjugate depth according to Hager's formula
                                  
                              if (ytemp>(ycrit_max(k)+d(k))/2d0)then
                                  ytemp = (ycrit_max(k)+d(k))/2d0 
                              endif
						    !ytemp = Yconjugate(k,d1,Q_allow)
                              !ytemp = d1
                          endif					
                      endif
                  endif
                  hIA = ytemp; hIC = Yreser								
			    !Including the slope of the inflowing pipe
			    !tempIA = hIA +dr + 0.5*S0(k)*dx(k)+vIA*vIA/(2.0*g) -hIC 
			    tempIA = hIA + dr - hIC 
			    
                  !The slope of the incoming pipe is considered
			    if (tempIA > Tol_int_10_2*d(k))then	  	    
                      flowcaseIA1 = 1; goto 20
			    else
				    flowcaseIA1 = 2; goto 20
                  endif    
              elseif (E11_infl <= Eres)then !Flow is from pond to pipe
                  flowcaseIA1 = 2; goto 20
              else
			    write(98,*),'E11 not>=<than Eres. Freesurface_flowreg'
                  write(99,*),'E11 not>=<than Eres. Freesurface_flowreg'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif   
          elseif (nodetype1 == 2)then !outflowing    
              if (E11_outf < Eres)then !flow is from pond to pipe     
                  yc_res = 2d0/3d0*Eres 
                  if (Eres > Ecrit_max(k))then
                      yc_res = ycrit_max(k); Eres =  Ecrit_max(k)
                  endif                      
                  if (yc_res < 0d0) yc_res = 0d0
                  call Area_from_H(k,yc_res,Ac_temp,Ts,RH,0)
                  Qc_temp = Ac_temp*sqrt(g*Ac_temp/Ts); Q_allow=Qc_temp
                  if (Q_allow <= 0d0)then
                      Q_allow = 0d0
                  elseif (Q_allow <= Qmin(k))then
                      Q_allow = Qmin(k)
                  elseif (Q_allow > Qcrit_maxIA(k))then
                      Q_allow = Qcrit_maxIA(k)
                  endif
                  ScIA = ((Q_allow*nm(k))/(Ac_temp*RH**(2d0/3d0)))**2d0
			    if (S0(k) <= ScIA)then; 
                      flow_regIA1 = 1 !flow is subcritical	
                  else
                      flow_regIA1 = 2 !flow is supercritical
                  endif		
                      
                  if (flow_regIA1 ==1)then
                      flowcaseIA1 = 2; goto 20
                  endif
                  if (flow_regIA1 ==2)then                          
                      if (Eres>E11_outf)then 
                          flow_regIA1=11; flowcaseIA1=1 !Flow is supercritical in the pipe, and critical at the outlet of the reservoir. Flow is from reservoir to pipe
                      else
                          flowcaseIA1 = 2; goto 20
                      endif
                  else 
                      flowcaseIA1 = 2; goto 20
                  endif
              elseif (E11_outf >= Eres)then !Flow is from pipe to pond
                  yc_11 = 2d0/3d0*E11_outf
                  if (E11_outf > Ecrit_max(k))then
                      yc_11 = ycrit_max(k); E11_outf =  Ecrit_max(k)
                  endif                                            
                      
                  if (yc_11 > Eres)then !Flow is from pipe to pond
                      flowcaseIA1 = 1; flow_regIA1 = 10 !flow is critical at the node (negative flow). Flow is from pipe to junction
                  elseif (yc_11 <  Eres)then
                      flowcaseIA1 = 2; goto 20
                  else
                      write(98,*),'yc_11 not>=<than Eres. Subr.Free'
                      write(99,*),'yc_11 not>=<than Eres. Subr.Free'
                      call endprog; GLOBAL_STATUS_FLAG = 1; return
                  endif
              else
			    write(98,*),'E11 not>=<than Eres. Freesurface_flowreg'
                  write(99,*),'E11 not>=<than Eres. Freesurface_flowreg'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return
              endif   
          else
			write(98,*),'Error in nodetype1, Freesurface_flowregime'
              write(99,*),'Error in nodetype1, Freesurface_flowregime'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      elseIf (sum == 1)then
          if (IDf11 == 0 .and. IDfb_reser ==1)then			    
              cond_mixed = 1 !Shooting flow from reservoir to pipe
          elseif (IDf11 == 1 .and. IDfb_reser ==0)then
	        cond_mixed = 2 !Shooting flow from pipe to reservoir
          else
			write(98,*),'cond_mixed .ne. 1,2. Freesurface_flowregime'
              write(99,*),'cond_mixed .ne. 1,2. Freesurface_flowregime'
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif           
                  
                  
          !yb = Yreser - dr; Qb = 0d0
          !call Area_from_H(k,yb,Ab,Ts,RH,IDfb_reser)          
          !call Pressure_Pho(k,y11,P_pho11,IDf11)
          !call Pressure_Pho(k,yb,P_phob,IDfb_reser)
          !FL2 = Q11*Q11/A11 + P_pho11
          !FR2 = Qb*Qb/Ab + P_phob
          !Wmagnit = sqrt((FL2 - FR2)/(A11 - Ab))
          !wtemp3 = (Q11-Qb)/(A11-Ab)
          !Wpred = Wmagnit*SIGN(1d0,wtemp3)
            
          
          
             !wtemp3 = (Q11-Qb)/(A11-Ab)	
             !Wpred = dabs(wtemp3)*SIGN(1d0,wtemp3)
             
             
          
      !    if (nodetype1 == 1)then !inflowing 
      !        if (cond_mixed == 1)then
      !            
      !        elseif (cond_mixed == 2)then
      !            
      !        else
			   ! write(98,*),'cond_mixed .ne. 1,2. Freesurface_flowregime'
      !            call endprog; GLOBAL_STATUS_FLAG = 1; return
		    !endif
      !    elseif (nodetype1 == 2)then !outflowing    
      !        
      !    else
      !        write(98,*),'cond_mixed .ne. 1,2. Freesurface_flowregime'
      !        call endprog; GLOBAL_STATUS_FLAG = 1; return
      !    endif
      else
		write(98,*),'sum .ne. 0,1,2. Subr. Freesurface_flowregime'
          write(99,*),'sum .ne. 0,1,2. Subr. Freesurface_flowregime'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

20    continue
	end subroutine