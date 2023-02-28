! This file is part of the ITM model. Last Updated 07/15/2020
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

      subroutine Fluxes_boundary_HLL_Leon(j,yb,Idb,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1,FF2,FF1L,FF1R,FF2L,FF2R)	
      !This routine is used for computing fluxes at a pure free surface flow interface. 
      use common_module
	implicit none
	integer n,inter_bound !inter_bound = 0, if the cell is Riemann problem is solved at an internal cell, 1 at a right boundary, and 2 at a left boundary 
      ! if (Nodetype(r,j) == 1)then !inflowing  1 is right boundary
      ! if (Nodetype(r,j) == 2)then !outflowing 2 is left boundary
	
      double precision AL,AR,hL,hR,uL,uR,SL,SR,QL,QR,phiL,phiR,yb
	double precision small,Area,Ts,RH,teta,drytempL,drytempR
	double precision FF1,FF2 !fluxes mass and momentum
      double precision FF1L,FF1R,FF2L,FF2R !fluxes intermediate state HLLS Riemann solver
	!double precision FF1,FF2,FF3 !fluxes intermediate state 
	double precision P_phoL,P_phoR,uStar,cStar,cL,cR,hCentL
      double precision hSurL,hCentR,hSurR
      double precision delta_Head,g_delta_I1,PL,PR,Aver_A,Aver_P,utilde
      double precision Sf,S1,S2,H1,H2,deltax
	integer j,i,Dry_bed_or_discontinuous,sumIDAROUND,IDL,IDR,Idb    
      character*25 temp_id      
      
      If (ISNAN(hL) .or. ISNAN(hR))then
          write(98,*),'hL or hR is NaN, Fluxes_boundary_HLL_Leon'
          write(99,*),'hL or hR is NaN, Fluxes_boundary_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return     
      endif       
     
      If (ISNAN(QL) .or. ISNAN(QR))then
          write(98,*),'QL or QR is NaN, Fluxes_boundary_HLL_Leon'
          write(99,*),'QL or QR is NaN, Fluxes_boundary_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return     
      endif       
      
      drytempL = hL-ydry_Cutoff(j)
      drytempR = hR-ydry_Cutoff(j)
      
c     Calculations are performed only when at least one of the cells is not in dry state. 
      if (IDL == 0)then 
          if(drytempL < 0d0)then
              hL = ydry(j); AL = Adry(j)
              QL = 0d0;  uL = 0d0
              cL = Celer_dry(j); phiL=phi_dry(j)
              P_phoL = P_pho_dry(j)
          else 
              call Area_from_H(j,hL,Area,Ts,RH,0); uL=QL/max(AL,Adry(j))
              cL = sqrt(g*AL/Ts); call Pressure_Pho(j,hL,P_phoL,0)
              teta = 2d0*ACOS(1d0-2d0*hL/d(j))		
              phiL = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)             
          endif          
      elseif (IDL == 1)then  
          cL = pc1(j); uL = QL/AL
          call CentroidHeight_from_H(j,hL,hCentL,hSurL,1) 	
          P_phoL = Aref(j)*g*(hCentL + hSurL)
          phiL = 6.41*sqrt(g*d(j)/8d0)
      else
          write(98,*),'IDL .ne. 0,1. Fluxes_boundary_HLL_Leon'
          write(99,*),'IDL .ne. 0,1. Fluxes_boundary_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif        
              
      if (IDR == 0)then   
           if(drytempR < 0d0)then
              hR = ydry(j); AR = Adry(j); QR = 0d0; 
              uR = 0d0;
              cR = Celer_dry(j); phiR = phi_dry(j); 
              P_phoR=P_pho_dry(j)
          else  
              call Area_from_H(j,hR,Area,Ts,RH,0); uR=QR/max(AR,Adry(j))
              cR = sqrt(g*AR/Ts);call Pressure_Pho(j,hR,P_phoR,0)
              teta = 2d0*ACOS(1d0-2d0*hR/d(j))		
              phiR = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
          endif
      elseif (IDR == 1)then 
          cR =  pc1(j); uR = QR/AR 
          call CentroidHeight_from_H(j,hR,hCentR,hSurR,1) 	
          P_phoR = Aref(j)*g*(hCentR+hSurR)
          phiR = 6.41*sqrt(g*d(j)/8d0)
      else
          write(98,*),'IDR .ne. 0,1. Fluxes_boundary_HLL_Leon'
          write(99,*),'IDR .ne. 0,1. Fluxes_boundary_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif    
      
c     Star velocity
      uStar = 1d0/2d0*(uL + uR + phiL - phiR)
      
      sumIDAROUND = IDL + IDR    
      if (sumIDAROUND == 0)then
          if (Idb == 0)then
              call Area_from_H(j,yb,Area,Ts,RH,Idb)
              cStar = sqrt(g*Area/Ts)
          else
              cStar = 0d0 !For sumIDAROUND = 1, 2, we will use use pc as the wave speed. 
          endif
      else
           cStar = pc
      endif
      
      SL = min (uL-cL, uStar - cStar)
      SR = max(uR+cR,  uStar + cStar) 
      
      deltax = dx(j)/2d0 !The Riemann solver already assumes the left has higher bottom elevantion than the right. This dx will be positive for all cases
      delta_Head = hR - hL - S0(j)*deltax !delta represents the spatial difference between the cell i+1 and i. S0 and dx are always positive
      g_delta_I1 =  P_phoR - P_phoL  !This already includes gravity so no need to multiple by gravity in Sdx
      call Area_from_H(j,hL,Area,Ts,RH,IDL); PL = Area/RH
      call Area_from_H(j,hR,Area,Ts,RH,IDR); PR = Area/RH
      Aver_A = (AL + AR)/2d0; Aver_P = (PL + PR)/2d0
      utilde = (uL*sqrt(AL) + uR*sqrt(AR))/(sqrt(AL) + sqrt(AR)) !Roe's approach
      Sf = utilde*abs(utilde)*nm(j)**2d0*(Aver_P/Aver_A)**(4d0/3d0)
      S1 = 0d0
      S2 = -g*Aver_A*delta_Head + g_delta_I1 - g*Aver_A*Sf*deltax !This is Sfdx. This already includes Dx. 
      !S2 = g*Aver_A*S0(j)*deltax - g*Aver_A*Sf*deltax !This is Sfdx. This already includes Dx. 
      
      if(SL > 0d0 .and. SR > 0d0)then   
c         Right-going supercritical flow            
		!FF1 = FL1; FF2 = FL2
          FF1L = FF1; FF1R = FF1 + S1; FF2L = FF2; FF2R = FF2 + S2
          !elseif (SR < 0d0)THEN	     
      elseif (SR < 0d0 .and. SL < 0d0)THEN
c         Left-going supercritical flow
          !FF1 = FR1; FF2 = FR2; 
          FF1L = FF1-S1; FF1R = FF1; FF2L = FF2-S2; FF2R = FF2
          else   
c         Subcritical flow     
		!FF1 = (SR*FL1-SL*FR1+SR*SL*(AR-AL))/(SR-SL). This is Fstar
 		!FF2 = (SR*FL2-SL*FR2+SR*SL*(QR-QL))/(SR-SL)
          H1 = -g*Aver_A*delta_Head + g_delta_I1 - g*Aver_A*Sf*deltax
          H1 = -H1/(SR*SL)
          H2 = 0d0              
          FF1L= FF1 + SL*(S1-SR*H1)/(SR-SL)
          FF1R= FF1 + SR*(S1-SL*H1)/(SR-SL) 
          FF2L= FF2 + SL*(S2-SR*H2)/(SR-SL)
          FF2R= FF2 + SR*(S2-SL*H2)/(SR-SL)
      endif           
900   return
      end subroutine
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
      
     