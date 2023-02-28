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

      subroutine Riemann_Mixed_HLL_Leon(j,sumIDAROUND,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,inter_bound,Qb)	
      !This routine is used for computing fluxes at a pure free surface flow interface. 
      use common_module
      use itm_accessors
	implicit none
	integer n,inter_bound !inter_bound = 0, if the cell is Riemann problem is solved at an internal cell, 1 at a right boundary, and 2 at a left boundary 
      ! if (Nodetype(r,j) == 1)then !inflowing  1 is right boundary
      ! if (Nodetype(r,j) == 2)then !outflowing 2 is left boundary
	double precision fvec(1), x(1)
	integer conver_result,info
      double precision AL,AR,QL,QR,cL,cR,hL,hR,uL,uR,SL,SR
	double precision small,Area,temp1,var1,Atemp,temp0,Ctemp
	double precision teta,Ts,RH,TOL_local 
	double precision FL1,FL2,FR1,FR2 !fluxes left and right
      double precision FF1L,FF1R,FF2L,FF2R !fluxes intermediate state HLLS Riemann solver
	!double precision FF1,FF2,FF3 !fluxes intermediate state 
	double precision P_phoL,P_phoR,P_phoStarL,P_phoStarR  !To compute average pressure 
      double precision constant_dry_depth_limit,Super_subcri_limit
      double precision hsL,hsR,AsL,AsR,uStar,cStar,Qstar,AStar,hStar
      double precision phiL,phiR,phiStar,P_phoStar
      double precision Linearized,depth_posit,Rarefact_Phi
      double precision drytempL,drytempR    
      double precision hCentL,hSurL,hCentR,hSurR,hCentStar,hSurStar
      double precision caver,uaver,delA,htemp,href_star,ytemp
      double precision delta_Head,g_delta_I1,PL,PR,Aver_A,Aver_P,utilde
      double precision Sf,S1,S2,H1,H2,deltax,Qb
      
      
	integer j,i,Dry_bed_or_discontinuous,sumIDAROUND,IDL,IDR      
      character(IDLEN) temp_id
      parameter (small = 1d-14)
      
      href_star = 0.95*Yref(j)  
      
      
      
      If (ISNAN(hL) .or. ISNAN(hR))then
          write(98,*),'hL or hR is NaN, Riemann_Mixed_HLL_Leon'
          write(99,*),'hL or hR is NaN, Riemann_Mixed_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return     
      endif 
      
      If (ISNAN(QL) .or. ISNAN(QR))then
          write(98,*),'QL or QR is NaN, Riemann_Mixed_HLL_Leon'
          write(99,*),'QL or QR is NaN, Riemann_Mixed_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return     
      endif       
      
      if (IDL + IDR .ne. sumIDAROUND)then
          write(98,*),'IDL+IDR.ne.sumIDAROUND, Riemann_Mixed_HLL_Leon'
          write(99,*),'IDL+IDR.ne.sumIDAROUND, Riemann_Mixed_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return    
      endif
      
         ! if (j == 11)then
         !     if  (T_GLOBAL>41168.9)then
         !         write(99,*),j,sumIDAROUND,IDL,IDR, 
         !&        hL,hR,AL,AR,QL,QR,FF1,FF2,FF3	
         !     endif
         !   endif
      
      
      if (IDL == 0)then
          if (hL > yref(j) .or. AL > Aref(j))then 
              write(98,*),'j,sumIDAROUND,IDL,IDR,hL,hR,AL,AR,QL,QR, 
     &    FF1L,FF1R,FF2L,FF2R,inter_bound',j,sumIDAROUND,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,inter_bound
              write(99,*),'j,sumIDAROUND,IDL,IDR,hL,hR,AL,AR,QL,QR, 
     &    FF1L,FF1R,FF2L,FF2R,inter_bound',j,sumIDAROUND,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,inter_bound
              write(98,*),'hL,AL,yref,Ar. Rie_Mix',hL,AL,yref(j),Aref(j)
              call endprog; GLOBAL_STATUS_FLAG = 1; return      
          endif  
      endif      
      if (IDR == 0)then
          if (hR > yref(j) .or. AR > Aref(j))then 
               write(98,*),'j,sumIDAROUND,IDL,IDR,hL,hR,AL,AR,QL,QR, 
     &        FF1L,FF1R,FF2L,FF2R,inter_bound',j,sumIDAROUND,IDL,IDR, 
     &        hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,inter_bound
              write(99,*),'j,sumIDAROUND,IDL,IDR,hL,hR,AL,AR,QL,QR, 
     &    FF1L,FF1R,FF2L,FF2R,inter_bound',j,sumIDAROUND,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,inter_bound
              write(98,*),'hR,AR,yref,Aref',hR,AR,yref(j),Aref(j)
              write(99,*),'hR,AR,yref,Aref',hR,AR,yref(j),Aref(j)
              call endprog; GLOBAL_STATUS_FLAG = 1; return      
          endif
      endif
         
           
      !Epsilon is defined in the common module as 10^-12
      Dry_bed_or_discontinuous = 0 !1 indicates that the flow is doscontinuos or dry bed
      
      drytempL = hL-ydry_Cutoff(j)
      drytempR = hR-ydry_Cutoff(j)
      
      !hL < ydry_Cutoff(j)
      !hL-ydry_Cutoff(j) <= 0d0
      
      
      
c     Calculations are performed only when at least one of the cells is not in dry state. 
      if (IDL == 0)then          
          if(drytempL < 0d0)then
              hL = ydry(j); AL = Adry(j)
              QL = 0d0;  uL = 0d0
              cL = Celer_dry(j); phiL=phi_dry(j)
              P_phoL = P_pho_dry(j)
          else 
              call Area_from_H(j,hL,Area,Ts,RH,0); 
              uL=QL/max(AL,Adry(j))
              QL = uL*AL; cL = sqrt(g*AL/Ts)
              !call Phi1(j,hL,phiL)
              call Pressure_Pho(j,hL,P_phoL,0)
              teta = 2d0*ACOS(1d0-2d0*hL/d(j))		
              phiL = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
          endif 
      elseif (IDL == 1)then  
          cL = pc1(j);uL = QL/AL; 
          call CentroidHeight_from_H(j,hL,hCentL,hSurL,1) 	
          P_phoL = Aref(j)*g*(hCentL + hSurL)
          phiL = 6.41*sqrt(g*d(j)/8d0)
      else
          write(98,*),'IDL .ne. 0,1. Riemann_Mixed_HLL_Leon'
          write(99,*),'IDL .ne. 0,1. Riemann_Mixed_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif  
      
              
      if (IDR == 0)then    
          if(drytempR < 0d0)then
              hR = ydry(j); AR = Adry(j); QR = 0d0; 
              uR = 0d0;
              cR = Celer_dry(j); phiR = phi_dry(j); 
              P_phoR=P_pho_dry(j)
          else 
              call Area_from_H(j,hR,Area,Ts,RH,0) 
              uR=QR/max(AR,Adry(j))
              QR = uR*AR; cR = sqrt(g*AR/Ts)
              !call Phi1(j,hR,phiR)
              call Pressure_Pho(j,hR,P_phoR,0)
              teta = 2d0*ACOS(1d0-2d0*hR/d(j))		
              phiR = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
          endif  
      elseif (IDR == 1)then 
          cR =  pc1(j); uR = QR/AR 
          call CentroidHeight_from_H(j,hR,hCentR,hSurR,1) 	
          P_phoR = Aref(j)*g*(hCentR+hSurR)
          phiR = 6.41*sqrt(g*d(j)/8d0)
      else
          write(98,*),'IDR .ne. 0,1. Riemann_Mixed_HLL_Leon'
          write(99,*),'IDR .ne. 0,1. Riemann_Mixed_HLL_Leon'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif   
      
c     When both left and right states are dry, calculations are not performed 
      if (sumIDAROUND ==0)then  
          if(drytempL < 0d0 .AND. drytempR < 0d0)then !Left and Right dry bed conditions	
              !FF1 = 0d0; FF2=fluxdry(j)
              FL1 = 0d0; FR1 = 0d0; FL2 = fluxdry(j); FR2 = fluxdry(j)
              sL = -Celer_dry(j); sR = Celer_dry(j)
              !sL = -10; sR = 10
              utilde = 0d0
              goto 250
              
              !FF1L = FR1; FF1R = FR1; FF2L = FR2; FF2R = FR2
              
              !goto 900  
          endif 
      endif        
      
      
c     Star velocity
      uStar = 1d0/2d0*(uL + uR + phiL - phiR)
      
c	Computation of fluxes left FL and FR      
      if (IDL == 0)then   
          FL1 = QL; FL2 = QL*QL/AL + P_phoL
      else
          P_phoL = g*Aref(j)*(hCentL + hSurL)
          FL1 = QL; FL2 = QL*QL/AL + P_phoL
      endif
          
      if (IDR == 0)then   
          FR1 = QR; FR2 = QR*QR/AR + P_phoR                     
      else
          P_phoR = g*Aref(j)*(hCentR + hSurR)
          FR1 = QR; FR2 = QR*QR/AR + P_phoR
      endif                   
     
  
c     Compute star region for calculation of SL and SR     
      if (sumIDAROUND ==0)then  
          if((drytempL < 0d0) .OR. (drytempR < 0d0))then 
              if(drytempL < 0d0)then!Left dry bed conditions	
                  !call Phi1(j,hR,phiR);
                  teta = 2d0*ACOS(1d0-2d0*hR/d(j))
                  phiR = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
                  cR = Celer_dry(j); uL=0d0
                  SL = uR - phiR; SR = uR + cR; phiL = phi_dry(j) !phy at dry conditions   
              elseif(drytempR < 0d0)then !Right dry bed conditions	 
                  teta = 2d0*ACOS(1d0-2d0*hL/d(j))
                  phiL = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)        
                  !call Phi1(j,hL,phiL);
                  cL=Celer_dry(j); uR=0d0 
		        SL = uL - cL; SR = uL + phiL; phiR = phi_dry(j) !phy at dry conditions  
              else 
                  write(98,*),'drytemp,Riemann_Mixed_HLL_Leon'
                  write(99,*),'drytemp,Riemann_Mixed_HLL_Leon'
		        call endprog; GLOBAL_STATUS_FLAG = 1; return 
              endif        
              goto 200
              else    
              !No dry-bed conditions                   
              If(min(hL,hR)<=10d0*ydry(j) .or. 
     &                    max(hL,hR)<=0.20*yref(j))then !This should be 20% of yref(j)              
                  !Linearization + depth positivity condition
                  teta = 2d0*ACOS(1d0-2d0*hL/d(j))
                  phiL = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
                  teta = 2d0*ACOS(1d0-2d0*hR/d(j))
                  phiR = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
	            !call Phi1(j,hL,phiL); 
                  !call Phi1(j,hR,phiR)	
                  
                  
	            AStar = depth_posit(AL,AR,phiL,phiR,uL,uR)	
                  call H_from_Area(j,Astar,hStar,927,0)
                  call Pressure_Pho(j,hStar,P_phoStar,0)  
                  call Area_from_H(j,hStar,Area,Ts,RH,0) 
                  cStar = sqrt(g*Astar/Ts)
                  ! we can change phi for both. 
              else
                  htemp = max(hL,hR)     
                  if (htemp < href_star)then         
                      !AStar = Linearized(AL,AR,cL,cR,uL,uR)
                      phiStar = Rarefact_Phi(phiL,phiR,uL,uR)
                      If (phiStar <= 0d0)then
                          phiStar = phi_dry(j)
                          call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                          write(98,*),'Pipe1=',trim(temp_id)   
                           write(98,*),'phiStar1 <= 0d0'
                            uStar = 0d0
      write(98, '(4I4, 10F20.4)'),j,sumIDAROUND,IDL,IDR, 
     &    hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R
                          !call endprog;GLOBAL_STATUS_FLAG=1;return  
      
                          SL = min(uL-cL, uR-cR)
                          SR = max(uL+cL, uR+cR)
                          goto 200
                      endif   
                      If (phiStar >= Phiref(j))then 
                          phiStar = Phiref(j)
                          SL = min(uL-cL, uR-cR)
                          SR = max(uL+cL, uR+cR)
                          goto 200
                      endif
                      call H_from_Phi_Raref(j,phiStar,
     &                            hStar,Astar,cStar)
                      !call H_from_Area(j,Astar,hStar,928,0)
                      call Pressure_Pho(j,hStar,P_phoStar,0)
                      !call Area_from_H(j,hStar,Astar,Ts,RH,0) 
                      !cStar = sqrt(g*Astar/Ts)
                  else 
                      !AStar = Linearized(AL,AR,cL,cR,uL,uR)
                      phiStar = Rarefact_Phi(phiL,phiR,uL,uR)
                      If (phiStar <= 0d0)then
                          phiStar = phi_dry(j)
                          call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                          write(98,*),'Pipe2=',trim(temp_id)   
                          write(98,*),'phiStar2 <= 0d0'                          
                          !call endprog; GLOBAL_STATUS_FLAG = 1; return  
                          cL = pc; cR = pc
                          
                          SL = min(uL-cL, uR-cR)
                          SR = max(uL+cL, uR+cR)
                          goto 200
                      endif 
                      If (phiStar >= Phiref(j))then 
	                    phiStar = Phiref(j)
                          
                          cL = pc; cR = pc
                          SL = min(uL-cL, uR-cR)
                          SR = max(uL+cL, uR+cR)
                          goto 200
                      endif   
                      call H_from_Phi_Raref(j,phiStar,hStar,Astar,cStar)
                      !call H_from_Area(j,Astar,hStar,928,0)
                      call Pressure_Pho(j,hStar,P_phoStar,0)
                      !call Area_from_H(j,hStar,Astar,Ts,RH,0) 
                      !cStar = sqrt(g*Astar/Ts)   
                  endif
              endif
          endif                      
      elseif (sumIDAROUND == 1 )then  !If flow is mixed, assume hStar = 0.7*d(j)              
              cL = pc; cR = pc; SL= uL-cL; SR= uR+cR; goto 200
      elseif (sumIDAROUND == 2)then
              cL = pc; cR = pc; SL= uL-cL; SR= uR+cR; goto 200 
                  
	        !delA = abs((AL-AR)/(5d-1*(AL+AR)))
	        !!delU = abs((UL-UR)/(5d-1*(UL+UR)))
	        !if (delA < 1.d-14)then
		       ! AStar = (AL+AR)/2d0
	        !else
		       ! AStar = Linearized(AL,AR,pc1(j),pc1(j),uL,uR)
	        !endif
	        !
	        !If (ISNAN(AStar))then 
         !         temp_id = ''; call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes			
         !         write(98,*),'NaN in Riemann_Mixed. Pipe3',temp_id
         !!             write(98,*),j,sumIDAROUND,IDL,IDR, 
         !!&                hL,hR,AL,AR,QL,QR,FF1,FF2,FF3
         !         call endprog; GLOBAL_STATUS_FLAG = 1; return
         !     endif	              
         !     call H_from_Area(j,AStar,hStar,415,1)!Water depth
         !     call CentroidHeight_from_H(j,hStar,hCentStar,hSurStar,1)
	        !caver = pc1(j); uaver = (uL+uR)/2d0
         !     Qstar = QL + (uaver-caver)*(AStar-AL)
	        !P_phoStar = Aref(j)*g*(hCentStar+hSurStar)
         !     !computation of fluxes (star region)	
	        !!FF1 = Qstar; FF2 = Qstar*Qstar/Astar + P_phoStar; FF3 = FF2
         !     
         !     FF1L = Qstar; FF1R = Qstar 
         !     FF2L = Qstar*Qstar/Astar + P_phoStar; FF2R = FF2L
         !     goto 900  
      else
              write(98,*),'sum .ne. 0,1,2 Riemann_Mixed_HLL_Leon'
              write(99,*),'sum .ne. 0,1,2 Riemann_Mixed_HLL_Leon'
	        call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif     
      
      
c     Speeds for rarefaction and shock waves      
      TOL_local = 1.d-6
      if (sumIDAROUND == 0)then
	    var1 = (AStar-AL)/(5d-1*(AStar+AL))
          if(abs(var1) < Tol_int_10_4 .or. AStar <= AL)then !Rarefaction wave
                  SL = min (uL-cL, uStar - cStar)
          else 
                  temp1 = (P_phoStar - P_phoL)*Astar/AL/(Astar-AL)	!shock wave	
		        if (temp1 < 0d0)then
                      write(98,*),'neg. sqrt.Riemann_Mixed_HLL_Leon 10'
                      write(99,*),'neg. sqrt.Riemann_Mixed_HLL_Leon 10'
                      SL = min(uL-cL, uR-cR)
                      SR = max(uL+cL, uR+cR)
                      goto 200
			        !call endprog; GLOBAL_STATUS_FLAG = 1; return
		        endif
		        SL = uL-sqrt(temp1)
          endif      
          var1 = (AStar-AR)/(5d-1*(AStar+AR))
	    if(abs(var1) < Tol_int_10_4 .or. AStar <= AR)then !Rarefaction wave
                  SR = max(uR+cR,  uStar + cStar) 
	    else
		        temp1 = (P_phoStar - P_phoR)*Astar/AR/(Astar-AR)  !shock wave	
		        if (temp1 < 0d0)then		    
			        write(98,*),'neg. sqrt.Riemann_Mixed_HLL_Leon 11'
                      write(99,*),'neg. sqrt.Riemann_Mixed_HLL_Leon 11'
                      SL = min(uL-cL, uR-cR)
                      SR = max(uL+cL, uR+cR)
                      goto 200
			        !call endprog; GLOBAL_STATUS_FLAG = 1; return
		        endif
		        SR = uR+sqrt(temp1) 		
          endif	 
      elseif (sumIDAROUND == 1)then
          SL = min (uL-cL, uStar - cStar)
          SR = max(uR+cR,  uStar + cStar)
      else
          write(98,*),'sum .ne. 0,1. Riemann_Mixed_HLL_Leon'	
		call endprog; GLOBAL_STATUS_FLAG = 1; return 
      endif
      
       
c	computation of fluxes
200   if(ISNAN(SL) .or. ISNAN(SR))then       
          !write(98,*), 'SL or SR are NaN, HLL Riemann_Mixed_HLL_Leon' 		
          !write(99,*), 'SL or SR are NaN, HLL Riemann_Mixed_HLL_Leon'
          !call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
          !write(98,*),'Pipe No ',temp_id
          !write(98,*),'SL,SR ',SL,SR
          !write(98,*),'cL,cR ',cL,cR
          !write(98,*),'uL,uR ',uL,uR
          !write(98,*),'Time = :',T_GLOBAL
          !write(98,*),'uStar,cStar',uStar,cStar
          !write(98,*),'phiL,phiR',phiL,phiR
          SL = min(uL-cL, uR-cR)
          SR = max(uL+cL, uR+cR)
		!call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif  
      
      
      utilde = (uL*sqrt(AL) + uR*sqrt(AR))/(sqrt(AL) + sqrt(AR)) !Roe's approach
  250 if (inter_bound == 0)then !check signs 
          deltax = dx(j) !This is okay
      elseif (inter_bound == 1 .or. inter_bound == 2)then !1 at a right boundary, !2 at a left boundary
          deltax = dx(j)/2d0 !The Riemann solver already assumes the left has higher bottom elevantion than the right. This dx will be positive for all cases
      else
          write(98,*),'inter_bound .ne. 0,1,2. Riemann_Mixed_HLL_Leon'	
		call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif
      
      delta_Head = hR - hL - S0(j)*deltax !delta represents the spatial difference between the cell i+1 and i. S0 and dx are always positive
      g_delta_I1 =  P_phoR - P_phoL  !This already includes gravity so no need to multiple by gravity in Sdx
      call Area_from_H(j,hL,Area,Ts,RH,IDL); PL = Area/RH
      call Area_from_H(j,hR,Area,Ts,RH,IDR); PR = Area/RH
      Aver_A = (AL + AR)/2d0; Aver_P = (PL + PR)/2d0      
      Sf = utilde*abs(utilde)*nm(j)**2d0*(Aver_P/Aver_A)**(4d0/3d0)
      S1 = 0d0
      S2 = -g*Aver_A*delta_Head + g_delta_I1 - g*Aver_A*Sf*deltax !This is Sfdx. This already includes Dx. 
      !S2 = g*Aver_A*S0(j)*deltax - g*Aver_A*Sf*deltax !This is Sfdx. This already includes Dx. 
      if(SL > 0d0 .and. SR > 0d0)then   
c         Right-going supercritical flow
          FF1L = FL1; FF1R = FL1 + S1; FF2L = FL2; FF2R = FL2 + S2
          !elseif (SR < 0d0)THEN	     
      elseif (SR < 0d0 .and. SL < 0d0)THEN
c         Left-going supercritical flow
          FF1L = FR1-S1; FF1R = FR1; FF2L = FR2-S2; FF2R = FR2
      else   
c         Subcritical flow     
		!FF1 = (SR*FL1-SL*FR1+SR*SL*(AR-AL))/(SR-SL), FF2 = (SR*FL2-SL*FR2+SR*SL*(QR-QL))/(SR-SL)
          H1 = -g*Aver_A*delta_Head + g_delta_I1 - 
     &     g*Aver_A*Sf*deltax          
          !H1 = g*Aver_A*S0(j)*deltax - g*Aver_A*Sf*deltax          
          H1 = -H1/(SR*SL) !(note that the sign of H is negative). Replace also in the routines fluxes. 
          H2 = 0d0              
          FF1L=(SR*FL1-SL*FR1-SR*SL*(AL-AR) + SL*(S1-SR*H1))/(SR-SL)
          FF1R=(SR*FL1-SL*FR1-SR*SL*(AL-AR) + SR*(S1-SL*H1))/(SR-SL) 
          FF2L=(SR*FL2-SL*FR2-SR*SL*(QL-QR) + SL*(S2-SR*H2))/(SR-SL)
          FF2R=(SR*FL2-SL*FR2-SR*SL*(QL-QR) + SR*(S2-SL*H2))/(SR-SL)
      endif 
900   continue

c     This is to calculate the flow discharge at the boundary. In internal cells, the value doesn't have any meaning.
      if (inter_bound == 0)then !This value does not matter as it is not used
          Qb = 0d0 
      elseif (inter_bound == 1 )then !1 at a right boundary. This is inflowing pipe. Nodetype(R,1) == 1, inflowing
          Qb = FF1L 
      elseif (inter_bound == 2)then !2 at a left boundary. Nodetype(R,1) == 2, outflowing	
          Qb = FF1R 
      else
          write(98,*),'inter_bound .ne. 0,1,2. Riemann_Mixed_HLL_Leon'	
		call endprog; GLOBAL_STATUS_FLAG = 1; return  
      endif
      end subroutine
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
      
     