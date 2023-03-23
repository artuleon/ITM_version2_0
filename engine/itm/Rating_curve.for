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

      subroutine Rating_curve(R)
      use common_module
      use itm_accessors
      implicit none
      integer iter,j,i,IH,R,sum,n
	double precision t,AA,RH,R1,F11,F12,Wpred
      double precision u11,y11,A11,Q11,c11,T1,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob,Ycrit
	double precision k1,x(10),fvec(10),TOL_CONST_STATE
	double precision hL,hR,AL,AR,QL,QR,AW0,QW0
	double precision Pw0,p1,FF1L,FF1R,FF2L,FF2R
	external rat_curve
	integer conver_result,info,Id1,Idb,IDF0,CODL
	character(IDLEN) temp_id	
	
	!This routine computes the flux for a rating curve boundary 	
	j = NodeID(R,1)	
	!Distance from pipe invert to weir (or spillway) crest	
	if (Nodetype(R,1) == 2)then !outflowing          
          IH = 3
          y11 = h0(j,IH); Q11 = Q0(j,IH); A11=A0(j,IH); ID1=IDFlow(j,IH)
      Elseif (Nodetype(R,1) == 1)then !inflowing
          IH = Nx(j)-2  
          y11 = h0(j,IH); Q11 = Q0(j,IH); A11=A0(j,IH); ID1=IDFlow(j,IH)
      Else
          write(98,*),'Nodetype(R,1) .ne. 1,2. Subr. Rating_curve'
          write(99,*),'Nodetype(R,1) .ne. 1,2. Subr. Rating_curve'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      Endif      
      
c     !Check if there is enough data in rating_curve 
      if (y11 >= Max_Head_rating_curve(R))then 
          if (y11 < yref(j))then !For pressurized flows, we can use the same rating curve as y = yref(k)
              temp_id = ''
              call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
              write(98,*),'Increase H-Q rating curve data. Node',temp_id
              write(99,*),'Increase H-Q rating curve data. Node',temp_id
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      endif
            
	!Type of flow at the boundary
	If (Id1 == 1)then !Pressurized flow
		Idb = 1
	elseIf (Id1 == 0)then !Open channel flow	
		Idb = 0
	else
	    write(98,*),'Id1 n.e. 0,1. Subr. Rating_curve'
          write(99,*),'Id1 n.e. 0,1. Subr. Rating_curve'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

	sum = Id1+Idb
	u11= Q11/A11      
      
      !Dry bed conditions
      if (Id1 == 0)then
          if (y11 < ydry_Cutoff(j))then
              !yb = ydry(j); Qb = 0d0; Idb = 0
              hL = ydry(j); hR = ydry(j); AL = Adry(j); AR = Adry(j)
              QL = 0d0; QR = 0d0
              call Riemann_Mixed_HLL_Leon(j,0,0,
     &        0,hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,0,Qb)
              
              if (Nodetype(R,1) == 1)then !inflowing	
                  Qbound(j,2) = Qb
			    Fdownst(j,1) = FF1L; Fdownst(j,2) = FF2L                  
              elseif (Nodetype(R,1) == 2)then !outflowing		
                  Qbound(j,1) = Qb
			    Fupst(j,1) = FF1R; Fupst(j,2) = FF2R
			else       
				write(98,*),'Unknown Nodetype(R,1). Subr. Rating'
                  write(99,*),'Unknown Nodetype(R,1). Subr. Rating'
				call endprog; GLOBAL_STATUS_FLAG = 1; return
			Endif               
              Endif  
              yb = 0d0
              goto 20
          endif   
      endif
	
	!Wave celerity
	call Pressure_Pho(j,y11,P_pho11,Id1)
	If (Id1==0)then
		call Area_from_H(j,y11,AA,Tb,RH,Id1); c11 = sqrt(g*A11/Tb)
	elseIf (Id1 == 1)then 
		c11 = pc1(j)
	else		
          write(98,*),'Id1 n.e. 0,1. Subr. Rating_curve'
          write(99,*),'Id1 n.e. 0,1. Subr. Rating_curve'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
			
	parint1000 = 0; param1 = A11;  param2 = c11; param3 = Q11	
	param4 = y11; parint1 = j; parint2 = Id1; parint3 = Idb
	parint4 = sum; parint5 = R
	n = 1
	x(1:n) = (/y11/)
	
	call hybrd1 (rat_curve,n,x,fvec,Tol_int_10_6,info)
	call converg (conver_result, info)	
	If(conver_result == 0)then
          yb = x(1) 
          If (Id1==0)then  
              If (yb >= yref(j)) yb = yref(j)                  
              if (yb < ydry(j)) yb = ydry(j)
          else if (Id1==1)then  
		 !         If (yb >= yref(j))then
		 !             call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		 !             write(99,*),'Flow depth exceeds pipe diameter 
		 !&                in rating curve at node ',temp_id
		 !             write(99,*),'Revise rating curve or increase pipe
		 !&                diameter connecting node ',temp_id
			   !     write(98,*),'Flow depth exceeds pipe diameter 
		 !&                in rating curve at node ',temp_id
		 !             write(98,*),'Revise rating curve or increase pipe
		 !&                diameter connecting node ',temp_id
			   !     call endprog; GLOBAL_STATUS_FLAG = 1; return  
		 !         endif
              
              If (yb >= Max_Head_rating_curve(R)-Tol_int_10_4)then
                  !yb = yref(j)
                  yb = Max_Head_rating_curve(R)-Tol_int_10_4
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif    
          else
              write(98,*),'Rating_curve. Id1 n.e. 0,1'
              write(99,*),'Rating_curve. Id1 n.e. 0,1'
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      ElseIf(conver_result == 1)then
          temp_id = ''
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
          write(99,*),'No converg. Subr. Rating_curve. Node =',temp_id
          yb = y11
          If (Id1==0)then  
              If (yb >= yref(j)) yb = yref(j)                  
              If (yb < ydry(j)) yb = ydry(j) 
          else if (Id1==1)then                 
              If (yb >= Max_Head_rating_curve(R)-Tol_int_10_4)then
                  !yb = yref(j)
                  yb = Max_Head_rating_curve(R)-Tol_int_10_4
              elseif (yb < ydry(j))then
                  yb = ydry(j) 
              endif    
          else
              write(98,*),'Id1 n.e. 0,1. Rating_curve'
              write(99,*),'Id1 n.e. 0,1. Rating_curve'
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      else
          write(98,*),'conver_result n.e. 0,1. Rating_curve'
          write(99,*),'conver_result n.e. 0,1. Rating_curve'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
      !Check if there is enough data in rating_curve for yb
      if (yb >= Max_Head_rating_curve(R))then 
          if (yb < yref(j))then
              temp_id = ''
              call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
              write(98,*),'Increase H-Q rating curve data. Node',temp_id
              write(99,*),'Increase H-Q rating curve data. Node',temp_id
              call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif
      endif
            
 3    call itm_get_Q_from_rat_curve(R,yb,Qb)	
            
      !To limit the withdrawal flow when the water is close to the weir invert 
      if (A11 > area_weir(R))then
           if (dabs(Qb) > (dx(j)/DT_GLOBAL)*(A11-area_weir(R)))then
              Qb = (dx(j)/DT_GLOBAL)*(A11-area_weir(R))
              p1 = abs(Drop(R,1)) !weir height 
              yb = p1
           endif
      endif
      
	if (Nodetype(R,1) == 2)then !outflowing
          Qb = -Qb	
	elseif (Nodetype(R,1) == 1)then !inflowing
          Qb = Qb	
	else          
		write(98,*),'Nodetype n.e. 1,2. rat_curve.'
          write(99,*),'Nodetype n.e. 1,2. rat_curve.'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif

	!Flux computation
10	call Area_from_H(j,yb,Ab,Tb,RH,Idb)
 	call Pressure_Pho(j,yb,P_phob,Idb)       
	F11 = Qb; F12 = Qb**2d0/Ab + P_phob
      
	if (Nodetype(R,1) == 2)then !outflowing
		Qbound(j,1) = Qb; Fupst(j,1) = F11; Fupst(j,2) = F12		
	elseif (Nodetype(R,1) == 1)then !inflowing
		Qbound(j,2) = Qb		
		Fdownst(j,1) = F11;	Fdownst(j,2) = F12		
	else
	    temp_id = ''; call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
		write(98,*),'Nodetype n.e. 1,2. Rat_curve. Node =',temp_id
          write(99,*),'Nodetype n.e. 1,2. Rat_curve. Node =',temp_id
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	Endif 
	
	!Water level over the weir or spillway crest       
      yres_jun_old(R) = y11
      yres_jun_old(R) = yb
20    return
      end

	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine rat_curve(n, x, fvec, iflag )
	use common_module
      use itm_accessors
	implicit none	
	integer j,n,iflag,IA,IDfbIA1,sum,R
	double precision fvec(n),x(n),Qb,ub1,temp8
	double precision AIA,TsIA,RHIA,u11,Q11,y11,A11,cb1,c11
      double precision E11
	character*25 temp_id
      integer :: seed
	double precision ran_number
	
	parint1000 = 0
	A11 = param1; c11 = param2; Q11 = param3; y11 = param4  
	j = parint1; IDfbIA1 = parint3
	sum = parint4; R = parint5
	u11 = Q11/A11
      E11 = y11 + (1.0/2.0*g)*u11**2.0
	If (x(1) < ydry(j)) x(1) = ydry(j)	
	If (x(1) >= Max_Head_rating_curve(R)) x(1) = Max_Head_rating_curve(R)
	
	 If (ISNAN(x(1)))then 	          
          call random_seed(seed)  
          call random_number(ran_number)
          if(sum == 0)then
              x(1) = ran_number*yref(j)   !y11  
          elseif(sum == 2)then
              x(1) = yref(j)  + ran_number*yref(j)
          else 
              write(98,*),'sum == 2 .ne. 0,2. rat_curve. '
              write(99,*),'sum == 2 .ne. 0,2. rat_curve. '
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
          endif	
	endif	
	
	call Area_from_H(j,x(1),AIA,TsIA,RHIA,IDfbIA1)
	cb1 = sqrt(g*AIA/TsIA)	
	
	!Compute flow discharge from rating curve
	call itm_get_Q_from_rat_curve(R,x(1),Qb)
	if (Nodetype(R,1) == 2)then !outflowing
		Qb = -Qb	
	elseif (Nodetype(R,1) == 1)then !inflowing
		Qb = Qb	
	else
		write(98,*),'Nodetype(R,1) n.e. 1,2. rat_curve.'
          write(99,*),'Nodetype(R,1) n.e. 1,2. rat_curve.'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
	
	ub1 = Qb/AIA
	if (Nodetype(R,1) == 2)then !outflowing
		If (sum == 0) then
			fvec(1) = ub1 - u11 - (cb1 + c11)*(AIA - A11)/(AIA + A11)
          elseif (sum == 2) then			
			!fvec(1) = ub1 - pc1(j)*LOG (AIA)-(u11 - pc1(j)*LOG(A11))  
              fvec(1) = ub1 - u11 - (cb1 + c11)*(AIA - A11)/(AIA + A11)
          else
              write(98,*),'sum n.e. 0,2. Subr. rat_curve.'			
              write(99,*),'sum n.e. 0,2. Subr. rat_curve.'
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif		
	elseif (Nodetype(R,1) == 1)then !inflowing
		If (sum == 0) then
			fvec(1) = ub1 - u11 + (cb1 + c11)*(AIA - A11)/(AIA + A11)	
          elseif (sum == 2) then
			!fvec(1) = ub1 + pc1(j)*LOG (AIA) - (u11 + pc1(j)*LOG(A11))
              fvec(1) = ub1 - u11 + (cb1 + c11)*(AIA - A11)/(AIA + A11)	
          else
              write(98,*),'sum n.e. 0,2. Subr. rat_curve.'			
              write(99,*),'sum n.e. 0,2. Subr. rat_curve.'			
			call endprog; GLOBAL_STATUS_FLAG = 1; return
		endif	
	else
	    write(98,*),'Nodetype(R,1) n.e. 1,2. Subr. rat_curve.'
          write(99,*),'Nodetype(R,1) n.e. 1,2. Subr. rat_curve.'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
	return
	end	