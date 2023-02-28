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

	subroutine junct2pipes_same_diam(R)
	!This routine computes the fluxes at a junction boundary of two pipes 
	!that have the same diameter with no inflow hydrograph.
	use common_module
	implicit none 
	integer j,k,r,IA,IC,IH,IK,sum,CODL,Idf0,i,IDL,IDR,j1,j2
	double precision Aw0,Qw0,Ab,Qb,yb,Wpred	
	double precision yL,yR,AL,AR,QL,QR,h0b1,A0b1,Q0b1,Dt,Pw0
	double precision signflow(2)
      double precision Qinf_old,Qinf_new,Qinflow,tim,Ts,RH
      double precision yjunction_temp, Q_temp,FF1L,FF1R,FF2L,FF2R
      double precision yser_temp, temp_sum
      character*25 temp_id
            
c     Identifying the first pipe and second pipe
      if (Nodetype(r,1) + Nodetype(r,2) == 3)then
          if (Nodetype(r,1) == 1)then
              j1 = 1; j2 = 2
          else
              j1 = 2; j2 = 1
          endif
      else
          j1 = 1; j2 = 2
      endif      
	     
	!values at adjacent cells
	j = j1 !left cell 
	k = NodeID(r,j)
	IA = k
	Dt = DT_GLOBAL
      tim = T_GLOBAL  
      
	if (Nodetype(r,j) == 1)then !inflowing
          IH = Nx(k)-2  !cell for mixed interface
          QL = Q0(k,IH); AL = A0(k,IH); yL = h0(k,IH)
          IDL = IDFlow(k,IH)
		signflow(j) = 1d0	!direction of flow discharge
      elseif (Nodetype(r,j) == 2)then !outflowing
          IH = 3  		
          QL = - Q0(k,IH); AL = A0(k,IH); yL=h0(k,IH)
          IDL = IDFlow(k,IH)
		signflow(j) = -1d0	
	else
		write(98,*),'Nodetype(r,j) unknown. junct2pipes_same_diam'
          write(99,*),'Nodetype(r,j) unknown. junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      IDf1(j1) = IDFlow(k,IH)
      

	j = j2 !right cell 
	k = NodeID(r,j)
	IC = k
	if (Nodetype(r,j) == 1)then !inflowing		
          Ik = Nx(k)-2  !cell for mixed interface
          QR = - Q0(k,Ik); AR = A0(k,Ik); yR=h0(k,Ik)
          IDR = IDFlow(k,Ik)
		signflow(j) = -1d0	!direction of flow discharge		
      elseif (Nodetype(r,j) == 2)then !outflowing
          Ik = 3  !cell for mixed interface  				
          QR = Q0(k,Ik); AR = A0(k,Ik); yR=h0(k,Ik)
          IDR = IDFlow(k,Ik)
		signflow(j) = 1d0	!direction of flow discharge
	else
		write(98,*),'Nodetype(r,j) unknown. junct2pipes_same_diam'
          write(99,*),'Nodetype(r,j) unknown. junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      IDf1(j2) = IDFlow(k,Ik)
      
	     
	sum = IDL+IDR          
      call Riemann_Mixed_HLL_Leon(IA,sum,
     &    IDL,IDR,yL,yR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,0,Qb) 
      !We are using 0 for inter_bound because the boundary is not left or right. We are bypassing it. 
 
 	j = j1
	k = NodeID(r,j)		
	if (Nodetype(r,j) == 1)then !inflowing
		Fdownst(k,1) = FF1L*signflow(j); Fdownst(k,2) = FF2L			        
	elseif(Nodetype(r,j) == 2)then !outflowing
		Fupst(k,1) = FF1L*signflow(j); Fupst(k,2) = FF2L
	else
		write(98,*),'Nodetype(r,j) .ne. 1,2'
		write(98,*),'subrout. junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
         
      j = j2	
	k = NodeID(r,j)		
	if (Nodetype(r,j) == 1)then !inflowing
		Fdownst(k,1) = FF1R*signflow(j)
		Fdownst(k,2) = FF2R		        
	elseif(Nodetype(r,j) == 2)then !outflowing
		Fupst(k,1) = FF1R*signflow(j)
		Fupst(k,2) = FF2R
	else
		write(98,*),'Nodetype(r,j) .ne. 1,2'
		write(98,*),'subrout. junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif      
     
c     Assigning IDflows at the boundaries      
	yres_jun_old(r) = (yL + yR)/2d0        
      j = j1	
	k = NodeID(r,j)		
	if (Nodetype(r,j) == 1)then !inflowing
          IH = Nx(k)-2 + 1 		        
	elseif(Nodetype(r,j) == 2)then !outflowing
		IH = 3 - 1	
	else
		write(98,*),'Nodetype(r,j) .ne. 1,2 junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif   
      IDFlow(k,IH) = IDf1(j2)
      
      j = j2	
	k = NodeID(r,j)		
	if (Nodetype(r,j) == 1)then !inflowing
          IH = Nx(k)-2 + 1 		        
	elseif(Nodetype(r,j) == 2)then !outflowing
		IH = 3 - 1	
	else
		write(98,*),'Nodetype(r,j) .ne. 1,2 junct2pipes_same_diam'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
        endif   
      IDFlow(k,IH) = IDf1(j1)    
      return	  
      end subroutine
