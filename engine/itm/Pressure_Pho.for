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
	
	Subroutine Pressure_Pho(j,y,P_pho,flow_type) 	
	use common_module    
      implicit none	
	integer j,flow_type
	double precision P_pho,y,temp1,temp2,temp3,temp4,A,Beta
      if(flow_type == 0)then
		temp1 = g/12d0/sqrt(y*(d(j)-y))						
		temp2 = (d(j)-y)*y*(3d0*d(j)*d(j)-4d0*d(j)*y+4d0*y*y)			
		temp3 = -3d0*d(j)*d(j)*(d(j)-2d0*y)*sqrt(d(j)-y)*sqrt(y)*		
     &    ATAN(sqrt(y)/sqrt(d(j)-y))	                    							
	    P_pho = temp1*(temp2+temp3)
          !Beta = ATAN(S0(j))	!Angle of sewer bottom slope
          !P_pho = P_pho*COS(Beta) !Corrected for inclined slopes
	elseif(flow_type == 1)then		
		A = Aref(j) + g*Aref(j)*(y-yref(j))/(pc1(j)*pc1(j))	
		temp 1 = pc*pc*(A-Aref(j))	
		if (temp 1 < -10.0)then
              temp 1 = -10.0
		endif
		P_pho = P_pho_ref(j) + temp 1 !P_pho_ref(j) is already corrected for the pipe slope
	else
		write(98,*), 'Pressure_Pho. Flow type not defined'
          write(99,*), 'Pressure_Pho. Flow type not defined'
		call endprog; GLOBAL_STATUS_FLAG = 1; return      
	endif
10    continue					        
      End

