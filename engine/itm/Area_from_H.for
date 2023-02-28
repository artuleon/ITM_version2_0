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

      subroutine Area_from_H(j,y,area,TH,RH,flow_type)	
	!This routine computes the hydraulic area from the piezometric head 
	use common_module
      implicit none		
      integer iter,j,code_error,flow_type
	double precision area,y,teta,TH,RH,Atemp	
	double precision const1,b,ALF,temp1,temp2,y_elip,v,a2
      If (ISNAN(y))then 
		write(98,*),'NaN is found in routine Area_from_H 22'
          write(99,*),'NaN is found in routine Area_from_H'
		!write(98,*),'y',y,'pipe No',j,'flow_type',flow_type
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

	if(flow_type == 0)then	
		teta = 2d0*ACOS(1d0-2d0*y/d(j))		
		area = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
		TH = d(j)*sin(teta/2d0)			
		RH = d(j)/4d0*(1d0-sin(teta)/teta)   
   !       if (y < (yref(j) + d(j))/2d0)then
   !           teta = 2d0*ACOS(1d0-2d0*y/d(j))		
			!area = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
			!TH = d(j)*sin(teta/2d0)			
			!RH = d(j)/4d0*(1d0-sin(teta)/teta)              
   !       else 
   !           write(98,*), 'Flow is open channel'
   !           write(98,*), 'y is larger than the pipe diameter'
		 !   write(98,*),'Subroutine Area_from_H' 
   !           write(99,*), 'Flow is open channel'
   !           write(99,*), 'y is larger than the pipe diameter'
		 !   write(99,*),'Subroutine Area_from_H' 
		 !   call Endprog
   !       endif 
	elseif(flow_type == 1)then
		area = Aref(j) + g*Aref(j)*(y-yref(j))/(pc1(j)*pc1(j))
		TH = g*Aref(j)/(pc1(j)*pc1(j)) !T = dA/dy for pressurized flows
		teta = 2d0*ACOS(1d0-2d0*yref(j)/d(j))
		RH = d(j)/4d0*(1d0-sin(teta)/teta)              
		temp1 = g*Aref(j)*(y-yref(j)) !See Pressure_Pho routine to understand this
		if (temp1 < -10.0)then
              temp1 = -10.0
		endif              
	    area = Aref(j) + temp1/(pc1(j)*pc1(j))
	else
		write(98,*),'Subr. Area_from_H. Flow type unknown',flow_type
          write(99,*),'Subr. Area_from_H. Flow type unknown',flow_type
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
	return		
      end
