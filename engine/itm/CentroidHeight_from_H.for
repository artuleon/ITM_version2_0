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
	
	Subroutine CentroidHeight_from_H(j,Y,h0Centroid,h0Surcharge,flow_type)
	use common_module
      implicit none  
      integer j,flow_type
	double precision A,Y,P_pho,h0Centroid,h0Surcharge
      
	If (ISNAN(Y))then 
		write(98,*),'Y is NaN in CentroidHeight_from_A' 		    
          write(99,*),'Y is NaN in CentroidHeight_from_A' 		    
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

5	if(flow_type == 0)then		        
            call Pressure_Pho(j,Y,P_pho,0)                  
            h0Centroid = P_pho/(A*g)
            h0Surcharge = 0d0
	elseif(flow_type == 1)then		    
            h0Centroid = haver_ref(j)
            h0Surcharge = Y - yref(j)
	else
		write(98,*), 'flow_type undef. CentroidHeight_from_A'
          write(98,*), 'flow_type undef. CentroidHeight_from_A'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif	
30	return		
      end
      
	
