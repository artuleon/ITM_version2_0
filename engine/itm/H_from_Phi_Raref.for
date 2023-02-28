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

      subroutine H_from_Phi_Raref(j,phi,y,A,c)
	use common_module
      implicit none
      integer j
	double precision y,A,c
      double precision teta,phi,deriv_phi
	double precision FF,DFF,DELY,temp0,temp1,temp2
	
	If (ISNAN(phi))then 
	      write(99,*),'phi is NaN, H_from_Phi_Raref'
	      write(98,*),'phi is NaN, H_from_Phi_Raref'
	      call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
      temp0 = 6.41*sqrt(g*d(j)/8d0)
       If (phi > temp0 .or. phi <= 0d0)then 
	      write(99,*),'phi is too big or < 0,phi,maxphi',phi,temp0
	      write(98,*),'phi is too big or < 0,phi,maxphi',phi,temp0
	      call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      
	teta = 4d0*ASIN(phi/temp0)
      y = d(j)/2d0*(1d0-COS(teta/2d0))
      A = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
      temp1 = g*d(j)*(teta-SIN(teta))
      temp2 = 8d0*SIN(teta/2d0)
      c = sqrt(temp1/temp2)
	
	return		
      end
