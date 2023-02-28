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

      subroutine comments (j) 
      !Purpose: To present changes that need to be done  and important comments to keep in mind
      
       !THINGS TO DO   
      !TEST for MULTIPLE CONDITIONS
      !rating Curve node plot seems wrong. can we modify it??
      !gate invert plot also seems wrong. Gate is closed 100% but gate invert in the graphics seems set at another levl. 
      
      !test HOTSTART
        !(1) (Solved) Sometimes, the model run and sometimes don't.
        !     How to clear all variables in Fortran. Use Deallocate.
        !     It seems the problem is solved now. If not, clear some variables.  
        !
        !(3) Rating curve is having flows even though there is no water. Also reservoir
        !    and other boundary conditions          
        !(4) URGENT. Check sign of H in fluxes. Replace also in the routine fluxes. 
        !(5) delta_Head = hR - hL - S0(j)*deltax. This is correct for the same pipe
        !    but not for the boundaries, especially two junction boundary. Check this.

	!Two-phase flows
      !Just to remember

	! 7. use this W. W = QL-QR/(AL-AR). This is the best.
	 
	end
