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

      subroutine interf_speed(j,AL,AR,QL,QR,CODL,Wpred)	      
	!This routine computes the interface speed of a mixed flow interface. 
	use common_module
	implicit none
	integer j,CODL
	double precision AL,AR,QL,QR,Wpred,wtemp3,wtemp4,u_max
	double precision P_phoL,P_phoR,uL,uR,hL,hR,TL,TR,RH
	!Sign and magnitude of an open channel-press.flow interface	
      u_max = 9.d-1*pc_mixed
	wtemp3 = (QR-QL)/(AR-AL)
	Wpred = wtemp3
	
      If(abs(Wpred) > u_max)then
            Wpred = u_max*SIGN(1d0,wtemp3)
      endif
      continue
      end subroutine
