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
      
      subroutine H_from_Area(j,A,Y,
     &      code_error,flow_type)
	use common_module
      implicit none  
      integer iter,j,code_error,flow_type
	integer n, iflag
	integer conver_result,info
	double precision fvec(1), x(1)
	double precision A,Y,teta
	double precision FF,DFF,DELY,yrel,tol_H_area
	double precision const1,b,ALF,temp1,temp2,y_elip,v,a2	
	
	tol_H_area = 1.d-12 !tol_higher  
	
	If (ISNAN(A))then 
		    write(98,*),'NaN is found in routine H_from_Area11.' 
              write(99,*),'NaN is found in routine H_from_Area11.' 
		    !write(98,*),'Pipe =',j,'code_error = ',code_error
		    !write(98,*),'A, Area_full(j)',A,Area_full(j)
		    !write(98,*),'flow_type',flow_type	
      !        write(99,*),'NaN is found in routine H_from_Area11.' 
		    !write(99,*),'Pipe =',j,'code_error = ',code_error
		    !write(99,*),'A, Area_full(j)',A,Area_full(j)
		    !write(99,*),'flow_type',flow_type
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

5	if(flow_type == 0)then
		        if(A >= A2_max(j))then
			            A = A2_max(j)
			            y = b2_max(j)
			            goto 30
		        endif	
		        if(A <= A2_min(j))then
			            A = A2_min(j)
			            y = b2_min(j)
			            goto 30
		        endif

				if(A < 0.05*Aref(j))then
					tol_H_area = 1.d-6
				elseif(A < 0.90*Aref(j))then
					tol_H_area = 1.d-10 !tol_higher  
				elseif (A <= Area_full(j))then			
						tol_H_area = 1.d-10 !tol_higher  
				else
					write(98,*),'H_from_Area. ID = 0 but A > A_full'
                      write(99,*),'H_from_Area. ID = 0 but A > A_full'
		            call endprog; GLOBAL_STATUS_FLAG = 1; return
	            endif  
            
				!Circular cross-section
				!Use newton iteration
				!Finding theta that corresponds to A
				teta = (48d0*A/d(j)**2d0)**(1d0/3d0) !Guess obtained using series expansions
				do iter = 1,numitera
					if (teta < 1.d-14)then
						teta = 1.d-14
					endif
					if (teta > (1d0-1.d-12)*2d0*PI)then
						teta = (1d0-1.d-12)*2d0*PI
					endif
			        							
					FF = d(j)**2d0/8d0*(teta-SIN(teta))-A		
					DFF = d(j)**2d0/8d0*(1d0-COS(teta))		
					DELY = FF/DFF							
					temp1 = teta - DELY/2d0 
					teta = teta - DELY					
					IF(ABS(DELY)<tol_H_area*temp1)goto 20 			
					if(iter == numitera)then			
						write(98,*),'H_from_Area_22. Theta error'
                          write(99,*),'H_from_Area_22. Theta error'
		 !                      write(98,*),'flow_type,d(j)',
		 !&                        flow_type,d(j)
		 !                         write(98,*),'A, ArefIj), Code_error',
		 !&                        A,Aref(j),code_error
		 !                         write(99,*),'Error finding theta. 	
		 !&                        Subroutine H_from_Area_22'                              
                          call endprog; GLOBAL_STATUS_FLAG = 1; return
                      endif													
 				enddo
20                Y = d(j)/2d0*(1d0-COS(teta/2d0))
	elseif(flow_type == 1)then
		    Y = yref(j) + pc1(j)*pc1(j)/(g*Aref(j))*(A - Aref(j)) 	
	else
		write(98,*), 'flow_type undef. H_from_Area', code_error
          write(99,*), 'flow_type undef. H_from_Area', code_error
		call endprog; GLOBAL_STATUS_FLAG = 1; return	
	endif	
30	return		
      end
      
	
