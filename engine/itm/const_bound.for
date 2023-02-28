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

      subroutine const_bound(R)
      use common_module    
      implicit none
      integer Typ,iter,j,R,sum,n,i
	double precision t,AA,RH,Ts,R1,F11,F12,Val,Wpred
      double precision y11,A11,Q11,P_pho11
	double precision ub,Qb,Yb,Ab,cb,Tb,P_phob,Ycrit
	double precision k1,x(10),fvec(10),TOL_CONST_STATE
	double precision hL,hR,AL,AR,QL,QR,AW0,QW0,cb1
	double precision Pw0,tol_local
      double precision FF11,FFb
      double precision  zL,zR,FF1,FF2,FF3 
      
	external const_disch,const_depth
	integer conver_result,info,Id1,Idb,Idb_old,IDF0,CODL
      integer sumID
      
	!This routine computes the flux when using a constant boundary (flow discharge or piezometric head)
	j = NodeID(R,1)	
	tol_const_state = Tol_int_10_4
	typ = BCnode(R)
	Val = const_depth_flow(R)
	
	if (Nodetype(R,1) == 2)then !outflowing
	        i = 3
	elseif (Nodetype(R,1) == 1)then !inflowing
              i = Nx(j)-2
	else
		    write(98,*),'Nodetype(R,1) .ne. 1,2. Const_bound'
              write(99,*),'Nodetype(R,1) .ne. 1,2. Const_bound'
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
      y11 = h0(j,i); A11 = A0(j,i); Q11 = Q0(j,i)
      Id1 = IdFlow(j,i)  	
	call Pressure_Pho(j,y11,P_pho11,Id1)      
      
	if (Typ.eq.11)then !Prescribed water level
		    !Riemann invariants Method
		    yb = Val              
              Idb = 0
		    If (yb > yref(j))then 
			    Idb = 1
              endif              
              sum = Id1+Idb !Sum of IDFlows (boundary and adjacent cell)
              
		    If (Idb==0)then
		       call Area_from_H(j,yb,Ab,Tb,RH,Idb)
		        cb1 = sqrt(g*Ab/Tb)	
	        else
		        cb1 = pc1(j)
              endif 
              
              call Pressure_Pho(j,y11,P_pho11,Id1)	
              FF11 = Q11*Q11/A11 + P_pho11              
              call Pressure_Pho(j,yb,P_phob,Idb)
              !FFb = Qb*Qb/Ab + P_phob
              
		    param1 = A11; param3 = Q11
		    param4 = Ab; param5 = cb1
              param6 = FF11 
              param7 = P_phob
	        parint1 = j;parint4 = sum;parint5 = R
              parint6 = Idb
		    n = 1
		    x(1:n) = (/Q11/)
              tol_local = Tol_int_10_8
		    call hybrd1 (const_depth,n,x,fvec,tol_local,info)
		    call converg (conver_result, info)
		    If (conver_result == 1)then !if there is no convergence
                  x(1) = Q11  
              endif
		    Qb = x(1)		    
		    goto 30
      elseif (Typ.eq.10) then !Prescribed discharge  
              call Pressure_Pho(j,y11,P_pho11,Id1)
              FF11 = Q11*Q11/A11 + P_pho11 
              
		    parint1000 = 0		   
		    param1 = A11;  param3 = Q11
		    param4 = Val
              param6 = FF11 
              
	        parint1 = j;parint2 = Id1 
		    parint5 = R
              !!parint3 = Idb; parint4 = sum; 
		    n = 1
		    x(1:n) = (/y11/)
              tol_local = Tol_int_10_6
              
		    call hybrd1 (const_disch,n,x,fvec,tol_local,info)
		    call converg (conver_result, info)	
		    
		    !If (parint1000 == 1 .or. conver_result == 1)goto 20 (This line doesn't work fine)	   
		    If(conver_result == 1)then	
			    parint1000 = 0
			    write(99,*),'No converg. const_bound. const_disch'
                  write(98,*),'No converg. const_bound. const_disch'
			    yb = y11
		    else
			    yb = x(1)
              endif                     
			
			If (Id1 == 1)then 
				Idb = 1
			else !Id1 = 0
				Idb = 0
				If (yb > yref(j))then 
					yb = yref(j)
				endif
			endif               
              
              call Area_from_H(j,yb,Ab,Ts,RH,Idb)              
			Qb = Val 
  	else
		    write(98,*),'Const_bound. BC not supp. (downst.) Pipe= ',j
              write(99,*),'Const_bound. BC not supp. (downst.) Pipe= ',j
		    call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif

30    if (Nodetype(R,1) == 2)then !outflowing
	    i = 2
          A0(j,i) = Ab; h0(j,i) = yb
		Q0(j,i) = Qb; IdFlow(j,i) = Idb
          sumID = IdFlow(j,2) + IdFlow(j,3) 
	elseif (Nodetype(R,1) == 1)then !inflowing
		i = Nx(j)-1  !Revise this everywhere 
		A0(j,i) = Ab; h0(j,i) = yb
		Q0(j,i) = Qb; IdFlow(j,i) = Idb  
          sumID = IdFlow(j,Nx(j)-2) + IdFlow(j,Nx(j)-1) 
	else
		write(98,*),'Subr. const_bound. Nodetype .ne. 1,2'
          write(99,*),'Subr. const_bound. Nodetype .ne. 1,2'
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif      
      
	parint1000 = 0
	call Pressure_Pho(j,yb,P_phob,Idb)
	FF1 = Qb; FF2 = Qb*Qb/Ab + P_phob  
	
	!Flux computation
	if (Nodetype(R,1) == 2)then !outflowing
		    Qbound(j,1) = Qb		   
		    Fupst(j,1) = FF1; Fupst(j,2) = FF2
		    yres_jun_old(R) = yb
      Elseif (Nodetype(R,1) == 1)then !inflowing
              i= Nx(j)-2
		    Qbound(j,2) = Qb		    
		    Fdownst(j,1) = FF1;	Fdownst(j,2) = FF2
		    yres_jun_old(R) = yb
	Endif
      return	
      end
      
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine const_disch(n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n,iflag,j,Idb,Id1,sum,R
	double precision fvec(n),x(n),Qb,Qb_Val,ub1,yb,temp8
	double precision Ab,Ts,RH,Q11,y11,A11,cb1
      double precision FF11, Area,FFb,P_phob
      double precision uHat,cHat,AHat,hHat
      double precision Beta,w,randomnumber 
	parint1000 = 0
	A11 = param1; Q11 = param3; Qb_Val = param4   
      FF11 = param6 
	j = parint1; Id1 =parint2; R = parint5
      
      !Boundaries can get depressurized only if boundary is open and or 
      !Adjacent cell is open channel 
      !open_closed_bound(R) == 0 !Boundary open to atmosphere
      !open_closed_bound(R) == 1 !Boundary NOT open to atmosphere
      
      yb = x(1)       
      
      If (ISNAN(yb))then 
		    write(98,*),'yb is NaN in const_boundAAA, yb = ', yb
              write(99,*),'yb is NaN in const_boundAAA, yb = ', yb
              CALL RANDOM_NUMBER(randomnumber) 
              yb = ydry(j) + randomnumber*(yref(j)-ydry(j))
              x(1) = yb
		    !call endprog; GLOBAL_STATUS_FLAG = 1; return
	endif
      
      If (Id1 == 1)then 
		Idb = 1
	elseif (Id1 == 0)then 
		Idb = 0
		If (yb > yref(j))then 
                yb = yref(j)				
		endif 
	else 
          write(98,*),' Const_disch7. Id1 .ne. 0,1'
          write(99,*),' Const_disch7. Id1 .ne. 0,1'
          call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif                      
          
      sum = Id1+Idb !Sum of IDFlows (boundary and adjacent cell)
      If (Idb == 0)then
		If (yb < ydry(j))then	
			x(1) = ydry(j); yb = ydry(j); Ab = Adry(j); Qb = 0d0
			goto 32
			!parint1000 = 1 
          endif
      endif 
     
      call Area_from_H(j,yb,Ab,Ts,RH,Idb)      
      If (Idb == 0)then
          Qb = Qb_Val
      elseif(Idb == 1)then
          Qb = Qb_Val*Ab/Aref(j) !Modified for pressurized flow
      else
          write(98,*),'Const_disch3. Idb .ne. 0,1'
          write(99,*),'Const_disch3. Idb .ne. 0,1'          
		call endprog; GLOBAL_STATUS_FLAG = 1; return
      endif
 32   ub1 = Qb/Ab
      Beta = ATAN(S0(j))	!Angle of sewer bottom slope
      
    !This only works if flow in both cells are of the same type. Do this everywhere  
      if (sum == 0 .or. sum == 2)then 
          AHat = (A11 + Ab)/2d0 
          uHat = (Qb + Q11)/(Ab + A11)
          
          If (ISNAN(AHat))then 
		    write(98,*),'AHat is NaN in const_bound, AHat = ', AHat
              write(99,*),'AHat is NaN in const_bound, AHat = ', AHat
              call endprog; GLOBAL_STATUS_FLAG = 1; return              
	    endif 
		call H_from_Area(j,AHat,hHat,112,Idb)           
		call Area_from_H(j,hHat,area,Ts,RH,Idb)	
          if (Idb == 0)then
              cHat = sqrt(g*AHat/Ts*COS(Beta)) 
              
              call Area_from_H(j,yb,area,Ts,RH,Idb)
              cb1 = sqrt(g*Ab/Ts*COS(Beta)) 
          else 
              cHat = pc
              cb1 = pc
          endif
      endif 
	
      if (Nodetype(R,1) == 2)then !outflowing
		!i = 3
		if (sum == 0 .or. sum == 2) then
			If (uHat < cHat) then  !subcritical  
					fvec(1) = Q11 - Qb - (uHat + cHat)*(A11 - Ab)	
			elseif (uHat >= cHat) then !critical flow (Fr =1)  %This is wrong
				!fvec(1) = ub1-cb1
				fvec(1) =  Qb/Ab - cb1
			else  
				write(98,*),'Const_depth. Unknow relation betwee uHat and cHat1'
				write(99,*),'Const_depth. Unknow relation betwee uHat and cHat1'
				call endprog; GLOBAL_STATUS_FLAG = 1; return		
			endif
		else
			write(98,*),'Const_depth., Nodetype == 2, sum n.e. 0,1,2'
              write(99,*),'Const_depth., Nodetype == 2, sum n.e. 0,1,2'
			call endprog; GLOBAL_STATUS_FLAG = 1; return	
		endif
      elseif (Nodetype(R,1) == 1)then !inflowing
              !i = Nx -2  
              if (sum == 0 .or. sum == 2) then
				If (uHat < cHat) then  !subcritical
					fvec(1) = Q11 - Qb - (uHat - cHat)*(A11 - Ab)	
					!fvec(1) =  FFb - FF11 - (uHat - cHat)*(Qb-Q11)
                            
				elseif (uHat >= cHat) then !critical flow (Fr =1)
					!fvec(1) = ub1-cb1
					fvec(1) =  Qb/Ab - cb1
				else  
					write(98,*),'Const_depth. Unknown relation betwee uHat and cHat2'
					write(99,*),'Const_depth. Unknown relation betwee uHat and cHat2'
					call endprog; GLOBAL_STATUS_FLAG = 1; return		
				endif              
              else
			    write(98,*),'Const_depth, Nodetype = 1, sumn.e.0,1,2'
                  write(99,*),'Const_depth, Nodetype = 1, sumn.e.0,1,2'
			    call endprog; GLOBAL_STATUS_FLAG = 1; return	
              endif
	endif
	return
      end	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	subroutine const_depth( n, x, fvec, iflag )
	use common_module
	implicit none	
	integer n,iflag,j,sum,R,Idb
	double precision fvec(n),x(n),yb,Qb,ub1
	double precision Ab,Q11,A11,cb1
      double precision FF11,FFb,FFb2
      double precision uHat,cHat,AHat,hHat
      double precision area,Ts,RH,Beta,w               
      
	parint1000 = 0
	A11 = param1; Q11 = param3; Ab = param4
	cb1 = param5; FF11 = param6; FFb2 = param7    
      
	j = parint1;sum = parint4; R = parint5
      Idb = parint6      
	Qb = x(1)          
      if (Ab <= Adry(j))then
          Ab = Adry(j)
      endif 
      Beta = ATAN(S0(j))	!Angle of sewer bottom slope	
      
      !This only works if flow in both cells are of the same type. Do this everywhere  
      if (sum == 0 .or. sum == 2)then 
          AHat = (A11 + Ab)/2d0 
          uHat = (Qb + Q11)/(Ab + A11)
		call H_from_Area(j,AHat,hHat,113,Idb) 
		call Area_from_H(j,hHat,area,Ts,RH,Idb)
          if (Idb == 0)then              
              cHat = sqrt(g*AHat/Ts*COS(Beta)) 
              call Area_from_H(j,yb,area,Ts,RH,Idb)
              cb1 = sqrt(g*Ab/Ts*COS(Beta)) 
          else 
              cHat = pc
              cb1 = pc
          endif
      endif 
       FFb = Qb*Qb/Ab + FFb2  
      
      if (Nodetype(R,1) == 2)then !outflowing
              !i = 3
              if (sum == 0 .or. sum == 2) then
				If (uHat < cHat) then  !subcritical                                
					fvec(1) = Q11 - Qb - (uHat + cHat)*(A11 - Ab)	
				elseif (uHat >= cHat) then !critical flow (Fr =1)
					!fvec(1) = ub1-cb1
					fvec(1) =  Qb/Ab - cb1
				else  
					write(98,*),'Const_depth, unknow relation between uHat and cHat1'
					write(99,*),'Const_depth, unknow relation between uHat and cHat1'
					call endprog; GLOBAL_STATUS_FLAG = 1; return		
				endif
              else if (sum == 1) then
				w = (Qb - Q11)/(Ab - A11)              
				fvec(1) =  FFb - FF11 - w*(Qb-Q11)
              else
				write(98,*),'Const_depth, Nodetype = 2, sumn.e.0,1,2'
                  write(99,*),'Const_depth, Nodetype = 2, sumn.e.0,1,2'
				call endprog; GLOBAL_STATUS_FLAG = 1; return	
              endif
      elseif (Nodetype(R,1) == 1)then !inflowing
              !i = Nx -2  
          if (sum == 0 .or. sum == 2) then
			If (uHat < cHat) then  !subcritical
				fvec(1) = Q11 - Qb - (uHat - cHat)*(A11 - Ab)	
			elseif (uHat >= cHat) then !critical flow (Fr =1)
				!fvec(1) = ub1-cb1
				fvec(1) =  Qb/Ab - cb1
			else  
				write(98,*),'Const_depth. unknown relat. uHat & cHat2'
                  write(99,*),'Const_depth. unknown relat. uHat & cHat2'
				call endprog; GLOBAL_STATUS_FLAG = 1; return		
			endif
          else if (sum == 1) then
              w = (Qb - Q11)/(Ab - A11)              
              fvec(1) =  FFb - FF11 - w*(Qb-Q11)
          else
			write(98,*),'Const_depth, Nodetype = 1, sum n.e. 0,1,2'
              write(99,*),'Const_depth, Nodetype = 1, sum n.e. 0,1,2'			
			call endprog; GLOBAL_STATUS_FLAG = 1; return	
          endif
	endif
	return
	end	
	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&