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

    Subroutine Init()
    !## Purpose: Read data and set up constants     
    use common_module
    use itm_accessors
    use itm_allocate
    implicit none
    
    integer i,j,k,p,q,Time_hydro
    integer nod
    integer R,S,T,RR    
    integer maxi
    integer num_max_hydr,pipe_max_crown,pump_ID_counter
    
    double precision teta
    double precision P_pho,conver,area,discharge,TH, p1  
    double precision RH,yy,htemp,crown_elev_max
    double precision Ynormal,Ycrit,Yconjugate,dxtemp,s_temp,ScIA    
    double precision temp100,temp101,y_temp
    !character*200  TITLE, Variab
    Integer Node(1000,10)
    
    Integer Reser_dropsh_ID(1000),hydrog_ID(1000)
    Integer Pip(1000),nodID(1000)   
    Integer L1,L2,L3,L4,IdFlow_temp
    double precision Q2,dh1,dh2,A,Ts,dmin,Amin,temcel,Vmin,Qb,Dt
    double precision sum_temp1,sum_temp2,sum_temp3,Stora_new 
    double precision dx1max, dx2max,dx3max,drop_min,tempvar
    !**** Added to common_module, allocated in project.f90
    !Integer Init_depth_type(1000)
    !double precision Init_disch(1000),Init_depth(1000)
    !****************************************************
    Integer R1,R2
    double precision Drop1,Drop2,P_phoIA,h0b1,A0b1,value_reser
    double precision temp1,temp2,temp3,temp4,dz,teta1,teta2
    double precision temp5,temp6,temp7,temp77
    double precision ALF,y_downs,Vol,y_reconR_dry
    double precision y_for_phi_min,Area_reconR_dry
    double precision x(1),x2(1),kkk
    double precision dr, diam_differ
    double precision min_depth,delta_depth,num_cells_drop
    double precision hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R
    double precision dry_diam_fraction_min,dry_diam_fraction_max
    double precision Ratio_dry,Interval_dry,y_dry_iterat,A_dry_iterat
    integer j_1,j_2
    character(IDLEN) temp_id,temp_id2    
    character(IDLEN) temp_gate_id   
    integer sum_drop,sum_drop2
    integer swmmRes
    character*1024 swmmMsg
    double precision AreaNodeminimum(Nnodes)
      NumVolErrPoints = 0
      numitera = 200   
      
  !Remove these variables from ITM Options
  !     Remove maximum time step
  !     Remove Maximum Number of Cells
  !     Remove Maximum number of Iterations for Convergence
  !     Remove Flow type
     
  !   if (itm_check_hotstart_status() == 1) then
      
! Removed by LR      
!      if (len(trim(hsfile_use)) > 0) then
!        write(99,*),'hotstart file'
!        DtMax = Min(DtMax1,Tstor,Tmax) !This is important when using hotstart file
!        return
!      endif
      
    ! Allocate general arrays
    call itm_allocate_general()
    
    
      !Internal Tolerances
      Tol_int_10_1 = 1.d-1
      Tol_int_10_2 = 1.d-2
      Tol_int_10_3 = 1.d-3
      Tol_int_10_4 = 1.d-4
      Tol_int_10_5 = 1.d-5
      Tol_int_10_6 = 1.d-6
      Tol_int_10_7 = 1.d-7
      Tol_int_10_8 = 1.d-8
      Tol_int_10_9 = 1.d-9
      Tol_int_10_10 = 1.d-10
      Tol_int_10_12 = 1.d-12
      Tol_int_10_14 = 1.d-14
      
      !Small tolerance
      Epsilon = 1.d-12
    
    parint1000 = 0  
    init_volume_counter = 0 !for initial volume
      
      !PI
    PI = ATAN(1d0)      !ATAN(1) =  PI/4
    PI = 4d0*PI         !VALUE OF PI    
    NR = Npipes         !Number of pipes in the system
      
    if (water_init_elevation < -99999.51 .or.  &
        water_init_elevation > -99999.49) then
        ini_cond = 1
    else
        ini_cond = 0
    endif

    ! Write the ITM and SWMM ID of the pipes to the debug file
    write(99,*)'_____________________________________________________'
    write(99,*) 'ITM, SWMM IDs of the and pipe type'
    write(99,*) ' ITM_ID, SWMM_ID, pipe type'
    do j = 1, NR
        temp_id = ''
        call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes   
        !write(99,*), 0.0001, pump_index(j)
        if (pump_index(j) > 0)then        
             WRITE(99,'(I4,A10, A15)'), j, trim(temp_id),'  Pump link'
        else
            WRITE(99,'(I4,A10, A15)'), j, trim(temp_id),'  Pipe' 
        endif 
    enddo    
    
    write(99,*)'_____________________________________________________'
    ! Write the ITM and SWMM ID of the nodes to the debug file
    write(99,*) 'ITM and SWMM IDs of the nodes'
    write(99,*) 'ITM_ID, SWMM_ID'
    do R = 1, Nnodes
        temp_id = ''
        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
        WRITE(99,'(I4,A10)'), R, trim(temp_id)
      enddo
      
      write(99,*)'_____________________________________________________'
      write(99,*) 'NODES with INFLOW'
      write(99,*) 'NODE, itm_has_inflow (1 = inflow, 0 = no inflow)'
    do R = 1, Nnodes
        temp_id = ''
        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
        WRITE(99,'(A8, I8)'), trim(temp_id),itm_has_inflow(R)
    enddo    
    
     
      
      ! Write hdrops_overf and  reser_maxdepth of the nodes to the debug file
      write(99,*)'_____________________________________________________'
      write(99,*)'Max. height of dropsh./junct.(hdrops_overf) are not'
      write(99,*)'the same as max. depth of reservoirs (reser_maxdepth)'
      write(99,*)'NODE, hdrops_overf, reser_maxdepth  Ares'
    do R = 1, Nnodes
        temp_id = ''
        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
        WRITE(99,'(A5, 3F9.2)'), trim(temp_id), hdrops_overf(R),  reser_maxdepth(R), Adrop(R)
    enddo
      write(99,*)'_____________________________________________________' 
      


    pc_press = pc

    !Read data file
    !## zb(j,1),zb(j,2) = upst. and downst. invert elevations 
    !## Length(j) = Length of pipes
    !## d(j) = Diameter of pipes at every reach 
    !## n(j) = Manning roughness coefficient (gravity flow. It may be changed 
    !## with f darcy for gravity and pressurized flows) 

!   variables SWMM  varaiables ITM
!   Elev                junct_elev
!   Node1               Node1
!   Node2               Node2
!     Length                Length
!   N                   nm
!   Z1                  zb(i,1)
!   Z                   zb(i,2)
!   Q0                  Init_disch(i)

    !open(UNIT=10,FILE='Reservoirs.csv',STATUS='Unknown')   
    !open(UNIT=15,FILE='Dropshafts.csv',STATUS='Unknown')
    
    !Enter boundary condition and type of dropshaft, No of hydrograph and 
    !specify if the boundary conditions it is open or pressurized. 

    !data Required ends here
    !##########################################################################################    
    system_pressur = 1 !1 System is only allowed to get pressurized
    !2 system is allow to get pressurized and depressurized
    IDpump(:) = 0
    !Read pumpling flow rates

    Nnod(:) = 0 
    inf(:,:) = 0; oufl(:,:) = 0 
    Noufl(:) = 0; Ninf(:) = 0
    NPipes_At_Node_with_Pumps(:) = 0  !For pumps
    !NPumpOufl(:) = 0 
    !NPumpinf(:) = 0
    
    
    Qpump(Nnodes) = 0d0
    Outflow_limited(:) = 0d0
    NodeID(:,:) = 0
    Node(:,:) = 0
    
    !To determine number of inflowing and outflowing pipes 
    ! and to count the number of pipes at each node (ALL PIPES)    
    
    !To determine number of inflowing and outflowing pipes 
    ! and to count the number of pipes at each node (Without PUMP)
    dmin = 10000d0
    do k=1,NR        
        if (pump_index(k) < 1)then !This is for regular links only (no pumps)        
            i = Node1(k) !Upstream node      
            Nnod(i) = Nnod(i)+1
            R = Nnod(i); Node(i,R) = k; Noufl(i) = Noufl(i)+1
            NodeID(i,R) = Node(i,R)
            Nodetype(i,R) = 2 !outflowing            
            R = Noufl(i); 
            !oufl(i,R)=k 
            Klocal(R,1) = EntranceLoss(k) !Entrance losses
        
            
            i = Node2(k) !downstream node
            Nnod(i) = Nnod(i)+1
            R = Nnod(i);Node(i,R) = k; Ninf(i) = Ninf(i)+1
            NodeID(i,R) = Node(i,R)
            Nodetype(i,R) = 1 !inflowing
            R = Ninf(i)
            !inf(i,R)=k 
            Klocal(R,2) = ExitLoss(k) !Exit losses  
           
            dmin = min(d(k),dmin)
        else if (pump_index(k) > 0)then !This is for pumps
            i = Node1(k) !Upstream node      
            NPipes_At_Node_with_Pumps(i) = NPipes_At_Node_with_Pumps(i)+1
            R = NPipes_At_Node_with_Pumps(i); NodePumpID(i,R) = k; 
            !NPumpOufl(i) = NPumpOufl(i)+1
            NodetypePump(i,R) = 2 !outflowing 
            
            i = Node2(k) !downstream node
            NPipes_At_Node_with_Pumps(i) = NPipes_At_Node_with_Pumps(i)+1
            R = NPipes_At_Node_with_Pumps(i); NodePumpID(i,R) = k; 
            !NPumpinf(i) = NPumpinf(i)+1
            NodetypePump(i,R) = 1 !inflowing
        else
            call itm_get_swmm_id(1, R, temp_id) ! 1 for pipes
            write(98,*),'Subr. INIT. Unknown condition for link ',trim(temp_id)
            write(99,*),'Subr. INIT. Unknown condition for link ',trim(temp_id)
            call endprog; GLOBAL_STATUS_FLAG = 1; return 
        endif         
    enddo
    Amin = PI*dmin*dmin/4d0
      
    do R = 1, Nnodes
        NodeNS(R) = Nnod(R) !Number of pipes connected to each node     
    enddo
      
      !To be used in sloped pipes for computing minimum y_dry
      !y_reconR_dry = 10000d0
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      

    !Preprocessing of input data
    !## Grid sizes for every reach
    dxtemp = 50d0
    maxi = 8  !let this as it is
    min_num_grids = max(min_num_grids,8) !Minimum number of grids can not be smaller than 8
    do j=1,NR
       if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)
            dxtemp = min(Length(j)/min_num_grids,dxtemp)
       endif       
    enddo
      
    !min_depth = 1d0/dry_diameter_fraction*dmin 
    min_depth = 1d0/10d0*dmin  !The whole diameter should be reconstructed in 10 cells
    WRITE(99,*),'Pipe ID    Dx(j)'
    Dx(:) = 10000
    do j=1,NR
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)
            delta_depth= dabs(zb(j,1) - zb(j,2)) 
            delta_depth= max(1d-12,delta_depth)
            num_cells_drop =delta_depth/min_depth
          
            Nx(j) = max(Int(Length(j)/dxtemp),Int(num_cells_drop+1))
            Dx(j) = Length(j)/Nx(j) 
            maxi = max(NX(j),maxi) 
          
            temp_id = ''
            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes   
            WRITE(99,'(A5, F6.1)'), trim(temp_id), Dx(j)
        endif        
    enddo
    maxi = maxi + 4 !4(to make second order the numerical scheme)      
    write(99,*),'Minimum Dx(j) = ', MINVAL(Dx(:))
    write(99,*)'_____________________________________________________' 
    call itm_allocate_parameters(maxi)
      !GLOBAL_STATUS_FLAG = 1
      !return
      
      !Gates
      gate_Activation(:) = 0 !0 if operation was not activated, 1 if gate operation was started or completed.       
      t_gate_act_initial(:) = 1.d+22 !This is the initial time at which gate will be operated 
      t_gate_act_final(:) = 1.d+22 !This is the final time at which gate will be operated 
      ClosureTime_Minutes_Gate(:) = 1 !This is the closure/opening rate of the gate in Minutes
           
      !This is for testing gates
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      R = 1
      call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes    
      Depth_check_Node_Gate(:) = temp_id
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !air pockets
    RoRef_air = 1.225 ! (kg/m3) at atmosph. pressure and T = 15 degrees C 
    hap(:,:) = 0d0  
    hapold(:,:) = 0d0
    volap(:,:) = 0d0
    dens(:,:) = RoRef_air

    !constants
    number_steps = 0
    sum_no_converg = 0  
    !k = politropic exponent for air: k = 1.0, 1.4 or somewhere between
    ka = 1.4
    
    CODE_STAR_REGION = 0 !Parameter to compute or not variables at star region   
    
    !Constants for pressurized flow
    RoRef =  1000d0     !kg/m3 (Reference density)  
    pc_air = 340d0
    Cd = 0.65 !air discharge coefficient
    Hb = 10.33 !Atmospheric pressure in meters of water
    
        
    Error_volume = 0d0 
    balance_volume = 0d0 !To balance conservation of volume problems
    Vol_lost_system = 0d0  !Volume lost in the system
    
    !Sloped pipe
    !Slope to be considered sloped pipe
    Min_Slope_for_Sloped_pipe = 0.08 !This could be moved to the GUI    
        
    !write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
    !write(99,*),'Boundary Condition type'
    !write(99,*),'4: Dropshaft with inflow hydrog.' !okay
    !write(99,*),'7: Junction of 2 or more pipes'   !okay
    !write(99,*),'10: Discharge Q constant' !okay
    !write(99,*),'11: Water depth constant' 
    !write(99,*),'20: Reserv. BC' !okay
    !write(99,*),'24: junction of 2 pipes without inflow    
    !write(99,*),'30: Rating curve BC
    !write(99,*),'40: Gate Boundary condition (Two pipes)
    !write(99,*),'41: Gate Boundary condition (one pipe)
 !     write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
    
Drop(:,:) = 0d0  !to be used for computing 
!mimimum water depth in dropshafts
!Computing drop heights at inlets and outlets 
!write(99,*),'Nnodes',Nnodes    
sum_drop2 = 0
        
!Calculating drops at each node
do R =1,Nnodes   
    do j=1,NodeNS(R)
        If(BCnode(R) .ne. 30)then  
            L3 = Node(R,j)
            if(Nodetype(R,j) == 1)then !inflowing
                Drop(R,j) = zb(L3,2)-junct_elev(R)
            elseif(Nodetype(R,j) == 2)then !outflowing      
                Drop(R,j) = zb(L3,1)-junct_elev(R)              
            else 
                write(98,*),'Subroutine Init. Nodetype .ne. 1,2' 
                write(99,*),'Subroutine Init. Nodetype .ne. 1,2' 
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif 
        endif        
    enddo
   
    do j=1,NodeNS(R) !For rating curve
        if (BCnode(R) == 30)then
            Drop(R,1) = weir_invert(R) - junct_elev(R)            
            call itm_get_Q_from_rat_curve(R,abs(Drop(R,1)),QL)	
            call itm_get_Q_from_rat_curve(R,abs(1.05*Drop(R,1)),QR)	
            if (dabs(QL) > 0.001 .or. dabs(QL-QR) < Tol_int_10_14)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Weir height in node ',trim(temp_id), ' is (m): ', abs(Drop(R,1)),' , but'
                write(98,*),'flow of rating curve for this height and below is not zero. Revise rating curve'
                write(99,*),'Weir height in node ',trim(temp_id), ' is (m) ', abs(Drop(R,1)),' , but'
                write(99,*),'flow of rating curve for this height and below is not zero. Revise rating curve'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
    enddo
enddo     
    
    
! Change the name of boundaries for dropshafts, junctions and reservoirs if a pump is connected to the node
do R =1,Nnodes 
    if (BCnode(R) == 4 .or. BCnode(R) == 7 .or. BCnode(R) == 20)then !Only for dropshafts, junctions and reservoirs              
            if (NodeNS(R) == 0)then !If true, all pipes connected to node are PUMPS
                !call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                !write(98,*),'A node in ITM needs to have at least one link that is not a pump link.'
                !write(98,*),'Node ',trim(temp_id), ' has only a pump link'                    
                if (BCnode(R) == 4 .or. BCnode(R) == 7)then
                    BCnode(R) = 4    
                elseif(BCnode(R) == 20)then
                    BCnode(R) = 20  
                else
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(98,*),'Unknown condition for Pump node ',trim(temp_id)
                    write(99,*),'Unknown condition for Pump node ',trim(temp_id)
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                endif
            endif                 
                
            if (BCnode(R) == 7)then
                if (NodeNS(R)>1)then  !NodeNS_No_Pump(R) = Number of regular (no pump) connected to each node
                    BCnode(R) = 7 
                elseif (NodeNS(R)==1)then
                    BCnode(R) = 4 
                else
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(98,*),'A node in ITM needs to have at least one link that is not a pump link (sewer pipe).'
                    write(98,*),'inflow hydrographs. There is an inflow'
                    write(98,*),'Node ',trim(temp_id), 'has only a pump link22'
                    write(99,*),'A node in ITM needs to have at least one link that is not a pump link (sewer pipe).'
                    write(99,*),'inflow hydrographs. There is an inflow'
                    write(99,*),'Node ',trim(temp_id), 'has only a pump link22'                    
                    call endprog; GLOBAL_STATUS_FLAG = 1; return                    
                endif
            endif   
            if (BCnode(R) == 7)then                    
                !To differentiate between a general node and the one that has two pipes with the same diameter and no inflows        
                If(NodeNS(R) == 2 .and. NPipes_At_Node_with_Pumps(R) == 0)then  !If Number of regular pipes (no pump) connected to the node= 2
                    !NPipes_At_Node_with_Pumps(R) is the number of pipes connected at the node including pumps
                    dr = abs(Drop(R,1)-Drop(R,2))
                    j_1 = NodeID(R,1); j_2 = NodeID(R,2)
                    diam_differ = abs(d(j_1)-d(j_2))                   
                    If (itm_has_inflow(R) ==1)then !there is inflow
                        temp_id = ''
                        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                        write(99,*), 'Junction GENERAL, Node = ',temp_id  
                    else  !there is NO inflow
                        If(diam_differ < 0.02*d(j_1) .and. dr < 0.02*d(j_1)) then
                            BCnode(R) = 24  !To this type of boundary (Two pipes with no inflow)
                                !we assign internally the ID of 24.                           
                            temp_id = ''
                            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                            write(99,*), 'TWO PIPES SAME DIAM, Node = ',temp_id
                        else
                            temp_id = ''
                            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                            write(99,*), 'Junction GENERAL, Node = ',temp_id  
                        endif    
                    endif 
                else
                    temp_id = ''
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(99,*), 'Junction GENERAL, Node =',temp_id  
                endif
            endif                 
    endif
enddo
write(99,*)'_____________________________________________________' 
write(99,*),'Pipe ID     Pipe slope'    
    
do R =1,Nnodes
    if (BCnode(R) .ne. 4 .and. BCnode(R) .ne. 7)then !Only for dropshafts and junctions
        if (itm_has_inflow(R) == 1)then
            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
            write(98,*),'Only dropshafts and junctions can have'
            write(98,*),'inflow hydrographs. There is an inflow'
            write(98,*),'hydrograph at node =',trim(temp_id)
            write(99,*),'Only dropshafts and junctions can have'
            write(99,*),'inflow hydrographs. There is an inflow'
            write(99,*),'hydrograph at node =',trim(temp_id)
            call endprog; GLOBAL_STATUS_FLAG = 1; return
        endif 
    endif 
enddo
    
    !Pipe slopes
    do j=1,NR   
        S0(j) = (zb(j,1) - zb(j,2))/Length(j)            
        call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
        WRITE(99,'(A5, F16.8)'),trim(temp_id),S0(j)             
            
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)       
            if (S0(j) < 0d0)then  !Negative slope
                call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                write(99,*),'pipe ID = ',temp_id,'slope = ',S0(j)
                write(99,*),'Pipe slope must be zero or positive' 
                    write(99,*),'Upstream node must have higher elevation'
                write(99,*),'Change direction of flow'                  
                    write(98,*),'pipe ID = ',temp_id,'slope = ',S0(j)
                write(98,*),'Pipe slope must be positive' 
                    write(98,*),'Upstream node must have higher elevation'
                write(98,*),'Change direction of flow'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif 
        endif
    enddo
      
    !Dry bed and free surface and pressurized flow limits
    dry_diam_fraction_min = 500d0 !1000 works very well  
    dry_diam_fraction_max = 50d0
    Ratio_dry = dry_diam_fraction_min/dry_diam_fraction_max
    Interval_dry = Ratio_dry
    temp1 = (1d0/dry_diam_fraction_min)*dmin    
    
do j=1,NR       
    b2_max(j) = (1d0-1.d-6)*d(j)  !maximum water depth for open channel (for searches)
    call Area_from_H(j,b2_max(j),A2_max(j),Ts,RH,0) !maximum area for open channel     
                
    !Full area
    Area_full(j) = PI/4d0*d(j)*d(j)     
          
    !Reference depth
    Yref(j) = yfree_press*d(j)
    if (yref(j) > 0.99*d(j))then
        yref(j) = 0.99*d(j) 
    endif 
          
    teta = 2d0*ACOS(1d0-2d0*Yref(j)/d(j))
    Phiref(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)           
                
    Aref(j) = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
    call Area_from_H(j,Yref(j),A,Ts,RH,0)
    celer_ref(j) = sqrt(g*A/Ts)     
    call Pressure_Pho(j,Yref(j),P_pho,0) !0 for free surface flow
    P_pho_ref(j) = P_pho    
    haver_ref(j) = P_pho/(Aref(j)*g)
          
          
    y_for_phi_max(j) = Yref(j)
    teta = 2d0*ACOS(1d0-2d0*y_for_phi_max(j)/d(j))
    phi_max(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
    
          
    !dry bed 
    !ydry(j) = max(1.d-3, 1d0/200d0*dmin) !Max (1mm, 1/200 dmin)
                
    !y_reconR_dry is added to make it homogeneous with steeper slopes
          
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !y_reconR_dry = y_reconR_dry/2d0  !y_reconR_dry/2d0 works great
       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !ydry(j) = 1d0/dry_diameter_fraction*dmin !Max (1mm, 1/200 dmin)
        !ydry_Cutoff(j) = 1.001*ydry(j)
    !AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)           
            kkk = 0d0
            do while (kkk <=  Interval_dry)
                kkk = kkk+1d0
                y_temp = temp1 + (kkk-1d0)*temp1
                ydry_Cutoff(j) = 1.00001*y_temp
                ydry(j) = 1.005*ydry_Cutoff(j)
                y_dry_iterat = ydry(j)               
              
                call Area_from_H(j,y_dry_iterat,A_dry_iterat,Ts,RH,0)
                A2_min(j) = 0.1*A_dry_iterat
                hL = y_dry_iterat; hR = hL
                AL = A_dry_iterat; AR=AL; QL=0d0; QR=0d0
              
                ydry(j) = y_dry_iterat !Max (1mm, 1/200 dmin)
              
              
                call Area_from_H(j,ydry(j),Adry(j),Ts,RH,0)
                Celer_dry(j) = sqrt(g*Adry(j)/Ts) !Celerity for dry conditions    
              
                teta = 2d0*ACOS(1d0-2d0*ydry(j)/d(j))
                phi_dry(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
              
                call Pressure_Pho(j,ydry(j),P_pho,0) !0 for free surface flow
                P_pho_dry(j) = P_pho
                fluxdry(j) = P_pho
              
                call Riemann_Mixed_HLL_Leon(j,0,0, &
                    0,hL,hR,AL,AR,QL,QR,FF1L,FF1R,FF2L,FF2R,0,Qb)
                A_dry_iterat = A_dry_iterat + DT_GLOBAL/Dx(j)*(FF1R-FF1L)                
                             
                If (ISNAN(A_dry_iterat))then 
                    goto 190
                endif
              
                call H_from_Area(j,A_dry_iterat,ydry(j),1022,0)
              
                if (ydry(j) >=  1.d-6*yref(j))then
                    ydry_Cutoff(j) = 1.00001*ydry(j)
                    kkk = Interval_dry + 1d0
                    goto 192                
                endif
190             continue        
            enddo
          
            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
            write(98,*),'Pipe = ',trim(temp_id)              
            write(98,*),'ydry is too small, ydry = ',ydry(j)
            write(99,*),'Pipe = ',trim(temp_id)              
            write(99,*),'ydry is too small, ydry = ',ydry(j)
            call endprog; GLOBAL_STATUS_FLAG = 1; return 
192         continue
          
            if (ydry(j) < 0d0 .or. ydry(j) > yref(j) .or. ISNAN(A_dry_iterat) ) then
                  write(98,*),'(ydry(j) < 0d0 .or. ydry(j) > d(j)' 
                  write(99,*),'(ydry(j) < 0d0 .or. ydry(j) > d(j)' 
                  call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                  write(99,*),'Pipe (SWMM)',trim(temp_id)              
                  write(99,*),'j,ydry(j)',j,ydry(j)
                  write(99,*),'Interval_dry',Interval_dry
                  call endprog; GLOBAL_STATUS_FLAG = 1; return 
            endif
          
            !Test various values of  dry_diameter_fraction  and values of  ydry_Cutoff.
                           
            b2_min(j) = 1.d-1*ydry(j)
                
            call Area_from_H(j,b2_min(j),A2_min(j),Ts,RH,0)           
            call Pressure_Pho(j,b2_max(j),P_pho_max(j),0) !0 for free surface flow
            call Pressure_Pho(j,b2_min(j),P_pho_min(j),0) !0 for free surface flow
               
            call Area_from_H(j,ydry(j),Adry(j),Ts,RH,0)
            Celer_dry(j) = sqrt(g*Adry(j)/Ts) !Celerity for dry conditions    
            call Area_from_H(j,ydry_Cutoff(j),Adry_Cutoff(j),Ts,RH,0)
            Celer_dry_Cutoff(j) = sqrt(g*Adry_Cutoff(j)/Ts) !Celerity for dry conditions  
          
            !call Phi1(j,ydry(j),phi_dry(j)) !Phi for dry conditions
            teta = 2d0*ACOS(1d0-2d0*ydry(j)/d(j))
            phi_dry(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
            teta = 2d0*ACOS(1d0-2d0*ydry_Cutoff(j)/d(j))
            phi_dry_Cutoff(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
          
            Vmin = sqrt(g*Adry(j)/Ts)
            Qmin(j) = 5.d-1*Adry(j)*Vmin    
                            
            !CFL for dry bed
            ydry_CFL(j) = ydry(j)
            teta = 2d0*ACOS(1d0-2d0*ydry_CFL(j)/d(j))                   
            Adry_CFL(j) = 1d0/8d0*(teta-SIN(teta))*d(j)*d(j)
          
            call Pressure_Pho(j,ydry(j),P_pho,0) !0 for free surface flow
            P_pho_dry(j) = P_pho
            fluxdry(j) = P_pho
          
            call Pressure_Pho(j,ydry_Cutoff(j),P_pho,0) !0 for free surface flow
            P_pho_dry_Cutoff(j) = P_pho
            fluxdry_Cutoff(j) = P_pho
          
            y_for_phi_min = b2_min(j)          
            teta = 2d0*ACOS(1d0-2d0*y_for_phi_min/d(j))
            phi_min(j) = 6.41*SIN(teta/4d0)*sqrt(g*d(j)/8d0)
            !call Phi1(j,y_for_phi_min,phi_min(j)) !Phi_max for iteration
    endif    
enddo 


!Area minimum at a junction
!AreaNodeminimum =  !This is the minimum area specified at the node. This area cannot be too small. 
do R =1,Nnodes  
    AreaNodeminimum(R) = 0d0
    do j=1,NodeNS(R)
        L3 = Node(R,j); A =  0.1*PI*d(L3)*d(L3)/4        
        AreaNodeminimum(R) = max(AreaNodeminimum(R),A)
    enddo
enddo
write(99,*)'_____________________________________________________' 
write(99,*),'NodeID   PipeID   Pipe drop'
!call endprog; GLOBAL_STATUS_FLAG = 1; return
ID_Number_of_zero_drops(:,:) = 2
do R =1,Nnodes
    !write(99,*),max_elev_crown (R),max_crown_pipe(R)
    Ares_junct(R) = Adrop(R)
    height_jun(R) = hdrops_overf(R) 
    crown_elev_max = -1000d0
    drop_min = 10000000d0   
    Number_of_zero_drops(R) = 0
    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
    do j=1,NodeNS(R)
        If(BCnode(R) .ne. 30)then  
            L3 = Node(R,j)
        
            call itm_get_swmm_id(1, L3, temp_id2) ! 1 for pipes        
            WRITE(99,'(A10, A10 F6.1)'),trim(temp_id),trim(temp_id2),Drop(R,j) 
                      
            !Number_of_zero_drops = Number of zero drops 
            if (dabs(Drop(R,j)) < 0.02*d(L3))then
                Number_of_zero_drops(R) =  Number_of_zero_drops(R) +1
                ID_Number_of_zero_drops(R,j) = 1 !1 means that the drop is zero, 2 means that the drop is not flat. 
            endif  
              
            if (Drop(R,j) < 0d0)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Drop height must be always positive except' 
                write(98,*),'for a rating curve boundary'
                write(98,*),'Error in input data. Subr. INIT'               
                write(98,*),'Node = ',trim(temp_id)
                write(99,*),'Drop height must be always positive except' 
                write(99,*),'for a rating curve boundary'
                write(99,*),'Error in input data. Subr. INIT'               
                write(99,*),'Node = ',trim(temp_id)
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
            if (Drop(R,j)+(1d0 + Tol_int_10_8)*Yref(L3) > crown_elev_max) then
                crown_elev_max = Drop(R,j)+(1d0 + Tol_int_10_8)*Yref(L3)
                pipe_max_crown = L3
            endif
            if(drop_min > Drop(R,j))then
                drop_min = Drop(R,j)
                !pipe_min_point = Node(R,j)
                !smin = S0(pipe_min_point)
            endif
        endif        
    enddo
    
    !dropmin(R) = drop_min 
        
    if (NodeNS(R) == 0)then
            dropmin(R) = 0d0 ! For pumps, we are not solving Riemann problem, so the storage starts at the botoom of the sump. 
    else 
            dropmin(R) = drop_min 
    endif        
          
    
    sum_drop = 0
    do j=1,NodeNS(R)
        L3 = Node(R,j)                  
        if (BCnode(R) == 4  .or. BCnode(R) == 7)then    
            if (itm_has_inflow(R) == 1 .and. Adrop(R) < AreaNodeminimum(R))then
                if(sum_drop < 1)then
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes                   
                    write(98,*),'There is an inflow hydrograph at node (SWMM) =', &
                        trim(temp_id), 'but the area at this node is very small or zero.'   
                    write(98,*),'Add an area for the pond at this drophaft'
                    write(98,*),'Minimum area (m^2) should be ', AreaNodeminimum(R)                                       
                    write(99,*),'There is an inflow hydrograph at node (SWMM) =', &
                    trim(temp_id), 'but the area at this node is very small or zero.'   
                    write(99,*),'Add an area for the pond at this drophaft'
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                endif
                sum_drop = sum_drop+1
                sum_drop2 = sum_drop2+1
            endif               
            endif              
        enddo
          
    max_elev_crown (R) = crown_elev_max
    max_crown_pipe(R) = pipe_max_crown  
    !write(99,*),'R,max_elev_crown (R)',R,max_elev_crown (R)
enddo   
write(99,*)'_____________________________________________________' 
write(99,*)'Node  Pipe  NodetypePump' 

do R =1,Nnodes    
    do i = 1,  NPipes_At_Node_with_Pumps(R)    
        j = NodePumpID(R,i) !Pipe ID with pump
        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
        call itm_get_swmm_id(1, j, temp_id2) ! 1 for pipes
        WRITE(98,'(A10, A10, I6)'),trim(temp_id),trim(temp_id2),NodetypePump(R,i)
        WRITE(99,'(A10, A10, I6)'),trim(temp_id),trim(temp_id2),NodetypePump(R,i)
    enddo
enddo

write(99,*)'_____________________________________________________' 
    
      
    do R =1,Nnodes                  
        if (BCnode(R) == 4.or. BCnode(R)==7)then
            if (Ares_junct(R) >= 100000.5)then !(100^2)  If Area is larger use a reservoir boundary
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Area of node ',temp_id
                write(98,*),'is larger than 10^5.'
                write(98,*),'Use a reservoir boundary for this node'  
                write(99,*),'Area of node ',temp_id
                write(99,*),'is larger than 10^5.'
                write(99,*),'Use a reservoir boundary for this node'  
                call endprog; GLOBAL_STATUS_FLAG = 1; return    
            endif              
            
            if (BCnode(R) == 4 .or. BCnode(R) == 7)then
                If (NPipes_At_Node_with_Pumps(R) > 0) then           
                    if (Ares_junct(R) < AreaNodeminimum(R))then                          
                        call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                        write(98,*),'Node is connected to pump link and node area is very small or zero. Node = ',trim(temp_id)
                        write(98,*),'Area of Node = ', Adrop(R)
                        write(98,*),'Minimum area (m^2) should be ', AreaNodeminimum(R)                           
                        write(99,*),'Node is connected to pump link and node area is very small or zero. Node = ',trim(temp_id)
                        write(99,*),'Area of Node = ', Adrop(R) 
                        call endprog; GLOBAL_STATUS_FLAG = 1; return    
                    endif 
                endif
            endif 
            
            if (BCnode(R) == 4 .or. BCnode(R) == 7)then                
                if (Ares_junct(R) < AreaNodeminimum(R))then                          
                    Ares_junct(R) = AreaNodeminimum(R)
                endif 
            endif             
        endif
    enddo      
    
    write(99,*)'_____________________________________________________' 
    write(99,*),'Node,   BCNode,   Number of pipes connected to node'
    !Assigning BCNode = 41 to gates that connects to one pipe only      
    do R =1,Nnodes    
          temp_id = ''
          temp_gate_id = ''
          if (BCnode(R) == 40)then !gate boundary
              If (NodeNS(R) == 1)then
                  BCnode(R) = 41 !Gate bundary one pipe
              endif
            
              
          !   This is to find out to check the gate operation based on threshop depths at nodes
              if(gate_thresholdDepth_TimeSpecified(R) == 1)then
                  do RR =1,Nnodes    
                      if (R .ne. RR)then
                          call itm_get_swmm_id(0, RR, temp_id) ! 0 for nodes    
                            temp_gate_id = Depth_check_Node_Gate(R)
                          if(temp_gate_id == temp_id)then
                              check_Node_Gate_ITM(R) = RR 
                              !write(98,*),' node XXXXXXXXXXXXXXXXXXXXXXX=',trim(temp_id)   
                              !call endprog; GLOBAL_STATUS_FLAG = 1; return
                              goto 198
                          endif
                      endif 
                  enddo  
 198              continue             
              endif 
          endif
          call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes   
          WRITE(99,'(A8, 2I8)'),trim(temp_id),BCnode(R),NodeNS(R)
    enddo
    !call endprog; GLOBAL_STATUS_FLAG = 1; return
    write(99,*)'_____________________________________________________'  
        
    
    do R =1,Nnodes        
            If(BCnode(R) == 30)then !Rating curve
                L3 = Node(R,1)                  
                if(abs(Drop(R,1)) > yref(L3))then
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(98,*),'There is a problem with the crest elevation'
                    write(98,*),'of the rating curve node. The crest elevation '
                   write(98,*),'must be between the invert and the crown of the '
                   write(98,*),'connecting pipe. Error in input data. Subr. INIT' 
                   write(98,*),'Problem in node = ',trim(temp_id)
                   write(99,*),'There is a problem with the crest elevation'
                   write(99,*),'of the rating curve node. The crest elevation '
                   write(99,*),'must be between the invert and the crown of the '
                   write(99,*),'connecting pipe. Error in input data. Subr. INIT' 
                   write(99,*),'Problem in node = ',trim(temp_id)                  
                   call endprog; GLOBAL_STATUS_FLAG = 1; return
                endif   
              elseif (BCnode(R) == 40)then !gate boundary
                if (itm_has_inflow(R) == 1)then 
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(98,*),'A gate boundary cannot have a hydrograph'
                    write(98,*),'Check gate at node ',trim(temp_id)
                    write(99,*),'A gate boundary cannot have a hydrograph'
                    write(99,*),'Check gate at node ',trim(temp_id)
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                elseif (NodeNS(R) .ne. 2) then !Number of pipes connected to each node
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    write(98,*),'A gate boundary must have one or two' 
                    write(98,*),'pipes connected to the boundary' 
                    write(98,*),'Check gate at node ',trim(temp_id)
                    write(99,*),'A gate boundary must have one or two' 
                    write(99,*),'pipes connected to the boundary'                   
                    write(99,*),'Check gate at node ',trim(temp_id)
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                else 
                    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                    temp1 = d(NodeID(R,1))-d(NodeID(R,2))
                    k = NodeID(R,1)
                    if (abs(temp1) > 0.01*d(k))then
                        write(98,*), 'Pipes connected to gate at node', &
                        trim(temp_id)
                        write(98,*), 'must have the same diameter'
                        write(99,*), 'Pipes connected to gate at node', &
                        trim(temp_id)
                        write(99,*), 'must have the same diameter'
                        call endprog; GLOBAL_STATUS_FLAG = 1; return
                    endif
                    
                    if (abs(Drop(R,1)) > 1.d-5*d(k) .or.  &
                        abs(Drop(R,2)) > 1.d-5*d(k))then
                        write(98,*),'The elevations of the inverts of the ', &
                        'pipes that connect a gate boundary'
                        write(98,*),'must be the same as that of the ', &
                        'invert of the gate. This means that'
                        write(98,*),'pipe offsets at the gate must be ', &
                        'zero. Check gate at node ',trim(temp_id)
                 
                        write(99,*),'The elevations of the inverts of ', &
                        'the pipes that connect a gate boundary'
                        write(99,*),'must be the same as that of the ', &
                        'invert of the gate. This means that'
                        write(99,*),'pipe offsets at the gate must be ', &
                        'zero. Check gate at node ',trim(temp_id)
                        call endprog; GLOBAL_STATUS_FLAG = 1; return
                    endif       
                endif
            endif                       
    enddo
    if (sum_drop2 > 0)then !To stop if there are no areas at dropshafts             
        write(98,*),'Add dropshaft areas for nodes listed above' 
        write(98,*),'Nodes are based on SWMM notation'
        write(99,*),'Add dropshaft areas for nodes listed above' 
        write(99,*),'Nodes are based on SWMM notation'
        call endprog; GLOBAL_STATUS_FLAG = 1; return
    endif
    
    
    !slopes 
    sum_drop = 0
    
    do j=1,NR  
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)         
            !To make sure that cell size is not too big
            s_temp = max(S0(j),1.d-12)      
            dx1max = 100d0
            dx2max = Yref(j)/(8d0*s_temp) !To avoid to touch the wall downstream 
            !dx2max = 1.5d0*ydry(j)/s_temp !1.5 to compensate the difference in area at the sides of the symmetric line 
            dx3max = min(dx1max,dx2max)
            if (Dx(j) >= dx3max)then
                sum_drop = sum_drop +1          
                temp_id = ''
                call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                write(98,*),'dx(cell size) in pipe (SWMM) ',trim(temp_id), ' is too large'
                write(98,*),'max. cell size in this pipe should be at most ', dx3max,' m'
                write(98,*),'current cell size in this pipe is ', dx(j),' m'
                write(99,*),'dx(cell size) in pipe (SWMM) ', trim(temp_id), ' is too large'
                write(99,*),'max. cell size in this pipe should be at most ', dx3max,' m'
                write(99,*),'current cell size in this pipe is ', dx(j),' m'                   
            endif
        endif
    enddo
    
    !It is divided by 10 because y_dry has to be much smaller
    !than y_reconR_dry to be able to track wet-dry interfaces 
    
    if (sum_drop > 0)then               
        write(98,*),'Increase the miminum number of grids in the user'
        write(98,*),'interface according with the recommendations'
        write(98,*),'indicated above'
        write(99,*),'Increase the miminum number of grids in the user'
        write(99,*),'interface according with the recommendations'
        write(99,*),'indicated above'
        call endprog; GLOBAL_STATUS_FLAG = 1; return
    endif
    
    !To make sure that boundary junctions have two or more pipes and that dropshafts
    !and reservoirs have only one pipe 
    do R=1,Nnodes
        If(BCnode(R) == 7)then
            if (NodeNS(R) < 2)then !NodeNS(R) = Number of pipes connected to each node  
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Node',trim(temp_id),'can not be a junction boundary'
                write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(98,*),'Node',trim(temp_id),'has less than 2 pipes'
                write(99,*),'Node',trim(temp_id),'can not be a junction boundary'
                write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(99,*),'Node',trim(temp_id),'has less than 2 pipes'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
    
        
       
        
!       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                
        If(BCnode(R) == 4)then !Dropshaft boundary
            if (Nnod(R) > 1)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Node',trim(temp_id),'can not be a dropshaft boundary'
                write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(98,*),'Node',trim(temp_id),'has more than one pipe'
                write(99,*),'Node',trim(temp_id),'can not be a dropshaft boundary'
                write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(99,*),'Node',trim(temp_id),'has more than one pipe'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
        
        
        If(BCnode(R) == 10 .or. BCnode(R) == 11)then !Constant boundary              
            write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
            write(99,*),'CONSTANT boundary at Node ',trim(temp_id)
            write(99,*),'This boundary type enforces the value' 
            write(99,*),'assigned. If you want not to enforce this'
            write(99,*),'value but instead for ITM to calculate the' 
            write(99,*),'fluxes according to the local flow type,'
            write(99,*),'use the dropshaft boundary instead. '
            write(99,*),'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
            if (Nnod(R) > 1)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Node',trim(temp_id),'has more than one pipe connected to BC'
                write(98,*),'Node',trim(temp_id),'can not be a constant boundary'
                write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'  
                write(99,*),'Node',trim(temp_id),'has more than one pipe connected to BC'
                write(99,*),'Node',trim(temp_id),'can not be a constant boundary'
                write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'          
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
        
        If(BCnode(R) == 30)then !Rating curve
            if (Nnod(R) > 1)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Node',trim(temp_id),'has more than one pipe to BC'
                write(98,*),'Node',trim(temp_id),'can not be a rating curve boundary'
                write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(99,*),'Node',trim(temp_id),'has more than one pipe connected to BC'
                write(99,*),'Node',trim(temp_id),'can not be a rating curve boundary'
                write(99,*),'Node',temp_id,'has',Nnod(R),'pipes'                        
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
        
        If(BCnode(R) == 20)then
            !To check that Reservoir BC is connected to only one pipe
            !Later I will add more pipes (Arturo Leon)
            if (Nnod(R) > 1)then
                call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
                write(98,*),'Node',trim(temp_id),'can not be a reservoir boundary'
                write(98,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(98,*),'Node',trim(temp_id),'has more than one pipe'
                write(99,*),'Node',trim(temp_id),'can not be a reservoir boundary'
                write(99,*),'Node',trim(temp_id),'has',Nnod(R),'pipes'
                write(99,*),'Node',trim(temp_id),'has more than one pipe'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif

            !To check that the maximum reservoir depth is not zero
            !call itm_get_max_curve_val_x(R, VALUE)
            call itm_get_max_curve_val_x(R, value_reser)            
            reser_maxdepth(R)  = value_reser
            call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes
            write(99,*),'Node, Max. Depth of reservoir ',temp_id, reser_maxdepth(R)      
            
            if(reser_maxdepth(R) <= 0d0)then
                write(98,*),'Reservoir located at node',R, &
                    'has a maximum reservoir depth equal to zero'
                write(98,*),'Please modify the value of the maximum reservoir depth'
                write(99,*),'Reservoir located at node',R, &
                    'has a maximum reservoir depth equal to zero'
                write(99,*),'Please modify the value of the maximum reservoir depth'
                call endprog; GLOBAL_STATUS_FLAG = 1; return
            endif
        endif
    enddo 
    
        
!Initial water depth for reservoirs and junctions   
do R=1,Nnodes       
    If(BCnode(R) == 20.or.BCnode(R) == 7.or.BCnode(R) == 4 &
           .or.BCnode(R) == 40)then !reservoirs and junctions  
        If (NodeNS(R) == 0)then 
            ydropmin(R) = 0d0
        else
            L3 = NodeID(R,1)
            tempvar = Drop(R,1) + ydry(L3) 
            ydropmin(R) = ydry(L3) !This is the minimum water depth at reservoirs, junctions and dropshafts        
            do j=1,NodeNS(R)
                L3 = NodeID(R,j)
                if (Drop(R,j) + ydry(L3) < tempvar)then 
                    tempvar = Drop(R,j) + ydry(L3)
                    ydropmin(R) = ydry(L3)
                endif
            enddo 
        endif
        !ydropmin(R) = 1.5*ydry(L3) !before it was 3*ydry 
        if (ini_cond == 1)then  !When constant water depth is used as initial condition
            !water_init_elevation = -10d0
            yres_jun_old(R) = max(water_init_elevation - junct_elev(R),flowdepth_res(R))
        else
            yres_jun_old(R) = flowdepth_res(R)  !Later we can add this
            !write (99,*),'flow_depth_reser 1',flowdepth_res(R)
        endif            
        if (yres_jun_old(R) < ydropmin(R))then
            yres_jun_old(R) = ydropmin(R); Outflow_limited(R) = 0
        endif            
    endif           
enddo
!call endprog; GLOBAL_STATUS_FLAG = 1; return 

 !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        !write(99,*), 'STOP HERE5444'   
      !call endprog; GLOBAL_STATUS_FLAG = 1; return 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 


    !Determining the maximum and minimum critical and normal flow at the inflow 
    !that can be conveyed in gravity flow regime
    do j=1,NR 
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)             
            ycrit_max(j) = min(yref(j),0.80*d(j))
            call Area_from_H(j,ycrit_max(j),A,Ts,RH,0)
            Qcrit_maxIA(j) = A*sqrt(g*A/Ts)
            temp1 = 1d0/(2d0*g*A**2d0)*Qcrit_maxIA(j)**2d0
            Ecrit_max(j) = ycrit_max(j) + temp1               
            Qnor_maxIA(j) = 1d0/nm(j)*A*RH**(2d0/3d0)*sqrt(S0(j))
            ycrit_min(j) = Ycrit(j,Qmin(j))
                        
            !Minimum conjugate depths (d1 and d2)               
            call Area_from_H(j,ycrit_min(j),A,Ts,RH,0)
            ScIA = ((Qmin(j)*nm(j))/(A*RH**(2d0/3d0)))**2d0
            if (S0(j) > 0.001*ScIA)then !flow may be supercritical
                d1_min(j) = Ynormal(j,Qmin(j))
                d2_min(j) = Yconjugate(j,d1_min(j),Qmin(j))             
            endif
        endif        
    enddo
    !Increasing number of cells by 4 (second-order)
    do j=1,NR   
            Nx(j) = Nx(j) + 4   
            if (pump_index(j) < 1)then
                if (Nx(j) > nxmax-1) then
                    write(99,*)'INIT - Too many cells in the model'
                    write(99,*)'The number of cells in one or more pipes are exceeding the max. number of cells defined in ITM simulation options'
                    write(98,*)'INIT - Too many cells in the model'
                    write(98,*)'The number of cells in one or more pipes are exceeding the max. number of cells defined in ITM simulation options'
                    call endprog; GLOBAL_STATUS_FLAG = 1; return 
                endif
            endif 
    enddo      
    Num_max_cells = maxval(Nx(:))
    call itm_allocate_cells(Num_max_cells)
      
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
    !Initial conditions
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    do j=1,NR  
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)    
            discharge = Init_disch(j)               
            Q0(j,:) = discharge
            if (Init_depth_type(j) == 1) then !constant
                h0(j,:) = Init_depth(j)             
            elseif (Init_depth_type(j) == 2)then !critical
                !critical depth 
                h0(j,:)  = Ycrit(j,discharge)               
            elseif (Init_depth_type(j) == 3)then  !normal                               
                if (S0(j) < 1.d-7)then
                    call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
                    write(99,*)'Normal depth can not be computed'   
                    write(99,*)'Negative slope was found in pipe',trim(temp_id)
                      write(98,*)'Normal depth can not be computed' 
                    write(98,*)'Negative slope was found in pipe',trim(temp_id)
                    GLOBAL_STATUS_FLAG = 1
                    return
                endif
                h0(j,:) = Ynormal(j,discharge)              
            else                
                write(99,*)'Type of initial flow depth'
                write(99,*)'not recognized. Subroutine INIT'
                  write(98,*)'Type of initial flow depth'
                write(98,*)'not recognized. Subroutine INIT'
                GLOBAL_STATUS_FLAG = 1
                return
            endif  
        endif         
    enddo   
    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    !Initial conditions when initial water depth is constant
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
215 if (ini_cond == 1)then 
        do j=1,NR   
            if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)                   
                discharge = 0d0
                Q0(j,:) = discharge
                R = Node1(j)
                do k=1,NodeNS(R)
                    L3 = NodeID(R,k)
                    if(L3 == j)then
                        do i = 3,Nx(j)-2
                            h0_Rec(j,i) = water_init_elevation - &
                                junct_elev(R) - Drop(R,k) + &
                                (i-25d-1)*s0(j)*dx(j)
                            h0(j,i) = h0_Rec(j,i)
                        enddo
                    endif
                enddo
            endif            
        enddo
    endif
    
    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
    !To determine if the flow is free surface or pressurized    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    do j=1,NR 
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)                        
            dz = abs(S0(j)*dx(j))             
            do i = 3,Nx(j)-2            
                if (h0(j,i) < ydry(j))then
                    h0(j,i) = ydry(j)
                    A0(j,i) = Adry(j)
                    Q0(j,i) = 0d0
                    IdFlow(j,i) = 0 !free surface flow
                  
                        h0Cent(j,i) = h0(j,i)
                        h0Sur(j,i) = 0                  
                elseif (h0(j,i) <= yref(j))then
                    IdFlow(j,i) = 0 !free surface flow                                              
                    if (h0(j,i)-dz/2d0 <= ydry(j))then
                        h0(j,i) = ydry(j)
                        A0(j,i) = Adry(j)
                        Q0(j,i) = 0d0
                    else                    
                        call Area_from_H(j,h0(j,i),area,TH,RH,0)    
                        A0(j,i) = area            
                        endif
                        h0Cent(j,i) = h0(j,i)     
                        h0Sur(j,i) = 0    
                elseif (h0(j,i) > yref(j))then
                    IdFlow(j,i) = 1 !pressurized flow
                        h0Cent(j,i) = h0(j,i)
                        h0Sur(j,i) = 0    
                    !Area is computed below according with pc1
                else
                    write(98,*),'IdFlow(j,i) .ne. 0,1 in Subr. INIT_101'
                    write(99,*),'IdFlow(j,i) .ne. 0,1 in Subr. INIT_101'
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                endif
            enddo   
        endif         
    enddo
    
    !pc_mixed = max(pc_mixed,1.5*maxval(celer_ref(:)))
    !Pressure wave celerity to use
           
      pc1(:) = pc
      temcel = 0.4*minval(Dx(:))/pc
      
    !For defining Ypressurized the pc for mixed flows must be used
    do j=1,NR
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)                   
            y_for_pressur(j) = (1d0 + 0.01)*Yref(j)  !It was 0.001
            Area_for_pressur(j) = Aref(j) + &
                g*Aref(j)*(y_for_pressur(j)-Yref(j))/(pc1(j)*pc1(j)) !this is very important
        endif        
    enddo

    !To determine if the flow is free surface or pressurized
    do j=1,NR     
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)                   
            do i = 3,Nx(j)-2
                if (IdFlow(j,i) == 1)then
                    A0(j,i) = Aref(j)+g*Aref(j)*(h0(j,i)-yref(j))/(pc1(j)*pc1(j))
                endif                       
            enddo   
        endif        
    enddo
    
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
    !Initial volume stored in tunnels, dropshafts and reservoirs
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sum_temp1 = 0d0
    do j = 1,NR
        if (pump_index(j) < 1)then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)                   
            z0(j,2) = zb(j,1) + 5d-1*S0(j)*dx(j)
            z0(j,Nx(j)-1) = zb(j,2) - 5d-1*S0(j)*dx(j)
              do i = 3,Nx(j)-2
                  z0(j,i) = zb(j,1)-(i-25d-1)*S0(j)*dx(j) !elevation of cell (mid-way)      
                  if(IdFlow(j,i) == 0)then
                    Area = A0(j,i)
                    if(Area > Aref(j))then
                        Area = Aref(j)
                    endif
                    sum_temp1 = sum_temp1+Area*dx(j)
                elseif(IdFlow(j,i) == 1)then
                    sum_temp1 = sum_temp1+Area_full(j)*dx(j)
                else
                    write(99,*), 'IdFlow(j,i) .ne. 0,1.Subr. INIT_102' 
                    write(98,*), 'IdFlow(j,i) .ne. 0,1.Subr. INIT_102' 
                    call endprog; GLOBAL_STATUS_FLAG = 1; return
                endif   
                    !Writing initial conditions (Water stage and flow discharge)
                    !write(99,*),'i,h+z(Water stage)', i, h0_Rec(j,i)+z0(j,i)
                    !write(99,*),'i,Q', i, Q0(j,i)
              enddo
        endif        
    enddo   
        
    sum_temp2 = 0d0
    sum_temp3 = 0d0                                         
    Vol_inflows = 0d0     
    write(99,*)'_____________________________________________________' 
    write(99,*),'BC Node and Boundary Condition Name    '
    write(99,*),'Node  BCName    '
     
    do R=1,Nnodes
          temp_id = ''; call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes          
          if (BCnode(R) == 4)then
            WRITE(99,'(A8, A30)'),trim(temp_id),'  4: Dropshaft'
          elseif (BCnode(R) == 7)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  7: Junction general'
          elseif (BCnode(R) == 10)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  10: Disch. Q const'    
          elseif (BCnode(R) == 11)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  11: Water depth const.'
          elseif (BCnode(R) == 20)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  20: Reserv.'        
          elseif (BCnode(R) == 24)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  24: 2 pipes, no infl.'
          elseif (BCnode(R) == 30)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  30: Rating curve'       
          elseif (BCnode(R) == 40)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  40: Gate (Two pipes)'
          elseif (BCnode(R) == 41)then
              WRITE(99,'(A8, A30)'),trim(temp_id),'  41: Gate (one pipe)'
          else
              write(98,*),'Boundary type is unknown or not supported'
              write(99,*),'Boundary type is unknown or not supported'
              call endprog; GLOBAL_STATUS_FLAG = 1; return     
          endif   
          
        !Volume at dropshafts and junctions     
        If(BCnode(R) == 7.or.BCnode(R) == 4)then !junctions (no junction with two pipes of same diameter) and dropsh.
            sum_temp2 = sum_temp2 + Ares_junct(R)*yres_jun_old(R)   
        endif
        !Volume at reservoirs
        If(BCnode(R) == 20)then
            call itm_get_storage(R,yres_jun_old(R),Stora_new)
            sum_temp3 = sum_temp3 + Stora_new   
          endif
        
        If(BCnode(R) == 30)then
            p1 = abs(Drop(R,1)) !Drop height 
            j = NodeID(R,1) !Pipe Id of weir
            call Area_from_H(j,p1,area,TH,RH,0)
            area_weir(R) =  area             
            
            !call itm_get_Q_from_rat_curve(R,p1,Qb)
            !if(abs(Qb) > Qmin(1)) then 
            !    temp_id = ''
            !    call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes           
            !    write(98,*),'There is a rating curve at node  ', trim(temp_id), ','
            !    write(98,*),'which has a weir of depth of p1 =', p1,'m, '
            !    write(98,*),'but, get_Q_from_rat_curve(R,p1,Qb) gives'
            !    write(98,*),'a flow discharge (Qb [m3/s]) > 0 for water depths'
            !    write(98,*),'smaller than p1. Qb [m3/s] =',Qb
            !    write(98,*),'Check rating curve at node',trim(temp_id)
            !    write(99,*),'There is a rating curve at node  ', trim(temp_id), ','
            !    write(99,*),'which has a weir of depth of p1 =', p1,'m, '
            !    write(99,*),'but, get_Q_from_rat_curve(R,p1,Qb) gives'
            !    write(99,*),'a flow discharge (Qb [m3/s])>0 for water depths'
            !    write(99,*),'smaller than p1. Qb [m3/s] =',Qb
            !    write(99,*),'Check rating curve at node',trim(temp_id)
            !    call endprog; GLOBAL_STATUS_FLAG = 1; return  
            !endif
            !!Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded. 
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            temp100 = 10.**14.
            call itm_get_Q_from_rat_curve(R,temp100,Qb)         
            Max_flow_rating_curve(R) = abs(Qb)
            call itm_get_max_rating_head(R,Max_Head_rating_curve(R))
            If (Max_Head_rating_curve(R) < (yref(j)-p1))then
                  temp_id = ''
                  call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes 
                  write(98,*),'No enough data in rat.curve.Node ', trim(temp_id)
                  write(98,*),'Extend rating curve for this node until water level over the weir reaches the pipe crown'
                  write(99,*),'No enough data in rat.curve.Node ', trim(temp_id)                  
                  write(99,*),'Extend rating curve for this node until water level over the weir reaches the pipe crown'
                  call endprog; GLOBAL_STATUS_FLAG = 1; return  
            endif
      endif         
      enddo
      write(99,*)'_____________________________________________________' 
! Code segment below was moved up inside the do loop since R at this
! point would be 1 greater than Nnodes and cause a runtime error
!       If(BCnode(R) == 30)then !Rating curve 
!              j = NodeID(R,1)
!              If (Max_Head_rating_curve(R) <= yref(j))then
!                  temp_id = ''
!                  call itm_get_swmm_id(0, R, temp_id) ! 0 for nodes    
!                  write(98,*),'No enough data in rat.curve.Node',
!     &             trim(temp_id)
!                  write(98,*),'Please extend rating curve'
!                  call endprog; GLOBAL_STATUS_FLAG = 1; return 
!              endif
!      endif
      
      

    Initial_volume_stored = sum_temp1 + sum_temp2 + sum_temp3
    Vol_stored_old_time = Initial_volume_stored
    Vol_entered_system = Initial_volume_stored !Vol_entered_system 
    Vol_entered_system = Initial_volume_stored !Vol_entered_system 
    !temp100 = 0d0  
    !call itm_conser_volume(T_GLOBAL,temp100)
    write(99,1005),'t (s)=',T_GLOBAL,' Initial_volume_stored (M3)=', &
            Vol_entered_system
    write(99,1005),'t (s)=',T_GLOBAL,' Initial volume lost (M3)=', &
            Vol_lost_system
    write(99,*)'_____________________________________________________'  
      !&    ' Vol. stored (M3) =',Volume_stored_current_step,
      !&    ' Vol. outflow (M3) = ',Vol_lost_system,
      !&    ' Error vol (%) = ',Error_volume

      
      !To determine if the boundary is open channel or pressurized      
      do R=1,Nnodes
            call Boundary_Open_Press(R)
            Hgate_open(R) = gate_data(R)%init_opening
      enddo  
      
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'t, reserv. depth2', T_global, yres_jun_old(18)
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      !write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$' 
      write(99,*),'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$1' 
      
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
    !Determination of the initial time step
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
      DtMax = Min(DtMax1,Tstor,Tmax)
 980    format(I2,3f12.6)
 990    format(I2,2f12.6)
 994    format(A20,1000I20)
 995    format(3000f12.6)
 996    format(A20,1000A20) 
1005    FORMAT (A10,F10.2,A20,ES13.4,A20,ES13.4,A20,ES13.4,A20,F5.1) 
1010    format(A10,A10,A50)
1015    format(A50)
      
      return
    end
      
      
    
    
    