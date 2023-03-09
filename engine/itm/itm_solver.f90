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

!==============================================================================
! This module implements the ITM solver over a single time step. 
!==============================================================================

module itm_solver
use common_module
use itm_accessors
use itm_pump
implicit none

private

public :: itm_solver_exec_step

contains

subroutine itm_solver_exec_step()
!==============================================================================
! Run the ITM solver over a single time step. 
!==============================================================================
    
    !0 Variables at star region will NOT be computed,1 will be computed 
    !It is not necessary to use CODE_STAR_REGION = 1 here because all
    !required variables at boundary are computed and retrieved below
    CODE_STAR_REGION = 0
    call get_pump_flows()
    call boundariesROE(T_GLOBAL, DT_GLOBAL)
    
    !Variables at star region ARE computed
    CODE_STAR_REGION = 1
    call get_link_flows()
    if (GLOBAL_STATUS_FLAG .eq. 1) return
    call check_conser_volume()
    
end subroutine itm_solver_exec_step


subroutine get_pump_flows
!====================================================================
! Find flow through all pump links.
!====================================================================
integer :: i, j, k, R
double precision depth_after_pumping, Storage_new, Stora_old
character(IDLEN) :: temp_id

    PumpFlowToNode = (/ (0d0, k = 1,Nnodes) /)
    Qpump_link = (/ (0d0, k = 1,Npipes) /)
    do j = 1, Npipes 
         if (pump_index(j) > 0) then 
             call pump_set_speed(j, T_GLOBAL)
             Qpump_link(j) = pump_find_flow(j)
             k = Node1(j) !Upstream node 
             PumpFlowToNode(k) = PumpFlowToNode(k) - Qpump_link(j)
             k = Node2(j) !downstream node
             PumpFlowToNode(k) = PumpFlowToNode(k) + Qpump_link(j)
         endif         
    end do
    
    !To ckeck that there is no excessive pumping from a sump node that may result in negative water depths    
    do R = 1, Nnodes
        if (NPipes_At_Node_with_Pumps(R) > 0)then
            If(BCnode(R) == 20)then
                call itm_get_storage(R,yres_jun_old(R),Stora_old) 
                Storage_new = Stora_old  + PumpFlowToNode(R)*DT_GLOBAL
                call itm_get_storage_depth(R,Storage_new, depth_after_pumping) 
            else
                depth_after_pumping =  yres_jun_old(R) + PumpFlowToNode(R)*DT_GLOBAL/Ares_junct(R)
            endif
            
            if (depth_after_pumping <= 0.30)then !If the sump node after pumping drops below 1 ft (0.30 m), set the pump flow equal to zero                         
               if (abs(PumpFlowToNode(R)) > 0.001)then
                    do i = 1,  NPipes_At_Node_with_Pumps(R)                  
                        if (NodetypePump(R,i) == 2)then !outflowing for pump
                            j = NodePumpID(R,i); Qpump_link(j) = 0d0
                        endif 
                    enddo 
               endif
            endif
        endif
    end do      
    
    PumpFlowToNode = (/ (0d0, k = 1,Nnodes) /)
    do j = 1, Npipes 
         if (pump_index(j) > 0) then   !Update pumping flows         
            k = Node1(j) !Upstream node 
            PumpFlowToNode(k) = PumpFlowToNode(k) - Qpump_link(j)
            k = Node2(j) !downstream node
            PumpFlowToNode(k) = PumpFlowToNode(k) + Qpump_link(j)
        endif            
    end do
    
end subroutine get_pump_flows


subroutine get_link_flows()
!====================================================================
! Find flow through all non-pump links.
!====================================================================
integer :: j

    do j = 1, Npipes            
        if (pump_index(j) < 1) then !When the link is not a pump
            call get_fluxes(j)
            call update_variables(j)
        endif
    end do
    
end subroutine get_link_flows    


subroutine get_fluxes(j)
!==============================================================================
! Compute fluxes for all cells within a specific pipe. 
!==============================================================================
integer, intent(in) :: j 
integer :: i, SumIDAROUND
real(8) :: AL, AR, hL, hR, QL, QR, Qb, FF1L, FF1R, FF2L, FF2R

    do i = 4, Nx(j) - 2                  
        sumIDAROUND = IdFlow(j, i-1) + IdFlow(j, i)               
        hL = h0(j,i-1)
        hR = h0(j,i)
        AL = A0(j,i-1)
        AR = A0(j,i)
        QL = Q0(j,i-1)
        QR = Q0(j,i)  
              
        call Riemann_Mixed_HLL_Leon(j, sumIDAROUND, IdFlow(j,i-1), &
            IdFlow(j,i), hL, hR, AL, AR, QL, QR, FF1L, FF1R,       &
            FF2L, FF2R, 0, Qb)
            
        !computation of fluxes               
        FFL1(j,i) = FF1L
        FFR1(j,i) = FF1R
        FFL2(j,i) = FF2L
        FFR2(j,i) = FF2R
    end do
                 
    !Fluxes at boundaries 
    FFR1(j,3) = Fupst(j,1)
    FFR2(j,3) = Fupst(j,2)
    FFL1(j,Nx(j)-1) = Fdownst(j,1) 
    FFL2(j,Nx(j)-1) = Fdownst(j,2)
end subroutine get_fluxes        

        
subroutine update_variables(j)
!==============================================================================
! Update flow and depth within each cell of a specific pipe at the end of the
! time step. 
!==============================================================================
integer, intent(in) :: j
integer :: i,myunit,myunit2
real(8) :: utemp, area, Ts, RH
character(IDLEN) :: temp_id

    do i = 3, Nx(j) - 2              
        Atemp1(i)= A0(j,i) + DT_GLOBAL/Dx(j) * (FFR1(j,i) - FFL1(j,i+1))

        If (ISNAN(Atemp1(i)))then
            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
            write(98,*),'Atemp1 is NaN, itm_solver, Pipe = ',trim(temp_id)
            write(99,*),'Atemp1 is NaN, itm_solver, Pipe = ',trim(temp_id)
            write(98,*),'i,Nx(j)-2',i,Nx(j)-2
            write(98,*),'A0(j,i)',A0(j,i)
            write(98,*),'FFR1(j,i)',FFR1(j,i)
            write(98,*),'FFL1(j,i+1)',FFL1(j,i+1)
            write(98,*),'Atemp1(i)',Atemp1(i)            
            call endprog; GLOBAL_STATUS_FLAG = 1; return
        endif
                  
        if (IdFlow(j,i) == 0) then 
            if (Atemp1(i) <= 1.0001*Adry(j)) then     
                Atemp1(i) = Adry(j)
                htemp1(i) = ydry(j)
                Qtemp1(i) = 0d0
                IdFlow1(i) = 0
                cycle 
            endif                             
                      
            if (Atemp1(i) < Aref(j)) then
                IdFlow1(i) = 0
            else
                IdFlow1(i) = 1
            endif
            
        else if(IdFlow(j,i) == 1) then 
            if (Atemp1(i) >= Aref(j)) then
                IdFlow1(i) = 1
            else
                IdFlow1(i) = IdFlow(j,i-1)*IdFlow(j,i+1)
            endif
        else  
            write(98,*),'itm_solver, IdFlow .ne. 0,1'
            write(99,*),'itm_solver, IdFlow .ne. 0,1'
            call endprog; GLOBAL_STATUS_FLAG = 1; return                     
        endif                  
                  
        call H_from_Area(j, Atemp1(i), htemp1(i), 125, IdFlow1(i))
        call Area_from_H(j, htemp1(i), area, Ts, RH, IdFlow1(i))
        Qtemp1(i) = Q0(j,i) + DT_GLOBAL / Dx(j) * (FFR2(j,i) - FFL2(j,i+1))
        utemp = dabs(Qtemp1(i) / Atemp1(i))
        
        If (ISNAN(utemp)) then   
            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
            write(99,*),'NaN is found in utemp. itm_solver. Pipe No',j,'cell=',i
            write(98,*),'NaN is found in utemp. itm_solver. Pipe No',j,'cell=',i
            utemp = 0d0;  Qtemp1(i) = 0d0
            call endprog; GLOBAL_STATUS_FLAG = 1; return  
        endif
        
        
        if (IdFlow1(i) == 0) then
            if (htemp1(i) < 0.05*yref(j) .or.   htemp1(i) < 0.03) then 
                if (utemp > 2d0) then
                    utemp = 2d0 * SIGN(1d0, Qtemp1(i))
                    Qtemp1(i) = utemp * Atemp1(i)
                endif 
            endif
        endif
            
        
        if (utemp > 20d0) then
            utemp = 20d0 * SIGN(1d0, Qtemp1(i))
            Qtemp1(i) = utemp * Atemp1(i)
        endif
                  
        If (ISNAN(htemp1(i)) .or. ISNAN(Qtemp1(i))) then   
            call itm_get_swmm_id(1, j, temp_id) ! 1 for pipes
            write(99,*),'NaN is found in itm_solver. Pipe No',j,'cell=',i
            write(98,*),'NaN is found in itm_solver. Pipe No',j,'cell=',i
            write(98,*),'Pipe ',trim(temp_id)   
            write(98,*),'Nx-2 cell =',Nx(j)-2 
            write(98,*),'htemp1(i))',htemp1(i)
            write(98,*),'Qtemp1(i)',Qtemp1(i)
            write(98,*),'Time = :',T_GLOBAL
            call endprog; GLOBAL_STATUS_FLAG = 1; return
        endif                                    
    end do
              
    !update flow type and incorporate source term to the solution       
    !call Source_term(j,DT_GLOBAL)      
    ! open file
    !open (100009, file='C:\Users\artul\OneDrive\Desktop\ITM_Riemann_FV_paper\ASCETemplateFINAL_ITM_PAPER_FINAL\ITM_INPUT_FILES\case3_H.txt', status='old')
    !open (100008, file='C:\Users\artul\OneDrive\Desktop\ITM_Riemann_FV_paper\ASCETemplateFINAL_ITM_PAPER_FINAL\ITM_INPUT_FILES\case3_V.txt', status='old')
    
    
    
    !open(newunit=myunit,access='sequential',file='C:\Users\artul\OneDrive\Desktop\ITM_Riemann_FV_paper\ASCETemplateFINAL_ITM_PAPER_FINAL\ITM_INPUT_FILES\case3_H.txt',position='append',status='old',action='write')
    !open(newunit=myunit2,access='sequential',file='C:\Users\artul\OneDrive\Desktop\ITM_Riemann_FV_paper\ASCETemplateFINAL_ITM_PAPER_FINAL\ITM_INPUT_FILES\case3_V.txt',position='append',status='old',action='write')
    do i = 3, Nx(j) - 2                        
        A0(j,i) = Atemp1(i)
        Q0(j,i) = Qtemp1(i)
        h0(j,i) = htemp1(i)
        IDFlow(j,i) = IdFlow1(i)  
        !Update h0Cent(j,i) and h0Sur(j,i)              
        call CentroidHeight_from_H(j, h0(j,i), h0Cent(j,i), h0Sur(j,i), IDFlow(j,i))
        
        !write(98,*),j,'Nx(j) - 2 ',Nx(j) - 2  
        
        !if (j == 2 .and. i == 49)then
        !    ! write to file
        !    write(myunit, *), T_GLOBAL,h0(j,i)                   
        !endif 
        !
        !
        !if (j == 2 .and. i == 21)then
        !    ! write to file
        !    utemp = Q0(j,i)/A0(j,i) 
        !    write(myunit2, *), T_GLOBAL,utemp            
        !endif 
    end do
    ! close file
    !close(myunit)
    !close(myunit2)
    
    !call endprog; GLOBAL_STATUS_FLAG = 1; return 
end subroutine update_variables        

         
subroutine check_conser_volume()
!==============================================================================
! Check conservation of volume at the end of a time step. 
!==============================================================================

    call itm_conser_volume(T_GLOBAL, DT_GLOBAL)
    if ((Istor == 1) .or. T_GLOBAL + DT_GLOBAL >= TMAX) then   
        WRITE(99,'(A22, 2F12.3, I10)'),'T, DT, No_convergence #', T_GLOBAL, DT_GLOBAL, sum_no_converg
 
        !Check cons. of volume
        write(99,1004),'t(s)=', T_GLOBAL,                            &
            ' Inflow total (M3)=', Vol_inflows,                      &
            ' Vol. stored at T (M3) =', Volume_stored_current_step,  &
            ' Outflow total (M3) = ', Vol_lost_system,               &
            ' Error vol (%) = ', Error_volume    
        write(99,1005),'t(s)=', T_GLOBAL,                            &
            ' Dropsh. at Delta T (M3)=', vol_dropshafts_time_step,   &
            ' Junctions at Delta T (M3) =', vol_junctions_time_step, &
            ' Reservoirs at Delta T (M3) = ', vol_reservoirs_time_step, &
            ' Pipes at Delta T (M3) = ', vol_pipes_time_step
        write(99,1006),'t(s)=', T_GLOBAL,                            &
            ' inflows at Delta T (M3)=', vol_inflows_time_step,      &
            ' delt_storage_N+1-N(M3) =', Volume_stored_current_step  &
                                         - Vol_stored_old_time,      &
            ' outflow at Delta T (M3) =', vol_lost_time_step,        &
            ' vol_balance at Delta T (M3) =', balance_volume_time_step
        
1004  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F8.2)
1005  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F16.2)
1006  FORMAT (A10,F10.2,A30,F16.2,A30,F16.2,A30,F16.2,A30,F16.2)
     
        if (ISNAN(Error_volume)) then 
            write(98,*), 'Error_volume is NaN. Subr. itm_solver'
            write(99,*), 'Error_volume is NaN. Subr. itm_solver'
            call endprog; GLOBAL_STATUS_FLAG = 1; return
        else if(abs(Error_volume) > 10d0) then            
            write(99,*), 'Warning : Error in the volume exceeds 10 %'   
        endif
    end if
    
end subroutine check_conser_volume

!------------------------------------------------------------------------------

end module itm_solver
