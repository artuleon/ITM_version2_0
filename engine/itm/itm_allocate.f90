!! This file is part of the ITM model.
!!
!! Copyright 2009 University of Illinois at Urbana-Champaign
!! Copyright 2011 Oregon State University, Corvallis
!!
!! Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
!!
!! ITM is a free software; you can redistribute it and/or modify it
!! under the terms of the GNU General Public License as published
!! by the Free Software Foundation; either version 2.0 of the
!! License, or (at your option) any later version.
!! 
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!! 
!! You should have received a copy of the GNU General Public License
!! along with this program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!
module itm_allocate
use common_module
implicit none
      
contains

subroutine itm_allocate_general()
integer :: NR

    NR = Npipes
    ALLOCATE (nodeID(Nnodes,10))
    ALLOCATE (NodePumpID(Nnodes,10))   
    
    ALLOCATE (NodeNS(Nnodes))
    ALLOCATE (Nodetype(Nnodes,10))
    ALLOCATE (NodetypePump(Nnodes,10))
    
    
    ALLOCATE (fd(Nlinks))
    ALLOCATE (Klocal(Nlinks,2))
    ALLOCATE (Nnod(Nnodes))     
    ALLOCATE (inf(Nnodes,3))
    ALLOCATE (oufl(Nnodes,3))
    ALLOCATE (Ninf(Nnodes))
    ALLOCATE (Noufl(Nnodes)) 
    ALLOCATE (V_over(Nnodes))
    ALLOCATE (outflow_limited(Nnodes))
    ALLOCATE (NPipes_At_Node_with_Pumps(Nnodes))
    
    ALLOCATE (Dx(NR))
    ALLOCATE (NX(NR))
    ALLOCATE (A_cell_dry_sloped(NR))
    ALLOCATE (A_open_sloped_pipe(NR))
    ALLOCATE (b2_max(NR))
    ALLOCATE (A2_max(NR))
    ALLOCATE (b2_min(NR))
    ALLOCATE (A2_min(NR))
    ALLOCATE (P_pho_min(NR))
    ALLOCATE (P_pho_max(NR))
    ALLOCATE (phi_max(NR))
    ALLOCATE (phi_min(NR))
    ALLOCATE (y_for_phi_max(NR))
    ALLOCATE (Qmin(NR))
    ALLOCATE (S0(NR))
    ALLOCATE (Qpump_link(NR)) 
        
    !Pressurization and Depressurization of the system  
    !Pumping rates at selected nodes.
    ALLOCATE (Qpump(Nnodes))
    ALLOCATE (t_begin_pump(Nnodes))
    ALLOCATE (IDpump(Nnodes)) !IDpump (0 No pumping, 1 pumping)
    
    !Boundaries         
    ALLOCATE (Qbound(NR,2))
    ALLOCATE (Fupst(NR,4))
    ALLOCATE (Fdownst(NR,4))

    !dry bed 
    ALLOCATE (ydry(NR))
    ALLOCATE (Adry(NR))
    ALLOCATE (fluxdry(NR))      
    ALLOCATE (Celer_dry(NR))
    ALLOCATE (phi_dry(NR))  
    ALLOCATE (ydry_CFL(NR))
    ALLOCATE (Adry_CFL(NR))
      
    !dry bed Cutoff. !Cutoff is 1/10 of dry regime 
    ALLOCATE (ydry_Cutoff(NR))
    ALLOCATE (Adry_Cutoff(NR))
    ALLOCATE (fluxdry_Cutoff(NR))      
    ALLOCATE (Celer_dry_Cutoff(NR))
    ALLOCATE (phi_dry_Cutoff(NR))
    ALLOCATE (P_pho_dry_Cutoff(NR))
        
    !Junction   
    ALLOCATE (Drop(Nnodes,10))
    ALLOCATE (Ares_junct(Nnodes))
    ALLOCATE (height_jun(Nnodes))
    ALLOCATE (yres_jun_old(Nnodes)) 
    ALLOCATE (max_elev_crown(Nnodes))
    ALLOCATE (max_crown_pipe(Nnodes))
    ALLOCATE (Number_of_zero_drops(Nnodes))
    ALLOCATE (ID_Number_of_zero_drops(Nnodes,10))
    ALLOCATE (PumpFlowToNode(Nnodes))
            
    !Gates
    ALLOCATE (Cd_gate(Nnodes))  
    ALLOCATE (Hgate_open(Nnodes))   
    ALLOCATE (h_gate_m(Nnodes))      
    ALLOCATE (gate_thresholdDepth_TimeSpecified(Nnodes)) !This defines if the Gate will be operated based on a threshold depth or 
    !if the operation is according to a time schedule (1: based on threshold depth, 2: based on time schedule)
    ALLOCATE (gate_Activation(Nnodes)) !!0 if operation was not activated, 1 if gate operation was started or completed.       
    ALLOCATE (t_gate_act_initial(Nnodes)) !This is the initial time at which gate will be operated 
    ALLOCATE (t_gate_act_final(Nnodes)) !This is the final time at which gate will be operated 
    ALLOCATE(character(IDLEN) :: Depth_check_Node_Gate(Nnodes))
    ALLOCATE (check_Node_Gate_ITM(Nnodes)) 
    ALLOCATE (ClosureTime_Minutes_Gate(Nnodes)) !This is the closure/opening rate of the gate 
    
    !Rating curve
    ALLOCATE (Max_flow_rating_curve(Nnodes))
    !Maximum flow specified at the rating curve. This is used to check if the water level in the rating curve is exceeded.       
    ALLOCATE (Max_Head_rating_curve(Nnodes))
    !Maximum Head specified at the rating curve. This is used to check if the water level in the rating curve is exceeded.  
    ALLOCATE (area_weir(Nnodes))
    !cross-section area of weir (bottom to weir crest). 
      
    !Reservoir  
    ALLOCATE (yres_up(50))  
    
    !Dropshafts
    ALLOCATE (yudrop_n(NR+2))
    ALLOCATE (yudrop_n_1(NR+2)) 
    ALLOCATE (ydropmin(Nnodes))
    ALLOCATE (dropmin(Nnodes))
    ALLOCATE (sum_dry_bed_node(Nnodes))
    ALLOCATE(NoConvergence_Junction_GLOBAL(Nnodes))
    
    !Air pockets 
    ALLOCATE (VOLap(NR,50))
    ALLOCATE (hap(NR,50))
    ALLOCATE (hapold(NR,50))
    ALLOCATE (VOLap_old(NR,50))
    ALLOCATE (dens_old(NR,50))
    ALLOCATE (dens(NR,50))
    ALLOCATE (XapMIN(NR,50))
    ALLOCATE (XapMAX(NR,50))
    ALLOCATE (Nap(NR))
    ALLOCATE (XapMINold(NR,50))
    ALLOCATE (XapMAXold(NR,50))
    ALLOCATE (Napold(NR))       

    !Parameters       
    ALLOCATE (AREA_FULL(NR))
    ALLOCATE (Area_for_pressur(NR))
    ALLOCATE (y_for_pressur(NR))
    ALLOCATE (Aref(NR))
    ALLOCATE (Phiref(NR))
    ALLOCATE (Yref(NR))
    ALLOCATE (haver_ref(NR))
    ALLOCATE (celer_ref(NR))
    ALLOCATE (P_pho_ref(NR))
    ALLOCATE (P_pho_dry(NR))    
    ALLOCATE (Qcrit_maxIA(NR))
    ALLOCATE (Qnor_maxIA(NR))
    ALLOCATE (ycrit_min(NR))
    ALLOCATE (ycrit_max(NR))
    ALLOCATE (Ecrit_max(NR))
    ALLOCATE (d1_min(NR))
    ALLOCATE (d2_min(NR))
    ALLOCATE (pc1(NR)) 
    ALLOCATE (fully_pressuri(NR))   

end subroutine itm_allocate_general


subroutine itm_allocate_parameters(maxi)
integer, intent(in) :: maxi
integer :: NR

    NR = Npipes
    ALLOCATE (ha(NR,maxi))
    ALLOCATE (h0(NR,maxi))
    ALLOCATE (IdFlow(NR,maxi))
    ALLOCATE (h0Cent(NR,maxi))
    ALLOCATE (h0Sur(NR,maxi))  
    ALLOCATE (h0_Rec(NR,maxi))
    ALLOCATE (IdFlow_REC_L(maxi))
    ALLOCATE (IdFlow_REC_R(maxi))
    ALLOCATE (A0(NR,maxi))
    ALLOCATE (Q0(NR,maxi))
    ALLOCATE (z0(NR,maxi))
    ALLOCATE (A0L(maxi))
    ALLOCATE (A0R(maxi))
    ALLOCATE (Q0L(maxi))
    ALLOCATE (Q0R(maxi))
    ALLOCATE (h0L(maxi))
    ALLOCATE (h0R(maxi))
 
    !Fluxes
    ALLOCATE (FFL1(NR,maxi))
    ALLOCATE (FFR1(NR,maxi))
    ALLOCATE (FFL2(NR,maxi))
    ALLOCATE (FFR2(NR,maxi))
    
end subroutine itm_allocate_parameters    


subroutine itm_allocate_cells(Num_max_cells)
integer, intent(in) :: Num_max_cells

    ALLOCATE (Atemp1(Num_max_cells))
    ALLOCATE (htemp1(Num_max_cells))
    ALLOCATE (Qtemp1(Num_max_cells))
    ALLOCATE (IdFlow1(Num_max_cells))
    
end subroutine itm_allocate_cells    

end module itm_allocate   
