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
module itm_deallocate
use common_module
implicit none

#define FREE(x) if (allocated(x)) deallocate(x)
      
contains      
      
subroutine itm_deallocate_all()
    
    FREE(IdFlow1)
    FREE(Qtemp1)
    FREE(htemp1)
    FREE(Atemp1)
    
    FREE(FFR2)
    FREE(FFL2)
    FREE(FFR1)
    FREE(FFL1)
    
    FREE(h0R)
    FREE(h0L)
    FREE(Q0R)
    FREE(Q0L)
    FREE(A0R)
    FREE(A0L)
    FREE(z0)
    FREE(Q0)
    FREE(A0)
    FREE(IdFlow_REC_R)
    FREE(IdFlow_REC_L)
    FREE(h0_Rec)
    FREE(h0Sur)
    FREE(h0Cent)
    FREE(IdFlow)
    FREE(h0)
    FREE(ha)
    
    FREE(fully_pressuri)
    FREE(pc1)
    FREE(d2_min)
    FREE(d1_min)
    FREE(Ecrit_max)
    FREE(ycrit_max)
    FREE(ycrit_min)
    FREE(Qnor_maxIA)
    FREE(Qcrit_maxIA)
    FREE(P_pho_dry)
    FREE(P_pho_ref)
    FREE(celer_ref)
    FREE(haver_ref)
    FREE(Yref)
    FREE(Phiref)
    FREE(Aref)
    FREE(y_for_pressur)
    FREE(Area_for_pressur)
    FREE(AREA_FULL)
    
    FREE(Napold)
    FREE(XapMAXold)
    FREE(XapMINold)
    FREE(Nap)
    FREE(XapMAX)
    FREE(XapMIN)
    FREE(dens)
    FREE(dens_old)
    FREE(VOLap_old)
    FREE(hapold)
    FREE(hap)
    FREE(VOLap)
    
    FREE(NoConvergence_Junction_GLOBAL)
    FREE(sum_dry_bed_node)
    FREE(dropmin)
    FREE(ydropmin)
    FREE(yudrop_n_1)
    FREE(yudrop_n)
    
    FREE(yres_up)
    FREE(area_weir)
    FREE(Max_Head_rating_curve)
    FREE(Max_flow_rating_curve)
    FREE(ClosureTime_Minutes_Gate)
    FREE(check_Node_Gate_ITM)
    FREE(Depth_check_Node_Gate)
    FREE(t_gate_act_final)
    FREE(t_gate_act_initial)
    FREE(gate_Activation)
    FREE(gate_thresholdDepth_TimeSpecified)
    FREE(h_gate_m)
    FREE(Hgate_open)
    FREE(Cd_gate)
    
    FREE(ID_Number_of_zero_drops)
    FREE(PumpFlowToNode)    
    
    
    FREE(Number_of_zero_drops)
    FREE(max_crown_pipe)
    FREE(max_elev_crown)
    FREE(yres_jun_old)
    FREE(height_jun)
    FREE(Ares_junct)
    FREE(Drop)
    
    FREE(P_pho_dry_Cutoff)
    FREE(phi_dry_Cutoff)
    FREE(Celer_dry_Cutoff)
    FREE(fluxdry_Cutoff)
    FREE(Adry_Cutoff)
    FREE(ydry_Cutoff)
    FREE(Adry_CFL)
    FREE(ydry_CFL)
    FREE(phi_dry)
    FREE(Celer_dry)
    FREE(fluxdry)
    FREE(Adry)
    FREE(ydry)
    FREE(Fdownst)
    FREE(Fupst)
    FREE(Qbound)
    
    FREE(IDpump)
    FREE(t_begin_pump)
    FREE(Qpump)
    
    FREE(S0)
    FREE(Qpump_link)
    
    
    FREE(Qmin)
    FREE(y_for_phi_max)
    FREE(phi_min)
    FREE(phi_max)
    FREE(P_pho_max)
    FREE(P_pho_min)
    FREE(A2_min)
    FREE(b2_min)
    FREE(A2_max)
    FREE(b2_max)
    FREE(A_open_sloped_pipe)
    FREE(A_cell_dry_sloped)
    
    FREE(NX)
    FREE(Dx)
    FREE(outflow_limited)
    FREE(NPipes_At_Node_with_Pumps)
    
    FREE(V_over)
    FREE(Noufl)
    FREE(Ninf)
    FREE(oufl)
    FREE(inf)
    FREE(Nnod)
    FREE(Klocal)
    FREE(fd)
    FREE(Nodetype)
    FREE(NodetypePump)    
    FREE(NodeNS)
    FREE(nodeID)
    FREE(NodePumpID)
    
    
end subroutine itm_deallocate_all

end module itm_deallocate
