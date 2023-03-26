!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_deallocate
!Description:  deallocates memory for arrays declared in common_module
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/20/2023
!******************************************************************************

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
    
    FREE(dropmin)
    FREE(ydropmin)
    FREE(yudrop_n_1)
    FREE(yudrop_n)
    
    FREE(NonPipeFlowToNode)    
    
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
    FREE(S0)
    
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
    FREE(Klocal)
    FREE(nonpipe_flow)
    
    FREE(outflow_limited)
    FREE(V_over)
    FREE(Nodetype)
    FREE(nodeID)
    FREE(NodeNS)
    
end subroutine itm_deallocate_all

end module itm_deallocate
