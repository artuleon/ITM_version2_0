!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_allocate
!Description:  allocates memory for arrays declared in common_module
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/01/2023
!******************************************************************************

module itm_allocate
use common_module
implicit none
      
contains

subroutine itm_allocate_general()
integer :: NR

    NR = Npipes
    ALLOCATE (NodeNS(Nnodes))
    ALLOCATE (nodeID(Nnodes,10))
    ALLOCATE (Nodetype(Nnodes,10))
    ALLOCATE (V_over(Nnodes))
    ALLOCATE (outflow_limited(Nnodes))
    
    ALLOCATE (nonpipe_flow(Nlinks-Npipes))
    
    ALLOCATE (Klocal(NR,2))
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
    ALLOCATE (NonPipeFlowToNode(Nnodes))
            
    !Dropshafts
    ALLOCATE (yudrop_n(NR+2))
    ALLOCATE (yudrop_n_1(NR+2)) 
    ALLOCATE (ydropmin(Nnodes))
    ALLOCATE (dropmin(Nnodes))
    
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
