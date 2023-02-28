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

MODULE COMMON_MODULE
use itm_table
implicit none
    
  ! Node & link types
  integer, parameter :: IDLEN = 40
  integer, parameter :: JUNCTION = 1
  integer, parameter :: BOUNDARY = 2
  integer, parameter :: GATE     = 3
  integer, parameter :: WEIR     = 4
  integer, parameter :: STORAGE  = 5
  integer, parameter :: PIPE     = 1
  integer, parameter :: PUMP     = 2
  
  ! Boundary condition types
  integer, parameter :: DROPSHAFT    = 4  !Dropshaft with inflow hydrog.
  integer, parameter :: JUNC2        = 7  !Junction of 2 or more pipes
  integer, parameter :: FLOW_BOUND   = 10 !Constant flow boundary
  integer, parameter :: DEPTH_BOUND  = 11 !Constant depth boundary
  integer, parameter :: RESERVOIR    = 20 !Reservoir boundary
  integer, parameter :: JUNC_NOFLOW  = 24 !Junction with no inflow
  integer, parameter :: RATE_CURVE   = 30 !Rating curve boundary
  integer, parameter :: GATE2        = 40 !Gate boundary - 2 pipes
  integer, parameter :: GATE1        = 41 !Gate boundary - 1 pipe
  
  ! Derived types
  type gate_t
    double precision :: init_opening
    double precision :: target_opening
    double precision :: opening_rate
    integer          :: control_tseries
    integer          :: control_node
    integer          :: control_curve
  end type gate_t

  type inflow_t
    integer          :: tseries
    double precision :: baseline
    double precision :: scale_factor
  end type inflow_t
  
  type pump_t
    double precision :: loss_coeff
    double precision :: friction_factor
    double precision :: init_setting
    double precision :: setting
    double precision :: max_head
    double precision :: max_flow
    integer          :: pump_curve
    integer          :: control_tseries
    integer          :: control_node
    integer          :: control_curve
  end type pump_t
      

!--------------------------------------------------------------------
! Number of objects
  integer :: Nnodes          ! number of nodes
  integer :: Nlinks          ! number of links
  integer :: Npipes          ! number of pipes
  integer :: Npumps          ! number of pumps
  integer :: Ncurves         ! number of data curves
  integer :: Ntseries        ! number of time series

! Node input variables
  character(IDLEN), ALLOCATABLE :: node_id(:)           ! node ID name
  integer,          ALLOCATABLE :: node_type(:)         ! node type code
  double precision, ALLOCATABLE :: junct_elev(:)        ! invert elevation
  double precision, ALLOCATABLE :: Adrop(:)             ! dropshaft area
  double precision, ALLOCATABLE :: hdrops_overf(:)	    ! dropshaft depth
  double precision, ALLOCATABLE :: const_depth_flow(:)  ! const. boundary depth/flow
  integer,          ALLOCATABLE :: open_closed_bound(:) ! open/closed to atmosphere
  integer,          ALLOCATABLE :: BCnode(:)            ! boundary condition type
  double precision, ALLOCATABLE :: flowdepth_res(:)	    ! storage node initial depth
  double precision, ALLOCATABLE :: reser_maxdepth(:)    ! storage node max. depth
  double precision, ALLOCATABLE :: Reser_outflow(:)     ! storage node outflow rate
  double precision, ALLOCATABLE :: PumpFlowToNode(:)    ! Sum of pump flows at nodes
  double precision, ALLOCATABLE :: weir_invert(:)       ! Weir invert elevation
  
  integer,          ALLOCATABLE :: node_curve(:)        ! curve used by node
  type(inflow_t),   ALLOCATABLE :: inflow(:)            ! lateral inflow
  type(gate_t),     ALLOCATABLE :: gate_data(:)         ! gate controls
  
  
! Link input variables  
  character(IDLEN), ALLOCATABLE :: link_id(:)           ! link ID name
  integer,          ALLOCATABLE :: link_type(:)         ! link type code
  integer,          ALLOCATABLE :: Node1(:)             ! upstream node index
  integer,          ALLOCATABLE :: Node2(:)             ! downstream node index
  double precision, ALLOCATABLE :: d(:)                 ! pipe diameter
  double precision, ALLOCATABLE :: Length(:)            ! pipe length
  double precision, ALLOCATABLE :: nm(:)                ! Manning roughness coeff.
  double precision, ALLOCATABLE :: EntranceLoss(:)      ! pipe entrance loss coeff.
  double precision, ALLOCATABLE :: ExitLoss(:)          ! pipe exit loss coeff.
  integer,          ALLOCATABLE :: Init_depth_type(:)   ! type of initial condition
  double precision, ALLOCATABLE :: Init_depth(:)        ! initial flow depth
  double precision, ALLOCATABLE :: Init_disch(:)        ! initial flow rate
  integer,          ALLOCATABLE :: pump_index(:)        ! index into Pump array
  type(pump_t),     ALLOCATABLE :: pump_data(:)         ! pump data
  double precision, ALLOCATABLE :: Qpump_link(:)        ! Flow discharges at pump links 
  
! Tabular input variables
  type(table_t),    ALLOCATABLE :: curve(:)             ! x-y curve data
  type(table_t),    ALLOCATABLE :: tseries(:)           ! time series data
  
! Analysis options inputs
  integer          :: NxMax
  integer          :: numitera
  integer          :: min_num_grids
  integer          :: MaxNumPlotCells
  integer          :: type_of_flow
  double precision :: pc
  double precision :: pcm
  double precision :: yfree_press
  double precision :: Tmax
  double precision :: Dtmax1
  double precision :: Tstor
  double precision :: T_START_REPORT
  double precision :: T_NEXT_REPORT
  double precision :: water_init_elevation          !For initial constant water level
  double precision :: tol
  double precision :: tol_lower
  double precision :: tol_very_low
  double precision :: tol_higher
  double precision :: tol_crit
  character(128)   :: project_title
  character(256)   :: hsfile_use
  character(256)   :: hsfile_save
 
!--------------------------------------------------------------------

! General parameters						
  double precision, ALLOCATABLE :: S0(:)         !sewer bottom slope	
  double precision, ALLOCATABLE :: z0(:,:)       !elevations of cells (mid point)	
  double precision, ALLOCATABLE :: zb(:,:)       !elevations 	
  integer,          ALLOCATABLE :: IdFlow(:,:)
  double precision, ALLOCATABLE :: Dx(:)

! Init
  integer, ALLOCATABLE :: inf(:,:)
  integer, ALLOCATABLE :: oufl(:,:) 
  integer, ALLOCATABLE :: Ninf(:)
  integer, ALLOCATABLE :: Noufl(:)
  integer, ALLOCATABLE :: Number_of_zero_drops(:)
  integer, ALLOCATABLE :: ID_Number_of_zero_drops(:,:)
      
  double precision, ALLOCATABLE :: Qcrit_maxIA(:)
  double precision, ALLOCATABLE :: Qnor_maxIA(:)
  double precision, ALLOCATABLE :: ycrit_min(:)
  double precision, ALLOCATABLE :: ycrit_max(:)
  double precision, ALLOCATABLE :: Ecrit_max(:)
  
  double precision, ALLOCATABLE :: d1_min(:)
  double precision, ALLOCATABLE :: d2_min(:)
      
! Boundaries		
  integer,          ALLOCATABLE :: Nnod(:)
  integer,          ALLOCATABLE :: line_elem(:,:)
  
  
! For hydrographs and reserv. 		
  double precision, ALLOCATABLE :: Qbound(:,:) !to store flow 

! Variables from the boundaries	
  integer,          ALLOCATABLE :: sum_dry_bed_node(:)
	
! Roughness
  double precision, ALLOCATABLE :: fd(:)         ! Darcy friction coefficient

! Parameters Star region 
  integer CODE_STAR_REGION  !Parameter to compute or not variables at star region
! CODE_STAR_REGION = 1. Variables at star region are computed. Otherwise, no. 

! Parameters open channel/pressurized flow
  double precision, ALLOCATABLE :: h0(:,:)       !flow depth
  double precision, ALLOCATABLE :: h0Cent(:,:)   !Centroid height to free surface 
  double precision, ALLOCATABLE :: h0Sur(:,:)    !Surcharge depth
  double precision, ALLOCATABLE :: h0_Rec(:,:)   !Water depth for reconstruction	

  double precision, ALLOCATABLE :: A0(:,:)
  double precision, ALLOCATABLE :: Q0(:,:)
  double precision, ALLOCATABLE :: AREA_FULL(:)
  double precision, ALLOCATABLE :: Area_for_pressur(:)
  double precision, ALLOCATABLE :: y_for_pressur(:)

  double precision, ALLOCATABLE :: Aref(:)
  double precision, ALLOCATABLE :: Phiref(:)
  double precision, ALLOCATABLE :: Yref(:)
  double precision, ALLOCATABLE :: haver_ref(:)
  double precision, ALLOCATABLE :: celer_ref(:) 
  double precision, ALLOCATABLE :: P_pho_ref(:) 
  double precision, ALLOCATABLE :: P_pho_dry(:) 

  double precision, ALLOCATABLE :: pc1(:)	
  integer,          ALLOCATABLE :: fully_pressuri(:)
! pref	= Reference pressure
! RoRef	= Reference density
! pc    = pressurized flow wave speed
  double precision RoRef
  double precision PI
  double precision pc_air
  double precision pc_press
  double precision pc_mixed

  double precision g                             !@ITM@IGNORE
  parameter (g = 9.8)                            !gravity (m/s2) !@ITM@IGNORE
  double precision Kloss                         !@ITM@IGNORE
  parameter (Kloss = 0.1)                        !@ITM@IGNORE !head loss coefficient k*u*abs(u)/(2g)
  
	
! values at adjacent cells and at boundaries at old time step	
! NodeNS(r) = Number of pipes connected to the node (1, 2, 3 ....)
! NodeID(r,j) = pipe ID(1, 2, 3, ...) 
! Nodetype = inflowing or outflowing
! maximum 10 pipes can be connected to each node
  integer, ALLOCATABLE :: NodeNS(:)
  integer, ALLOCATABLE :: NodeID(:,:)	
  integer, ALLOCATABLE :: Nodetype(:,:)
  
  
  !For pumps
    integer, ALLOCATABLE :: NodePumpID(:,:)	
    integer, ALLOCATABLE :: NodetypePump(:,:)
	
! Parameters air pocket
! air pressure head expressed in equivalent water head
  double precision, ALLOCATABLE :: ha(:,:) 

! X minimum and maximum cell IDS for air pockets
  integer, ALLOCATABLE :: XapMIN(:,:)
  integer, ALLOCATABLE :: XapMAX(:,:) !new
  integer, ALLOCATABLE :: XapMINold(:,:)
  integer, ALLOCATABLE :: XapMAXold(:,:) !old

! Number of air pockets
  integer, ALLOCATABLE :: Nap(:)
  integer, ALLOCATABLE :: Napold(:)

! Volume of air pocket
  double precision, ALLOCATABLE :: VOLap(:,:)
  double precision, ALLOCATABLE :: VOLap_old(:,:) 

! Air pressure in water column 
  double precision, ALLOCATABLE :: hap(:,:)
  double precision, ALLOCATABLE :: hapold(:,:)

! Politropic exponent for air
  double precision ka                            ! ka = 1.0, 1.4 or somewhere between
  double precision Cd                            ! Cd = air discharge coefficient

! Air density
  double precision, ALLOCATABLE :: dens_old(:,:)
  double precision, ALLOCATABLE :: dens(:,:)

! Various	
  double precision, ALLOCATABLE :: Atemp1(:)
  double precision, ALLOCATABLE :: htemp1(:)
  double precision, ALLOCATABLE :: Qtemp1(:)
  integer,          ALLOCATABLE :: IdFlow1(:)	
  integer,          ALLOCATABLE :: IdFlow_REC_L(:) !Flow types for reconstruction (LEFT)
  integer,          ALLOCATABLE :: IdFlow_REC_R(:) !Flow types for reconstruction (RIGHT)	
  double precision, ALLOCATABLE :: A0L(:)
  double precision, ALLOCATABLE :: A0R(:)
  double precision, ALLOCATABLE :: Q0L(:)
  double precision, ALLOCATABLE :: Q0R(:) 
  integer                       :: pressurized
  integer,          ALLOCATABLE :: NoConvergence_Junction_GLOBAL(:)

!## Upstream IDs

!## Junction
  integer,          ALLOCATABLE :: Junct(:,:)
  integer,          ALLOCATABLE :: max_crown_pipe(:)	
  integer,          ALLOCATABLE :: NPipes_At_Node_with_Pumps(:)	
  
  
  double precision, ALLOCATABLE :: Drop(:,:)
  double precision, ALLOCATABLE :: Ares_junct(:)
  double precision, ALLOCATABLE :: yres_jun_old(:)	
  double precision, ALLOCATABLE :: height_jun(:)
  double precision, ALLOCATABLE :: max_elev_crown(:)   
  
		
!## Reservoir 	
  double precision, ALLOCATABLE :: yres_up(:)
  double precision, ALLOCATABLE :: Outflow_limited(:) !allowed outflow

!## Dropshaft
  double precision, ALLOCATABLE :: V_over(:)

!(len=1024)

  double precision, ALLOCATABLE :: yudrop_n(:)
  double precision, ALLOCATABLE :: yudrop_n_1(:)
  double precision, ALLOCATABLE :: ydropmin(:)
  double precision, ALLOCATABLE :: dropmin(:)
			
!## Variables at interfaces (i+1/2)	
  double precision, ALLOCATABLE :: Fupst(:,:)
  double precision, ALLOCATABLE :: Fdownst(:,:)      
  double precision, ALLOCATABLE :: Pres_pho_Bound(:,:)		
  double precision, ALLOCATABLE :: FFL1(:,:)     !Fluxes Left first variable
  double precision, ALLOCATABLE :: FFL2(:,:)     !Fluxes Left first variable
  double precision, ALLOCATABLE :: FFR1(:,:)     !Fluxes Right second variable
  double precision, ALLOCATABLE :: FFR2(:,:)     !Fluxes Right second variable
	
! Constants	
! Hb = Atmospheric pressure in m of water
! RoRef_air =  air density
  double precision dt2
  double precision Hb
  double precision RoRef_air	
  integer number_steps
  integer NR                                     !NR = number of reaches
  integer, ALLOCATABLE :: NX(:)
  integer Num_max_cells                          !maximum number of cells as a 
                                                 !  result of discretization
! Dry bed 
  double precision, ALLOCATABLE :: ydry(:)      
  double precision, ALLOCATABLE :: Adry(:)
  double precision, ALLOCATABLE :: phi_dry(:)	
  double precision, ALLOCATABLE :: Celer_dry(:)
  double precision, ALLOCATABLE :: ydry_Cutoff(:)     !Cutoff is 1/10 of dry regime 
  double precision, ALLOCATABLE :: Adry_Cutoff(:)      
  double precision, ALLOCATABLE :: phi_dry_Cutoff(:)      
  double precision, ALLOCATABLE :: Celer_dry_Cutoff(:)      
  double precision, ALLOCATABLE :: P_pho_dry_Cutoff(:)    
  double precision, ALLOCATABLE :: fluxdry_Cutoff(:)              
  double precision, ALLOCATABLE :: ydry_CFL(:)
  double precision, ALLOCATABLE :: Adry_CFL(:)        !For Courant stab. 
  double precision, ALLOCATABLE :: fluxdry(:)         !Flux due to minimum water depth 
	
! Sloped pipe
  double precision Min_Slope_for_Sloped_pipe
  double precision, ALLOCATABLE :: A_cell_dry_sloped(:)
  double precision, ALLOCATABLE :: A_open_sloped_pipe(:)
  double precision, ALLOCATABLE :: b2_max(:)     !maximum depth of pipe in the vertical axis
  double precision, ALLOCATABLE :: A2_max(:)     !maximum area of pipe in the vertical axis
                                                 ! b2_min and A2_min are for iteration purposes
  double precision, ALLOCATABLE :: b2_min(:)     !minimum depth of pipe in the vertical axis
  double precision, ALLOCATABLE :: A2_min(:)     !minimum area of pipe in the vertical axis	
  double precision, ALLOCATABLE :: P_pho_min(:)  !minimum P_pho for iteration purp
  double precision, ALLOCATABLE :: P_pho_max(:)  !maximum P_pho for iteration purp
  double precision, ALLOCATABLE :: phi_max(:)    !minimum phi for iteration purp
  double precision, ALLOCATABLE :: phi_min(:)    !maximum phi for iteration purp	
  double precision, ALLOCATABLE :: y_for_phi_max(:)  !maximum depth for phi in iteration

! Dtmax
  double precision DtMax	

! Internal Tolerances
  double precision Tol_int_10_1
  double precision Tol_int_10_2
  double precision Tol_int_10_3
  double precision Tol_int_10_4
  double precision Tol_int_10_5
  double precision Tol_int_10_6
  double precision Tol_int_10_7
  double precision Tol_int_10_8
  double precision Tol_int_10_9
  double precision Tol_int_10_10
  double precision Tol_int_10_12
  double precision Tol_int_10_14

! Stored volume
  double precision Vol_entered_system

! Inflows volume
  double precision Vol_inflows      

! Volumes
  double precision Vol_lost_system
  double precision vol_reservoirs_time_step
  double precision vol_rating_time_step
  double precision vol_dropshafts_time_step 
  double precision vol_junctions_time_step
  double precision vol_pipes_time_step
  double precision vol_inflows_time_step
  double precision vol_const_bound_time_step

! Various
  integer SUM_VAR
  integer sum_no_converg
  double precision, ALLOCATABLE :: Klocal(:,:)

! Rating curve
  ! Maximum flow specified at the rating curve. This is used to check if the
  ! water level in the rating curve is exceeded. 
  double precision, ALLOCATABLE :: Max_flow_rating_curve(:) 
  double precision, ALLOCATABLE :: Max_Head_rating_curve(:) 
      
! Cross-section area of weir (bottom to weir crest). 
  double precision, ALLOCATABLE :: area_weir(:) 

! Gates
  double precision, ALLOCATABLE :: Cd_gate(:)	
  double precision, ALLOCATABLE :: Hgate_open(:) !Gate opening in percentage
  double precision, ALLOCATABLE :: h_gate_m(:)   !Height of gate opening in m

  ! This defines if the Gate will be operated based on a threshold depth or 
  ! if the operation is according to a time schedule (1: based on threshold depth,
  ! 2: based on time schedule)
  integer,          ALLOCATABLE :: gate_thresholdDepth_TimeSpecified(:)

 ! 0 if operation was not activated, 1 if gate operation was started or completed.
  integer,          ALLOCATABLE :: gate_Activation(:) 

  ! Initial & final times when gate operated
  double precision, ALLOCATABLE :: t_gate_act_initial(:)
  double precision, ALLOCATABLE :: t_gate_act_final(:)

  !The depth in this node will be checked to open or close the gate  
  character(IDLEN), ALLOCATABLE :: Depth_check_Node_Gate(:)

  !Same as Depth_check_Node_Gate but in ITM value (i.e., integer)
  integer,          ALLOCATABLE :: check_Node_Gate_ITM(:)       
  double precision, ALLOCATABLE :: ClosureTime_Minutes_Gate(:) !closure/opening rate of the gate 

! Volume check       
  double precision vol_reserv_outflow_time_step
  double precision balance_volume_time_step
  double precision vol_lost_time_step
  double precision Vol_stored_old_time
  double precision Volume_stored_current_step
  double precision Error_volume
  double precision balance_volume
  double precision Q1(10)
  double precision A1(10)
  double precision u1(10)
  double precision y1(10)
  double precision z1(10)      
  double precision c1(10)
  double precision P_pho1(10)	
  double precision dropIA(10)
  double precision Ab_oldIA(10)
  double precision Qb_oldIA(10)
  double precision yb_oldIA(10)
  double precision phi11(10)
  double precision phi1IA(10)
  double precision yw01(10)
  double precision Aw01(10)
  double precision Qw01(10)
  double precision y_drybed(10)
  double precision Q_drybed(10)
  integer IDf1(10)
  integer IDfbIA(10)
  integer IDfbIA_old(10)
  integer drybed(10) 
  integer flow_regIA(10)
  integer flowcaseIA(10)
  integer SumIDFIA(10)
  integer solve_full_eq(10)
  integer init_volume_counter
  integer cond_mixed1(10)
  integer Idf01(10)

  integer temp9

  integer Istor	
  double precision T_GLOBAL
  double precision DT_GLOBAL
  double precision TIME_BEGIN
  double precision TIME_END

  integer GLOBAL_STATUS_FLAG
  double precision convergen
  double precision Epsilon !small tolerance

  double precision Initial_volume_stored !was added

  character (len=1024) :: INPUT_FILE_NAME        !@ITM@IGNORE
  character (len=1024) :: INPUT_DIRECTORY        !@ITM@IGNORE
  integer INPUT_FILES_OPEN                       !@ITM@IGNORE

  character*1000 error_message                   !@ITM@IGNORE
  integer error_message_len                      !@ITM@IGNORE
  parameter (error_message_len = 1000)           !@ITM@IGNORE

  integer NumVolErrPoints                        !@ITM@IGNORE

! Parameters used for solving non-linear equations
  double precision paramOP1
  double precision paramOP2
  double precision paramOP3
  double precision paramOP4 
  double precision paramOP5
  double precision paramOP6
  double precision paramOP7
  double precision paramOP8
  double precision paramOP9
  double precision param1
  double precision param2
  double precision param3
  double precision param4 
  double precision param5
  double precision param6
  double precision param7
  double precision param8 
  double precision param9
  double precision param10
  double precision param11
  double precision param12 
  double precision param13
  double precision param14
  double precision param15
  double precision param16 
  double precision param17
  double precision param18
  double precision param19
  double precision param20 	
  double precision param21
  double precision param22
  double precision param23
  double precision param24 
  double precision param25
  double precision param26
  double precision param27
  double precision param28 
  double precision param29
  double precision param30
  double precision param31
  double precision param32 
  double precision param33
  double precision param34
  double precision param35
  double precision param36
  double precision param_ener

  integer parintOP1
  integer parintOP2
  integer parint1
  integer parint2
  integer parint3
  integer parint4
  integer parint5
  integer parint6
  integer parint7
  integer parint8
  integer parint9
  integer parint10
  integer parint11
  integer parint12
  integer parint13
  integer parint14
  integer parint15
  integer parint16
  integer parint17
  integer parint18
  integer parint1000
  integer Counter_printing

  integer sumIA
  integer sumIB
  integer sumIC

  double precision, ALLOCATABLE :: Qmin(:)
  integer code_vol_bal
  double precision Vol_bal
  integer ini_cond                               !Initial condition = constant water depth
  integer sum_temp                               !Initial condition = constant water depth
  double precision temp_outflow                  !Temporal value

! Pumping rates at selected nodes. 
  double precision, ALLOCATABLE :: Qpump(:)
  double precision, ALLOCATABLE :: t_begin_pump(:)
  integer,          ALLOCATABLE :: IDpump(:)

  integer CURRENT_REVISION                       !@ITM@IGNORE
  parameter (CURRENT_REVISION = 213)             !@ITM@IGNORE

!!!!!! Parameters that don't need to get saved to hotstart file go here.
  double precision, ALLOCATABLE :: h0L(:)
  double precision, ALLOCATABLE :: h0R(:) 

!  Pressurization and Depressurization of the system
  integer system_pressur 


END MODULE COMMON_MODULE 
