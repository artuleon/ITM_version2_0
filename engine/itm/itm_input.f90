!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_input
!Description:  readS ITM data from an input file.
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/01/2023
!******************************************************************************

module itm_input
use common_module
use itm_table
use itm_output, only : itm_output_write_title
implicit none

#define FREE(x) if (allocated(x)) deallocate(x)

private

public itm_input_open, itm_input_close
  
contains
  
subroutine itm_input_open(file_name)
!==============================================================================
! Read ITM input data from a specially formatted text file.
!==============================================================================
character(*), intent(in ) :: file_name
integer :: error_code

    ! Open the input file
    open(10, file = file_name, status = 'old', iostat = error_code)
    if (error_code .NE. 0) then
        write(98,*) ' Could not open ITM input file ', file_name
        GLOBAL_STATUS_FLAG = 1
        return
    end if

    ! Read number of each type of object
    read(10, '(A)') project_title
    call itm_output_write_title()
    read(10,*) Nnodes
    read(10,*) Npipes
    read(10,*) Npumps
    read(10,*) Norifs
    read(10,*) Nweirs
    read(10,*) Noutlets
    read(10,*) Ncurves
    read(10,*) Ntseries
    Nlinks = Npipes + Npumps + Norifs + Nweirs + Noutlets
    
    ! Allocate memory for input data arrays
    call allocate_input_arrays
    
    ! Read input data
    call read_input_file
    close(10)
    
    ! Initialize project variables
    call adjust_boundary_conditions
    call set_pump_limits
    
end subroutine itm_input_open

      
subroutine allocate_input_arrays
!==============================================================================
! Allocate memory for arrays that hold input data.
!==============================================================================
integer :: i
    allocate(inflow(Nnodes))
    allocate(curve(Ncurves))
    allocate(tseries(Ntseries))
    
    allocate(node_id(Nnodes))
    allocate(node_type(Nnodes))
    allocate(node_curve(Nnodes))
    allocate(junct_elev(Nnodes))
    allocate(Adrop(Nnodes))
    allocate(hdrops_overf(Nnodes))
    allocate(const_depth_flow(Nnodes))
    allocate(open_closed_bound(Nnodes))
    allocate(BCnode(Nnodes))
    allocate(flowdepth_res(Nnodes))
    allocate(reser_maxdepth(Nnodes))
    allocate(reser_outflow(Nnodes))

    allocate(link_id(Nlinks))
    allocate(link_type(Nlinks))
    allocate(Node1(Nlinks))
    allocate(Node2(Nlinks))
    allocate(zb(Nlinks,2))			    
    allocate(length(Nlinks))
    allocate(d(Nlinks))
    allocate(nm(Nlinks))
    allocate(EntranceLoss(Nlinks))
    allocate(ExitLoss(Nlinks))
    allocate(Init_depth_type(Nlinks))
    allocate(Init_depth(Nlinks))
    allocate(Init_disch(Nlinks))
    
    allocate(pumps(Npumps))
    allocate(orifs(Norifs))
    allocate(weirs(Nweirs))
    allocate(outlets(Noutlets))
    allocate(link_type_index(Nlinks))
    
    node_type         = JUNCTION
    node_curve        = 0
    junct_elev        = 0
    Adrop             = 0
    hdrops_overf      = 0
    const_depth_flow  = 0
    open_closed_bound = 0
    BCnode            = 0
    flowdepth_res     = 0
    reser_maxdepth    = 0
    reser_outflow     = 0
    weir_invert       = 0
    link_type         = PIPE
    link_type_index   = 0
  
    ! Initialize inflow properties
    do i = 1, Nnodes
        inflow(i)%tseries            = 0
        inflow(i)%baseline           = 0d0
        inflow(i).scale_factor       = 1d0
    end do

end subroutine allocate_input_arrays
  
  
subroutine itm_input_close
!==============================================================================
! Free memory used to hold input data.
!==============================================================================
integer :: i

    FREE(link_type_index)
    FREE(outlets)
    FREE(weirs)
    FREE(orifs)
    FREE(pumps)
    FREE(Init_disch)
    FREE(Init_depth)
    FREE(Init_depth_type)  
    FREE(ExitLoss)
    FREE(EntranceLoss)
    FREE(nm)
    FREE(d)
    FREE(length)
    FREE(zb)
    FREE(Node2)
    FREE(Node1)
    FREE(link_type)
    FREE(link_id)

    FREE(reser_outflow)
    FREE(reser_maxdepth)
    FREE(flowdepth_res)
    FREE(BCnode)
    FREE(open_closed_bound)
    FREE(const_depth_flow)
    FREE(hdrops_overf)
    FREE(Adrop)
    FREE(junct_elev)
    FREE(node_curve)
    FREE(node_type)
    FREE(node_id)
 
    do i = 1, Ntseries
        call table_delete(tseries(i))
    end do
    do i = 1, Ncurves
        call table_delete(curve(i))
    end do
    
    FREE(tseries)
    FREE(curve)
    FREE(inflow)
    
end subroutine itm_input_close
  
  
subroutine read_input_file
!==============================================================================
! Read each section of the ITM input file.
!==============================================================================
integer :: index
logical :: is_open

    inquire(unit = 10, opened = is_open)
    if (is_open) then
        index = 0
        call read_junction_data(index)
        call read_boundary_data(index)
        call read_storage_data(index)
        index = 0
        call read_pipe_data(index)
        call read_pump_data(index)
        call read_orif_data(index)
        call read_weir_data(index)
        call read_outlet_data(index)
        
        call read_inflow_data
        call read_curve_data
        call read_tseries_data
        call read_options_data
        call read_hotstart_data
    end if
    
end subroutine read_input_file
  
  
subroutine read_junction_data(index)
!==============================================================================
! Read input data for junction nodes.
!==============================================================================
integer, intent(inout) :: index
integer :: i, n, dropshaft
real(8) :: invert, max_depth, init_depth, area
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, invert, max_depth, init_depth, area, dropshaft
        index = index + 1
        node_id(index)           = id
        node_type(index)         = JUNCTION
        junct_elev(index)        = invert
        hdrops_overf(index)      = max_depth
        flowdepth_res(index)     = init_depth
        Adrop(index)             = area
        open_closed_bound(index) = dropshaft
        BCnode(index)            = JUNC2
    end do
    
end subroutine read_junction_data  
  
  
subroutine read_boundary_data(index)
!==============================================================================
! Read input data for constant boundary nodes.
!==============================================================================
integer, intent(inout) :: index
integer  :: i, n, const_depth_or_flow, ventilated
real(8)  :: invert, const_value
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, invert, const_depth_or_flow, const_value, ventilated
        index = index + 1
        node_id(index)           = id
        node_type(index)         = BOUNDARY
        junct_elev(index)        = invert
        BCnode(index)            = const_depth_or_flow
        const_depth_flow(index)  = const_value
        open_closed_bound(index) = ventilated
    end do
    
end subroutine read_boundary_data
  
  
subroutine read_storage_data(index)
!==============================================================================
! Read input data storage nodes.
!==============================================================================
integer, intent(inout) :: index  
integer :: i, n, storage_curve
real(8) :: invert, max_depth, init_depth, outflow
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, invert, max_depth, init_depth, storage_curve, outflow
        index = index + 1
        node_id(index)           = id
        node_type(index)         = STORAGE
        junct_elev(index)        = invert
        reser_maxdepth(index)    = max_depth
        flowdepth_res(index)     = init_depth
        node_curve(index)        = storage_curve
        reser_outflow(index)     = outflow
        BCnode(index)            = RESERVOIR
        open_closed_bound(index) = 0
    end do  
  
end subroutine read_storage_data

  
subroutine read_pipe_data(index)
!==============================================================================
! Read input data for pipe links.
!==============================================================================
integer, intent(inout) :: index
integer :: i, n, n1, n2, itype 
real(8) :: diam, len, rough, off1, off2, loss1, loss2, q0, d0 
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, n1, n2, diam, len, rough, off1, off2, &
            loss1, loss2, q0, d0, itype
        index = index + 1
        link_id(index)         = id
        link_type(index)       = PIPE
        link_type_index(index) = index
        Node1(index)           = n1
        Node2(index)           = n2
        d(index)               = diam
        length(index)          = len
        nm(index)              = rough
        zb(index, 1)           = junct_elev(n1) + abs(off1)
        zb(index, 2)           = junct_elev(n2) + abs(off2)
        EntranceLoss(index)    = loss1
        ExitLoss(index)        = loss2
        Init_disch(index)      = q0  
        Init_depth(index)      = d0
        Init_depth_type(index) = itype
    end do
    
end subroutine read_pipe_data  


subroutine read_pump_data(index)
!==============================================================================
! Read input data for pipes with pumps
!==============================================================================
integer, intent(inout) :: index
integer :: i, k, n, n1, n2, ctrl_type
integer :: pmp_curve, ctrl_series, ctrl_node, ctrl_curve
real(8) :: diam, length, kloss, fric_factor, setting, close_rate
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, n1, n2, pmp_curve, diam, length, fric_factor, kloss

        index = index + 1
        link_id(index)         = id
        link_type(index)       = PUMP
        link_type_index(index) = i
        Node1(index)           = n1
        Node2(index)           = n2
        
        pumps(i)%pump_curve      = pmp_curve
        pumps(i)%pipe_diam       = diam
        pumps(i)%pipe_length     = length
        pumps(i)%friction_factor = fric_factor
        pumps(i)%loss_coeff      = kloss

        read(10,*) ctrl_type, setting, close_rate, ctrl_series, ctrl_node, ctrl_curve
        
        pumps(i)%control%setting  = setting
        pumps(i)%control%tseries  = ctrl_series
        pumps(i)%control%node     = ctrl_node
        pumps(i)%control%curve    = ctrl_curve
        ! Close rate doesn't apply to pumps
    end do
    
end subroutine read_pump_data  


subroutine read_orif_data(index)
!==============================================================================
! Read input data for orifices
!==============================================================================
integer, intent(inout) :: index
integer :: i, k, n, n1, n2, ctrl_type
integer :: orif_type, shape, flapgate, ctrl_series, ctrl_node, ctrl_curve
real(8) :: offset, height, width, coeff, setting, adjust_rate
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, n1, n2, orif_type, shape, height, width, offset, coeff, flapgate

        index = index + 1
        link_id(index)         = id
        link_type(index)       = ORIFICE
        link_type_index(index) = i
        Node1(index)           = n1
        Node2(index)           = n2
        
        orifs(i)%orif_type = orif_type
        orifs(i)%shape    = shape
        orifs(i)%height   = height
        orifs(i)%width    = width
        orifs(i)%offset   = offset
        orifs(i)%coeff    = coeff
        orifs(i)%flapgate = flapgate

        read(10,*) ctrl_type, setting, adjust_rate, ctrl_series, ctrl_node, ctrl_curve
        
        orifs(i)%control%setting     = setting
        orifs(i)%control%adjust_rate = adjust_rate
        orifs(i)%control%tseries     = ctrl_series
        orifs(i)%control%node        = ctrl_node
        orifs(i)%control%curve       = ctrl_curve
    end do
    
end subroutine read_orif_data  


subroutine read_weir_data(index)
!==============================================================================
! Read input data for weirs
!==============================================================================
integer, intent(inout) :: index
integer :: i, k, n, n1, n2, ctrl_type
integer :: weir_type, contractions, can_surcharge, ctrl_series, ctrl_node, ctrl_curve
real(8) :: offset, height, length, slope, coeff, end_coeff, setting, adjust_rate
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, n1, n2, weir_type, height, length, offset, coeff, &
            contractions, can_surcharge

        index = index + 1
        link_id(index)         = id
        link_type(index)       = WEIR
        link_type_index(index) = i
        Node1(index)           = n1
        Node2(index)           = n2
        
        weirs(i)%weir_type     = weir_type
        weirs(i)%height        = height
        weirs(i)%length        = length
        weirs(i)%offset        = offset
        weirs(i)%coeff         = coeff
        weirs(i)%contractions  = contractions
        weirs(i)%can_surcharge = can_surcharge
       ! weirs(i)%slope         = slope
       ! weirs(i)%end_coeff     = end_coeff

        read(10,*) ctrl_type, setting, adjust_rate, ctrl_series, ctrl_node, ctrl_curve
        
        weirs(i)%control%setting     = setting
        weirs(i)%control%adjust_rate = adjust_rate
        weirs(i)%control%tseries     = ctrl_series
        weirs(i)%control%node        = ctrl_node
        weirs(i)%control%curve       = ctrl_curve
    end do
    
end subroutine read_weir_data  


subroutine read_outlet_data(index)
!==============================================================================
! Read input data for weirs
!==============================================================================
integer, intent(inout) :: index
integer :: i, k, n, n1, n2
integer :: rating_curve
real(8) :: offset
character(IDLEN) :: id

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) id, n1, n2, offset, rating_curve

        index = index + 1
        link_id(index)         = id
        link_type(index)       = OUTLET
        link_type_index(index) = i
        Node1(index)           = n1
        Node2(index)           = n2
        
        outlets(i)%offset        = offset
        outlets(i)%rating_curve  = rating_curve
    end do
    
end subroutine read_outlet_data  


subroutine read_inflow_data
!==============================================================================
! Read input data for external inflows.
!==============================================================================
integer :: i, n, index, ts_index
real(8) :: base, sfactor

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) index, ts_index, sfactor, base
        inflow(index)%tseries      = ts_index
        inflow(index)%scale_factor = sfactor
        inflow(index)%baseline     = base
    end do
    
end subroutine read_inflow_data

  
subroutine read_curve_data
!==============================================================================
! Read curve data points from input file.
!==============================================================================
integer :: i, j, n, pts
real(8) :: x, y

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) pts
        call table_init(curve(i), pts)
        do j = 1, pts
            read(10, *) x, y
            call table_append(curve(i), x, y)
        end do
    end do
    
end subroutine read_curve_data

  
subroutine read_tseries_data
!==============================================================================
! Read time series data points from input file.
!==============================================================================
integer :: i, j, n, pts
real(8) :: x, y

    read(10, *) n
    if (n == 0) return
    do i = 1, n
        read(10, *) pts
        call table_init(tseries(i), pts)
        do j = 1, pts
            read(10, *) x, y
            call table_append(tseries(i), x, y)
         end do
    end do
    
end subroutine read_tseries_data

  
subroutine read_options_data
!==============================================================================
! Read options data from input file.
!==============================================================================

    read(10, *) nxmax
    read(10, *) pc
    read(10, *) yfree_press
    read(10, *) Tmax
    read(10, *) Dtmax1
    read(10, *) Tstor
    read(10, *) T_START_REPORT
    read(10, *) MaxNumPlotCells
    read(10, *) water_init_elevation
    T_NEXT_REPORT = T_START_REPORT
    numitera      = 200
    min_num_grids = 50
    pcm           = 50
    tol           = 0.0000001
    tol_lower     = 0.01
    tol_very_low  = 0.01
    tol_higher    = 0.0000000000001
    tol_crit      = 0.001
    type_of_flow  = -1
    
end subroutine read_options_data


subroutine read_hotstart_data
!==============================================================================
! Read names of hotstart files.
!==============================================================================

    read(10, '(A)') hsfile_use
    if (trim(hsfile_use) == '*') hsfile_use = ''
    read(10, '(A)') hsfile_save
    if (trim(hsfile_save) == '*') hsfile_save = ''

end subroutine read_hotstart_data


subroutine adjust_boundary_conditions
!==============================================================================
! Initialize and adjust nodal boundary conditions.
!==============================================================================
integer :: i
integer, allocatable :: degree(:)

    ! Determine # of connecting links (degree) for each node
    allocate(degree(Nnodes))
    degree = 0
    do i = 1, Nlinks
        degree(Node1(i)) = degree(Node1(i)) + 1
        degree(Node2(i)) = degree(Node2(i)) + 1
    end do
  
    ! Adjust boundary condition for junctions of degree 1
    do i = 1, Nnodes
        if ((BCnode(i) == JUNC2) .and. (degree(i) == 1)) then
            BCnode(i) = DROPSHAFT
        end if
    end do
    deallocate(degree)
  
    ! Set constant water depth initial condition
    if (water_init_elevation < -99999.51 .or. water_init_elevation > -99999.49) then
        ini_cond = 1
    else
        ini_cond = 0
    endif
    
end subroutine adjust_boundary_conditions


subroutine set_pump_limits
!==============================================================================
! Set maximum head & flow at nominal speed for each pump.
!==============================================================================
integer :: i, c, j

    do i = 1, Npumps
        c = pumps(i)%pump_curve        
        pumps(i)%max_head = table_lookup(curve(c), 0d0, .TRUE.)
        pumps(i)%max_flow = table_get_x_max(curve(c))        
    end do
    
end subroutine set_pump_limits

end module itm_input
