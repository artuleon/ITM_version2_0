! This file is part of the ITM model.
!
! Copyright 2009 University of Illinois at Urbana-Champaign
! Copyright 2011 Oregon State University, Corvallis
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
!--------------------------------------------------------------------

!==============================================================================
! This module is used to write ITM output to files.
!==============================================================================
    
module itm_output
use common_module
use itm_accessors
implicit none
  
#define FREE(x) if (allocated(x)) deallocate(x)

! These variables are used to collect results statistics
real(8), private, allocatable :: avg_node_depth(:)
real(8), private, allocatable :: max_node_depth(:)
real(8), private, allocatable :: max_depth_time(:)
real(8), private, allocatable :: max_link_flow(:)
real(8), private, allocatable :: max_flow_time(:)
real(8), private, allocatable :: max_link_veloc(:)
real(8), private, allocatable :: max_link_depth(:)

integer, private :: magic_number = 516114523
integer, private :: version = 15001
integer, private :: num_steps

contains
    
subroutine itm_output_open(out_file_name, itm_file_name)
!==============================================================================
! Open the binary output files that save results at each reporting period
!==============================================================================
character(*), intent(in ) :: out_file_name
character(*), intent(in ) :: itm_file_name

integer :: error_code
integer :: cms_units_code = 3    ! cms flow units
integer :: num_node_inputs = 3   ! type, invert, max depth
integer :: num_link_inputs = 5   ! type, up/dn offsets, diam, length
integer :: num_node_outputs = 4  ! depth, head, volume, inflow
integer :: num_link_outputs = 4  ! flow, depth, velocity, Froude
    
    ! Open binary output results file 
    open(94, file = out_file_name, access = 'stream', iostat = error_code)
    if (error_code .NE. 0) then
        write(98,*) ' Could not open binary output file ', out_file_name
        GLOBAL_STATUS_FLAG = 1
        return
    end if
    
    ! Open ITM pipe cell results file
    open(95, file = itm_file_name, access = 'stream', iostat = error_code)
    if (error_code .NE. 0) then
        write(98,*) ' Could not open binary ITM pipe cell output file ', itm_file_name
        GLOBAL_STATUS_FLAG = 1        
        return
    end if
    
    ! Save max. number of cells to pipe plot file
    write(95) MaxNumPlotCells
        
    ! Write opening records to the binary output file
    write(94) magic_number, version, cms_units_code
    write(94) Nnodes, Nlinks
    write(94) num_node_inputs, num_link_inputs, num_node_outputs, num_link_outputs
    call save_input_data(num_node_inputs, num_link_inputs)
   
    ! Allocate memory for summary statistics
    allocate(avg_node_depth(Nnodes))
    allocate(max_node_depth(Nnodes))
    allocate(max_depth_time(Nnodes))
    allocate(max_link_flow(Nlinks))
    allocate(max_flow_time(Nlinks))
    allocate(max_link_veloc(Nlinks))
    allocate(max_link_depth(Nlinks))
    
    ! Initialize summary statistics
    avg_node_depth = 0d0
    max_node_depth = 0d0
    max_depth_time = 0d0
    max_link_flow  = 0d0
    max_flow_time  = 0d0
    max_link_veloc = 0d0
    max_link_depth = 0d0
    num_steps      = 0
    
end subroutine itm_output_open

  
subroutine itm_output_close
!==============================================================================
! Close the binary results files
!==============================================================================

    write(94) num_steps, GLOBAL_STATUS_FLAG, magic_number
    close(94)
    close(95)
    FREE(avg_node_depth)
    FREE(max_node_depth)
    FREE(max_depth_time)
    FREE(max_link_flow)
    FREE(max_flow_time)
    FREE(max_link_veloc)
    FREE(max_link_depth)
    
end subroutine itm_output_close


subroutine itm_output_write_title()
!==============================================================================
! Write a header and project title to the formatted report file.
!==============================================================================

    write(98,*) ' ILLINOIS TRANSIENT MODEL - VERSION 1.5'
    write(98,*) ' ======================================'
    write(98,*) '  '
    if (len_trim(project_title) > 0) then
        write(98, '(2x,a)') project_title
        write(98,*) '  '
    end if

end subroutine itm_output_write_title


subroutine itm_output_save_results(current_time)
!==============================================================================
! Save computed results to the binary output files at the current elapsed time
! in seconds.
!==============================================================================
real(8), intent(in) :: current_time
integer :: i, j, num_cells
logical :: is_open
    
    inquire(unit = 94, opened = is_open)
    if (is_open == .false.) return
   
    ! Update number of reporting steps
    num_steps = num_steps + 1
    
    ! Write current elapsed time (seconds) to binary output file
    write(94) current_time

    ! Save results for each node
    do i = 1, Nnodes
        call save_node_results(i, current_time)
    end do

    ! Save results for each link
    num_cells = MaxNumPlotCells	      
    do j = 1, Nlinks
        call save_link_results(j, current_time)
        call save_pipe_cell_results(j, num_cells, current_time)
    end do
    
end subroutine itm_output_save_results

  
subroutine save_node_results(i, current_time)
!==============================================================================
!  Save node results to the binary output file at the current time.
!==============================================================================
integer, intent(in) :: i
real(8), intent(in) :: current_time
real(8) :: depth, head, volume, lat_flow, invert
    
    ! Find water depth & head at node
    invert = junct_elev(i)
    depth = itm_get_node_depth(i)
    head = depth + invert
    
    ! Adjust depth at WEIR nodes (where it's computed as height above crest) 
    if (node_type(i) .eq. WEIR) then
        if (depth <= 0d0) then
            depth = 0d0
            head = weir_invert(i)
        else
            depth = depth + (junct_elev(i) - weir_invert(i))
            head = weir_invert(i) + depth
        end if
    end if
    
    ! Update node statistics
    avg_node_depth(i) = avg_node_depth(i) + depth
    if (depth > max_node_depth(i)) then
        max_node_depth(i) = depth
        max_depth_time(i) = current_time
    end if
    
    ! Find node's volume & lateral inflow
    call itm_get_storage(i, depth, volume)
    call itm_get_inflow(i, current_time, lat_flow)
    
    ! Write node results as 4-byte reals to binary output file
    write(94) real(depth), real(head), real(volume), real(lat_flow)
    
end subroutine save_node_results
  
    
subroutine save_link_results(j, current_time)
!==============================================================================
!  Save link results to the binary output file at the current time.
!==============================================================================
integer, intent(in) :: j
real(8), intent(in) :: current_time
integer :: cell
real(8) :: flow, depth, veloc, froude

    ! Find results at mid-point of link
    cell = 1 + int(Nx(j) / 2)	    
    
    if (pump_index(j) > 0) then !Pump case (pump_index(j) = 0 is a regular link, pump_index(j) > 0 is a pump)    
        flow = Qpump_link(j)
        depth = d(j)
        !veloc = abs(flow) / Area_full(j)
        veloc = flow / Area_full(j)
        froude = -10000d0
    else
        flow = Q0(j, cell) 
        depth = min(h0(j, cell), d(j))
        !veloc = abs(Q0(j, cell) / A0(j, cell))
        veloc = Q0(j, cell) / A0(j, cell)
        froude = abs(veloc) / sqrt(g * A0(j, cell) / get_surface_width(j, cell))
    end if
    
    
    ! Update link statistics
    if (abs(flow) > max_link_flow(j)) then
        max_link_flow(j) = abs(flow)
        max_flow_time(j) = current_time
    end if
    max_link_veloc(j) = max(max_link_veloc(j), abs(veloc))
    max_link_depth(j) = max(max_link_depth(j), depth)
    
    ! Save results as 4-byte reals to binary output file
    write(94) real(flow), real(depth), real(veloc), real(froude)
    
end subroutine save_link_results    

  
function get_surface_width(link_index, cell_num) result(width)
!==============================================================================
!  Find the water surface width across a given pipe cell.
!==============================================================================
integer, intent(in) :: link_index, cell_num
real(8) :: width
real(8) :: AA, Ts, RH

    call Area_from_H(link_index, h0(link_index, cell_num), &
        AA, Ts, RH, IDflow(link_index, cell_num))
    width = Ts
    
end function get_surface_width
  

subroutine save_pipe_cell_results(j, num_cells, current_time)
!==============================================================================
!  Save the water depths along the cells of a pipe at the current time.
!==============================================================================
integer, intent(in   ) :: j
integer, intent(inout) :: num_cells
real(8), intent(in   ) :: current_time
real(8) :: stations(num_cells)  ! x-station
real(8) :: depths(num_cells)    ! depth at x-station
real(8) :: dxp, x1p, x2p, sum_temp2
integer :: i, k, lim1, lim2, p 
real(8) :: zero = 0d0
logical :: is_open
    
    inquire(unit = 95, opened = is_open)
    if (is_open == .false.) return

    if (num_cells >= Nx(j) - 4) then
        num_cells = Nx(j) - 4
        do i = 3, Nx(j) - 2
            stations(i-2) = (i - 2.5) * dx(j)
            depths(i-2) = h0(j,i)  !note that elevation was not added
        end do
    else
        dxp = Length(j) / num_cells
        do k = 1, num_cells
            call get_cell_limits(j, k, dxp, x1p, x2p, lim1, lim2)    
            call get_cell_result(j, x1p, x2p, lim1, lim2, &
                stations(k), depths(k))
        end do
    end if

    do i = 1, num_cells
        if (depths(i) < 0d0 .or. pump_index(j) > 0) then  !fully_pressuri(j) == 2 never assigned
            depths(i) = yref(j)
        end if
    end do
    
    write(95) current_time, j, num_cells
    write(95) (stations(k), k=1, num_cells)
    write(95) (depths(k), k=1, num_cells)
    write(95) (zero, k = 2 * num_cells + 1, 2 * MaxNumPlotCells)
    
end subroutine save_pipe_cell_results 
  
  
subroutine get_cell_limits(j, k, dxp, x1p, x2p, lim1, lim2)
!==============================================================================
!  Find a range of pipe cells over which depths are averaged.
!==============================================================================
integer, intent(in ) :: j, k
real(8), intent(in ) :: dxp
real(8), intent(out) :: x1p, x2p
integer, intent(out) :: lim1, lim2
real(8) :: con1, con2
 
    x1p = (k-1) * dxp
    x2p = k * dxp			
    con1 = x1p / dx(j)
    con2 = x2p / dx(j)
    if (int(con1) < 0) then
        lim1 = 1
    else
        lim1 = 1+int(con1)
    end if
    if (int(con2) >= Nx(j) - 4) then
        lim2 = Nx(j) - 4
    else
        lim2 = 1+int(con2)
    end if
    
end subroutine get_cell_limits
  

subroutine get_cell_result(j, x1p, x2p, lim1, lim2, station, depth)
!==============================================================================
!  Get the average depth within a range of pipe cells.
!==============================================================================
integer, intent(in ) :: j, lim1, lim2
real(8), intent(in ) :: x1p, x2p
real(8), intent(out) :: station, depth
integer :: i, p
real(8) :: sum_temp2

    ! For the first and last points of plotting of each pipe, the first 
    ! and last (respectively) values of the actual cells are used.
    ! This is done to improve the quality of the plots near boundaries.

    if (lim1 == 1) then
            
        station = 0.5 * dx(j)
        if (BCnode(Node1(j)) == 24) then
            depth = h0(j,5)
        else
            depth = h0(j,3)
        end if
          
    else if (lim2 == Nx(j) - 4) then
            
        station = (Nx(j) - 4 - 0.5) * dx(j)
        if (BCnode(Node2(j)) == 24) then
            depth = h0(j, Nx(j) - 4)
        else
            depth = h0(j, Nx(j) - 2)
        end if
          
    else
            
        sum_temp2 = 0d0
        do i = lim1, lim2
            p = i+2
            sum_temp2 = sum_temp2 + h0(j,p)				
        end do
        station = (x1p + x2p) / 2d0
        depth = sum_temp2 / (1 + lim2 - lim1)
          
    end if
    
end subroutine get_cell_result
  

subroutine save_input_data(num_node_inputs, num_link_inputs)
!==============================================================================
!  Save selected node and pipe input data to the binary output file that are
!  needed to generate pipe profile plots in the GUI.
!  (Note: data saved as 4-byte reals.)
!==============================================================================
integer, intent(in) :: num_node_inputs, num_link_inputs
integer :: i, j
real(8) :: max_depth, offset1, offset2, invert

    do i = 1, Nnodes
        invert = junct_elev(i)
        select case (node_type(i))
        case (JUNCTION)
            max_depth = hdrops_overf(i)
        case (STORAGE)
            max_depth = reser_maxdepth(i)
        case (WEIR)
            max_depth = hdrops_overf(i)
            invert = weir_invert(i)
        case default
            max_depth = 0d0
        end select
        write(94) node_type(i), real(invert), real(max_depth)        
    end do
    
    do i = 1, Npipes
        offset1 = zb(i,1) - junct_elev(Node1(i))
        offset2 = zb(i,2) - junct_elev(Node2(i))
        write(94) link_type(i), real(offset1), real(offset2), &
            real(d(i)), real(length(i))
    end do
    
end subroutine save_input_data
  

subroutine itm_output_write_report()
!==============================================================================
! Write summary results to the report text file.
!==============================================================================

    if (GLOBAL_STATUS_FLAG .ne. 0) return
    call write_option_summary()
    if (num_steps .eq. 0) then
        write(98,*) ' Simulation was stopped before reporting start time.'
        return
    end if
    call write_volume_balance()
    call write_node_summary()
    call write_link_summary()
    write(98,*) ' '    
    write(98, '(2X,a,f0.2,a)') 'Total elapsed time: ', time_end - time_begin, ' seconds'

end subroutine itm_output_write_report


subroutine write_option_summary()
!==============================================================================
! Write analysis options summary to the report text file.
!==============================================================================
character(len=22), dimension(8) :: option_txt
    
    option_txt = [character(len=22) :: &
        'Total Duration .......', &
        'Report Start .........', &
        'Report Step ..........', &
        'Max. Time Step .......', &
        'Wave Celerity ........', &
        'Max. No. Cells .......', &
        'Use Hotstart File ....', &
        'Save Hotstart File ...']
    
    write(98,*) ' ================'
    write(98,*) ' Analysis Options'
    write(98,*) ' ================'
    write(98, '(2x, a, f8.2, a)') option_txt(1), Tmax/3600d0, ' hrs'
    write(98, '(2x, a, f8.2, a)') option_txt(2), T_START_REPORT/3600d0, ' hrs'
    write(98, '(2x, a, f8.3, a)') option_txt(3), Tstor, ' sec'
    write(98, '(2x, a, f8.3, a)') option_txt(4), DTmax, ' sec'
    write(98, '(2x, a, f8.2, a)') option_txt(5), pc, ' m/s'
    write(98, '(2x, a, i8)')      option_txt(6), nxmax
    write(98,*) '  '
 
end subroutine write_option_summary


subroutine write_volume_balance()
!==============================================================================
! Write system volume balance
!==============================================================================
real(8) :: v0, vin, vout, vf

    v0   = Initial_volume_stored
    vin  = Vol_inflows
    vout = Vol_lost_system
    vf   = Volume_stored_current_step
    write(98,*) ' '
    write(98,*) ' =========================='
    write(98,*) ' System Volumes (1000 cu m)'
    write(98,*) ' =========================='
    write(98, '(2x, a, f9.2)') 'Initial Volume : ', v0/ 1d3
    write(98, '(2x, a, f9.2)') 'Inflow Volume  : ', vin/ 1d3
    write(98, '(2x, a, f9.2)') 'Outflow Volume : ', vout/ 1d3
    write(98, '(2x, a, f9.2)') 'Final Volume   : ', vf / 1d3
    write(98,*) ' --------------------------'
    write(98, '(2x, a, f9.2)') 'Percent Error  : ', &
        1d2 * (v0 + vin - vout - vf) / (v0 + vin)
    write(98,*) ' '
    
end subroutine write_volume_balance    


subroutine write_node_summary()
!==============================================================================
! Write node results summary table to the report text file.
!==============================================================================
integer :: i, days, hrs, mins
real(8) :: avg_depth
character(64) :: fmt
character(len=11), dimension(5) :: types

    types = [character(len=11) :: &
        'Junction   ', &
        'Boundary   ', &
        'Gate       ', &
        'Weir       ', &
        'Storage    ']
    fmt = '(2x, a20, 1x, a11, 1x, f7.2, 2x, f7.2, 1x, i5, 2x, i2, a1, i2.2)'
    
    write(98,*) '  '
    write(98,*) ' =================='
    write(98,*) ' Node Depth Summary'
    write(98,*) ' =================='
    write(98,*) '  '
    write(98,*) ' ---------------------------------------------------------------'
    write(98,*) '                                  Average  Maximum  Time of Max.'
    write(98,*) '                                    Depth    Depth  Depth Occur.'
    write(98,*) ' Node                 Type         Meters   Meters  days hr:min '
    write(98,*) ' ---------------------------------------------------------------'
    
    do i = 1, Nnodes
        avg_depth = avg_node_depth(i)/num_steps
        call convert_time(max_depth_time(i), days, hrs, mins)
        write(98, fmt) node_id(i), types(node_type(i)), avg_depth, &
            max_node_depth(i), days, hrs, ':', mins
    end do
    write(98,*) ' '
    
end subroutine write_node_summary


subroutine write_link_summary()
!==============================================================================
! Write link results summary table to the report text file.
!==============================================================================
integer :: i, days, hrs, mins
real(8) :: avg_depth
character(64) :: fmt

    fmt = '(2x, a20, 1x, a8, 2x, f7.2, 2x, i4, 2x, i2, a1, i2.2, 2x, f7.2)'
    write(98,*) '  '
    write(98,*) ' ================='
    write(98,*) ' Link Flow Summary'
    write(98,*) ' ================='
    write(98,*) '  '
    write(98,*) ' -----------------------------------------------------------'
    write(98,*) '                                Maximum  Time of Max'
    write(98,*) '                                   Flow  Flow Occur.     Max'
    write(98,*) ' Link                 Type          CMS  days hr:min   Depth'
    write(98,*) ' -----------------------------------------------------------'

    do i = 1, Npipes
        call convert_time(max_flow_time(i), days, hrs, mins)
        write(98, fmt) link_id(i), 'Conduit', max_link_flow(i), &
            days, hrs, ':', mins, max_link_depth(i)
    end do
    write(98,*) ' '
    
end subroutine write_link_summary

    
subroutine convert_time(secs, days, hrs, mins)
!==============================================================================
! Converts seconds to days, hours, minutes
!==============================================================================
real(8), intent(in ) :: secs
integer, intent(out) :: days, hrs, mins
real(8) :: secs_left, real_days, real_hrs, real_mins

    secs_left = secs
    real_days = secs_left / 86400d0
    days = int(real_days)
    secs_left = secs_left - days * 86400d0
    real_hrs = secs_left / 3600d0
    hrs = int(real_hrs)
    secs_left = secs_left - hrs * 3600d0
    real_mins = secs_left / 60d0
    mins = nint(real_mins)    

end subroutine convert_time

end module itm_output
