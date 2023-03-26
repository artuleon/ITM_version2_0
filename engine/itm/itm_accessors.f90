!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_accessors
!Description:  retrieves certain project properties
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/01/2023
!******************************************************************************

module itm_accessors
use common_module
use itm_table
implicit none
  
contains

subroutine itm_get_swmm_id(want_link, index, id)
!====================================================================
! Retrieve the ID name of a node or link.
!====================================================================
integer,          intent(in ) :: want_link
integer,          intent(in ) :: index
character(IDLEN), intent(out) :: id

    if (want_link == 1) then
        id = link_id(index)
    else
        id = node_id(index)
    end if

end subroutine itm_get_swmm_id


integer function itm_has_inflow(node_index)
!====================================================================
! Determine if a node receives external inflow or not.
!====================================================================
integer, intent(in) :: node_index

    if ((inflow(node_index)%tseries == 0) .and. &
        (inflow(node_index)%baseline == 0)) then
        itm_has_inflow = 0
    else
        itm_has_inflow = 1
    end if

end function itm_has_inflow


subroutine itm_get_inflow(node_index, time, flow)
!====================================================================
! Retrieve the rate of external inflow to a node at a specific time.
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: time
real(8), intent(out) :: flow
integer :: ts

    flow = 0.0
    ts = inflow(node_index)%tseries
    if (ts > 0) then
        flow = table_tseries_lookup(tseries(ts), time, .TRUE., .FALSE.)
        flow = flow * inflow(node_index)%scale_factor
    end if
    flow = flow + inflow(node_index)%baseline
    
end subroutine itm_get_inflow


function itm_get_node_depth(node_index) result(depth)
!====================================================================
! Retrieve the current water depth for a node.
!====================================================================
integer, intent(in) :: node_index
real(8)             :: depth

    depth = max(yres_jun_old(node_index), 0d0)

end function itm_get_node_depth


subroutine itm_get_max_curve_val_x(node_index, x)
!====================================================================
! Retrieve the largest x-value for a curve.
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(out) :: x
integer :: k

    x = 0d0
    k = node_curve(node_index)
    if (k > 0) then
        x = table_get_x_max(curve(k))
    end if

end subroutine itm_get_max_curve_val_x 


subroutine itm_get_Q_from_rat_curve(node_index, h, q)
!====================================================================
! Retrieve the flow value from a rating curve for a given head.
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: h
real(8), intent(out) :: q
integer :: k

    q = 0d0
    k = node_curve(node_index)
    if (k > 0) then
        q = table_lookup(curve(k), h, .TRUE.)
    end if

end subroutine itm_get_Q_from_rat_curve


subroutine itm_get_max_rating_head(node_index, max_head)
!====================================================================
! Retrieve the maximum head value for a rating curve
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(out) :: max_head

    call itm_get_max_curve_val_x(node_index, max_head)

end subroutine itm_get_max_rating_head


subroutine itm_get_storage_volume(node_index, depth, volume)
!====================================================================
! Retrieve the volume in a storage node at a given water depth. 
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: depth
real(8), intent(out) :: volume
integer :: k

    volume = 0.0
    if (depth == 0.0 .or. node_type(node_index) .ne. STORAGE) return
    k = node_curve(node_index)
    if (k > 0) then
        volume = table_lookup(curve(k), depth, .TRUE.)
    end if

end subroutine itm_get_storage_volume


subroutine itm_get_storage_depth(node_index, volume, depth)
!====================================================================
! Retrieve the water depth in a storage node at a given stored volume. 
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: volume
real(8), intent(out) :: depth
integer :: k

    depth = 0.0
    if (volume == 0.0 .or. node_type(node_index) .ne. STORAGE) return
    k = node_curve(node_index)
    if (k > 0) then
        depth = table_reverse_lookup(curve(k), volume, .TRUE.)
    end if

end subroutine itm_get_storage_depth

!--------------------------------------------------------------------

end module itm_accessors
    
