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


subroutine itm_get_storage(node_index, depth, storage)
!====================================================================
! Retrieve the volume in a storage node at a given water depth. 
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: depth
real(8), intent(out) :: storage
integer :: k

    storage = 0.0
    if (depth == 0.0) return
    k = node_curve(node_index)
    if (k > 0) then
        storage = table_lookup(curve(k), depth, .TRUE.)
    end if

end subroutine itm_get_storage


subroutine itm_get_storage_depth(node_index, storage, depth)
!====================================================================
! Retrieve the water depth in a storage node at a given stored volume. 
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: storage
real(8), intent(out) :: depth
integer :: k

    depth = 0.0
    if (storage == 0.0) return
    k = node_curve(node_index)
    if (k > 0) then
        depth = table_reverse_lookup(curve(k), storage, .TRUE.)
    end if

end subroutine itm_get_storage_depth


function itm_get_gate_opening(node_index, time, dt, opening) &
    result(new_opening)
!====================================================================
! Retrieve the percent open of a gate node at a specific time
! (if time controlled) or for a specific control node depth.
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: time          ! elapsed time
real(8), intent(in ) :: dt            ! time step
real(8), intent(in ) :: opening       ! current percent opening
real(8)              :: new_opening   ! new percent opening

real(8) :: target_opening, orate, ostep, delta
integer :: i, j, k

character(64) :: fmt

    ! Retrieve gate control parameters
    i = gate_data(node_index)%control_tseries
    j = gate_data(node_index)%control_node
    k = gate_data(node_index)%control_curve
    orate = gate_data(node_index)%opening_rate * 60d0
    
    ! Find a new target percent opening
    if (i > 0) then
        target_opening = table_tseries_lookup(tseries(i), time, .FALSE., .TRUE.)
    else if (j > 0 .and. k > 0) then
        target_opening = table_lookup(curve(k), yres_jun_old(j), .FALSE.)
    else
        target_opening = opening
    end if
    gate_data(node_index)%target_opening = target_opening
    
    ! New opening equals target if current opening equals target
    ! or time to open is 0 
    if (target_opening .eq. opening .or. orate .eq. 0d0) then
        new_opening = target_opening
        
    ! Otherwise change in current opening is fractional difference between it
    ! and target opening achieved over the current time step
    else
        delta = (target_opening - opening) / 100d0
        ostep = dt / orate
        if (ostep + 0.001 >= abs(delta)) then
            new_opening = target_opening
        else
            new_opening = opening + sign(1d0, delta)*ostep*100d0
        end if
    end if
    
    ! Make sure new opening is feasible
    new_opening = min(new_opening, 100d0)
    new_opening = max(new_opening, 0d0)
    
end function itm_get_gate_opening

  
subroutine itm_get_gate_loss_coeff(node_index, opening, coeff)
!====================================================================
! Retrieve the head loss coefficient for a gate node at a specific
! percent open.
!====================================================================
integer, intent(in ) :: node_index
real(8), intent(in ) :: opening    
real(8), intent(out) :: coeff
integer :: k

    k = node_curve(node_index)
    if (k > 0) then
        coeff = table_lookup(curve(k), opening, .TRUE.)
    else
        coeff = -1d0
    end if
    
end subroutine itm_get_gate_loss_coeff

!--------------------------------------------------------------------

end module itm_accessors
    
