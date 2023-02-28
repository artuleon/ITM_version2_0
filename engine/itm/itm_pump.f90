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

!====================================================================
! This module is used to model a pump contained in a pipe link.
!
! pump_data(:) is an array of pump_t data types, as defined in
! the common_module, that contains the properties of each pump. The
! pump_index(:) array has an entry for each pipe which is 0 if the
! pipe doesn't have a pump or an index into the pump_data array if
! it does have one.
!====================================================================
    
module itm_pump
use common_module
use itm_accessors
use itm_table
implicit none

real(8), parameter :: EPS = 1.0d-4
  
contains
    
function pump_find_flow(link_index) result(pump_flow)
!====================================================================
! Find the flow through a pump link.
!
! We first adjust the head H for each flow value Q on the pump's
! pump curve by subtracting k*Q^2, where k is the piping + local
! loss coefficient. Then we interpolate the adjusted curve to find
! the flow for the head difference between the pump's end nodes.
!====================================================================
integer, intent(in ) :: link_index
real(8)              :: pump_flow

real(8) :: pump_head, k, s, q1, q2, h1, h2
integer :: i, j, j1, j2, c

    ! Check that link is a pump
    pump_flow = 0d0       
    i = pump_index(link_index)
    if (i < 1) return
      
    ! End nodes of pump link
    j1 = Node1(link_index)
    j2 = Node2(link_index)
      
    ! Cannot pump if suction side water depth is below 0.45 m(1.5 ft)
    if  (yres_jun_old(j1) < 0.45) then !This will return zero flow
    !    if (i .eq. 3) write(98,'(f12.4,a)') T_GLOBAL, "   Depth too low"
        return
    end if

    ! Find head that pump must supply
    h1 = yres_jun_old(j1) + junct_elev(j1)
    h2 = yres_jun_old(j2) + junct_elev(j2)
    pump_head = h2 - h1
    
    ! Local loss coeff. (k), pump curve index (c) & fractional speed setting
    k = get_loss_coeff(link_index, i)
    c = pump_data(i)%pump_curve
    s = pump_data(i)%setting / 1d2
    
    ! If pump head is negative then pump behaves as a pipe
    if (pump_head < 0d0) then
        pump_flow = sqrt(-pump_head / k)
        return
    end if
    
    ! Find first point on adjusted pump curve
    q1 = curve(c)%x(1) * s
    h1 = curve(c)%y(1) * s * s
    h1 = h1 - k * q1 * q1
    
    ! Required head is above upper end of pump curve
    if (pump_head > h1) then
        return
    end if
    
    ! Find portion of adjusted pump curve that brackets required head
    ! and interpolate a flow value
    do j = 2, curve(c)%size
        q2 = curve(c)%x(j) * s
        h2 = curve(c)%y(j) * s * s
        h2 = h2 - k * q2 * q2
        h2 = max(h2, 0d0)
        if (pump_head <= h1 .and. pump_head >= h2) then
            pump_flow = table_interpolate(pump_head, h1, q1, h2, q2)
            return
        end if
        q1 = q2
        h1 = h2
    end do
    
    ! In case static head is below lower end of pump curve
    pump_flow = q2
    
end function pump_find_flow


function get_loss_coeff(link_index, i) result(k)
!====================================================================
! Determine a flow-based loss coeff. for piping + local losses
!====================================================================
integer, intent(in) :: link_index, i
real(8)             :: k

real(8) :: ag, f, k_piping, k_local

!   k_piping = f(L/D) / A^2 / (2g)
!   k_local = (Entry + Exit + Local Loss) Coeff / A^2 / (2g)
!   ag = (A^2)(2g) = (2*9.81)((PI/4)(d^2))^2 = 12.11(d^4)
    ag = 12.11 * d(link_index)**4
    f = pump_data(i)%friction_factor
    k_piping = f * length(link_index) / d(link_index) / ag
    k_local = pump_data(i)%loss_coeff
    k_local = (k_local + EntranceLoss(link_index) + ExitLoss(link_index)) / ag
    k = k_piping + k_local

end function get_loss_coeff


subroutine pump_set_speed(link_index, time)
!====================================================================
! Set a pump's speed at given time.
!====================================================================
integer, intent(in) :: link_index
real(8), intent(in) :: time

integer :: p, i, n, k
real(8) :: s, depth

    ! Identify index of pump associated with the link
    p = pump_index(link_index)
    if (p .eq. 0) return
    
    ! Get current speed setting
    s = pump_data(p)%setting
    
    ! Get control parameters
    i = pump_data(p)%control_tseries
    n = pump_data(p)%control_node
    k = pump_data(p)%control_curve
    
    ! Pump uses time series control
    if (i > 0) then
        s = table_tseries_lookup(tseries(i), time, .TRUE.)
        
    ! Pump uses node depth control
    else if (n > 0 .and. k > 0) then
        depth = itm_get_node_depth(n)
        s = table_lookup(curve(k), depth)
    end if

    ! Set new pump speed
    pump_data(p)%setting = s
    
end subroutine pump_set_speed

end module itm_pump
    