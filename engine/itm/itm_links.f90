!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_links
!Description:  computes flow in non-pipe links
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/01/2023
!******************************************************************************

module itm_links
use common_module
use itm_accessors
use itm_table
implicit none

real(8), parameter :: EPS = 1.0d-4
  
contains
    
function find_pump_flow(link_index) result(pump_flow)
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

real(8) :: h1, h2, pump_head, k, s
integer :: i, j, j1, j2, c

    ! Check that link is a pump
    pump_flow = 0d0       
    if (link_type(link_index) .ne. PUMP) return
    i = link_type_index(link_index)
    if (i < 1 .or. i > Npumps) return
      
    ! End nodes of pump link
    j1 = Node1(link_index)
    j2 = Node2(link_index)
      
    ! Cannot pump if suction side water depth is below 0.45 m (1.5 ft)
   if  (yres_jun_old(j1) < 0.45) return
    
    ! Find speed control setting
    call apply_control(pumps(i)%control, T_GLOBAL, DT_GLOBAL)

    ! Find head that pump must supply
    h1 = yres_jun_old(j1) + junct_elev(j1)
    h2 = yres_jun_old(j2) + junct_elev(j2)
    pump_head = h2 - h1
    
    ! Local loss coeff. (k), pump curve index (c) & fractional speed setting
    k = pump_loss_coeff(link_index, i)
    c = pumps(i)%pump_curve
    s = pumps(i)%control%setting / 1d2
    
    ! If pump head is negative then pump behaves as a pipe
    if (pump_head < 0d0) then
        if (k > 0d0) then
            pump_flow = sqrt(-pump_head / k)
        end if

    ! Otherwise find flow from pump curve
    else
        pump_flow = pump_curve_flow(pump_head, k, c, s)
    end if
    
    ! Limit pump flow if necessary
    call limit_link_flow(link_index, pump_flow)    
    
end function find_pump_flow


function pump_loss_coeff(link_index, pump_index) result(k)
!====================================================================
! Determine a flow-based loss coeff. for piping + local losses
!====================================================================
integer, intent(in) :: link_index, pump_index
real(8)             :: k

real(8) :: ag, d, f, k_piping, k_local

    k = pumps(pump_index)%loss_coeff
    d = pumps(pump_index)%pipe_diam
    if (d <= 0d0) return 
    f = pumps(pump_index)%friction_factor
    ag = 12.11 * d**4
    k_piping = f * pumps(pump_index)%pipe_length / d / ag
    k = k + k_piping

end function pump_loss_coeff


function pump_curve_flow(head, k, c, s) result(flow)
!====================================================================
! Find flow for a given head from a pump curve
!====================================================================
real(8), intent(in) :: head    ! required head
real(8), intent(in) :: k       ! local head loss coeff.
real(8), intent(in) :: s       ! pump speed setting
integer, intent(in) :: c       ! pump curve index
real(8) :: flow

real(8) :: q1, q2, h1, h2
integer :: j

    ! Find first point on speed adjusted pump curve
    q1 = curve(c)%x(1) * s
    h1 = curve(c)%y(1) * s * s
    h1 = h1 - k * q1 * q1
    
    ! No flow if required head is above upper end of pump curve
    if (head > h1) then
        flow = 0d0
        return
    end if
    
    ! Find portion of adjusted pump curve that brackets required head
    ! and interpolate a flow value
    do j = 2, curve(c)%size
        q2 = curve(c)%x(j) * s
        h2 = curve(c)%y(j) * s * s
        h2 = h2 - k * q2 * q2
        h2 = max(h2, 0d0)
        if (head <= h1 .and. head >= h2) then
            flow = table_interpolate(head, h1, q1, h2, q2)
            return
        end if
        q1 = q2
        h1 = h2
    end do
    
    ! In case static head is below lower end of pump curve
    flow = q2
    
end function pump_curve_flow

!------------------------------------------------------------------------------

function find_orif_flow(link_index) result(orif_flow)
!====================================================================
! Find the flow through an orifice link.
!====================================================================
integer, intent(in ) :: link_index
real(8)              :: orif_flow

integer i, direction
real(8) c_orif, c_weir, f, head

    ! Check that link is an orifice
    orif_flow = 0d0       
    if (link_type(link_index) .ne. ORIFICE) return
    i = link_type_index(link_index)
    if (i < 1 .or. i > Norifs) return
      
    ! Control orifice's opening
    call apply_control(orifs(i)%control, T_GLOBAL, DT_GLOBAL)
    
    ! Discharge coeffs. for submerged & unsubmerged conditions
    call orifice_coeffs(link_index, c_orif, c_weir)
    
    ! Head across orifice & degree that opening is full (f)
    call orifice_head(link_index, head, f, direction)
    
    ! No flow if upstream water level below orifice opening (head <= 0)
    if (head <= 0d0) return
    
    ! Do not allow reverse flow if flap gate present
    if (direction .eq. -1 .and. orifs(i)%flapgate .eq. 1) return
    
    ! Compute flow through orifice:
    ! orifice opening not full so it acts as a weir
    if (f < 1d0) then
        orif_flow = direction * c_weir * f**1.5
    ! otherwise normal orifice eqation applies
    else
        orif_flow = direction * c_orif * sqrt(head) 
    end if    

    ! Limit orifice flow if necessary
    call limit_link_flow(link_index, orif_flow)    

end function find_orif_flow    


subroutine orifice_coeffs(link_index, c_orif, c_weir)
!====================================================================
! Computes effective flow coeficients for both submerged (c_orif)
! and unsubmerged (c_weir) orifices
!
! NOTE: Assumes orifice is a side orifice with rectangular shape.
!====================================================================
integer, intent(in ) :: link_index
real(8), intent(out) :: c_orif, c_weir

integer :: i
real(8) :: ht, area, f

    i = link_type_index(link_index)
    ht = orifs(i)%height * orifs(i)%control%setting / 1d2
    f = ht * orifs(i)%width * sqrt(2d0 * g)
    c_orif = orifs(i)%coeff * f
    c_weir = c_orif * sqrt(ht/2d0)
    
end subroutine orifice_coeffs


subroutine orifice_head(link_index, head, f, direction)
!====================================================================
! Finds the head drop across an orifice.
!
! NOTE: Assumes a side orifice.
!====================================================================
integer, intent(in ) :: link_index
real(8), intent(out) :: head
real(8), intent(out) :: f
integer, intent(out) :: direction

integer :: i, j1, j2;
real(8) :: h1, h2, hcrest, hcrown, hmidpt

    ! Indexes of end nodes and link's orifice
    j1 = Node1(link_index)
    j2 = Node2(link_index)
    i  = link_type_index(link_index)

    ! Heads at upstream & downstream nodes
    call find_link_heads(link_index, h1, h2, direction)
    
   ! Elevations of orifice crest and crown
    hcrest = junct_elev(j1) + orifs(i)%offset
    hcrown = hcrest + orifs(i)%height * orifs(i)%control%setting / 1d2
    hmidpt = (hcrest + hcrown) / 2d0

    ! Fraction of orifice opening filled by inlet water level
    f = 1d0
    if ( h1 < hcrown .and. hcrown > hcrest ) then
        f = (h1 - hcrest) / (hcrown - hcrest)
    end if

    ! Head seen by orifice
    if ( f < 1.0 ) then
        head = h1 - hcrest
    else if ( h2 < hmidpt ) then
        head = h1 - hmidpt
    else
        head = h1 - h2;
    end if

end subroutine orifice_head

!------------------------------------------------------------------------------   
    
function find_weir_flow(link_index) result(weir_flow)
!====================================================================
! Find the flow through a weir link.
!====================================================================
integer, intent(in ) :: link_index
real(8)              :: weir_flow

integer i, j1, direction
real(8) c_weir, c_orif, f, hcrest, hcrown, head, h1, h2

    ! Check that link is a weir
    weir_flow = 0d0       
    if (link_type(link_index) .ne. WEIR) return
    i = link_type_index(link_index)
    if (i < 1 .or. i > Nweirs) return
      
    ! Control weir's opening
    call apply_control(weirs(i)%control, T_GLOBAL, DT_GLOBAL)
 
   ! Elevations of weir crest and crown
    j1 = Node1(link_index)
    hcrest = junct_elev(j1) + weirs(i)%offset
    hcrown = hcrest + weirs(i)%height

    ! Adjusted crest ht. for partially open weir
    hcrest = hcrest + (1.0 - weirs(i)%control%setting/100d0) * weirs(i)%height
    
    ! Heads on upstream side (h1) & downstream side (h2) of weir
    call find_link_heads(link_index, h1, h2, direction)
    
    ! No flow if upstream water level below weir crest
    if (h1 <= hcrest) return
    
    ! Weir is surcharged
    if (h1 >= hcrown) then
        
        ! Treat it as an orifice if it can surcharge
        if (weirs(i)%can_surcharge .eq. 1) then
            weir_flow = direction * &
                surcharged_weir_flow(link_index, hcrest, hcrown, h1, h2)
            call limit_link_flow(link_index, weir_flow)    
            return
            
        ! Otherwise limit head to height of opening
        else
            head = hcrown - hcrest
        end if

    ! Weir not surcharged so head is height above crest
    else
        head = h1 - hcrest
    end if
    
    ! Compute flow through weir
    weir_flow = direction * normal_weir_flow(link_index, head)
    
    ! Apply Villemonte eqn. to correct for submergence
    if (h2 > hcrest) then
        call Villemonte_correction(h1, h2, hcrest, weir_flow)
    end if

    ! Limit weir flow if necessary
    call limit_link_flow(link_index, weir_flow)

end function find_weir_flow


function surcharged_weir_flow(link_index, hcrest, hcrown, h1, h2) result(weir_flow)
!====================================================================
! Finds flow through a surcharged weir acting as an orifice
!====================================================================
integer, intent(in) :: link_index
real(8), intent(in) :: hcrest, hcrown, h1, h2
real(8) :: weir_flow

integer :: i
real(8) :: y, head, c_orif

    ! Head seen by equivalent orifice
    y = (hcrest + hcrown) / 2d0
    if ( h2 < y ) then
        head = h1 - y
    else
        head = h1 - h2
    end if
    
    ! Flow coeff. for equivalent orifice
    y = hcrown - hcrest
    c_orif = normal_weir_flow(link_index, y) / sqrt(y/2d0)
    
    ! Flow through equivalent orifice
    weir_flow = c_orif * sqrt(head)

end function surcharged_weir_flow


function normal_weir_flow(link_index, head) result(weir_flow)
!====================================================================
! Computes flow through a weir using the standard weir formula.
!====================================================================
integer, intent(in) :: link_index
real(8), intent(in) :: head
real(8) :: weir_flow

integer :: i
real(8) :: length

    i = link_type_index(link_index)
    length = weirs(i)%length - 0.1 * weirs(i)%contractions * head
    length = max(length, 0d0)
    weir_flow = weirs(i)%coeff * length * head**1.5

end function normal_weir_flow


subroutine Villemonte_correction(h1, h2, hcrest, flow)
!====================================================================
! Applies Villemonte correction for submerged flow over a weir.
!====================================================================
real(8), intent(in)    :: h1, h2, hcrest
real(8), intent(inout) :: flow

real(8) :: ratio, vc

    ratio = (h2 - hcrest) / (h1 - hcrest)
    vc = 1.0d0 - ratio**1.85
    flow = flow * vc**0.385d0

end subroutine Villemonte_correction

!------------------------------------------------------------------------------

function find_outlet_flow(link_index) result(outlet_flow)
!====================================================================
! Find the flow through an outlet link.
!====================================================================
integer, intent(in ) :: link_index
real(8)              :: outlet_flow

integer :: i, k, j1, direction
real(8) :: h1, h2, hcrest

    ! Check that link is an outlet
    outlet_flow = 0d0       
    if (link_type(link_index) .ne. OUTLET) return
    i = link_type_index(link_index)
    if (i < 1 .or. i > Noutlets) return

    ! Check that outlet has a rating curve
    k = outlets(i)%rating_curve
    if (k < 1 .or. k > Ncurves) return
    
    ! Elevation of outlet opening
    j1 =  Node1(link_index)
    hcrest = junct_elev(j1) + outlets(i)%offset
    
    ! Heads at end nodes of outlet link
    call find_link_heads(link_index, h1, h2, direction)
    
    ! No reverse flow if outlet has a flap gate
    if (direction < 0 .and. outlets(i)%flapgate .eq. 1) return
    
    ! No flow if upstream head below bottom of outlet opening
    if (h1 <= hcrest) return
    
    ! Use rating curve to find outlet flow
    h1 = h1 - hcrest
    outlet_flow = direction * table_lookup(curve(k), h1, .TRUE.)    
    
end function find_outlet_flow

!------------------------------------------------------------------------------

subroutine apply_control(control, time, dt)
!====================================================================
! Finds a new control setting for a pump or regulator link
!====================================================================
type(control_t), intent(inout ) :: control   ! control data
real(8),         intent(in )    :: time      ! elapsed time
real(8),         intent(in )    :: dt        ! time step

real(8) :: adjustment, delta
integer :: i, j, k

associate (setting         => control%setting, &
           target_setting  => control%target_setting, &
           adjust_rate     => control%adjust_rate)

    ! Retrieve control parameters
    i = control%tseries  ! Control time series index
    j = control%node     ! Control node index
    k = control%curve    ! Control curve index
    
    ! Find a new target setting
    if (i > 0) then
        target_setting = table_tseries_lookup(tseries(i), time, .FALSE., .TRUE.)
    else if (j > 0 .and. k > 0) then
        target_setting = table_lookup(curve(k), yres_jun_old(j), .FALSE.)
    else
        target_setting = setting
    end if
    
    ! New setting equals target if current setting equals target
    ! or adustment rate is 0 
    if (target_setting .eq. setting .or. adjust_rate .eq. 0d0) then
        setting = target_setting
        
    ! Otherwise change in current setting is fractional difference between it
    ! and target achieved over the current time step
    else
        delta = (target_setting - setting) / 100d0
        adjustment = dt / (adjust_rate * 60d0)
        if (adjustment + 0.001 >= abs(delta)) then
            setting = target_setting
        else
            setting = setting + sign(1d0, delta)*adjustment*100d0
        end if
    end if
    
    ! Make sure new setting is feasible
    setting = min(setting, 100d0)
    setting = max(setting, 0d0)

end associate
end subroutine apply_control


subroutine find_link_heads(link_index, h1, h2, direction)
!====================================================================
! Finds heads on either side of link.
!====================================================================
integer, intent(in ) :: link_index
real(8), intent(out) :: h1
real(8), intent(out) :: h2
integer, intent(out) :: direction

integer :: j1, j2
real(8) :: head

    ! Get indexes of end nodes
    j1 = Node1(link_index)
    j2 = Node2(link_index)

    ! Find heads at upstream & downstream nodes
    h1 = yres_jun_old(j1) + junct_elev(j1)
    h2 = yres_jun_old(j2) + junct_elev(j2)
    direction = 1
    
    ! Exchange h1 and h2 for reverse flow
    if ( h2 > h1 ) then
        head = h1
        h1 = h2
        h2 = head
        direction = -1
    end if

end subroutine find_link_heads    


subroutine limit_link_flow(link_index, link_flow) 
!====================================================================
! Limit link flow to prevent inlet node from going dry and
! outlet node from overflowing
!====================================================================
integer, intent(in   ) :: link_index
real(8), intent(inout) :: link_flow
real(8) :: new_depth
integer :: j

    ! Prevent depth at link inlet node from dropping below 0.3 m
    j = Node1(link_index)
    if (link_flow < 0d0) j = Node2(link_index)
    if (BCnode(j) .eq. RESERVOIR) then
        call limit_reserv_outflow(j, link_flow)
    else if (Ares_junct(j) > 0d0) then   
        new_depth = yres_jun_old(j) - link_flow * DT_GLOBAL / Ares_junct(j)
        if (new_depth < 0d0) then
            link_flow = yres_jun_old(j) * Ares_junct(j) / DT_GLOBAL
        end if
    end if
        
    ! Prevent depth at link outlet node from exceeding max. depth
    j = Node2(link_index)
    if (link_flow < 0d0) j = Node1(link_index)
    if (BCnode(j) .eq. RESERVOIR) then
        call limit_reserv_inflow(j, link_flow)
    else if (Ares_junct(j) > 0d0) then
        new_depth = yres_jun_old(j) + link_flow*DT_GLOBAL/Ares_junct(j)
        if (new_depth > hdrops_overf(j)) then
            link_flow = (hdrops_overf(j) - yres_jun_old(j)) * Ares_junct(j) / DT_GLOBAL
        end if
    end if

end subroutine limit_link_flow


subroutine limit_reserv_outflow(node_index, link_flow)
!====================================================================
! Limit link flow to prevent Reservoir node from going dry.
!====================================================================
integer, intent(in   ) :: node_index
real(8), intent(inout) :: link_flow

real(8) :: new_depth, min_depth, old_volume, new_volume

    if (BCnode(node_index) .ne. RESERVOIR) return
    min_depth = 0.30
    call itm_get_storage_volume(node_index, yres_jun_old(node_index), old_volume)
    new_volume = old_volume - link_flow * DT_GLOBAL
    call itm_get_storage_depth(node_index, new_volume, new_depth)
    if (new_depth <= min_depth) then
        call itm_get_storage_volume(node_index, min_depth, new_volume)
        link_flow = (old_volume - new_volume) / DT_GLOBAL
    end if
    
    end subroutine limit_reserv_outflow
    
    
subroutine limit_reserv_inflow(node_index, link_flow)    
!====================================================================
! Limit link flow to prevent Reservoir node from overflowing.
!====================================================================
integer, intent(in   ) :: node_index
real(8), intent(inout) :: link_flow

real(8) :: new_depth, min_depth, old_volume, new_volume
    
    if (BCnode(node_index) .ne. RESERVOIR) return
    call itm_get_storage_volume(node_index, yres_jun_old(node_index), old_volume)
    new_volume = old_volume + link_flow * DT_GLOBAL
    call itm_get_storage_depth(node_index, new_volume, new_depth)
    if (new_depth > reser_maxdepth(node_index)) then
        call itm_get_storage_volume(node_index, reser_maxdepth(node_index), new_volume)
        link_flow = (new_volume - old_volume) / DT_GLOBAL    
    end if       

end subroutine limit_reserv_inflow
        
end module itm_links
    