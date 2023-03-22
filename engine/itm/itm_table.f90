!******************************************************************************
!Project:      ITM (Illinois Transient Model)
!Version:      2.0
!Module:       itm_table
!Description:  represents a table of X-Y values
!Authors:      see AUTHORS
!Copyright:    see LICENSE
!License:      see LICENSE
!Last Updated: 03/01/2023
!******************************************************************************

! This module defines a derived type used to store pairs of double
! precision values in a 2-D table that can represent either the X-Y
! data points of a curve or the time-value data in a time series.
! A table's data entries must have X-values in ascending order.
    
module itm_table
implicit none

type table_t
  integer :: size = 0       ! number of entries in the table
  integer :: capacity = 0   ! maximum size of the table
  integer :: index = 1      ! current position in the table
  real(8), dimension(:), allocatable :: x   ! array of X-values
  real(8), dimension(:), allocatable :: y   ! array of Y-values
end type table_t
    
contains
    
subroutine table_init(table, n)
!====================================================================
! Create a table with capacity n.
!====================================================================
type(table_t), intent(inout) :: table
integer,       intent(in   ) :: n
  
    ! Delete the table if it already exists
    call table_delete(table)
  
    ! Create a table of capacity n
    allocate(table%x(n))
    allocate(table%y(n))
    if (allocated(table%x) .AND. allocated(table%y)) then
        table%capacity = n
    end if
    
end subroutine table_init


subroutine table_delete(table)
!====================================================================
! Deallocate the memory used by a table.
!====================================================================
type(table_t), intent(inout) :: table
    
    table%capacity = 0
    table%size = 0
    table%index = 1
    if (allocated(table%y)) deallocate(table%y)     
    if (allocated(table%x)) deallocate(table%x)
    
end subroutine table_delete


subroutine table_append(table, x_value, y_value)
!====================================================================
! Append a new data point to a table.
!====================================================================
type(table_t),    intent(inout) :: table
real(8),          intent(in   ) :: x_value, y_value
  
    if (table%size < table%capacity) then
        table%size = table%size + 1
        table%x(table%size) = x_value
        table%y(table%size) = y_value
    end if

end subroutine table_append


function table_get_x_max(table) result(x_max)
!====================================================================
! Return the largest x-value in a table.
! (Table entries have x-values in ascending order.)
!====================================================================
type(table_t), intent(in) :: table
real(8) :: x_max

    x_max = 0d0
    if (table%size > 0) then
        x_max = table%x(table%size)
    end if

end function table_get_x_max


function table_lookup(table, x_value, interpolate) result(y_value)
!====================================================================
! Return the y-value associated with an x-value in a table.
!====================================================================
type(table_t), intent(in) :: table
real(8),       intent(in) :: x_value
logical,       intent(in) :: interpolate
real(8) :: y_value
integer :: i
associate (size => table%size, &
           x    => table%x, &
           y    => table%y)
    
    if (size == 0) then
        y_value = 0
        return    
    end if
    if (x_value <= x(1)) then
        y_value = y(1)
    else if (x_value >= x(size)) then
        y_value = y(size)
    else
        do i = 2, size
            
            if (x_value < x(i)) then
                if (interpolate == .TRUE.) then
                    y_value = table_interpolate(x_value, x(i-1), y(i-1), x(i), y(i))
                else
                    y_value = y(i-1)
                end if
                exit
            end if
            
        end do
    end if
    
end associate
end function table_lookup


function table_reverse_lookup(table, y_value, interpolate) result(x_value)
!====================================================================
! Return the x-value associated with a y-value in a table.
! Assumes y-entries are either in ascending or descending order.
!====================================================================
type(table_t), intent(in) :: table
real(8),       intent(in) :: y_value
logical,       intent(in) :: interpolate
real(8) :: x_value
integer :: i
logical :: y_ascending
associate (size => table%size, &
           x    => table%x, &
           y    => table%y)
    
    ! Table has no entries
    if (size == 0) then
        x_value = 0
        return    
    end if
    
    ! y-values are in ascending order
    y_ascending = (y(size) > y(1))
    if (y_ascending) then
        ! lookup y is below first entry
        if (y_value <= y(1)) then
            x_value = x(1)
            return
        end if
        ! lookup y is beyond last entry
        if (y_value >= y(size)) then 
            x_value = x(size)
            return
        end if
        ! bracket lookup y and interpolate
        do i = 2, size
            if (y_value < y(i)) then
                if (interpolate == .TRUE.) then
                    x_value = table_interpolate(y_value, y(i-1), x(i-1), y(i), x(i))
                else
                    x_value = x(i-1)
                end if
                return
            end if
        end do
        
    ! y-values are in descending order    
    else
        ! lookup y is above first entry
        if (y_value >= y(1)) then
            x_value = x(1)
            return
        end if
        ! lookup y is below last entry
        if (y_value <= y(size)) then
            x_value = x(size)
            return
        end if
        ! bracket lookup y and interpolate
        do i = 2, size
            if (y_value > y(i)) then
                if (interpolate == .TRUE.) then
                    x_value = table_interpolate(y_value, y(i-1), x(i-1), y(i), x(i))
                else
                    x_value = x(i-1)
                end if
                return
            end if
        end do
    end if
end associate
end function table_reverse_lookup


function table_tseries_lookup(table, time, interpolate, extend) result(value)
!====================================================================
! Return the value associated with a time in a time series table.
!====================================================================
type(table_t),    intent(inout) :: table
real(8),          intent(in   ) :: time
logical,          intent(in   ) :: interpolate
logical,          intent(in   ) :: extend
real(8) :: value
integer :: i
associate (size   => table%size, &
           index  => table%index, &
           times  => table%x, &
           values => table%y)
    
    if (size == 0) then
        value = 0
        return
    end if
        
    ! time is before start of table
    if (time < times(1)) then
        if (extend == .TRUE.) then
            value = values(1)
        else
            value = 0.0
        end if
            
    ! time is beyond end of table
    else if (time > times(size)) then
        if (extend == .TRUE.) then
            value = values(size)
        else
          value = 0.0
        end if

    else if (index == size) then
        value = values(size)

    ! time lies within table, at or beyond current table index
    else
        do i = index + 1, size
            if (time < times(i)) then
                
                if (interpolate == .TRUE.) then
                    value = table_interpolate(time, times(i-1), values(i-1), times(i), values(i))
                else
                    value = values(i-1)
                end if
                table%index = i - 1;
                exit
            end if
        end do
    end if
    
end associate
end function table_tseries_lookup


function table_interpolate(x, x1, y1, x2, y2) result(y)
!====================================================================
! Linearly interpolate a y-value for x between points x1,y1 and x2,y2.
!====================================================================
real(8), intent(in) :: x, x1, y1, x2, y2
real(8) :: y
double precision :: dx

    dx = x2 - x1
    ! Avoid division by 0
    if (abs(dx) < epsilon(dx)) then
        y = (y1 + y2) / 2
    else
        y = y1 + (x - x1) * (y2 - y1) / dx
    end if
    
end function table_interpolate

!--------------------------------------------------------------------

end module itm_table
