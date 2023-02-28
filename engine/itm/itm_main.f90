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

!==============================================================================
! This file is the main module of the ITM model. 
!==============================================================================

!DEC$ IF DEFINED (ITM_USE_DLL)
!DEC$ ELSE

program ITM_MAIN
!==============================================================================
! Entry point for ITM when compiled as a command line executable. 
!==============================================================================
use common_module
implicit none 

character(1024) :: input_file, report_file, output_file, itm_file
integer :: num_args

    num_args = iargc()
    if (num_args < 4) then
        write(*,*) 'Must pass input, report, and output files to program.'
        GLOBAL_STATUS_FLAG = 1
        return
    endif

    call getarg(1, input_file)
    call getarg(2, report_file)
    call getarg(3, output_file)
    call getarg(4, itm_file)

    ITM_DLL_INIT(input_file, report_file, output_file, itm_file, 'debug.txt', &
        len(input_file), len(report_file), len(output_file), len(itm_file),   &
        len('debug.txt'))
    
    
     
    !Loop on time             
    do while (T_GLOBAL.lt.TMAX)
        ITM_EXEC_STEP
    end do      
    
    call ITM_DLL_END()
    
end program ITM_MAIN

!DEC$ ENDIF


function ITM_DLL_INIT(input_file, report_file, output_file, itm_file,  &
                      debug_file, input_file_len, report_file_len,     &
                      output_file_len, itm_file_len, debug_file_len)
!==============================================================================
! Initialize the ITM system.
!==============================================================================

!DEC$ IF DEFINED (ITM_USE_DLL)
!DEC$ ATTRIBUTES DLLEXPORT :: ITM_DLL_INIT
!DEC$ ATTRIBUTES STDCALL   :: ITM_DLL_INIT
!DEC$ ATTRIBUTES REFERENCE :: input_file, report_file, output_file
!DEC$ ATTRIBUTES REFERENCE :: itm_file, debug_file
!DEC$ ENDIF

use common_module
use itm_input
use itm_output
use itm_hotstart
implicit none 

integer, intent(in) :: input_file_len
integer, intent(in) :: report_file_len
integer, intent(in) :: output_file_len
integer, intent(in) :: itm_file_len
integer, intent(in) :: debug_file_len
character (len=input_file_len), intent(in)  :: input_file
character (len=report_file_len), intent(in) :: report_file
character (len=output_file_len), intent(in) :: output_file
character (len=itm_file_len), intent(in)    :: itm_file
character (len=debug_file_len), intent(in)  :: debug_file
integer :: ITM_DLL_INIT
 
!DEC$ IF DEFINED (_WIN32)
    call enable_64_bit_precision()
!DEC$ ENDIF

    call CPU_TIME(TIME_BEGIN)
    GLOBAL_STATUS_FLAG = 0
    T_GLOBAL = 0
    DT_GLOBAL = 0
    open(98, file = report_file, status='REPLACE')
    open(99, file = debug_file, status='REPLACE')

    ! Read input data
    call itm_input_open(input_file)
    
    ! Initialize ITM solver
    if (GLOBAL_STATUS_FLAG == 0) call init()
    
    ! Open output files
    if (GLOBAL_STATUS_FLAG == 0) call itm_output_open(output_file, itm_file)
    
    ! Use hotstart file if supplied
    if (GLOBAL_STATUS_FLAG == 0) call itm_hotstart_use()        
    ITM_DLL_INIT = GLOBAL_STATUS_FLAG

end function ITM_DLL_INIT


function ITM_DLL_END()
!==============================================================================
! Close down the ITM system
!==============================================================================
!DEC$ IF DEFINED (ITM_USE_DLL)
!DEC$ ATTRIBUTES DLLEXPORT :: ITM_DLL_END
!DEC$ ATTRIBUTES STDCALL   :: ITM_DLL_END
!DEC$ ENDIF

use common_module
use itm_input
use itm_output
use itm_deallocate
use itm_hotstart
integer :: ITM_DLL_END

    ! Save_hotstart file if specified
    if (GLOBAL_STATUS_FLAG == 0) call itm_hotstart_save()
    
    ! Write summary results to report file
    call CPU_TIME (time_end)
    call itm_output_write_report()
    
    ! Close down the ITM system
    call itm_deallocate_all()
    call itm_output_close()
    call itm_input_close()
    close(99)
    close(98)
    ITM_DLL_END = GLOBAL_STATUS_FLAG
    
end function ITM_DLL_END


function ITM_EXEC_STEP(CurTime)
!==============================================================================
! Solve the ITM system over a single time step.
!==============================================================================
!DEC$ IF DEFINED (ITM_USE_DLL)
!DEC$ ATTRIBUTES DLLEXPORT :: ITM_EXEC_STEP
!DEC$ ATTRIBUTES STDCALL   :: ITM_EXEC_STEP
!DEC$ ATTRIBUTES REFERENCE :: CurTime
!DEC$ ENDIF

use common_module
use itm_solver
use itm_output
implicit none 
    
real(8), intent(inout) :: CurTime
integer :: ITM_EXEC_STEP
real(8) :: Dt_Courant
    
    if (GLOBAL_STATUS_FLAG .ne. 0) then
        ITM_EXEC_STEP = GLOBAL_STATUS_FLAG
        return
    endif   
    
    ! Time step: Courant criteria      
    number_steps = number_steps + 1     
    call CFL_time_step(T_GLOBAL, DT_GLOBAL, Dt_Courant)               
    DT_GLOBAL = Dt_Courant !time step for Courant criteria      
    
    ! Determine if we should save the results at this point
    istor = 0
    if (T_GLOBAL + 0.001 >= T_NEXT_REPORT) then
        istor = 1
    else if (T_GLOBAL + DT_GLOBAL > T_NEXT_REPORT) then
        DT_GLOBAL = T_NEXT_REPORT - T_GLOBAL
    endif
    
    ! Solve system heads & flows at current time step
    call itm_solver_exec_step()
    if (GLOBAL_STATUS_FLAG .ne. 0) then
        CurTime = 0d0
        ITM_EXEC_STEP = GLOBAL_STATUS_FLAG
        return
    end if
    
    ! Save results to file
    if (istor .eq. 1) then
        call itm_output_save_results(T_NEXT_REPORT)      
        T_NEXT_REPORT = T_NEXT_REPORT + Tstor
    end if
    
    ! Assign new storage as old storage 
    Vol_stored_old_time = Volume_stored_current_step      
     
    ! Update elapsed time 
    T_GLOBAL = T_GLOBAL + DT_GLOBAL
    
    ! Check if no more time remains
    if (T_GLOBAL >= TMAX) then
        
        ! Save current results if next report time reached
        if (T_GLOBAL + 0.001 >= T_NEXT_REPORT) then  
            call itm_output_save_results(T_NEXT_REPORT)
        end if
        
        ! Set current time to 0 marking end of simulation
        CurTime = 0d0
        
    ! Otherwise update current time
    else
        CurTime = T_GLOBAL
    endif
      
    ITM_EXEC_STEP = GLOBAL_STATUS_FLAG
    
end function ITM_EXEC_STEP


function ITM_GET_MASS_BAL_ERROR(continuityError)
!==============================================================================
! Retrieve the simulation's mass balance error.
!==============================================================================
!DEC$ IF DEFINED (ITM_USE_DLL)
!DEC$ ATTRIBUTES DLLEXPORT :: ITM_GET_MASS_BAL_ERROR
!DEC$ ATTRIBUTES STDCALL   :: ITM_GET_MASS_BAL_ERROR
!DEC$ ATTRIBUTES REFERENCE :: continuityError
!DEC$ ENDIF

use common_module
implicit none 
    
integer ITM_GET_MASS_BAL_ERROR
real(8), intent(out) :: continuityError

    continuityError = Error_Volume
    ITM_GET_MASS_BAL_ERROR = GLOBAL_STATUS_FLAG
    
end function ITM_GET_MASS_BAL_ERROR

    
subroutine enable_64_bit_precision()
!==============================================================================
! Enables 64-bit floating point precision. Should only be used for a Win32
! build configuration.
!==============================================================================
USE DFLIB
implicit none 
integer(2) :: control, holdcontrol, newcontrol

    call GETCONTROLFPQQ(control)
    ! Clear any existing precision flags.
    holdcontrol = (control .AND. (.NOT. FPCW$MCW_PC))
    newcontrol = holdcontrol .OR. FPCW$64
    ! Set precision to 64 bits.
    call SETCONTROLFPQQ(newcontrol)

end subroutine enable_64_bit_precision    
