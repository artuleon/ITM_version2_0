!! This file is part of the ITM model.
!!
!! Copyright 2009 University of Illinois at Urbana-Champaign
!! Copyright 2011 Oregon State University, Corvallis
!!
!! Authors: Arturo S. Leon (Hydraulics), Nils Oberg (User interface)
!!
!! ITM is a free software; you can redistribute it and/or modify it
!! under the terms of the GNU General Public License as published
!! by the Free Software Foundation; either version 2.0 of the
!! License, or (at your option) any later version.
!! 
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!! 
!! You should have received a copy of the GNU General Public License
!! along with this program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!
    
module itm_hotstart
use common_module
implicit none

contains
    
subroutine itm_hotstart_use()
!==============================================================================
! Initialize the state of the ITM system with a hotstart file.
!==============================================================================
integer :: error_code

    if (len(trim(hsfile_use)) == 0) return
    open(77, file=hsfile_use, status='OLD', form='BINARY', iostat=error_code)
    if (error_code .ne. 0) then
        write(98,*) ' Could not open hotstart file ', hsfile_use
        GLOBAL_STATUS_FLAG = 1
        return
    end if
    if (itm_hotstart_verify() == .TRUE.) then
        call itm_hotstart_read()
    else
        write(98,*) ' Hot start file is not compatible with current project.'
        GLOBAL_STATUS_FLAG = 1
    end if
    close(77)
    
end subroutine itm_hotstart_use
        
    
subroutine itm_hotstart_save()
!==============================================================================
! Save the state of the ITM system to a hotstart file.
!==============================================================================
integer :: error_code

    if (len(trim(hsfile_save)) == 0) return
    open(77, file=hsfile_save, status='REPLACE', form='BINARY', iostat=error_code)
    if (error_code .ne. 0) then
        write(98,*) ' Could not open hotstart file ', hsfile_use
        GLOBAL_STATUS_FLAG = 1
        return
    end if
    call itm_hotstart_write()
    close(77)
    
end subroutine itm_hotstart_save
    
    
subroutine itm_hotstart_write()
!==============================================================================
! Write the contents of a hotstart file.
!==============================================================================
    
    write(77) 376
    write(77) Nnodes
    write(77) Nlinks
    write(77) Npipes
    write(77) Num_max_cells
    write(77) Volume_stored_current_step
    
    write(77) IdFlow
    write(77) Qbound
    write(77) h0
    write(77) A0
    write(77) Q0
    write(77) Atemp1
    write(77) htemp1
    write(77) Qtemp1
    write(77) IdFlow1
    write(77) yres_jun_old
    write(77) Fupst
    write(77) Fdownst
    write(77) h0_Rec

    write(77) Cd_gate
    write(77) Hgate_open
    write(77) h_gate_m
    
end subroutine itm_hotstart_write


function itm_hotstart_verify result(verified)
!==============================================================================
! Verify that a hotstart file belongs to the current ITM project.
!==============================================================================
logical :: verified
integer :: version, nodes, links, pipes, cells

    verified = .FALSE.
    read(77) version, nodes, links, pipes, cells
    if (version /= 376) return
    if (nodes /= Nnodes) return
    if (links /= Nlinks) return
    if (pipes /= Npipes) return
    if (cells /= Num_max_cells) return
    verified = .TRUE.
    
end function itm_hotstart_verify


subroutine itm_hotstart_read
!==============================================================================
! Read the contents of a hotstart file.
!==============================================================================

    read(77) Initial_volume_stored
    
    read(77) IdFlow
    read(77) Qbound
    read(77) h0
    read(77) A0
    read(77) Q0
    read(77) Atemp1
    read(77) htemp1
    read(77) Qtemp1
    read(77) IdFlow1
    read(77) yres_jun_old
    read(77) Fupst
    read(77) Fdownst
    read(77) h0_Rec
    read(77) Cd_gate
    read(77) Hgate_open
    read(77) h_gate_m

end subroutine itm_hotstart_read

end module itm_hotstart

