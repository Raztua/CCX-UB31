!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     Module holding shared data for UCONN6 6-DOF connector element
!     and ASCE 41-17 nonlinear plastic hinge constitutive tracking.
!
      module uconn6_module
        implicit none
        real*8, allocatable, save :: uconn6_stiff(:,:)
        real*8, allocatable, save :: uconn6_tm(:,:,:)
        real*8, allocatable, save :: uconn6_forces(:,:)
        real*8, allocatable, save :: uconn6_rel_disp(:,:)
        integer, allocatable, save :: uconn6_is_nonlinear(:)
        real*8, allocatable, save :: uconn6_asce41(:,:)
        real*8, allocatable, save :: uconn6_state(:,:)
!
!       Output controls for *USER CONNECTOR OUTPUT
        integer, parameter :: UCONN_MAX_TARGETS = 50
        logical, save :: uconn_out_active = .false.
        integer, save :: uconn_out_num_targets = 0
        character*80, save :: uconn_out_target_filename(50)
        character*80, save :: uconn_out_target_elset(50)
        logical, save :: uconn_out_target_file_init(50) = .false.
        character*4, save :: uconn_out_inc_mode = 'LAST'
        integer, save :: uconn_out_inc_freq = 1
        integer, save :: uconn_out_inc_list(50) = 0
        integer, save :: uconn_out_ninc_list = 0
        character*6, save :: uconn_out_coords_mode = 'LOCAL '
        logical, save :: uconn_out_flag_f = .true.
        logical, save :: uconn_out_flag_u = .true.
        logical, save :: uconn_out_flag_state = .true.
        logical, allocatable, save :: uconn_out_elem_active(:,:)
      end module uconn6_module
