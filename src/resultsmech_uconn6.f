!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     Internal force and stress recovery for UCONN6 connector elements
!     with ASCE 41-17 plastic hinge state tracking.
!
      subroutine resultsmech_uconn6(co,kon,ipkon,lakon,ne,v,
     &  stx,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
     &  ielmat,ielorien,norien,orab,ntmat_,t0,t1,ithermal,prestr,
     &  iprestr,eme,iperturb,fn,iout,qa,vold,nmethod,
     &  veold,dtime,time,ttime,plicon,nplicon,plkcon,nplkcon,
     &  xstateini,xstiff,xstate,npmat_,matname,mi,ielas,icmd,
     &  ncmat_,nstate_,stiini,vini,ener,eei,enerini,istep,iinc,
     &  reltime,calcul_fn,calcul_qa,calcul_cauchy,nener,
     &  ikin,nal,ne0,thicke,emeini,nelem,ielprop,prop,t0g,t1g)
!
      use uconn6_module
      implicit none
!
      character*8 lakon(*)
      character*80 matname(*)
!
      integer kon(*),mi(*),
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(mi(3),*),
     &  ielorien(mi(3),*),ntmat_,ipkon(*),ne0,
     &  istep,iinc,ne,ithermal(*),iprestr,
     &  nener,norien,iperturb(*),iout,
     &  nal,icmd,nmethod,ielas,
     &  ncmat_,nstate_,ikin,ielprop(*),
     &  nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_,calcul_fn,
     &  calcul_cauchy,calcul_qa,nelem,
     &  n1,n2,i,j,k,ip,nl_dof,perf_level,itarg,iunit
      logical is_open, has_target, is_active_inc
      character*10 perf_str
!
      real*8 co(3,*),v(0:mi(2),*),stiini(6,mi(1),*),t0g(2,*),t1g(2,*),
     &     stx(6,mi(1),*),prop(*),elcon(0:ncmat_,ntmat_,*),
     &     rhcon(0:1,ntmat_,*),alcon(0:6,ntmat_,*),vini(0:mi(2),*),
     &     alzero(*),orab(7,*),fn(0:mi(2),*),t0(*),t1(*),
     &     prestr(6,mi(1),*),eme(6,mi(1),*),vold(0:mi(2),*),
     &     ener(2,mi(1),*),eei(6,mi(1),*),enerini(2,mi(1),*),
     &     veold(0:mi(2),*),qa(*),dtime,time,ttime,
     &     plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &     xstiff(27,mi(1),*),xstate(nstate_,mi(1),*),
     &     xstateini(nstate_,mi(1),*),reltime,thicke(mi(3),*),
     &     emeini(6,mi(1),*),
     &     kstiff(6),t_mat(3,3),
     &     u1(6),u2(6),du_glob(6),du_loc(6),f_loc(6),f_glob(6),
     &     f_res,k_tang,f_out(6),du_out(6),yield_ratio,
     &     plastic_def,k_tang_val,my_val,th_y_val
!
      n1 = kon(ipkon(nelem) + 1)
      n2 = kon(ipkon(nelem) + 2)
!
      kstiff(:) = 0.0d0
      t_mat(:,:) = 0.0d0
      t_mat(1,1) = 1.0d0
      t_mat(2,2) = 1.0d0
      t_mat(3,3) = 1.0d0
!
      if(allocated(uconn6_stiff)) then
         if(nelem.le.size(uconn6_stiff, 2)) then
            kstiff(:) = uconn6_stiff(:, nelem)
            t_mat(:,:) = uconn6_tm(:,:, nelem)
         endif
      elseif(ielprop(nelem).gt.0) then
         ip = ielprop(nelem)
         kstiff(1:6) = prop(ip:ip+5)
         t_mat(1,1) = prop(ip+6)
         t_mat(1,2) = prop(ip+7)
         t_mat(1,3) = prop(ip+8)
         t_mat(2,1) = prop(ip+9)
         t_mat(2,2) = prop(ip+10)
         t_mat(2,3) = prop(ip+11)
         t_mat(3,1) = prop(ip+12)
         t_mat(3,2) = prop(ip+13)
         t_mat(3,3) = prop(ip+14)
      endif
!
      do i=1,6
         u1(i) = v(i, n1)
         u2(i) = v(i, n2)
         du_glob(i) = u2(i) - u1(i)
      enddo
!
      du_loc(:) = 0.0d0
      do i=1,3
         do j=1,3
            du_loc(i)   = du_loc(i)   + t_mat(i,j) * du_glob(j)
            du_loc(i+3) = du_loc(i+3) + t_mat(i,j) * du_glob(j+3)
         enddo
      enddo
!
      do i=1,6
         f_loc(i) = kstiff(i) * du_loc(i)
      enddo
!
!     If nonlinear ASCE 41 plastic hinge active, compute resisting force
!
      if(allocated(uconn6_is_nonlinear)) then
         if(nelem.le.size(uconn6_is_nonlinear)) then
            if(uconn6_is_nonlinear(nelem).eq.1) then
               nl_dof = int(uconn6_asce41(8, nelem))
               if(nl_dof.ge.1.and.nl_dof.le.6) then
                  call uconn_asce41_eval(nelem, nl_dof, du_loc(nl_dof),
     &                                  f_res, k_tang, perf_level)
                  f_loc(nl_dof) = f_res
                  call uconn_asce41_update_state(nelem, nl_dof,
     &                 du_loc(nl_dof), f_res, k_tang, perf_level)
               endif
            endif
         endif
      endif
!
      if(allocated(uconn6_forces)) then
         if(nelem.le.size(uconn6_forces, 2)) then
            uconn6_forces(:, nelem) = f_loc(:)
            uconn6_rel_disp(:, nelem) = du_loc(:)
         endif
      endif
!
      do i=1,6
         stx(i, 1, nelem) = f_loc(i)
      enddo
!
!     Transform local forces back to global coordinates:
!     f_glob = T^T * f_loc
!
      f_glob(:) = 0.0d0
      do i=1,3
         do j=1,3
            f_glob(i)   = f_glob(i)   + t_mat(j,i) * f_loc(j)
            f_glob(i+3) = f_glob(i+3) + t_mat(j,i) * f_loc(j+3)
         enddo
      enddo
!
      if(calcul_fn.eq.1) then
         do i=1,6
            fn(i, n1) = fn(i, n1) - f_glob(i)
            fn(i, n2) = fn(i, n2) + f_glob(i)
         enddo
      endif
!
!     Stream output to CSV if *USER CONNECTOR OUTPUT is active
!
      if (.not. uconn_out_active) return
!
      call check_uconn_inc_active(istep, iinc, iout,
     &     uconn_out_inc_mode, uconn_out_inc_freq,
     &     uconn_out_inc_list, uconn_out_ninc_list, is_active_inc)
      if (.not. is_active_inc) return
!
      has_target = .false.
      if (allocated(uconn_out_elem_active)) then
         if (nelem .le. size(uconn_out_elem_active, 1)) then
            do itarg = 1, uconn_out_num_targets
               if (uconn_out_elem_active(nelem, itarg)) then
                  has_target = .true.
                  exit
               endif
            enddo
         endif
      endif
      if (.not. has_target) return
!
      do itarg = 1, uconn_out_num_targets
         if (uconn_out_elem_active(nelem, itarg)) then
            iunit = 80 + itarg
            inquire(unit=iunit, opened=is_open)
            if (.not. is_open) then
               if (istep .eq. 1 .and. iinc .le. 1 .and.
     &             .not. uconn_out_target_file_init(itarg)) then
                  open(iunit, file=trim(uconn_out_target_filename(itarg)
     &                 ), status='unknown', position='rewind')
                  call write_uconn_csv_header(iunit, uconn_out_flag_f,
     &                 uconn_out_flag_u, uconn_out_flag_state)
                  uconn_out_target_file_init(itarg) = .true.
               else
                  open(iunit, file=trim(uconn_out_target_filename(itarg)
     &                 ), status='unknown', position='append')
               endif
            endif
         endif
      enddo
!
      if (uconn_out_coords_mode .eq. 'GLOBAL') then
         f_out(:) = f_glob(:)
         du_out(:) = du_glob(:)
      else
         f_out(:) = f_loc(:)
         du_out(:) = du_loc(:)
      endif
!
      perf_str = 'Elastic'
      yield_ratio = 0.0d0
      plastic_def = 0.0d0
      k_tang_val = 0.0d0
!
      if (allocated(uconn6_is_nonlinear)) then
         if (nelem .le. size(uconn6_is_nonlinear)) then
            if (uconn6_is_nonlinear(nelem) .eq. 1) then
               nl_dof = int(uconn6_asce41(8, nelem))
               if (allocated(uconn6_state)) then
                  if (nelem .le. size(uconn6_state, 2)) then
                     perf_level = int(uconn6_state(6, nelem))
                  endif
               endif
               if (perf_level .eq. 0) then
                  perf_str = 'Elastic'
               elseif (perf_level .eq. 1) then
                  perf_str = 'IO'
               elseif (perf_level .eq. 2) then
                  perf_str = 'LS'
               elseif (perf_level .eq. 3) then
                  perf_str = 'CP'
               else
                  perf_str = 'Failure'
               endif
               my_val = uconn6_asce41(1, nelem)
               th_y_val = uconn6_asce41(2, nelem)
               if (my_val .gt. 1.0d-12) then
                  yield_ratio = dabs(f_loc(nl_dof)) / my_val
               endif
               if (th_y_val .gt. 1.0d-12) then
                  plastic_def = max(0.0d0, dabs(du_loc(nl_dof))-th_y_val
     &                              )
               endif
               k_tang_val = k_tang
            endif
         endif
      endif
!
      do itarg = 1, uconn_out_num_targets
         if (uconn_out_elem_active(nelem, itarg)) then
            iunit = 80 + itarg
            call write_uconn_row(iunit, istep, iinc, time, nelem,
     &           n1, n2, uconn_out_flag_f, f_out, uconn_out_flag_u,
     &           du_out, uconn_out_flag_state, perf_str, yield_ratio,
     &           plastic_def, k_tang_val)
            flush(iunit)
         endif
      enddo
!
      return
      end
!
! ======================================================================
!     Helper routine to write dynamic connector CSV header
! ======================================================================
      subroutine write_uconn_csv_header(iunit, flag_f, flag_u,
     &     flag_state)
      implicit none
      integer iunit
      logical flag_f, flag_u, flag_state
      character*500 h_line
!
      h_line = 'Step,Increment,Time,Element,Node1,Node2'
      if (flag_f) then
         h_line = trim(h_line) // ',Fx,Fy,Fz,Mx,My,Mz'
      endif
      if (flag_u) then
         h_line = trim(h_line) // ',dUx,dUy,dUz,dRotX,dRotY,dRotZ'
      endif
      if (flag_state) then
         h_line = trim(h_line) //
     &        ',ASCE41_State,Yield_Ratio,Plastic_Def,Tangent_K'
      endif
      write(iunit, '(A)') trim(h_line)
      end subroutine write_uconn_csv_header
!
! ======================================================================
!     Helper routine to write single connector CSV row
! ======================================================================
      subroutine write_uconn_row(iunit, istep, iinc, time_val,
     &     nelem, n1, n2, flag_f, f_v, flag_u, du_v, flag_state,
     &     state_str, y_ratio, p_def, k_t)
      implicit none
      integer iunit, istep, iinc, nelem, n1, n2
      logical flag_f, flag_u, flag_state
      real*8 time_val, f_v(6), du_v(6), y_ratio, p_def, k_t
      character*(*) state_str
      character*800 l_buf
      character*200 n_buf
!
      write(l_buf, '(I4,A,I6,A,F14.6,A,I8,A,I8,A,I8)')
     &     istep, ',', iinc, ',', time_val, ',', nelem, ',',
     &     n1, ',', n2
!
      if (flag_f) then
         write(n_buf, 1005)
     &        ',', f_v(1), ',', f_v(2), ',', f_v(3),
     &        ',', f_v(4), ',', f_v(5), ',', f_v(6)
 1005    format(A,F16.4,A,F16.4,A,F16.4,A,F16.4,A,F16.4,A,F16.4)
         l_buf = trim(l_buf) // trim(n_buf)
      endif
!
      if (flag_u) then
         write(n_buf, 1006)
     &        ',', du_v(1), ',', du_v(2), ',', du_v(3),
     &        ',', du_v(4), ',', du_v(5), ',', du_v(6)
 1006    format(A,F16.6,A,F16.6,A,F16.6,A,F16.6,A,F16.6,A,F16.6)
         l_buf = trim(l_buf) // trim(n_buf)
      endif
!
      if (flag_state) then
         write(n_buf, '(A,A,A,F12.4,A,F16.6,A,E16.6)')
     &        ',', trim(state_str), ',', y_ratio, ',', p_def,
     &        ',', k_t
         l_buf = trim(l_buf) // trim(n_buf)
      endif
!
      write(iunit, '(A)') trim(l_buf)
      end subroutine write_uconn_row
!
! ======================================================================
!     Helper routine to check if current increment produces output
! ======================================================================
      subroutine check_uconn_inc_active(istep, iinc, iout, inc_mode,
     &     inc_freq, inc_list, ninc_list, is_active)
      implicit none
      integer istep, iinc, iout, inc_freq, inc_list(50), ninc_list
      character*4 inc_mode
      logical is_active
      integer k
!
      is_active = .false.
      if (iout .le. 0 .and. iout .ne. -2) return
!
      if (inc_mode .eq. 'ALL ') then
         is_active = .true.
      elseif (inc_mode .eq. 'FREQ') then
         if (inc_freq .gt. 0) then
            if (iinc .gt. 0 .and. mod(iinc, inc_freq) .eq. 0) then
               is_active = .true.
            endif
         endif
         if (iout .eq. 2 .or. iout .eq. -2) is_active = .true.
      elseif (inc_mode .eq. 'LIST') then
         do k = 1, ninc_list
            if (iinc .eq. inc_list(k)) then
               is_active = .true.
               exit
            endif
         enddo
         if (iout .eq. 2 .or. iout .eq. -2) is_active = .true.
      else
         if (iout .eq. 2 .or. iout .eq. -2 .or. iinc .le. 1) then
            is_active = .true.
         endif
      endif
      end subroutine check_uconn_inc_active
