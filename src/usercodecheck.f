!     ==================================================================
!     usercodecheck.f
!     Parsers and Master Evaluation Engine for Beam Code Checking
!     Supports Eurocode 3 (EN 1993-1-1) and AISC 360-16 / 360-22
!     ==================================================================

!     ==================================================================
!     userbeamdesign: Parses *USER BEAM DESIGN
!     ==================================================================
      subroutine userbeamdesign(inpc,textpart,set,istartset,iendset,
     &  ialset,nset,lakon,ne,irstrt,istep,istat,n,key,iline,ipol,inl,
     &  ipoinp,inp,ipoinpc,ier)
      use ub31_module
      use codecheck_module
      implicit none

      character*1 inpc(*)
      character*8 lakon(*)
      character*81 set(*)
      character*132 textpart(16)
      integer istartset(*), iendset(*), ialset(*), irstrt(*)
      integer ipoinp(2,*), inp(3,*), ipoinpc(0:*)
      integer nset, ne, istep, istat, n, key, iline, ipol, inl, ier

      character*132 elset_name, token
      integer i, j, eq_pos
      logical has_elset
      real*8 val_num

      call ensure_codecheck_alloc(ne)

      has_elset = .false.
      elset_name = ' '

      do i = 2, n
         token = textpart(i)
         if (token(1:6) .eq. 'ELSET=') then
            elset_name = token(7:86)
            has_elset = .true.
         else
            call parse_design_keyval(token, elset_name, has_elset,
     &           set, nset, istartset, iendset, ialset, lakon, ne)
         endif
      enddo

      do
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if ((istat .lt. 0) .or. (key .eq. 1)) exit
         if (n .le. 0) cycle

         do i = 1, n
            token = textpart(i)
            eq_pos = index(token, '=')
            if (eq_pos .gt. 1) then
               call parse_design_keyval(token, elset_name, has_elset,
     &              set, nset, istartset, iendset, ialset, lakon, ne)
            else
               read(token, *, iostat=j) val_num
               if (j .eq. 0) then
                  if (i .eq. 1) call apply_design_param('LCR_Y', 
     &                 val_num, elset_name, has_elset, set, nset, 
     &                 istartset, iendset, ialset, lakon, ne)
                  if (i .eq. 2) call apply_design_param('LCR_Z', 
     &                 val_num, elset_name, has_elset, set, nset, 
     &                 istartset, iendset, ialset, lakon, ne)
                  if (i .eq. 3) call apply_design_param('L_LT',  
     &                 val_num, elset_name, has_elset, set, nset, 
     &                 istartset, iendset, ialset, lakon, ne)
                  if (i .eq. 4) call apply_design_param('C1',    
     &                 val_num, elset_name, has_elset, set, nset, 
     &                 istartset, iendset, ialset, lakon, ne)
                  if (i .eq. 5) call apply_design_param('FY',    
     &                 val_num, elset_name, has_elset, set, nset, 
     &                 istartset, iendset, ialset, lakon, ne)
               endif
            endif
         enddo
      enddo

      end subroutine userbeamdesign


!     ==================================================================
!     userbeamdesignoverrides: Parses *USER BEAM DESIGN OVERRIDES
!     ==================================================================
      subroutine userbeamdesignoverrides(inpc,textpart,set,istartset,
     &  iendset,ialset,nset,lakon,ne,irstrt,istep,istat,n,key,iline,
     &  ipol,inl,ipoinp,inp,ipoinpc,ier)
      use ub31_module
      use codecheck_module
      implicit none

      character*1 inpc(*)
      character*8 lakon(*)
      character*81 set(*)
      character*132 textpart(16)
      integer istartset(*), iendset(*), ialset(*), irstrt(*)
      integer ipoinp(2,*), inp(3,*), ipoinpc(0:*)
      integer nset, ne, istep, istat, n, key, iline, ipol, inl, ier

      character*80 col_names(16)
      integer num_cols, i, j, ielem, istat_val, eq_pos
      character*132 target_token, token, p_name
      real*8 val_num
      logical is_first_line, has_header_def
      logical is_hdr_tok

      call ensure_codecheck_alloc(ne)

      num_cols = 6
      has_header_def = .false.
      is_first_line = .true.

      col_names(1) = 'TARGET'
      col_names(2) = 'LCR_Y'
      col_names(3) = 'LCR_Z'
      col_names(4) = 'L_LT'
      col_names(5) = 'C1'
      col_names(6) = 'FY'

      do
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if ((istat .lt. 0) .or. (key .eq. 1)) exit
         if (n .le. 0) cycle

         if (is_first_line) then
            is_first_line = .false.
            if (is_hdr_tok(textpart(1))) then
               num_cols = min(n, 16)
               do i = 1, num_cols
                  col_names(i) = textpart(i)(1:80)
                  call clean_col_name(col_names(i))
               enddo
               has_header_def = .true.
               cycle
            endif
         endif

         target_token = textpart(1)
         call clean_col_name(target_token)
         if (len_trim(target_token) .eq. 0) cycle

         read(target_token, *, iostat=istat_val) ielem
         if (istat_val .eq. 0 .and. ielem .gt. 0) then
            do i = 2, n
               token = textpart(i)
               eq_pos = index(token, '=')
               if (eq_pos .gt. 1) then
                  p_name = token(1:eq_pos-1)
                  read(token(eq_pos+1:), *, iostat=j) val_num
                  if (j .eq. 0) then
                     call apply_single_elem_param(p_name, val_num, 
     &                    ielem, lakon, ne)
                  endif
               else
                  read(token, *, iostat=j) val_num
                  if (j .eq. 0 .and. i .le. num_cols) then
                     call apply_single_elem_param(col_names(i), 
     &                    val_num, ielem, lakon, ne)
                  endif
               endif
            enddo
         else
            do i = 2, n
               token = textpart(i)
               eq_pos = index(token, '=')
               if (eq_pos .gt. 1) then
                  p_name = token(1:eq_pos-1)
                  read(token(eq_pos+1:), *, iostat=j) val_num
                  if (j .eq. 0) then
                     call apply_design_param(p_name, val_num, 
     &                    target_token, .true., set, nset, 
     &                    istartset, iendset, ialset, lakon, ne)
                  endif
               else
                  read(token, *, iostat=j) val_num
                  if (j .eq. 0 .and. i .le. num_cols) then
                     call apply_design_param(col_names(i), val_num, 
     &                    target_token, .true., set, nset, 
     &                    istartset, iendset, ialset, lakon, ne)
                  endif
               endif
            enddo
         endif
      enddo

      end subroutine userbeamdesignoverrides


!     ==================================================================
!     userbeamcheck: Parses *USER BEAM CHECK
!     ==================================================================
      subroutine userbeamcheck(inpc,textpart,set,istartset,iendset,
     &  ialset,nset,lakon,ne,irstrt,istep,istat,n,key,iline,ipol,inl,
     &  ipoinp,inp,ipoinpc,jobnamec,ier)
      use ub31_module
      use codecheck_module
      implicit none

      character*1 inpc(*)
      character*8 lakon(*)
      character*81 set(*)
      character*132 textpart(16), jobnamec(*)
      integer istartset(*), iendset(*), ialset(*), irstrt(*)
      integer ipoinp(2,*), inp(3,*), ipoinpc(0:*)
      integer nset, ne, istep, istat, n, key, iline, ipol, inl, ier

      character*132 token
      integer i, j, len_job, istat_val

      call ensure_codecheck_alloc(ne)

      codecheck_active = .true.
      codecheck_code = 'EC3'
      codecheck_elset = ' '
      codecheck_filename = ' '
      codecheck_output_level = 1
      codecheck_subdivisions = out_subdivisions

      do i = 2, n
         token = textpart(i)
         if (token(1:5) .eq. 'CODE=') then
            codecheck_code = 'EC3'
         elseif (token(1:5) .eq. 'FILE=') then
            codecheck_filename = token(6:132)
         elseif (token(1:6) .eq. 'ELSET=') then
            codecheck_elset = token(7:86)
         elseif (token(1:7) .eq. 'OUTPUT=') then
            if (token(8:10) .eq. 'GOV') then
               codecheck_output_level = 2
            elseif (token(8:10) .eq. 'ENV') then
               codecheck_output_level = 3
            elseif (token(8:11) .eq. 'FAIL') then
               codecheck_output_level = 4
            else
               codecheck_output_level = 1
            endif
         elseif (token(1:13) .eq. 'SUBDIVISIONS=') then
            read(token(14:), *, iostat=istat_val) codecheck_subdivisions
            if (istat_val .ne. 0 .or. 
     &          codecheck_subdivisions .lt. 1) then
               codecheck_subdivisions = 10
            endif
         elseif (token(1:9) .eq. 'SEGMENTS=') then
            read(token(10:), *, iostat=istat_val) codecheck_subdivisions
            if (istat_val .ne. 0 .or. 
     &          codecheck_subdivisions .lt. 1) then
               codecheck_subdivisions = 10
            endif
         elseif (token(1:9) .eq. 'STATIONS=') then
            read(token(10:), *, iostat=istat_val) j
            if (istat_val .ne. 0 .or. j .lt. 2) then
               codecheck_subdivisions = 10
            else
               codecheck_subdivisions = j - 1
            endif
         endif
      enddo

      if (len_trim(codecheck_filename) .eq. 0) then
         len_job = len_trim(jobnamec(1))
         if (len_job .gt. 0) then
            codecheck_filename = jobnamec(1)(1:len_job)//
     &           '_ec3_codecheck.csv'
         else
            codecheck_filename = 'beam_ec3_codecheck.csv'
         endif
      endif

      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &     ipoinp,inp,ipoinpc)

      end subroutine userbeamcheck


!     ==================================================================
!     run_beam_codecheck: Evaluates beam code checking across all beams
!     ==================================================================
!     run_beam_codecheck: Evaluates beam code checking across all beams
!     ==================================================================
      subroutine run_beam_codecheck(co, kon, ipkon, lakon, ne, nk,
     &  ielmat, matname, istep, iinc, mi, ielprop, prop, jobnamec,
     &  set, nset, istartset, iendset, ialset)
      use ub31_module
      use codecheck_module
      implicit none

      integer mi(*)
      real*8 co(3,*), prop(*)
      integer kon(*), ipkon(*), ielmat(mi(3),*), ielprop(*)
      integer istartset(*), iendset(*), ialset(*)
      integer ne, nk, istep, iinc, nset
      character*8 lakon(*)
      character*80 matname(*)
      character*81 set(*)
      character*132 jobnamec(*)

      integer ielem, indexp, indexe, n1, n2, j, k, st_idx
      integer iset_filter, id_filter, ipos_f, csv_unit, num_checked
      integer num_pass, num_fail, max_struct_elem, top_k, itop, jtop
      integer gov_idx, n_stations, i_lo, i_hi
      real*8 xl(3,2), dl, sect_type, dims(6)
      real*8 fx_st, vy_st, vz_st, tx_st, my_st, mz_st
      real*8 lcr_y_eff, lcr_z_eff, l_lt_eff, c1_eff, fy_eff
      real*8 E_eff, nu_eff, gm0_eff, gm1_eff
      real*8 phic_eff, phib_eff, phiv_eff, phit_eff
      real*8 uc_tot_st, uc_ax_st, uc_sh_st, uc_bnd_st, uc_buck_st
      real*8 uc_ltb_st, uc_stab_st, xi_station, x_loc, r_pos, frac
      real*8 max_tot, max_ax, max_sh, max_bnd, max_stab, max_ltb
      real*8 max_struct_uc
      character*20 gov_mode_st, gov_mode_elem, max_struct_mode
      integer gov_st
      character*81 elset_f
      character*4 st_tag
      logical is_open
      logical, allocatable :: in_filter(:)

!     EC3 Precomputed Capacities
      real*8 Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z
      real*8 Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd
      real*8 lbar_y, lbar_z, Cmy, Cmz, CmLT
      integer iclass

!     Top-N member tracking for large models
      integer top_elem(10)
      real*8 top_uc(10)

!     High-Resolution Timers
      integer c_start, c_eval_done, c_io_done, c_rate, c_max
      real*8 t_eval_ms, t_io_ms, t_tot_ms

      if (.not. codecheck_active) return

      call system_clock(c_start, c_rate, c_max)

      call ensure_codecheck_alloc(ne)

      if (codecheck_evaluated_step .eq. istep .and. 
     &    codecheck_evaluated_inc .eq. iinc) return
      codecheck_evaluated_step = istep
      codecheck_evaluated_inc = iinc

      iset_filter = 0
      if (len_trim(codecheck_elset) .gt. 0) then
         elset_f = codecheck_elset
         elset_f(81:81) = ' '
         ipos_f = index(elset_f, ' ')
         if (ipos_f .le. 1) ipos_f = len_trim(elset_f) + 1
         elset_f(ipos_f:ipos_f) = 'E'
         call cident81(set, elset_f, nset, id_filter)
         if (id_filter .gt. 0) then
            if (elset_f .eq. set(id_filter)) iset_filter = id_filter
         endif
      endif

      allocate(in_filter(max(ne, 1)))
      in_filter = .true.
      if (iset_filter .gt. 0) then
         in_filter = .false.
         do j = istartset(iset_filter), iendset(iset_filter)
            if (ialset(j) .gt. 0) then
               if (ialset(j) .le. ne) in_filter(ialset(j)) = .true.
            else
               k = ialset(j-2)
               do
                  k = k - ialset(j)
                  if (k .ge. ialset(j-1)) exit
                  if (k .le. ne) in_filter(k) = .true.
               enddo
            endif
         enddo
      endif

      csv_unit = 99
      inquire(unit=csv_unit, opened=is_open)
      if (is_open) close(csv_unit)

      if (codecheck_output_level .ne. 3) then
         if (istep .le. 1 .and. iinc .le. 1) then
            open(csv_unit, file=trim(codecheck_filename),
     &           status='replace')
            write(csv_unit, '(A)') 'STEP,INC,ELEM,STATION,XI,X_LOC_M,'//
     &        'FX_N,VY_N,VZ_N,TX_NM,MY_NM,MZ_NM,UC_AX,UC_SH,UC_BND,'//
     &        'UC_LTB,UC_STAB,UC_TOT,GOV_MODE'
         else
            open(csv_unit, file=trim(codecheck_filename),
     &           status='unknown', position='append')
         endif
      endif

!     ==================================================================
!     1. Parallel Code-Check Evaluation Phase (OpenMP Multi-Core)
!     ==================================================================
!$omp parallel do default(shared)
!$omp& private(ielem, indexe, n1, n2, xl, dl, indexp, sect_type, dims)
!$omp& private(lcr_y_eff, lcr_z_eff, l_lt_eff, c1_eff, fy_eff, E_eff)
!$omp& private(nu_eff, gm0_eff, gm1_eff, phic_eff, phib_eff, phiv_eff)
!$omp& private(phit_eff, Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z)
!$omp& private(Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd, lbar_y, lbar_z)
!$omp& private(Cmy, Cmz, CmLT, iclass)
!$omp& private(max_tot, max_ax, max_sh, max_bnd, max_stab, max_ltb)
!$omp& private(gov_st, gov_mode_elem, st_idx, xi_station, x_loc)
!$omp& private(fx_st, vy_st, vz_st, tx_st, my_st, mz_st)
!$omp& private(uc_tot_st, uc_ax_st, uc_sh_st, uc_bnd_st, uc_buck_st)
!$omp& private(uc_ltb_st, uc_stab_st, gov_mode_st, gov_idx)
!$omp& private(n_stations, i_lo, i_hi, r_pos, frac)
!$omp& schedule(guided)
      do ielem = 1, ne
         if (ipkon(ielem) .lt. 0) cycle
         if (lakon(ielem)(1:2) .ne. 'UB') cycle
         if (.not. in_filter(ielem)) cycle

         indexe = ipkon(ielem)
         n1 = kon(indexe+1)
         n2 = kon(indexe+2)

         xl(1,1) = co(1,n1); xl(2,1) = co(2,n1); xl(3,1) = co(3,n1)
         xl(1,2) = co(1,n2); xl(2,2) = co(2,n2); xl(3,2) = co(3,n2)
         dl = dsqrt((xl(1,2)-xl(1,1))**2 + (xl(2,2)-xl(2,1))**2 + 
     &              (xl(3,2)-xl(3,1))**2)
         if (dl .le. 1.0d-6) dl = 1.0d0

         indexp = ielprop(ielem)
         if (indexp .le. 0) cycle

         sect_type = prop(indexp+1)
         dims(1)   = prop(indexp+2)
         dims(2)   = prop(indexp+3)
         dims(3)   = prop(indexp+4)
         dims(4)   = prop(indexp+5)
         dims(5)   = prop(indexp+6)
         dims(6)   = prop(indexp+7)

         if (lcr_y(ielem) .gt. 0.d0) then
            lcr_y_eff = lcr_y(ielem)
         else
            lcr_y_eff = k_y(ielem) * dl
         endif

         if (lcr_z(ielem) .gt. 0.d0) then
            lcr_z_eff = lcr_z(ielem)
         else
            lcr_z_eff = k_z(ielem) * dl
         endif

         if (l_lt(ielem) .gt. 0.d0) then
            l_lt_eff = l_lt(ielem)
         else
            l_lt_eff = k_lt(ielem) * dl
         endif

         c1_eff = c1_lt(ielem)
         if (fy_user(ielem) .gt. 0.d0) then
            fy_eff = fy_user(ielem)
         else
            fy_eff = elem_fy(ielem)
         endif

         E_eff    = elem_E(ielem)
         nu_eff   = elem_nu(ielem)
         gm0_eff  = gamma_m0(ielem)
         gm1_eff  = gamma_m1(ielem)
         phic_eff = phi_c(ielem)
         phib_eff = phi_b(ielem)
         phiv_eff = phi_v(ielem)
         phit_eff = phi_t(ielem)

!        Precompute EC3 Member-Level Capacities (Once per element)
         call calc_ec3_member_capacities(dl, lcr_y_eff, lcr_z_eff,
     &        l_lt_eff, c1_eff, fy_eff, E_eff, nu_eff, gm0_eff,
     &        gm1_eff, sect_type, dims, Nt_Rd, Nc_Rd, Vpl_y,
     &        Vpl_z, Mc_y, Mc_z, Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd,
     &        lbar_y, lbar_z, Cmy, Cmz, CmLT, iclass)

         max_tot = 0.d0
         max_ax = 0.d0
         max_sh = 0.d0
         max_bnd = 0.d0
         max_stab = 0.d0
         max_ltb = 0.d0
         gov_st = 0
         gov_mode_elem = ' '

         n_stations = codecheck_subdivisions + 1
         do st_idx = 1, n_stations
            xi_station = dble(st_idx - 1) / dble(codecheck_subdivisions)
            x_loc = xi_station * dl

            if (allocated(ub31_for)) then
               r_pos = xi_station * 10.d0
               i_lo = min(max(int(r_pos) + 1, 1), 10)
               i_hi = i_lo + 1
               frac = r_pos - dble(i_lo - 1)
               if (frac .lt. 0.d0) frac = 0.d0
               if (frac .gt. 1.d0) frac = 1.d0
               if (xi_station .ge. 1.0d0) then
                  i_lo = 11; i_hi = 11; frac = 0.d0
               endif
               fx_st = (1.d0 - frac)*ub31_for(1, i_lo, ielem) + 
     &                 frac*ub31_for(1, i_hi, ielem)
               vy_st = (1.d0 - frac)*ub31_for(2, i_lo, ielem) + 
     &                 frac*ub31_for(2, i_hi, ielem)
               vz_st = (1.d0 - frac)*ub31_for(3, i_lo, ielem) + 
     &                 frac*ub31_for(3, i_hi, ielem)
               tx_st = (1.d0 - frac)*ub31_for(4, i_lo, ielem) + 
     &                 frac*ub31_for(4, i_hi, ielem)
               my_st = (1.d0 - frac)*ub31_for(5, i_lo, ielem) + 
     &                 frac*ub31_for(5, i_hi, ielem)
               mz_st = (1.d0 - frac)*ub31_for(6, i_lo, ielem) + 
     &                 frac*ub31_for(6, i_hi, ielem)
            else
               fx_st = 0.d0; vy_st = 0.d0; vz_st = 0.d0
               tx_st = 0.d0; my_st = 0.d0; mz_st = 0.d0
            endif

            call calc_ec3_station_check(fx_st, vy_st, vz_st, tx_st,
     &           my_st, mz_st, Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y,
     &           Mc_z, Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd, lbar_y,
     &           lbar_z, Cmy, Cmz, CmLT, iclass,
     &           uc_tot_st, uc_ax_st, uc_sh_st, uc_bnd_st, 
     &           uc_buck_st, uc_ltb_st, uc_stab_st, gov_mode_st)

            if (st_idx .le. 11) then
               station_uc(1, st_idx, ielem) = uc_tot_st
               station_uc(2, st_idx, ielem) = uc_ax_st
               station_uc(3, st_idx, ielem) = uc_sh_st
               station_uc(4, st_idx, ielem) = uc_bnd_st
               station_uc(5, st_idx, ielem) = uc_stab_st
               station_uc(6, st_idx, ielem) = uc_ltb_st
            endif

            if (uc_tot_st .ge. max_tot) then
               max_tot = uc_tot_st
               gov_st = st_idx - 1
               gov_mode_elem = gov_mode_st
            endif

            max_ax   = max(max_ax, uc_ax_st)
            max_sh   = max(max_sh, uc_sh_st)
            max_bnd  = max(max_bnd, uc_bnd_st)
            max_stab = max(max_stab, uc_stab_st)
            max_ltb  = max(max_ltb, uc_ltb_st)
         enddo

         if (codecheck_output_level .eq. 3) then
            if (max_tot .gt. env_max_uc(ielem)) then
               gov_idx = gov_st + 1
               env_max_uc(ielem)        = max_tot
               env_gov_step(ielem)      = istep
               env_gov_inc(ielem)       = iinc
               env_gov_station(ielem)   = gov_st
               env_gov_xi(ielem)        = dble(gov_st) / 
     &                                    dble(codecheck_subdivisions)
               env_gov_xloc(ielem)      = (dble(gov_st) / 
     &                                    dble(codecheck_subdivisions))
     &                                    * dl
               if (allocated(ub31_for)) then
                  r_pos = env_gov_xi(ielem) * 10.d0
                  i_lo = min(max(int(r_pos) + 1, 1), 10)
                  i_hi = i_lo + 1
                  frac = r_pos - dble(i_lo - 1)
                  if (frac .lt. 0.d0) frac = 0.d0
                  if (frac .gt. 1.d0) frac = 1.d0
                  if (env_gov_xi(ielem) .ge. 1.0d0) then
                     i_lo = 11; i_hi = 11; frac = 0.d0
                  endif
                  env_gov_forces(1, ielem) = (1.d0-frac)*
     &                 ub31_for(1, i_lo, ielem) + 
     &                 frac*ub31_for(1, i_hi, ielem)
                  env_gov_forces(2, ielem) = (1.d0-frac)*
     &                 ub31_for(2, i_lo, ielem) + 
     &                 frac*ub31_for(2, i_hi, ielem)
                  env_gov_forces(3, ielem) = (1.d0-frac)*
     &                 ub31_for(3, i_lo, ielem) + 
     &                 frac*ub31_for(3, i_hi, ielem)
                  env_gov_forces(4, ielem) = (1.d0-frac)*
     &                 ub31_for(4, i_lo, ielem) + 
     &                 frac*ub31_for(4, i_hi, ielem)
                  env_gov_forces(5, ielem) = (1.d0-frac)*
     &                 ub31_for(5, i_lo, ielem) + 
     &                 frac*ub31_for(5, i_hi, ielem)
                  env_gov_forces(6, ielem) = (1.d0-frac)*
     &                 ub31_for(6, i_lo, ielem) + 
     &                 frac*ub31_for(6, i_hi, ielem)
               endif
               env_gov_ucs(1, ielem)    = max_ax
               env_gov_ucs(2, ielem)    = max_sh
               env_gov_ucs(3, ielem)    = max_bnd
               env_gov_ucs(4, ielem)    = max_ltb
               env_gov_ucs(5, ielem)    = max_stab
               env_gov_mode(ielem)      = gov_mode_elem
            endif
         endif

         uc_tot(ielem) = max_tot
         uc_ax(ielem)  = max_ax
         uc_sh(ielem)  = max_sh
         uc_bnd(ielem) = max_bnd
         uc_stab(ielem)= max_stab
         uc_ltb(ielem) = max_ltb
         uc_gov_station(ielem) = gov_st
         uc_gov_mode(ielem)    = gov_mode_elem
      enddo
!$omp end parallel do

!     ==================================================================
!     2. Summary Reduction Phase
!     ==================================================================
      num_checked = 0
      num_pass = 0
      num_fail = 0
      max_struct_uc = 0.d0
      max_struct_elem = 0
      max_struct_mode = ' '

      do itop = 1, 10
         top_elem(itop) = 0
         top_uc(itop) = -1.d0
      enddo

      do ielem = 1, ne
         if (ipkon(ielem) .lt. 0) cycle
         if (lakon(ielem)(1:2) .ne. 'UB') cycle
         if (.not. in_filter(ielem)) cycle

         num_checked = num_checked + 1
         if (uc_tot(ielem) .le. 1.0d0) then
            num_pass = num_pass + 1
         else
            num_fail = num_fail + 1
         endif

         if (uc_tot(ielem) .gt. max_struct_uc) then
            max_struct_uc = uc_tot(ielem)
            max_struct_elem = ielem
            max_struct_mode = uc_gov_mode(ielem)
         endif

         do itop = 1, 10
            if (uc_tot(ielem) .gt. top_uc(itop)) then
               do jtop = 10, itop + 1, -1
                  top_uc(jtop) = top_uc(jtop-1)
                  top_elem(jtop) = top_elem(jtop-1)
               enddo
               top_uc(itop) = uc_tot(ielem)
               top_elem(itop) = ielem
               exit
            endif
         enddo
      enddo

      call system_clock(c_eval_done, c_rate, c_max)

!     ==================================================================
!     3. CSV File Streaming Phase (According to OUTPUT Level)
!     ==================================================================
      if (codecheck_output_level .eq. 1 .or. 
     &    codecheck_output_level .eq. 2 .or. 
     &    codecheck_output_level .eq. 4) then
         do ielem = 1, ne
            if (ipkon(ielem) .lt. 0) cycle
            if (lakon(ielem)(1:2) .ne. 'UB') cycle
            if (.not. in_filter(ielem)) cycle

            indexe = ipkon(ielem)
            n1 = kon(indexe+1)
            n2 = kon(indexe+2)
            dl = dsqrt((co(1,n2)-co(1,n1))**2 + (co(2,n2)-co(2,n1))**2 + 
     &                 (co(3,n2)-co(3,n1))**2)
            if (dl .le. 1.0d-6) dl = 1.0d0

            if (codecheck_output_level .eq. 1 .or. 
     &          codecheck_output_level .eq. 4) then
               do st_idx = 1, codecheck_subdivisions + 1
                  xi_station = dble(st_idx - 1) / 
     &                         dble(codecheck_subdivisions)
                  x_loc = xi_station * dl
                  if (allocated(ub31_for)) then
                     r_pos = xi_station * 10.d0
                     i_lo = min(max(int(r_pos) + 1, 1), 10)
                     i_hi = i_lo + 1
                     frac = r_pos - dble(i_lo - 1)
                     if (frac .lt. 0.d0) frac = 0.d0
                     if (frac .gt. 1.d0) frac = 1.d0
                     if (xi_station .ge. 1.0d0) then
                        i_lo = 11; i_hi = 11; frac = 0.d0
                     endif
                     fx_st = (1.d0 - frac)*ub31_for(1, i_lo, ielem) + 
     &                       frac*ub31_for(1, i_hi, ielem)
                     vy_st = (1.d0 - frac)*ub31_for(2, i_lo, ielem) + 
     &                       frac*ub31_for(2, i_hi, ielem)
                     vz_st = (1.d0 - frac)*ub31_for(3, i_lo, ielem) + 
     &                       frac*ub31_for(3, i_hi, ielem)
                     tx_st = (1.d0 - frac)*ub31_for(4, i_lo, ielem) + 
     &                       frac*ub31_for(4, i_hi, ielem)
                     my_st = (1.d0 - frac)*ub31_for(5, i_lo, ielem) + 
     &                       frac*ub31_for(5, i_hi, ielem)
                     mz_st = (1.d0 - frac)*ub31_for(6, i_lo, ielem) + 
     &                       frac*ub31_for(6, i_hi, ielem)
                  else
                     fx_st = 0.d0; vy_st = 0.d0; vz_st = 0.d0
                     tx_st = 0.d0; my_st = 0.d0; mz_st = 0.d0
                  endif

                  if (st_idx .le. 11) then
                     uc_tot_st  = station_uc(1, st_idx, ielem)
                     uc_ax_st   = station_uc(2, st_idx, ielem)
                     uc_sh_st   = station_uc(3, st_idx, ielem)
                     uc_bnd_st  = station_uc(4, st_idx, ielem)
                     uc_stab_st = station_uc(5, st_idx, ielem)
                     uc_ltb_st  = station_uc(6, st_idx, ielem)
                     gov_mode_st = uc_gov_mode(ielem)
                  else
                     uc_tot_st  = uc_tot(ielem)
                     uc_ax_st   = uc_ax(ielem)
                     uc_sh_st   = uc_sh(ielem)
                     uc_bnd_st  = uc_bnd(ielem)
                     uc_stab_st = uc_stab(ielem)
                     uc_ltb_st  = uc_ltb(ielem)
                     gov_mode_st = uc_gov_mode(ielem)
                  endif

                  if (codecheck_output_level .eq. 1 .or. 
     &                (codecheck_output_level .eq. 4 .and. 
     &                 uc_tot_st .gt. 1.0d0)) then
                     write(csv_unit, '(I5,A,I3,A,I8,A,I3,A,F6.2,A,'//
     &                 'F10.4,6(A,1PE13.5),6(A,0PF10.4),A,A)')
     &                 istep, ',', iinc, ',', ielem, ',', st_idx-1, ',',
     &                 xi_station, ',', x_loc,
     &                 ',', fx_st, ',', vy_st, ',', vz_st,
     &                 ',', tx_st, ',', my_st, ',', mz_st,
     &                 ',', uc_ax_st, ',', uc_sh_st, ',', uc_bnd_st,
     &                 ',', uc_ltb_st, ',', uc_stab_st, ',', uc_tot_st,
     &                 ',', trim(gov_mode_st)
                  endif
               enddo
            elseif (codecheck_output_level .eq. 2) then
               gov_st = uc_gov_station(ielem)
               xi_station = dble(gov_st) / dble(codecheck_subdivisions)
               x_loc = xi_station * dl
               if (allocated(ub31_for)) then
                  r_pos = xi_station * 10.d0
                  i_lo = min(max(int(r_pos) + 1, 1), 10)
                  i_hi = i_lo + 1
                  frac = r_pos - dble(i_lo - 1)
                  if (frac .lt. 0.d0) frac = 0.d0
                  if (frac .gt. 1.d0) frac = 1.d0
                  if (xi_station .ge. 1.0d0) then
                     i_lo = 11; i_hi = 11; frac = 0.d0
                  endif
                  fx_st = (1.d0 - frac)*ub31_for(1, i_lo, ielem) + 
     &                    frac*ub31_for(1, i_hi, ielem)
                  vy_st = (1.d0 - frac)*ub31_for(2, i_lo, ielem) + 
     &                    frac*ub31_for(2, i_hi, ielem)
                  vz_st = (1.d0 - frac)*ub31_for(3, i_lo, ielem) + 
     &                    frac*ub31_for(3, i_hi, ielem)
                  tx_st = (1.d0 - frac)*ub31_for(4, i_lo, ielem) + 
     &                    frac*ub31_for(4, i_hi, ielem)
                  my_st = (1.d0 - frac)*ub31_for(5, i_lo, ielem) + 
     &                    frac*ub31_for(5, i_hi, ielem)
                  mz_st = (1.d0 - frac)*ub31_for(6, i_lo, ielem) + 
     &                    frac*ub31_for(6, i_hi, ielem)
               else
                  fx_st = 0.d0; vy_st = 0.d0; vz_st = 0.d0
                  tx_st = 0.d0; my_st = 0.d0; mz_st = 0.d0
               endif
               write(csv_unit, '(I5,A,I3,A,I8,A,I3,A,F6.2,A,F10.4,'//
     &           '6(A,1PE13.5),6(A,0PF10.4),A,A)')
     &           istep, ',', iinc, ',', ielem, ',', gov_st, ',',
     &           xi_station, ',', x_loc,
     &           ',', fx_st, ',', vy_st, ',', vz_st,
     &           ',', tx_st, ',', my_st, ',', mz_st,
     &           ',', uc_ax(ielem), ',', uc_sh(ielem), ',', 
     &           uc_bnd(ielem), ',', uc_ltb(ielem), ',', 
     &           uc_stab(ielem), ',', uc_tot(ielem),
     &           ',', trim(uc_gov_mode(ielem))
            endif
         enddo
         close(csv_unit)
      endif

      if (codecheck_output_level .eq. 3) then
         open(csv_unit, file=trim(codecheck_filename),
     &        status='replace')
         write(csv_unit, '(A)') 'GOV_STEP,GOV_INC,ELEM,STATION,XI,'//
     &     'X_LOC_M,FX_N,VY_N,VZ_N,TX_NM,MY_NM,MZ_NM,UC_AX,UC_SH,'//
     &     'UC_BND,UC_LTB,UC_STAB,UC_TOT,GOV_MODE'
         do ielem = 1, ne
            if (ipkon(ielem) .ge. 0 .and. 
     &          lakon(ielem)(1:2) .eq. 'UB' .and.
     &          in_filter(ielem)) then
               write(csv_unit, '(I5,A,I3,A,I8,A,I3,A,F6.2,A,'//
     &           'F10.4,6(A,1PE13.5),6(A,0PF10.4),A,A)')
     &           env_gov_step(ielem), ',', env_gov_inc(ielem), ',',
     &           ielem, ',', env_gov_station(ielem), ',',
     &           env_gov_xi(ielem), ',', env_gov_xloc(ielem),
     &           ',', env_gov_forces(1, ielem), ',', 
     &           env_gov_forces(2, ielem), ',', 
     &           env_gov_forces(3, ielem), ',', 
     &           env_gov_forces(4, ielem), ',', 
     &           env_gov_forces(5, ielem), ',', 
     &           env_gov_forces(6, ielem),
     &           ',', env_gov_ucs(1, ielem), ',', 
     &           env_gov_ucs(2, ielem), ',', 
     &           env_gov_ucs(3, ielem), ',', 
     &           env_gov_ucs(4, ielem), ',', 
     &           env_gov_ucs(5, ielem), ',', 
     &           env_max_uc(ielem),
     &           ',', trim(env_gov_mode(ielem))
            endif
         enddo
         close(csv_unit)
      endif

      call system_clock(c_io_done, c_rate, c_max)

      if (c_rate .gt. 0) then
         t_eval_ms = dble(c_eval_done - c_start) * 1000.d0 / 
     &               dble(c_rate)
         t_io_ms   = dble(c_io_done - c_eval_done) * 1000.d0 /
     &               dble(c_rate)
         t_tot_ms  = dble(c_io_done - c_start) * 1000.d0 / 
     &               dble(c_rate)
      else
         t_eval_ms = 0.d0
         t_io_ms = 0.d0
         t_tot_ms = 0.d0
      endif

!     Print Console Summary Table
      write(*,*) ' '
      write(*,*) '=================================================='
      write(*,*) ' CALCULIX BEAM CODE CHECK [EUROCODE 3] - STEP ',
     &           istep, ' INC ', iinc
      write(*,*) ' Results written to: ', trim(codecheck_filename)
      write(*,*) '=================================================='

      if (num_checked .le. 30) then
         write(*,'(A)') 
     &     '  ELEM  STYPE   L(m)    Lcr_y   Lcr_z   L_LT '//
     &     '   UC_AX   UC_SH  UC_BND  UC_LTB UC_STAB  UC_MAX  GOV_MODE'
         write(*,*) 
     &     '--------------------------------------------------'
         do ielem = 1, ne
            if (ipkon(ielem) .lt. 0) cycle
            if (lakon(ielem)(1:2) .ne. 'UB') cycle
            if (.not. in_filter(ielem)) cycle

            indexp = ielprop(ielem)
            if (indexp .le. 0) cycle
            sect_type = prop(indexp+1)
            st_tag = 'RECT'
            if (nint(sect_type) .eq. 2) st_tag = 'PIPE'
            if (nint(sect_type) .eq. 3) st_tag = 'I-BM'
            if (nint(sect_type) .eq. 4) st_tag = 'TEE '
            if (nint(sect_type) .eq. 5) st_tag = 'CHAN'
            if (nint(sect_type) .eq. 6) st_tag = 'ANGL'
            if (nint(sect_type) .eq. 7) st_tag = 'BOX '

            indexe = ipkon(ielem)
            n1 = kon(indexe+1)
            n2 = kon(indexe+2)
            dl = dsqrt((co(1,n2)-co(1,n1))**2 + (co(2,n2)-co(2,n1))**2 + 
     &                 (co(3,n2)-co(3,n1))**2)

            if (lcr_y(ielem) .gt. 0.d0) then
               lcr_y_eff = lcr_y(ielem)
            else
               lcr_y_eff = k_y(ielem) * dl
            endif
            if (lcr_z(ielem) .gt. 0.d0) then
               lcr_z_eff = lcr_z(ielem)
            else
               lcr_z_eff = k_z(ielem) * dl
            endif
            if (l_lt(ielem) .gt. 0.d0) then
               l_lt_eff = l_lt(ielem)
            else
               l_lt_eff = k_lt(ielem) * dl
            endif

            if (uc_tot(ielem) .le. 1.0d0) then
               write(*,'(I6,2X,A4,F8.3,3F8.3,6F8.3,2X,A16,A)') 
     &           ielem, st_tag, dl, lcr_y_eff, lcr_z_eff, l_lt_eff,
     &           uc_ax(ielem), uc_sh(ielem), uc_bnd(ielem),
     &           uc_ltb(ielem), uc_stab(ielem), uc_tot(ielem), 
     &           trim(uc_gov_mode(ielem)), ' (OK)'
            else
               write(*,'(I6,2X,A4,F8.3,3F8.3,6F8.3,2X,A16,A)') 
     &           ielem, st_tag, dl, lcr_y_eff, lcr_z_eff, l_lt_eff,
     &           uc_ax(ielem), uc_sh(ielem), uc_bnd(ielem),
     &           uc_ltb(ielem), uc_stab(ielem), uc_tot(ielem), 
     &           trim(uc_gov_mode(ielem)), ' (FAIL)'
            endif
         enddo
      else
!        Summary Mode for Large Structures
         write(*,'(A)') 
     &     '  TOP GOVERNING MEMBERS (Highest Unity Checks):'
         write(*,'(A)') 
     &     '  ELEM  STYPE   L(m)    Lcr_y   Lcr_z   L_LT '//
     &     '   UC_AX   UC_SH  UC_BND  UC_LTB UC_STAB  UC_MAX  GOV_MODE'
         write(*,*) 
     &     '--------------------------------------------------'
         top_k = min(10, num_checked)
         do itop = 1, top_k
            ielem = top_elem(itop)
            if (ielem .le. 0) cycle

            indexp = ielprop(ielem)
            sect_type = prop(indexp+1)
            st_tag = 'RECT'
            if (nint(sect_type) .eq. 2) st_tag = 'PIPE'
            if (nint(sect_type) .eq. 3) st_tag = 'I-BM'
            if (nint(sect_type) .eq. 4) st_tag = 'TEE '
            if (nint(sect_type) .eq. 5) st_tag = 'CHAN'
            if (nint(sect_type) .eq. 6) st_tag = 'ANGL'
            if (nint(sect_type) .eq. 7) st_tag = 'BOX '

            indexe = ipkon(ielem)
            n1 = kon(indexe+1)
            n2 = kon(indexe+2)
            dl = dsqrt((co(1,n2)-co(1,n1))**2 + (co(2,n2)-co(2,n1))**2 + 
     &                 (co(3,n2)-co(3,n1))**2)

            if (lcr_y(ielem) .gt. 0.d0) then
               lcr_y_eff = lcr_y(ielem)
            else
               lcr_y_eff = k_y(ielem) * dl
            endif
            if (lcr_z(ielem) .gt. 0.d0) then
               lcr_z_eff = lcr_z(ielem)
            else
               lcr_z_eff = k_z(ielem) * dl
            endif
            if (l_lt(ielem) .gt. 0.d0) then
               l_lt_eff = l_lt(ielem)
            else
               l_lt_eff = k_lt(ielem) * dl
            endif

            if (uc_tot(ielem) .le. 1.0d0) then
               write(*,'(I6,2X,A4,F8.3,3F8.3,6F8.3,2X,A16,A)') 
     &           ielem, st_tag, dl, lcr_y_eff, lcr_z_eff, l_lt_eff,
     &           uc_ax(ielem), uc_sh(ielem), uc_bnd(ielem),
     &           uc_ltb(ielem), uc_stab(ielem), uc_tot(ielem), 
     &           trim(uc_gov_mode(ielem)), ' (OK)'
            else
               write(*,'(I6,2X,A4,F8.3,3F8.3,6F8.3,2X,A16,A)') 
     &           ielem, st_tag, dl, lcr_y_eff, lcr_z_eff, l_lt_eff,
     &           uc_ax(ielem), uc_sh(ielem), uc_bnd(ielem),
     &           uc_ltb(ielem), uc_stab(ielem), uc_tot(ielem), 
     &           trim(uc_gov_mode(ielem)), ' (FAIL)'
            endif
         enddo
      endif

      write(*,*) '--------------------------------------------------'
      write(*,'(A,I6,A,I6,A,I6,A,F7.4,A,I6,A,A)')
     &  '  Total Beams: ', num_checked, ' | Passed: ', num_pass,
     &  ' | Overstressed: ', num_fail, ' | Max UC: ', max_struct_uc,
     &  ' (Elem ', max_struct_elem, ' - ', trim(max_struct_mode)//')'
      write(*,'(A,F8.2,A,F8.2,A,F8.2,A)')
     &  '  [Profiler] CodeCheck Timing: Compute = ', t_eval_ms,
     &  ' ms | CSV I/O = ', t_io_ms, ' ms | Total = ', t_tot_ms, ' ms'
      write(*,*) '=================================================='
      write(*,*) ' '

      deallocate(in_filter)

      end subroutine run_beam_codecheck


!     ==================================================================
!     Helper Routines for Parameter Parsing
!     ==================================================================
      subroutine parse_design_keyval(token, elset_name, has_elset,
     &  set, nset, istartset, iendset, ialset, lakon, ne)
      implicit none
      character*(*) token, elset_name
      character*81 set(*)
      character*8 lakon(*)
      integer istartset(*), iendset(*), ialset(*)
      integer nset, ne
      logical has_elset

      character*80 p_name
      integer eq_pos, istat
      real*8 val_num

      eq_pos = index(token, '=')
      if (eq_pos .le. 1) return

      p_name = token(1:eq_pos-1)
      read(token(eq_pos+1:), *, iostat=istat) val_num
      if (istat .ne. 0) return

      call apply_design_param(p_name, val_num, elset_name, has_elset,
     &     set, nset, istartset, iendset, ialset, lakon, ne)
      end subroutine parse_design_keyval


      subroutine apply_design_param(p_name_in, val_num, elset_name,
     &  has_elset, set, nset, istartset, iendset, ialset, lakon, ne)
      use codecheck_module
      implicit none

      character*(*) p_name_in, elset_name
      character*81 set(*), elset
      character*8 lakon(*)
      integer istartset(*), iendset(*), ialset(*)
      integer nset, ne
      logical has_elset
      real*8 val_num

      integer id, iset, ipos, j, elem, ielem
      character*80 p_name

      p_name = p_name_in
      call clean_col_name(p_name)

      if (has_elset .and. len_trim(elset_name) .gt. 0) then
         elset = elset_name
         elset(81:81) = ' '
         ipos = index(elset, ' ')
         if (ipos .le. 1) ipos = len_trim(elset) + 1
         elset(ipos:ipos) = 'E'
         call cident81(set, elset, nset, id)
         iset = nset + 1
         if (id .gt. 0) then
            if (elset .eq. set(id)) iset = id
         endif
         if (iset .le. nset) then
            do j = istartset(iset), iendset(iset)
               elem = ialset(j)
               if (elem .gt. 0 .and. elem .le. size(lcr_y)) then
                  call apply_single_elem_param(p_name, val_num, 
     &                 elem, lakon, ne)
               endif
            enddo
         endif
      else
         do ielem = 1, ne
            call apply_single_elem_param(p_name, val_num, 
     &           ielem, lakon, ne)
         enddo
      endif

      end subroutine apply_design_param


      subroutine apply_single_elem_param(p_name_in, val_num, ielem, 
     &  lakon, ne)
      use codecheck_module
      implicit none
      character*(*) p_name_in
      character*8 lakon(*)
      integer ielem, ne
      real*8 val_num
      character*80 p_name

      if (ielem .le. 0 .or. ielem .gt. size(lcr_y)) return

      p_name = p_name_in
      call clean_col_name(p_name)

      if (p_name(1:5) .eq. 'LCR_Y' .or. p_name(1:4) .eq. 'LCRY') then
         lcr_y(ielem) = val_num
      elseif (p_name(1:5) .eq. 'LCR_Z' .or. 
     &        p_name(1:4) .eq. 'LCRZ') then
         lcr_z(ielem) = val_num
      elseif (p_name(1:4) .eq. 'L_LT' .or. p_name(1:3) .eq. 'LLT') then
         l_lt(ielem) = val_num
      elseif (p_name(1:2) .eq. 'KY') then
         k_y(ielem) = val_num
      elseif (p_name(1:2) .eq. 'KZ') then
         k_z(ielem) = val_num
      elseif (p_name(1:4) .eq. 'K_LT' .or. p_name(1:3) .eq. 'KLT') then
         k_lt(ielem) = val_num
      elseif (p_name(1:2) .eq. 'C1' .or. p_name(1:2) .eq. 'CB') then
         c1_lt(ielem) = val_num
      elseif (p_name(1:2) .eq. 'FY') then
         fy_user(ielem) = val_num
      elseif (p_name(1:8) .eq. 'GAMMA_M0' .or. 
     &        p_name(1:7) .eq. 'GAMMAM0') then
         gamma_m0(ielem) = val_num
      elseif (p_name(1:8) .eq. 'GAMMA_M1' .or. 
     &        p_name(1:7) .eq. 'GAMMAM1') then
         gamma_m1(ielem) = val_num
      elseif (p_name(1:5) .eq. 'PHI_C' .or. 
     &        p_name(1:4) .eq. 'PHIC') then
         phi_c(ielem) = val_num
      elseif (p_name(1:5) .eq. 'PHI_B' .or. 
     &        p_name(1:4) .eq. 'PHIB') then
         phi_b(ielem) = val_num
      elseif (p_name(1:5) .eq. 'PHI_V' .or. 
     &        p_name(1:4) .eq. 'PHIV') then
         phi_v(ielem) = val_num
      elseif (p_name(1:5) .eq. 'PHI_T' .or. 
     &        p_name(1:4) .eq. 'PHIT') then
         phi_t(ielem) = val_num
      endif

      end subroutine apply_single_elem_param


      logical function is_hdr_tok(str)
      implicit none
      character*(*) str
      character*80 s
      s = str
      call clean_col_name(s)
      is_hdr_tok = .false.
      if (s(1:6) .eq. 'TARGET' .or. s(1:4) .eq. 'ELEM' .or.
     &    s(1:7) .eq. 'ELEMENT' .or. s(1:5) .eq. 'ELSET' .or.
     &    s(1:6) .eq. 'MEMBER' .or. s(1:2) .eq. 'ID' .or.
     &    s(1:5) .eq. 'LCR_Y' .or. s(1:5) .eq. 'LCR_Z' .or.
     &    s(1:4) .eq. 'L_LT' .or. s(1:2) .eq. 'C1' .or.
     &    s(1:2) .eq. 'CB' .or. s(1:2) .eq. 'FY') then
         is_hdr_tok = .true.
      endif
      end function is_hdr_tok


      subroutine clean_col_name(str)
      implicit none
      character*(*) str
      integer i, j, l
      character*132 tmp

      l = len_trim(str)
      tmp = ' '
      j = 0
      do i = 1, l
         if (str(i:i) .eq. '*' .or. str(i:i) .eq. ' ' .or. 
     &       str(i:i) .eq. ',' .or. str(i:i) .eq. '(' .or. 
     &       str(i:i) .eq. ')') cycle
         j = j + 1
         if (str(i:i) .ge. 'a' .and. str(i:i) .le. 'z') then
            tmp(j:j) = char(ichar(str(i:i)) - 32)
         else
            tmp(j:j) = str(i:i)
         endif
      enddo
      str = tmp(1:len(str))
      end subroutine clean_col_name
