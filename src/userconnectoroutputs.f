!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     Subroutine to parse *USER CONNECTOR OUTPUT cards and configure
!     zero-RAM streaming of UCONN6 connector forces, relative 
!     displacements, and ASCE 41-17 plastic hinge states to CSV.
!
      subroutine userconnectoroutputs(inpc,textpart,set,istartset,
     &  iendset,ialset,nset,lakon,ne,irstrt,istep,istat,n,key,iline,
     &  ipol,inl,ipoinp,inp,ipoinpc,jobnamec,ier)
!
      use uconn6_module
      implicit none
!
      character*1 inpc(*)
      character*8 lakon(*)
      character*81 set(*), elset
      character*132 textpart(16)
      character*132 jobnamec(*)
      integer istartset(*), iendset(*), ialset(*), ipoinpc(0:*)
      integer iline, ipol, inl, ipoinp(2,*), inp(3,*), nset
      integer istep, istat, n, key, irstrt(*), ier, ne
!
      integer i, j, m, ipos, id_set, elem, len_t, i_num
      integer n_files, n_elsets, itarg, len_job, ipos_slash
      character*80 token, dir_prefix
      character*80 elset_list(UCONN_MAX_TARGETS)
      character*80 file_list(UCONN_MAX_TARGETS)
      character*500 header_str
      character*4 inc_mode_in
      integer inc_freq_in
      integer inc_list_in(50), ninc_list_in
      character*6 coords_mode_in
      logical flag_f_in, flag_u_in, flag_state_in
!
      inc_mode_in = 'LAST'
      inc_freq_in = 1
      ninc_list_in = 0
      coords_mode_in = 'LOCAL '
      flag_f_in = .false.
      flag_u_in = .false.
      flag_state_in = .false.
!
      header_str = ' '
      do i = 2, n
         if (i .eq. 2) then
            header_str = trim(textpart(i))
         else
            header_str = trim(header_str) // ',' // trim(textpart(i))
         endif
      enddo
!
      n_elsets = 0
      n_files = 0
      call get_uconn_param_tuple_or_val(header_str, 'ELSET=',
     &     elset_list, n_elsets, UCONN_MAX_TARGETS)
      call get_uconn_param_tuple_or_val(header_str, 'FILE=',
     &     file_list, n_files, UCONN_MAX_TARGETS)
!
      do i = 2, n
         token = adjustl(textpart(i)(1:80))
         len_t = len_trim(token)
         if (len_t .eq. 0) cycle
!
         ipos = index(token, '=')
         if (ipos .gt. 0) then
            do m = 1, ipos
               if (token(m:m) .ge. 'a' .and. token(m:m) .le. 'z') then
                  token(m:m) = char(ichar(token(m:m)) - 32)
               endif
            enddo
         else
            do m = 1, len_t
               if (token(m:m) .ge. 'a' .and. token(m:m) .le. 'z') then
                  token(m:m) = char(ichar(token(m:m)) - 32)
               endif
            enddo
         endif
!
         if (token(1:6) .eq. 'ELSET=' .or.
     &       token(1:5) .eq. 'FILE=') then
            cycle
         elseif (token(1:10) .eq. 'INCREMENT=') then
            if (token(11:14) .eq. 'LAST') then
               inc_mode_in = 'LAST'
            elseif (token(11:13) .eq. 'ALL') then
               inc_mode_in = 'ALL '
            elseif (token(11:15) .eq. 'FREQ=') then
               inc_mode_in = 'FREQ'
               read(token(16:len_t), *, iostat=ier) inc_freq_in
               if (ier .ne. 0 .or. inc_freq_in .le. 0) inc_freq_in = 1
            elseif (token(11:11) .eq. '(') then
               inc_mode_in = 'LIST'
               ninc_list_in = 0
               call parse_uconn_inc_list(token(12:len_t),
     &              inc_list_in, ninc_list_in)
            else
               read(token(11:len_t), *, iostat=ier) i_num
               if (ier .eq. 0 .and. i_num .gt. 0) then
                  inc_mode_in = 'FREQ'
                  inc_freq_in = i_num
               endif
            endif
         elseif (token(1:12) .eq. 'COORDINATES=') then
            if (token(13:18) .eq. 'GLOBAL') then
               coords_mode_in = 'GLOBAL'
            else
               coords_mode_in = 'LOCAL '
            endif
         endif
      enddo
!
      if (n_elsets .le. 0) then
         uconn_out_num_targets = 1
         uconn_out_target_elset(1) = ' '
         if (n_files .ge. 1) then
            uconn_out_target_filename(1) = file_list(1)
         else
            uconn_out_target_filename(1) =
     &           trim(jobnamec(1)) // '_connectors.csv'
         endif
      else
         uconn_out_num_targets = n_elsets
         do itarg = 1, uconn_out_num_targets
            uconn_out_target_elset(itarg) = elset_list(itarg)
            if (itarg .le. n_files) then
               uconn_out_target_filename(itarg) = file_list(itarg)
            else
               uconn_out_target_filename(itarg) =
     &              trim(jobnamec(1)) // '_' //
     &              trim(elset_list(itarg)) // '.csv'
            endif
         enddo
      endif
!
      dir_prefix = ' '
      len_job = len_trim(jobnamec(1))
      ipos_slash = 0
      do m = len_job, 1, -1
         if (jobnamec(1)(m:m) .eq. '/' .or.
     &       jobnamec(1)(m:m) .eq. '\') then
            ipos_slash = m
            exit
         endif
      enddo
      if (ipos_slash .gt. 0) then
         dir_prefix = jobnamec(1)(1:ipos_slash)
      endif
!
      if (ipos_slash .gt. 0) then
         do itarg = 1, uconn_out_num_targets
            if (index(uconn_out_target_filename(itarg), '/') .eq. 0
     &          .and. index(uconn_out_target_filename(itarg),
     &                      '\') .eq. 0) then
               uconn_out_target_filename(itarg) = trim(dir_prefix) //
     &              trim(uconn_out_target_filename(itarg))
            endif
         enddo
      endif
!
      uconn_out_inc_mode = inc_mode_in
      uconn_out_inc_freq = inc_freq_in
      uconn_out_inc_list = inc_list_in
      uconn_out_ninc_list = ninc_list_in
      uconn_out_coords_mode = coords_mode_in
!
      if (allocated(uconn_out_elem_active))
     &     deallocate(uconn_out_elem_active)
      allocate(uconn_out_elem_active(max(ne, 100000),
     &         UCONN_MAX_TARGETS))
      uconn_out_elem_active = .false.
!
      do itarg = 1, uconn_out_num_targets
         uconn_out_target_file_init(itarg) = .false.
         if (uconn_out_target_elset(itarg) .eq. ' ') then
            do elem = 1, max(ne, 1)
               if (elem .le. size(uconn_out_elem_active, 1)) then
                  if (lakon(elem)(1:5) .eq. 'UCONN') then
                     uconn_out_elem_active(elem, itarg) = .true.
                  endif
               endif
            enddo
         else
            elset = uconn_out_target_elset(itarg)
            ipos = index(elset, ' ')
            if (ipos .le. 1) ipos = len_trim(elset) + 1
            elset(ipos:ipos) = 'E'
            call cident81(set, elset, nset, id_set)
            if (id_set .le. 0 .or. elset .ne. set(id_set)) then
               if (uconn_out_target_elset(itarg)(1:4) .eq. 'ALL ' .or.
     &             uconn_out_target_elset(itarg)(1:5) .eq. 'EALL ' .or.
     &             uconn_out_target_elset(itarg)(1:1) .eq. '*') then
                  do elem = 1, max(ne, 1)
                     if (elem .le. size(uconn_out_elem_active, 1)) then
                        if (lakon(elem)(1:5) .eq. 'UCONN') then
                           uconn_out_elem_active(elem, itarg) = .true.
                        endif
                     endif
                  enddo
               else
                  elset(ipos:ipos) = ' '
                  write(*,*) '*WARNING reading *USER CONNECTOR OUTPUT: '
                  write(*,*) '         element set ', trim(elset),
     &                       ' has not yet been defined.'
                  call inputwarning(inpc,ipoinpc,iline,
     &                 "*USER CONNECTOR OUTPUT%")
               endif
            else
               do j = istartset(id_set), iendset(id_set)
                  if (ialset(j) .gt. 0) then
                     elem = ialset(j)
                     if (elem .le. size(uconn_out_elem_active, 1)) then
                        if (lakon(elem)(1:5) .eq. 'UCONN') then
                           uconn_out_elem_active(elem, itarg) = .true.
                        endif
                     endif
                  endif
               enddo
            endif
         endif
      enddo
!
!     Read variable selection data lines (e.g. F, U, STATE, ALL)
!
      do
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if (istat .lt. 0 .or. key .eq. 1) exit
!
         do i = 1, n
            token = adjustl(textpart(i)(1:80))
            len_t = len_trim(token)
            if (len_t .eq. 0) cycle
            do m = 1, len_t
               if (token(m:m) .ge. 'a' .and. token(m:m) .le. 'z') then
                  token(m:m) = char(ichar(token(m:m)) - 32)
               endif
            enddo
!
            if (token(1:3) .eq. 'ALL') then
               flag_f_in = .true.
               flag_u_in = .true.
               flag_state_in = .true.
            elseif (token(1:1) .eq. 'F') then
               flag_f_in = .true.
            elseif (token(1:1) .eq. 'U') then
               flag_u_in = .true.
            elseif (token(1:5) .eq. 'STATE' .or.
     &              token(1:6) .eq. 'ASCE41') then
               flag_state_in = .true.
            endif
         enddo
      enddo
!
      if (.not. flag_f_in .and. .not. flag_u_in .and.
     &    .not. flag_state_in) then
         flag_f_in = .true.
         flag_u_in = .true.
         flag_state_in = .true.
      endif
!
      uconn_out_flag_f = flag_f_in
      uconn_out_flag_u = flag_u_in
      uconn_out_flag_state = flag_state_in
      uconn_out_active = .true.
!
      return
      end
!
! ======================================================================
!     Helper: Parse list of integers from tuple string
! ======================================================================
      subroutine parse_uconn_inc_list(str, list, count)
      implicit none
      character*(*) str
      integer list(50), count
      integer i, j, len_s, val, ier
      character*20 num_str
!
      count = 0
      len_s = len_trim(str)
      i = 1
      do while (i .le. len_s .and. count .lt. 50)
         do while (i .le. len_s .and. (str(i:i) .eq. ' ' .or.
     &             str(i:i) .eq. ',' .or. str(i:i) .eq. '('))
            i = i + 1
         enddo
         if (i .gt. len_s .or. str(i:i) .eq. ')') exit
         j = i
         do while (j .le. len_s .and. str(j:j) .ne. ',' .and.
     &             str(j:j) .ne. ')' .and. str(j:j) .ne. ' ')
            j = j + 1
         enddo
         num_str = str(i:j-1)
         read(num_str, *, iostat=ier) val
         if (ier .eq. 0 .and. val .gt. 0) then
            count = count + 1
            list(count) = val
         endif
         i = j + 1
      enddo
      end subroutine parse_uconn_inc_list
!
! ======================================================================
!     Helper: Extract parameter values or tuples from header line
! ======================================================================
      subroutine get_uconn_param_tuple_or_val(header_str, param_name,
     &     items, n_items, max_items)
      implicit none
      character*(*) header_str, param_name
      character*80 items(*)
      integer n_items, max_items
!
      integer h_len, p_len, i_pos, start_idx, end_idx, i, j
      character*500 h_upper, p_upper, val_str
!
      n_items = 0
      h_len = len_trim(header_str)
      p_len = len_trim(param_name)
      if (h_len .eq. 0 .or. p_len .eq. 0) return
!
      h_upper = header_str
      do i = 1, h_len
         if (h_upper(i:i) .ge. 'a' .and. h_upper(i:i) .le. 'z') then
            h_upper(i:i) = char(ichar(h_upper(i:i)) - 32)
         endif
      enddo
      p_upper = param_name
      do i = 1, p_len
         if (p_upper(i:i) .ge. 'a' .and. p_upper(i:i) .le. 'z') then
            p_upper(i:i) = char(ichar(p_upper(i:i)) - 32)
         endif
      enddo
!
      i_pos = index(h_upper(1:h_len), p_upper(1:p_len))
      if (i_pos .le. 0) return
!
      start_idx = i_pos + p_len
      do while (start_idx .le. h_len .and.
     &          header_str(start_idx:start_idx) .eq. ' ')
         start_idx = start_idx + 1
      enddo
      if (start_idx .gt. h_len) return
!
      if (header_str(start_idx:start_idx) .eq. '(') then
         start_idx = start_idx + 1
         end_idx = index(header_str(start_idx:h_len), ')')
         if (end_idx .gt. 0) then
            end_idx = start_idx + end_idx - 2
         else
            end_idx = h_len
         endif
         val_str = header_str(start_idx:end_idx)
      else
         end_idx = start_idx
         do while (end_idx .le. h_len .and.
     &             header_str(end_idx:end_idx) .ne. ',')
            end_idx = end_idx + 1
         enddo
         end_idx = end_idx - 1
         val_str = header_str(start_idx:end_idx)
      endif
!
      i = 1
      do while (i .le. len_trim(val_str) .and. n_items .lt. max_items)
         do while (i .le. len_trim(val_str) .and.
     &             val_str(i:i) .eq. ' ')
            i = i + 1
         enddo
         if (i .gt. len_trim(val_str)) exit
         j = i
         do while (j .le. len_trim(val_str) .and.
     &             val_str(j:j) .ne. ',')
            j = j + 1
         enddo
         n_items = n_items + 1
         items(n_items) = adjustl(val_str(i:j-1))
         i = j + 1
      enddo
      end subroutine get_uconn_param_tuple_or_val
