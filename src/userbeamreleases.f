!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine userbeamreleases(inpc,textpart,set,istartset,iendset,
     &  ialset,nset,lakon,ne,irstrt,istep,istat,n,key,iline,ipol,inl,
     &  ipoinp,inp,ipoinpc,ier)
!
!     reading the input deck: *USER BEAM RELEASE
!
!     Allows individual beam elements or ELSET subsets to override
!     section-level release codes without modifying section definitions.
!
!     Syntax:
!       *USER BEAM RELEASE
!       <element_id_or_elset>, <node_end (1 or 2)>, <code (M1, M2, T, M1-M2, ALLM, or int)>
!
!     Both ends can be configured independently across multiple data lines.
!
      use ub31_module
      implicit none
!
      character*1 inpc(*)
      character*8 lakon(*)
      character*80 elset_header
      character*81 set(*), elset
      character*132 textpart(16)
!
      integer istartset(*), iendset(*), ialset(*), irstrt(*),
     &  ipoinp(2,*), inp(3,*), ipoinpc(0:*)
      integer nset, ne, istep, istat, n, key, iline, ipol, inl, ier
      integer i, ielem, inode_end, code_val
      integer ierr_temp, cur_size, new_size, last_code_idx, istat_k
      integer, allocatable :: temp_rel(:,:)
      real*8, allocatable :: temp_spring(:,:,:)
      real*8 spring_k
      logical elset_from_header, has_spring_val
!
      if ((istep .gt. 0) .and. (irstrt(1) .ge. 0)) then
         write(*,*) 
     &     '*ERROR reading *USER BEAM RELEASE: '
     &     //'*USER BEAM RELEASE should'
         write(*,*) '  be placed before all step definitions'
         ier = 1
         return
      endif
!
!     Ensure ielrelease and relspring are allocated and sized properly
!
      cur_size = max(ne, 100000)
      if (.not. allocated(ielrelease)) then
         allocate(ielrelease(3, cur_size))
         ielrelease = -1
         allocate(relspring(6, 3, cur_size))
         relspring = 0.d0
      else
         if (size(ielrelease, 2) .lt. cur_size) then
            allocate(temp_rel(3, cur_size))
            temp_rel = -1
            temp_rel(1:3, 1:size(ielrelease, 2)) = 
     &         ielrelease(1:3, 1:size(ielrelease, 2))
            call move_alloc(temp_rel, ielrelease)

            allocate(temp_spring(6, 3, cur_size))
            temp_spring = 0.d0
            temp_spring(1:6, 1:3, 1:size(relspring, 3)) = 
     &         relspring(1:6, 1:3, 1:size(relspring, 3))
            call move_alloc(temp_spring, relspring)
         endif
      endif
!
      elset_from_header = .false.
      elset_header = ' '
      do i = 2, n
         if (textpart(i)(1:6) .eq. 'ELSET=') then
            elset_header = textpart(i)(7:86)
            elset_from_header = .true.
         endif
      enddo
!
!     Read data lines
!
      do
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if ((istat .lt. 0) .or. (key .eq. 1)) exit
         if (n .le. 0) cycle
!
         spring_k = 0.d0
         has_spring_val = .false.
!
         if (elset_from_header) then
!           Format: <node_end>, <code> [, spring_k]
            if (n .lt. 2) cycle
            elset = elset_header
            call parse_node_end(textpart(1), inode_end, ierr_temp)
            if (ierr_temp .ne. 0) then
               write(*,*) '*ERROR in *USER BEAM RELEASE: invalid end ',
     &              trim(textpart(1))
               call inputerror(inpc,ipoinpc,iline,
     &              "*USER BEAM RELEASE%",ier)
               return
            endif
!
            last_code_idx = n
            if (n .ge. 3) then
               read(textpart(n), *, iostat=istat_k) spring_k
               if (istat_k .eq. 0 .and. spring_k .ge. 0.d0) then
                  call parse_single_release_code(textpart(n), code_val, 
     &                 ierr_temp)
                  if (ierr_temp .ne. 0 .or. 
     &                (index(textpart(n),'.').gt.0 .or. 
     &                 index(textpart(n),'E').gt.0 .or. 
     &                 index(textpart(n),'e').gt.0 .or. 
     &                 spring_k .gt. 63.d0)) then
                     has_spring_val = .true.
                     last_code_idx = n - 1
                  else
                     spring_k = 0.d0
                  endif
               else
                  spring_k = 0.d0
               endif
            endif
!
            call parse_combined_codes(textpart, 2, last_code_idx, 
     &           code_val, ierr_temp)
            if (ierr_temp .ne. 0) then
               write(*,*) '*ERROR in *USER BEAM RELEASE: invalid code'
               call inputerror(inpc,ipoinpc,iline,
     &              "*USER BEAM RELEASE%",ier)
               return
            endif
            call apply_elset_release(elset, inode_end, code_val, 
     &           spring_k, set, nset, istartset, iendset, ialset, 
     &           lakon, ne, inpc, ipoinpc, iline, ier)
            if (ier .ne. 0) return
         else
!           Format: <element_id_or_elset>, <node_end>, <code> [, spring_k]
            if (n .lt. 3) then
               write(*,*) '*ERROR reading *USER BEAM RELEASE: '
               write(*,*) '       line requires: <elem_or_elset>, '//
     &              '<end>, <code>, [spring_k]'
               call inputerror(inpc,ipoinpc,iline,
     &              "*USER BEAM RELEASE%",ier)
               return
            endif
!
            call parse_node_end(textpart(2), inode_end, ierr_temp)
            if (ierr_temp .ne. 0) then
               write(*,*) '*ERROR in *USER BEAM RELEASE: invalid end ',
     &              trim(textpart(2))
               call inputerror(inpc,ipoinpc,iline,
     &              "*USER BEAM RELEASE%",ier)
               return
            endif
!
            last_code_idx = n
            if (n .ge. 4) then
               read(textpart(n), *, iostat=istat_k) spring_k
               if (istat_k .eq. 0 .and. spring_k .ge. 0.d0) then
                  call parse_single_release_code(textpart(n), code_val, 
     &                 ierr_temp)
                  if (ierr_temp .ne. 0 .or. 
     &                (index(textpart(n),'.').gt.0 .or. 
     &                 index(textpart(n),'E').gt.0 .or. 
     &                 index(textpart(n),'e').gt.0 .or. 
     &                 spring_k .gt. 63.d0)) then
                     has_spring_val = .true.
                     last_code_idx = n - 1
                  else
                     spring_k = 0.d0
                  endif
               else
                  spring_k = 0.d0
               endif
            endif
!
            call parse_combined_codes(textpart, 3, last_code_idx, 
     &           code_val, ierr_temp)
            if (ierr_temp .ne. 0) then
               write(*,*) '*ERROR in *USER BEAM RELEASE: invalid code'
               call inputerror(inpc,ipoinpc,iline,
     &              "*USER BEAM RELEASE%",ier)
               return
            endif
!
!           Check if textpart(1) is an element number or ELSET name
!
            read(textpart(1), *, iostat=istat) ielem
            if (istat .eq. 0 .and. ielem .gt. 0) then
!              Single element override
               if (ielem .gt. size(ielrelease, 2)) then
                  new_size = max(ielem, size(ielrelease, 2) * 2)
                  allocate(temp_rel(3, new_size))
                  temp_rel = -1
                  temp_rel(1:3, 1:size(ielrelease, 2)) = 
     &               ielrelease(1:3, 1:size(ielrelease, 2))
                  call move_alloc(temp_rel, ielrelease)

                  allocate(temp_spring(6, 3, new_size))
                  temp_spring = 0.d0
                  temp_spring(1:6, 1:3, 1:size(relspring, 3)) = 
     &               relspring(1:6, 1:3, 1:size(relspring, 3))
                  call move_alloc(temp_spring, relspring)
               endif
               if (lakon(ielem)(1:2) .ne. 'UB') then
                  write(*,*) '*ERROR in *USER BEAM RELEASE: element ',
     &                 ielem, ' is not a user beam element.'
                  ier = 1
                  return
               endif
               if (inode_end .eq. 0) then
                  call set_single_end_release(ielem, 1, code_val, 
     &                                        spring_k)
                  call set_single_end_release(ielem, 2, code_val, 
     &                                        spring_k)
               else
                  call set_single_end_release(ielem, inode_end, 
     &                                        code_val, spring_k)
               endif
            else
!              ELSET name override
               elset = textpart(1)(1:80)
               call apply_elset_release(elset, inode_end, code_val, 
     &              spring_k, set, nset, istartset, iendset, ialset, 
     &              lakon, ne, inpc, ipoinpc, iline, ier)
               if (ier .ne. 0) return
            endif
         endif
      enddo
!
      return
      end subroutine userbeamreleases
!
! ==============================================================================
!     Helper routine to set releases on a single element end
! ==============================================================================
      subroutine set_single_end_release(ielem, end_idx, code_val, 
     &                                  spring_k)
      use ub31_module
      implicit none
      integer ielem, end_idx, code_val, k
      real*8 spring_k
!
      if (ielrelease(end_idx, ielem) .lt. 0) then
         ielrelease(end_idx, ielem) = code_val
      else
         ielrelease(end_idx, ielem) = ior(ielrelease(end_idx, ielem), 
     &                                    code_val)
      endif
!
      do k = 1, 6
         if (iand(code_val, 2**(k-1)) .ne. 0) then
            relspring(k, end_idx, ielem) = spring_k
         endif
      enddo
      end subroutine set_single_end_release
!
! ==============================================================================
!     Helper routine to apply releases to an ELSET
! ==============================================================================
      subroutine apply_elset_release(elset_in, inode_end, code_val, 
     &     spring_k, set, nset, istartset, iendset, ialset, lakon, 
     &     ne, inpc, ipoinpc, iline, ier)
      use ub31_module
      implicit none
      character*(*) elset_in
      character*81 set(*), elset
      character*8 lakon(*)
      character*1 inpc(*)
      integer istartset(*), iendset(*), ialset(*), ipoinpc(0:*)
      integer inode_end, code_val, nset, ne, iline, ier
      integer id, iset, ipos, j, k, elem, new_size
      integer, allocatable :: temp_rel(:,:)
      real*8, allocatable :: temp_spring(:,:,:)
      real*8 spring_k
!
      elset = elset_in
      elset(81:81) = ' '
      ipos = index(elset, ' ')
      if (ipos .le. 1) ipos = len_trim(elset) + 1
      elset(ipos:ipos) = 'E'
      call cident81(set, elset, nset, id)
      iset = nset + 1
      if (id .gt. 0) then
         if (elset .eq. set(id)) then
            iset = id
         endif
      endif
      if (iset .gt. nset) then
         elset(ipos:ipos) = ' '
         write(*,*) '*ERROR reading *USER BEAM RELEASE: element set ',
     &        elset(1:ipos), ' has not yet been defined.'
         call inputerror(inpc,ipoinpc,iline,
     &        "*USER BEAM RELEASE%",ier)
         return
      endif
!
      do j = istartset(iset), iendset(iset)
         if (ialset(j) .gt. 0) then
            elem = ialset(j)
            if (elem .gt. size(ielrelease, 2)) then
               new_size = max(elem, size(ielrelease, 2) * 2)
               allocate(temp_rel(3, new_size))
               temp_rel = -1
               temp_rel(1:3, 1:size(ielrelease, 2)) = 
     &            ielrelease(1:3, 1:size(ielrelease, 2))
               call move_alloc(temp_rel, ielrelease)

               allocate(temp_spring(6, 3, new_size))
               temp_spring = 0.d0
               temp_spring(1:6, 1:3, 1:size(relspring, 3)) = 
     &            relspring(1:6, 1:3, 1:size(relspring, 3))
               call move_alloc(temp_spring, relspring)
            endif
            if (lakon(elem)(1:2) .eq. 'UB') then
               if (inode_end .eq. 0) then
                  call set_single_end_release(elem, 1, code_val, 
     &                                        spring_k)
                  call set_single_end_release(elem, 2, code_val, 
     &                                        spring_k)
               else
                  call set_single_end_release(elem, inode_end, 
     &                                        code_val, spring_k)
               endif
            endif
         else
            k = ialset(j-2)
            do
               k = k - ialset(j)
               if (k .ge. ialset(j-1)) exit
               elem = k
               if (elem .gt. size(ielrelease, 2)) then
                  new_size = max(elem, size(ielrelease, 2) * 2)
                  allocate(temp_rel(3, new_size))
                  temp_rel = -1
                  temp_rel(1:3, 1:size(ielrelease, 2)) = 
     &               ielrelease(1:3, 1:size(ielrelease, 2))
                  call move_alloc(temp_rel, ielrelease)

                  allocate(temp_spring(6, 3, new_size))
                  temp_spring = 0.d0
                  temp_spring(1:6, 1:3, 1:size(relspring, 3)) = 
     &               relspring(1:6, 1:3, 1:size(relspring, 3))
                  call move_alloc(temp_spring, relspring)
               endif
               if (lakon(elem)(1:2) .eq. 'UB') then
                  if (inode_end .eq. 0) then
                     call set_single_end_release(elem, 1, code_val, 
     &                                           spring_k)
                     call set_single_end_release(elem, 2, code_val, 
     &                                           spring_k)
                  else
                     call set_single_end_release(elem, inode_end, 
     &                                           code_val, spring_k)
                  endif
               endif
            enddo
         endif
      enddo
      end subroutine apply_elset_release
!
! ==============================================================================
!     Helper routine to parse node end (1, 2, 3, ALL, BOTH)
! ==============================================================================
      subroutine parse_node_end(str, inode_end, ierr)
      implicit none
      character*(*) str
      integer inode_end, ierr
      character*80 s
      integer istat, len_s, m
!
      s = adjustl(str)
      len_s = len_trim(s)
      inode_end = 0
      ierr = 0
      if (len_s .eq. 0) then
         ierr = 1
         return
      endif
      do m = 1, len_s
         if (s(m:m) .ge. 'a' .and. s(m:m) .le. 'z') then
            s(m:m) = char(ichar(s(m:m)) - 32)
         endif
      enddo
!
      if (s(1:len_s) .eq. '1' .or. s(1:len_s) .eq. 'NODE1' .or.
     &    s(1:len_s) .eq. 'START') then
         inode_end = 1
      elseif (s(1:len_s) .eq. '2' .or. s(1:len_s) .eq. 'NODE2' .or.
     &        s(1:len_s) .eq. 'END') then
         inode_end = 2
      elseif (s(1:len_s) .eq. '3' .or. s(1:len_s) .eq. 'NODE3') then
         inode_end = 3
      elseif (s(1:len_s) .eq. 'ALL' .or. s(1:len_s) .eq. 'BOTH' .or.
     &        s(1:len_s) .eq. '0') then
         inode_end = 0
      else
         read(s(1:len_s), *, iostat=istat) inode_end
         if (istat .ne. 0 .or. inode_end .lt. 1 .or. 
     &       inode_end .gt. 3) then
            ierr = 1
         endif
      endif
      end subroutine parse_node_end
!
! ==============================================================================
!     Helper routine to parse release codes across tokens
! ==============================================================================
      subroutine parse_combined_codes(textpart, start_idx, end_idx, 
     &     code_out, ierr)
      implicit none
      character*132 textpart(16)
      integer start_idx, end_idx, code_out, ierr
      integer i, cval, ierr_temp
!
      code_out = 0
      ierr = 0
      do i = start_idx, end_idx
         if (len_trim(textpart(i)) .eq. 0) cycle
         call parse_single_release_code(textpart(i), cval, ierr_temp)
         if (ierr_temp .ne. 0) then
            ierr = 1
            return
         endif
         code_out = ior(code_out, cval)
      enddo
      end subroutine parse_combined_codes
!
! ==============================================================================
!     Helper routine to parse individual release code string
! ==============================================================================
      subroutine parse_single_release_code(str, cval, ierr)
      implicit none
      character*(*) str
      integer cval, ierr
      character*80 s
      integer istat, len_s, m
!
      s = adjustl(str)
      len_s = len_trim(s)
      cval = 0
      ierr = 0
      if (len_s .eq. 0) return
!
      do m = 1, len_s
         if (s(m:m) .ge. 'a' .and. s(m:m) .le. 'z') then
            s(m:m) = char(ichar(s(m:m)) - 32)
         endif
      enddo
!
      if (s(1:len_s) .eq. 'M1' .or. s(1:len_s) .eq. 'RY' .or.
     &    s(1:len_s) .eq. 'ROTY' .or. s(1:len_s) .eq. 'MY') then
         cval = 16
      elseif (s(1:len_s) .eq. 'M2' .or. s(1:len_s) .eq. 'RZ' .or.
     &        s(1:len_s) .eq. 'ROTZ' .or. s(1:len_s) .eq. 'MZ') then
         cval = 32
      elseif (s(1:len_s) .eq. 'T' .or. s(1:len_s) .eq. 'RX' .or.
     &        s(1:len_s) .eq. 'ROTX' .or. s(1:len_s) .eq. 'MX' .or.
     &        s(1:len_s) .eq. 'TOR' .or.
     &        s(1:len_s) .eq. 'TORSION') then
         cval = 8
      elseif (s(1:len_s) .eq. 'UX' .or. s(1:len_s) .eq. 'U1' .or.
     &        s(1:len_s) .eq. 'AXIAL' .or. s(1:len_s) .eq. 'N' .or.
     &        s(1:len_s) .eq. 'P0') then
         cval = 1
      elseif (s(1:len_s) .eq. 'UY' .or. s(1:len_s) .eq. 'U2' .or.
     &        s(1:len_s) .eq. 'V1' .or. s(1:len_s) .eq. 'VY' .or.
     &        s(1:len_s) .eq. 'SHEAR1') then
         cval = 2
      elseif (s(1:len_s) .eq. 'UZ' .or. s(1:len_s) .eq. 'U3' .or.
     &        s(1:len_s) .eq. 'V2' .or. s(1:len_s) .eq. 'VZ' .or.
     &        s(1:len_s) .eq. 'SHEAR2') then
         cval = 4
      elseif (s(1:len_s) .eq. 'M1-M2' .or. s(1:len_s) .eq. 'M2-M1' .or.
     &        s(1:len_s) .eq. 'M1_M2' .or. s(1:len_s) .eq. 'M1+M2' .or.
     &        s(1:len_s) .eq. 'M2_M1' .or. s(1:len_s) .eq. 'M2+M1' .or.
     &        s(1:len_s) .eq. 'RY-RZ' .or. s(1:len_s) .eq. 'MY-MZ') then
         cval = 48
      elseif (s(1:len_s) .eq. 'ALLM' .or. s(1:len_s) .eq. 'ALL_M' .or.
     &        s(1:len_s) .eq. 'ALL-M' .or. s(1:len_s) .eq. 'BALL' .or.
     &        s(1:len_s) .eq. 'SPHERICAL' .or.
     &        s(1:len_s) .eq. 'M1-M2-T' .or.
     &        s(1:len_s) .eq. 'T-M1-M2') then
         cval = 56
      elseif (s(1:len_s) .eq. 'ALL' .or. s(1:len_s) .eq. 'FREE' .or.
     &        s(1:len_s) .eq. 'DISCONNECTED') then
         cval = 63
      elseif (s(1:len_s) .eq. 'NONE' .or. s(1:len_s) .eq. 'FIXED' .or.
     &        s(1:len_s) .eq. 'RIGID' .or. s(1:len_s) .eq. '0') then
         cval = 0
      else
         read(s(1:len_s), *, iostat=istat) cval
         if (istat .ne. 0 .or. cval .lt. 0 .or. cval .gt. 63) then
            ierr = 1
            cval = 0
         endif
      endif
      end subroutine parse_single_release_code
