!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
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
!     ==================================================================
!     userloadcombinations.f
!     Synthesizes active loads for a new step from factored prior steps
!     ==================================================================
      subroutine userloadcombinations(inpc,textpart,istep,istat,n,key,
     &     iline,ipol,inl,ipoinp,inp,ipoinpc,
     &     nodeforc,ndirforc,xforc,iamforc,ikforc,ilforc,
     &     nforc,nforc_,cload_flag,
     &     nelemload,sideload,xload,iamload,nload,nload_,dload_flag,
     &     cbody,ibody,xbody,xbodyold,nbody,nbody_,nam,ier)
        use usercomb_module
        implicit none

        character*1 inpc(*)
        character*132 textpart(16)
        character*20 sideload(*)
        character*81 cbody(*)

        integer istep, istat, n, key, iline, ipol, inl, ier, nam
        integer ipoinp(2,*), inp(3,*), ipoinpc(0:*)
        integer nodeforc(2,*), ndirforc(*), iamforc(*), nforc, nforc_
        integer ikforc(*), ilforc(*)
        integer nelemload(2,*), iamload(2,*), nload, nload_
        integer ibody(3,*), nbody, nbody_
        real*8 xforc(*), xload(2,*), xbody(7,*), xbodyold(7,*)
        logical cload_flag, dload_flag

        character*80 lc_token, factor_token, test_tok, comb_name
        integer j, k, lc_src, istat_val, node, dir, elem, m, jstart
        integer idof, id, test_int
        real*8 factor, val, val1, val2
        logical found, first_line

        if (istep .lt. 1) then
           write(*,*) '*ERROR reading *USER LOAD COMBINATION:'
           write(*,*) '       *USER LOAD COMBINATION must be used'
           write(*,*) '       inside a *STEP block'
           ier = 1
           return
        endif

!       Clear existing active loads in current step (fresh combination)
        nforc = 0
        nload = 0
        nbody = 0
        cload_flag = .true.
        dload_flag = .true.

        first_line = .true.
        comb_name = ' '

        write(*, '(A, I0, A)') ' === Synthesizing *USER LOAD '//
     &       'COMBINATION for Step ', istep, ' ==='

!       Read combination definition data lines
        do
           call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &          ipoinp,inp,ipoinpc)
           if ((istat .lt. 0) .or. (key .eq. 1)) exit
           if (n .lt. 1) cycle

!          Check if first token is a combination name
           jstart = 1
           test_tok = trim(adjustl(textpart(1)))
           read(test_tok, *, iostat=istat_val) test_int
           if (istat_val .ne. 0 .or. mod(n, 2) .eq. 1) then
              comb_name = test_tok
              if (first_line .and. len_trim(comb_name) .gt. 0) then
                 write(*, '(A, A)') '   Combination Name: ',
     &                trim(comb_name)
                 first_line = .false.
              endif
              jstart = 2
           endif

           if (jstart .gt. n) cycle

!          Parse (load_case, factor) pairs on this line
           do j = jstart, n, 2
              if (j + 1 .gt. n) then
                 write(*,*) '*WARNING reading *USER LOAD COMBINATION:'
                 write(*,*) '         missing factor for token: ',
     &                trim(textpart(j))
                 exit
              endif

              lc_token = trim(adjustl(textpart(j)))
              factor_token = trim(adjustl(textpart(j+1)))

              if (len_trim(lc_token) .eq. 0 .or.
     &            len_trim(factor_token) .eq. 0) exit

              read(lc_token, *, iostat=istat_val) lc_src
              if (istat_val .ne. 0 .or. lc_src .lt. 1) then
                 write(*,*) '*WARNING reading *USER LOAD COMBINATION:'
                 write(*,*) '         invalid load case step number: ',
     &                trim(lc_token)
                 cycle
              endif

              read(factor_token, *, iostat=istat_val) factor
              if (istat_val .ne. 0) then
                 write(*,*) '*WARNING reading *USER LOAD COMBINATION:'
                 write(*,*) '         invalid factor value: ',
     &                trim(factor_token)
                 cycle
              endif

              if (lc_src .ge. istep .or. lc_src .gt. MAX_COMB_STEPS)
     &             then
                 write(*,*) '*ERROR reading *USER LOAD COMBINATION:'
                 write(*,*) '       referenced step ', lc_src,
     &                ' must be a previously defined step'
                 write(*,*) '       (< ', istep, ')'
                 ier = 1
                 return
              endif

              if (lib_steps(lc_src)%is_saved .ne. 1) then
                 write(*,*) '*WARNING reading *USER LOAD COMBINATION:'
                 write(*,*) '         no loads in Step ', lc_src
                 cycle
              endif

              write(*, '(A, F8.3, A, I0)') '   + Factored Load: ',
     &             factor, ' * Step ', lc_src

!             ----------------------------------------------------------
!             1. Synthesize Concentrated Loads (CLOAD)
!             ----------------------------------------------------------
              do k = 1, lib_steps(lc_src)%nforc
                 node = lib_steps(lc_src)%nodeforc(1, k)
                 dir  = lib_steps(lc_src)%ndirforc(k)
                 val  = factor * lib_steps(lc_src)%xforc(k)

                 found = .false.
                 do m = 1, nforc
                    if (nodeforc(1, m) .eq. node .and.
     &                  ndirforc(m) .eq. dir) then
                       xforc(m) = xforc(m) + val
                       found = .true.
                       exit
                    endif
                 enddo

                 if (.not. found) then
                    if (nforc .ge. nforc_) then
                       write(*,*) '*ERROR in userloadcombinations: '//
     &                      'increase nforc_'
                       ier = 1
                       return
                    endif
                    nforc = nforc + 1
                    nodeforc(1, nforc) = node
                    nodeforc(2, nforc) = 
     &                   lib_steps(lc_src)%nodeforc(2, k)
                    ndirforc(nforc)    = dir
                    xforc(nforc)       = val
                    if (nam .gt. 0) then
                       iamforc(nforc)  = 
     &                      lib_steps(lc_src)%iamforc(k)
                    endif

                    idof = 8 * (node - 1) + dir
                    call nident(ikforc, idof, nforc - 1, id)
                    do m = nforc, id + 2, -1
                       ikforc(m) = ikforc(m - 1)
                       ilforc(m) = ilforc(m - 1)
                    enddo
                    ikforc(id + 1) = idof
                    ilforc(id + 1) = nforc
                 endif
              enddo

!             ----------------------------------------------------------
!             2. Synthesize Distributed Loads (DLOAD)
!             ----------------------------------------------------------
              do k = 1, lib_steps(lc_src)%nload
                 elem = lib_steps(lc_src)%nelemload(1, k)
                 val1 = factor * lib_steps(lc_src)%xload(1, k)
                 val2 = factor * lib_steps(lc_src)%xload(2, k)

                 found = .false.
                 do m = 1, nload
                    if (nelemload(1, m) .eq. elem .and.
     &                  sideload(m) .eq. 
     &                  lib_steps(lc_src)%sideload(k)) then
                       xload(1, m) = xload(1, m) + val1
                       xload(2, m) = xload(2, m) + val2
                       found = .true.
                       exit
                    endif
                 enddo

                 if (.not. found) then
                    if (nload .ge. nload_) then
                       write(*,*) '*ERROR in userloadcombinations: '//
     &                      'increase nload_'
                       ier = 1
                       return
                    endif
                    nload = nload + 1
                    nelemload(1, nload) = elem
                    nelemload(2, nload) = 
     &                   lib_steps(lc_src)%nelemload(2, k)
                    sideload(nload)     = 
     &                   lib_steps(lc_src)%sideload(k)
                    xload(1, nload)     = val1
                    xload(2, nload)     = val2
                    if (nam .gt. 0) then
                       iamload(1, nload) = 
     &                      lib_steps(lc_src)%iamload(1, k)
                       iamload(2, nload) = 
     &                      lib_steps(lc_src)%iamload(2, k)
                    endif
                 endif
              enddo

!             ----------------------------------------------------------
!             3. Synthesize Body / Gravity Loads (BODY / GRAV)
!             ----------------------------------------------------------
              do k = 1, lib_steps(lc_src)%nbody
                 found = .false.
                 do m = 1, nbody
                    if (cbody(m) .eq. 
     &                   lib_steps(lc_src)%cbody(k)) then
                       do dir = 1, 7
                          xbody(dir, m) = xbody(dir, m) +
     &                      factor * lib_steps(lc_src)%xbody(dir, k)
                       enddo
                       found = .true.
                       exit
                    endif
                 enddo

                 if (.not. found) then
                    if (nbody .ge. nbody_) then
                       write(*,*) '*ERROR in userloadcombinations: '//
     &                      'increase nbody_'
                       ier = 1
                       return
                    endif
                    nbody = nbody + 1
                    cbody(nbody) = lib_steps(lc_src)%cbody(k)
                    ibody(1, nbody) = lib_steps(lc_src)%ibody(1, k)
                    ibody(2, nbody) = lib_steps(lc_src)%ibody(2, k)
                    ibody(3, nbody) = lib_steps(lc_src)%ibody(3, k)
                    do dir = 1, 7
                       xbody(dir, nbody) = factor *
     &                      lib_steps(lc_src)%xbody(dir, k)
                       xbodyold(dir, nbody) = 0.d0
                    enddo
                 endif
              enddo

           enddo
        enddo

        write(*, '(A, I0, A, I0, A, I0)')
     &       '   -> Synthesized Total: CLOAD = ', nforc,
     &       ', DLOAD = ', nload, ', BODY = ', nbody

        return
      end subroutine userloadcombinations
