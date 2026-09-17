!     ==================================================================
!     usercomb_module.f
!     Holds step load libraries and load combination functions for CCX
!     Optimized Dynamic Memory Layout with Per-Step Allocation
!     ==================================================================
      module usercomb_module
        implicit none

        integer, parameter :: MAX_COMB_STEPS = 500

        type :: step_load_type
           integer :: is_saved
           integer :: nforc
           integer, allocatable :: nodeforc(:,:)
           integer, allocatable :: ndirforc(:)
           real*8, allocatable  :: xforc(:)
           integer, allocatable :: iamforc(:)

           integer :: nload
           integer, allocatable :: nelemload(:,:)
           character*20, allocatable :: sideload(:)
           real*8, allocatable  :: xload(:,:)
           integer, allocatable :: iamload(:,:)

           integer :: nbody
           character*81, allocatable :: cbody(:)
           integer, allocatable :: ibody(:,:)
           real*8, allocatable  :: xbody(:,:)
           real*8, allocatable  :: xbodyold(:,:)
        end type step_load_type

        type(step_load_type), save :: lib_steps(MAX_COMB_STEPS)
        integer, save :: lib_initialized = 0

      contains

!       ================================================================
!       save_step_loads
!       Snapshots active applied loads at the end of each step
!       ================================================================
        subroutine save_step_loads(istep, nforc, nodeforc, ndirforc,
     &       xforc, iamforc, nforc_, nload, nelemload, sideload,
     &       xload, iamload, nload_, nbody, cbody, ibody, xbody,
     &       xbodyold, nbody_, nam)
          implicit none

          integer istep, nforc, nforc_, nload, nload_, nbody
          integer nbody_, nam
          integer nodeforc(2,*), ndirforc(*), iamforc(*)
          integer nelemload(2,*), iamload(2,*), ibody(3,*)
          character*20 sideload(*)
          character*81 cbody(*)
          real*8 xforc(*), xload(2,*), xbody(7,*), xbodyold(7,*)
          integer j, i

          if (istep .lt. 1 .or. istep .gt. MAX_COMB_STEPS) return

          if (lib_initialized .eq. 0) then
             do i = 1, MAX_COMB_STEPS
                lib_steps(i)%is_saved = 0
                lib_steps(i)%nforc = 0
                lib_steps(i)%nload = 0
                lib_steps(i)%nbody = 0
             enddo
             lib_initialized = 1
          endif

!         Deallocate existing if step is being rewritten
          if (allocated(lib_steps(istep)%nodeforc)) 
     &         deallocate(lib_steps(istep)%nodeforc)
          if (allocated(lib_steps(istep)%ndirforc)) 
     &         deallocate(lib_steps(istep)%ndirforc)
          if (allocated(lib_steps(istep)%xforc)) 
     &         deallocate(lib_steps(istep)%xforc)
          if (allocated(lib_steps(istep)%iamforc)) 
     &         deallocate(lib_steps(istep)%iamforc)

          if (allocated(lib_steps(istep)%nelemload)) 
     &         deallocate(lib_steps(istep)%nelemload)
          if (allocated(lib_steps(istep)%sideload)) 
     &         deallocate(lib_steps(istep)%sideload)
          if (allocated(lib_steps(istep)%xload)) 
     &         deallocate(lib_steps(istep)%xload)
          if (allocated(lib_steps(istep)%iamload)) 
     &         deallocate(lib_steps(istep)%iamload)

          if (allocated(lib_steps(istep)%cbody)) 
     &         deallocate(lib_steps(istep)%cbody)
          if (allocated(lib_steps(istep)%ibody)) 
     &         deallocate(lib_steps(istep)%ibody)
          if (allocated(lib_steps(istep)%xbody)) 
     &         deallocate(lib_steps(istep)%xbody)
          if (allocated(lib_steps(istep)%xbodyold)) 
     &         deallocate(lib_steps(istep)%xbodyold)

!         Save concentrated forces
          lib_steps(istep)%nforc = nforc
          if (nforc .gt. 0) then
             allocate(lib_steps(istep)%nodeforc(2, nforc))
             allocate(lib_steps(istep)%ndirforc(nforc))
             allocate(lib_steps(istep)%xforc(nforc))
             allocate(lib_steps(istep)%iamforc(nforc))

             do j = 1, nforc
                lib_steps(istep)%nodeforc(1, j) = nodeforc(1, j)
                lib_steps(istep)%nodeforc(2, j) = nodeforc(2, j)
                lib_steps(istep)%ndirforc(j)    = ndirforc(j)
                lib_steps(istep)%xforc(j)       = xforc(j)
                if (nam .gt. 0) then
                   lib_steps(istep)%iamforc(j)  = iamforc(j)
                else
                   lib_steps(istep)%iamforc(j)  = 0
                endif
             enddo
          endif

!         Save distributed loads
          lib_steps(istep)%nload = nload
          if (nload .gt. 0) then
             allocate(lib_steps(istep)%nelemload(2, nload))
             allocate(lib_steps(istep)%sideload(nload))
             allocate(lib_steps(istep)%xload(2, nload))
             allocate(lib_steps(istep)%iamload(2, nload))

             do j = 1, nload
                lib_steps(istep)%nelemload(1, j) = nelemload(1, j)
                lib_steps(istep)%nelemload(2, j) = nelemload(2, j)
                lib_steps(istep)%sideload(j)     = sideload(j)
                lib_steps(istep)%xload(1, j)     = xload(1, j)
                lib_steps(istep)%xload(2, j)     = xload(2, j)
                if (nam .gt. 0) then
                   lib_steps(istep)%iamload(1, j) = iamload(1, j)
                   lib_steps(istep)%iamload(2, j) = iamload(2, j)
                else
                   lib_steps(istep)%iamload(1, j) = 0
                   lib_steps(istep)%iamload(2, j) = 0
                endif
             enddo
          endif

!         Save body / gravity loads
          lib_steps(istep)%nbody = nbody
          if (nbody .gt. 0) then
             allocate(lib_steps(istep)%cbody(nbody))
             allocate(lib_steps(istep)%ibody(3, nbody))
             allocate(lib_steps(istep)%xbody(7, nbody))
             allocate(lib_steps(istep)%xbodyold(7, nbody))

             do j = 1, nbody
                lib_steps(istep)%cbody(j)       = cbody(j)
                lib_steps(istep)%ibody(1, j)    = ibody(1, j)
                lib_steps(istep)%ibody(2, j)    = ibody(2, j)
                lib_steps(istep)%ibody(3, j)    = ibody(3, j)
                lib_steps(istep)%xbody(1, j)    = xbody(1, j)
                lib_steps(istep)%xbody(2, j)    = xbody(2, j)
                lib_steps(istep)%xbody(3, j)    = xbody(3, j)
                lib_steps(istep)%xbody(4, j)    = xbody(4, j)
                lib_steps(istep)%xbody(5, j)    = xbody(5, j)
                lib_steps(istep)%xbody(6, j)    = xbody(6, j)
                lib_steps(istep)%xbody(7, j)    = xbody(7, j)
                lib_steps(istep)%xbodyold(1, j) = xbodyold(1, j)
             enddo
          endif

          lib_steps(istep)%is_saved = 1
        end subroutine save_step_loads

      end module usercomb_module
