!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     Subroutine to parse *USER CONNECTOR cards for UCONN6
!     with linear springs or nonlinear ASCE 41-17 plastic hinge backbones.
!
      subroutine userconnectors(inpc,textpart,set,istartset,iendset,
     &  ialset,nset,ielmat,matname,nmat,ielorien,orname,norien,
     &  lakon,irstrt,istep,istat,n,key,iline,ipol,inl,ipoinp,inp,
     &  ipoinpc,mi,ielprop,nprop,nprop_,prop,nelcon,ier)
!
      use uconn6_module
      implicit none
!
      character*1 inpc(*)
      character*8 lakon(*)
      character*80 matname(*),orname(*),orientation
      character*81 set(*),elset
      character*132 textpart(16)
!
      integer istartset(*),iendset(*),ialset(*),mi(*),ielmat(mi(3),*),
     &  ipoinpc(0:*),ielorien(mi(3),*),iline,ipol,inl,ipoinp(2,*),
     &  inp(3,*),nset,nmat,norien,istep,istat,n,key,i,j,k,
     &  ipos,irstrt(*),nelcon(2,*),ier,
     &  ielprop(*),nprop,nprop_,iset,ndprop,max_ne,cur_ne,
     &  read_cnt,is_asce41,nl_dof
!
      real*8 prop(*),kstiff(6),t_mat(3,3),asce_params(12)
      real*8, allocatable :: temp_stiff(:,:),temp_tm(:,:,:),
     &  temp_forces(:,:),temp_rel_disp(:,:),temp_asce(:,:),
     &  temp_state(:,:)
      integer, allocatable :: temp_is_nl(:)
!
      if((istep.gt.0).and.(irstrt(1).ge.0)) then
         write(*,*) '*ERROR reading *USER CONNECTOR: '
         write(*,*) '       *USER CONNECTOR should be before step'
         ier=1
         return
      endif
!
      elset=' '
      orientation=' '
      is_asce41 = 0
      kstiff(:) = 0.0d0
      asce_params(:) = 0.0d0
      t_mat(:,:) = 0.0d0
      t_mat(1,1) = 1.0d0
      t_mat(2,2) = 1.0d0
      t_mat(3,3) = 1.0d0
!
!     Parse header arguments
!
      do i=2,n
         if(textpart(i)(1:6).eq.'ELSET=') then
            elset=textpart(i)(7:86)
            elset(81:81)=' '
            ipos=index(elset,' ')
            elset(ipos:ipos)='E'
         elseif(textpart(i)(1:12).eq.'ORIENTATION=') then
            orientation=textpart(i)(13:92)
         elseif(index(textpart(i),'ASCE41').gt.0 .or.
     &          index(textpart(i),'NONLINEAR').gt.0 .or.
     &          index(textpart(i),'PLASTIC').gt.0) then
            is_asce41 = 1
         endif
      enddo
!
      if(elset(1:1).eq.' ') then
         write(*,*) '*ERROR reading *USER CONNECTOR: ELSET required'
         call inputerror(inpc,ipoinpc,iline,"*USER CONNECTOR%",ier)
         return
      endif
!
!     Find ELSET in set tables
!
      call cident81(set,elset,nset,iset)
      if(iset.le.0.or.set(iset).ne.elset) then
         write(*,*) '*ERROR: element set not found: ',trim(elset)
         call inputerror(inpc,ipoinpc,iline,"*USER CONNECTOR%",ier)
         return
      endif
!
!     Read Data Line 1: K_ux, K_uy, K_uz, K_rx, K_ry, K_rz
!
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &                ipoinp,inp,ipoinpc)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*) '*ERROR reading *USER CONNECTOR: data missing'
         call inputerror(inpc,ipoinpc,iline,"*USER CONNECTOR%",ier)
         return
      endif
!
      read_cnt = 0
      do i=1,n
         if(textpart(i)(1:1).ne.' '.and.read_cnt.lt.6) then
            read_cnt = read_cnt + 1
            read(textpart(i)(1:20),*,iostat=istat) kstiff(read_cnt)
            if(istat.ne.0) kstiff(read_cnt) = 0.0d0
         endif
      enddo
!
!     If ASCE 41 nonlinear, read Data Line 2:
!     My, theta_y, theta_cap, c_res, theta_u, [alpha_hard, dof_idx, P_crit, du_crit, du_cap_c, c_res_c]
!
      if(is_asce41.eq.1) then
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &                   ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) then
            write(*,*) '*ERROR: ASCE 41 parameters missing'
            call inputerror(inpc,ipoinpc,iline,"*USER CONNECTOR%",ier)
            return
         endif
         read_cnt = 0
         do i=1,n
            if(textpart(i)(1:1).ne.' '.and.read_cnt.lt.12) then
               read_cnt = read_cnt + 1
               read(textpart(i)(1:20),*,iostat=istat)
     &              asce_params(read_cnt)
               if(istat.ne.0) asce_params(read_cnt) = 0.0d0
            endif
         enddo
         if(asce_params(8).le.0.5d0) asce_params(8) = 5.0d0
         nl_dof = int(asce_params(8))
         if(nl_dof.ge.1.and.nl_dof.le.6) then
            if(kstiff(nl_dof).le.1.0d-6.and.asce_params(2).gt.0.0d0)
     &         then
               kstiff(nl_dof) = asce_params(1) / asce_params(2)
            endif
         endif
      endif
!
!     Determine maximum element ID in the set
!
      max_ne = 1000
      do j=istartset(iset),iendset(iset)
         k = ialset(j)
         if(k.gt.max_ne) max_ne = k
      enddo
      max_ne = max_ne + 1000
!
!     Allocate or resize uconn6 arrays in module
!
      if(.not.allocated(uconn6_stiff)) then
         allocate(uconn6_stiff(6, max_ne))
         allocate(uconn6_tm(3, 3, max_ne))
         allocate(uconn6_forces(6, max_ne))
         allocate(uconn6_rel_disp(6, max_ne))
         allocate(uconn6_is_nonlinear(max_ne))
         allocate(uconn6_asce41(12, max_ne))
         allocate(uconn6_state(10, max_ne))
         uconn6_stiff(:,:) = 0.0d0
         uconn6_tm(:,:,:) = 0.0d0
         uconn6_forces(:,:) = 0.0d0
         uconn6_rel_disp(:,:) = 0.0d0
         uconn6_is_nonlinear(:) = 0
         uconn6_asce41(:,:) = 0.0d0
         uconn6_state(:,:) = 0.0d0
         do k=1,max_ne
            uconn6_tm(1,1,k) = 1.0d0
            uconn6_tm(2,2,k) = 1.0d0
            uconn6_tm(3,3,k) = 1.0d0
         enddo
      elseif(size(uconn6_stiff, 2).lt.max_ne) then
         cur_ne = size(uconn6_stiff, 2)
         allocate(temp_stiff(6, cur_ne))
         allocate(temp_tm(3, 3, cur_ne))
         allocate(temp_forces(6, cur_ne))
         allocate(temp_rel_disp(6, cur_ne))
         allocate(temp_is_nl(cur_ne))
         allocate(temp_asce(12, cur_ne))
         allocate(temp_state(10, cur_ne))
         temp_stiff = uconn6_stiff
         temp_tm = uconn6_tm
         temp_forces = uconn6_forces
         temp_rel_disp = uconn6_rel_disp
         temp_is_nl = uconn6_is_nonlinear
         temp_asce = uconn6_asce41
         temp_state = uconn6_state
         deallocate(uconn6_stiff, uconn6_tm, uconn6_forces)
         deallocate(uconn6_rel_disp, uconn6_is_nonlinear)
         deallocate(uconn6_asce41, uconn6_state)
         allocate(uconn6_stiff(6, max_ne))
         allocate(uconn6_tm(3, 3, max_ne))
         allocate(uconn6_forces(6, max_ne))
         allocate(uconn6_rel_disp(6, max_ne))
         allocate(uconn6_is_nonlinear(max_ne))
         allocate(uconn6_asce41(12, max_ne))
         allocate(uconn6_state(10, max_ne))
         uconn6_stiff(:,:) = 0.0d0
         uconn6_tm(:,:,:) = 0.0d0
         uconn6_forces(:,:) = 0.0d0
         uconn6_rel_disp(:,:) = 0.0d0
         uconn6_is_nonlinear(:) = 0
         uconn6_asce41(:,:) = 0.0d0
         uconn6_state(:,:) = 0.0d0
         do k=1,max_ne
            uconn6_tm(1,1,k) = 1.0d0
            uconn6_tm(2,2,k) = 1.0d0
            uconn6_tm(3,3,k) = 1.0d0
         enddo
         uconn6_stiff(:, 1:cur_ne) = temp_stiff
         uconn6_tm(:,:, 1:cur_ne) = temp_tm
         uconn6_forces(:, 1:cur_ne) = temp_forces
         uconn6_rel_disp(:, 1:cur_ne) = temp_rel_disp
         uconn6_is_nonlinear(1:cur_ne) = temp_is_nl
         uconn6_asce41(:, 1:cur_ne) = temp_asce
         uconn6_state(:, 1:cur_ne) = temp_state
         deallocate(temp_stiff, temp_tm, temp_forces)
         deallocate(temp_rel_disp, temp_is_nl, temp_asce, temp_state)
      endif
!
!     Assign connector properties to each element in ELSET
!
      ndprop = 16
      if(nprop+ndprop.le.nprop_) then
         prop(nprop+1:nprop+6) = kstiff(1:6)
         prop(nprop+7) = t_mat(1,1)
         prop(nprop+8) = t_mat(1,2)
         prop(nprop+9) = t_mat(1,3)
         prop(nprop+10) = t_mat(2,1)
         prop(nprop+11) = t_mat(2,2)
         prop(nprop+12) = t_mat(2,3)
         prop(nprop+13) = t_mat(3,1)
         prop(nprop+14) = t_mat(3,2)
         prop(nprop+15) = t_mat(3,3)
         prop(nprop+16) = dble(is_asce41)
         do j=istartset(iset),iendset(iset)
            k = ialset(j)
            if(k.gt.0) then
               uconn6_stiff(:, k) = kstiff(:)
               uconn6_tm(:,:, k) = t_mat(:,:)
               uconn6_is_nonlinear(k) = is_asce41
               uconn6_asce41(:, k) = asce_params(:)
               ielprop(k) = nprop + 1
            endif
         enddo
         nprop = nprop + ndprop
      else
         do j=istartset(iset),iendset(iset)
            k = ialset(j)
            if(k.gt.0) then
               uconn6_stiff(:, k) = kstiff(:)
               uconn6_tm(:,:, k) = t_mat(:,:)
               uconn6_is_nonlinear(k) = is_asce41
               uconn6_asce41(:, k) = asce_params(:)
            endif
         enddo
      endif
!
      return
      end
