!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     Element kernel for UCONN6: 2-node, 6-DOF Zero-Length Connector
!     with ASCE 41-17 nonlinear plastic hinge tangent stiffness.
!
      subroutine e_c3d_uconn6(co,kon,lakonl,p1,p2,omx,bodyfx,nbody,s,
     &     sm,ff,nelem,nmethod,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
     &     alzero,ielmat,ielorien,norien,orab,ntmat_,
     &     t0,t1,ithermal,vold,iperturb,nelemload,
     &     sideload,xload,nload,idist,sti,stx,iexpl,plicon,
     &     nplicon,plkcon,nplkcon,xstiff,npmat_,dtime,
     &     matname,mi,ncmat_,mass,stiffness,buckling,rhsi,intscheme,
     &     ttime,time,istep,iinc,coriolis,xloadold,reltime,
     &     ipompc,nodempc,coefmpc,nmpc,ikmpc,ilmpc,veold,
     &     ne0,ipkon,thicke,
     &     integerglob,doubleglob,tieset,istartset,iendset,ialset,ntie,
     &     nasym,ielprop,prop)
!
      use uconn6_module
      implicit none
!
      integer mass(*),stiffness,buckling,rhsi,coriolis
      character*8 lakonl
      character*20 sideload(*)
      character*80 matname(*)
      character*81 tieset(3,*)
!
      integer kon(*),nelemload(2,*),nbody,nelem,mi(*),ielprop(*),
     &     ithermal(*),iperturb(*),nload,idist,nmethod,nelcon(2,*),
     &     nrhcon(*),nalcon(2,*),ielmat(mi(3),*),ielorien(mi(3),*),
     &     ipkon(*),ntmat_,norien,iexpl,ncmat_,intscheme,istep,iinc,
     &     ipompc(*),nodempc(3,*),nmpc,ikmpc(*),ilmpc(*),ne0,
     &     istartset(*),iendset(*),ialset(*),ntie,integerglob(*),
     &     nasym,nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_,
     &     i,j,k,ip,n1,n2,nl_dof,perf_level
!
      real*8 co(3,*),veold(0:mi(2),*),s(60,60),sm(60,60),ff(60),
     &     bodyfx(3),p1(3),p2(3),rhcon(0:1,ntmat_,*),reltime,prop(*),
     &     alcon(0:6,ntmat_,*),alzero(*),orab(7,*),t0(*),t1(*),
     &     xloadold(2,*),vold(0:mi(2),*),xload(2,*),omx,sti(6,mi(1),*),
     &     stx(6,mi(1),*),coefmpc(*),thicke(mi(3),*),doubleglob(*),
     &     plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &     xstiff(27,mi(1),*),dtime,ttime,time,
     &     elcon(0:ncmat_,ntmat_,*),
     &     kstiff(6),t_mat(3,3),k_trans(3,3),k_rot(3,3),
     &     kt_glob(3,3),kr_glob(3,3),k_glob(6,6),
     &     u1(6),u2(6),du_glob(6),du_loc(6),f_res,k_tang
!
      s(:,:) = 0.0d0
      sm(:,:) = 0.0d0
      ff(:) = 0.0d0
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
!     Evaluate current relative displacement for nonlinear tangent
!
      n1 = kon(ipkon(nelem) + 1)
      n2 = kon(ipkon(nelem) + 2)
      do i=1,6
         u1(i) = vold(i, n1)
         u2(i) = vold(i, n2)
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
!     If nonlinear ASCE 41 plastic hinge is active, get tangent stiffness
!
      if(allocated(uconn6_is_nonlinear)) then
         if(nelem.le.size(uconn6_is_nonlinear)) then
            if(uconn6_is_nonlinear(nelem).eq.1) then
               nl_dof = int(uconn6_asce41(8, nelem))
               if(nl_dof.ge.1.and.nl_dof.le.6) then
                  call uconn_asce41_eval(nelem, nl_dof, du_loc(nl_dof),
     &                                  f_res, k_tang, perf_level)
                  kstiff(nl_dof) = k_tang
               endif
            endif
         endif
      endif
!
      k_trans(:,:) = 0.0d0
      k_trans(1,1) = kstiff(1)
      k_trans(2,2) = kstiff(2)
      k_trans(3,3) = kstiff(3)
!
      k_rot(:,:) = 0.0d0
      k_rot(1,1) = kstiff(4)
      k_rot(2,2) = kstiff(5)
      k_rot(3,3) = kstiff(6)
!
      kt_glob(:,:) = 0.0d0
      kr_glob(:,:) = 0.0d0
      do i=1,3
         do j=1,3
            do k=1,3
               kt_glob(i,j) = kt_glob(i,j) +
     &              t_mat(k,i) * k_trans(k,k) * t_mat(k,j)
               kr_glob(i,j) = kr_glob(i,j) +
     &              t_mat(k,i) * k_rot(k,k) * t_mat(k,j)
            enddo
         enddo
      enddo
!
      k_glob(:,:) = 0.0d0
      k_glob(1:3, 1:3) = kt_glob(1:3, 1:3)
      k_glob(4:6, 4:6) = kr_glob(1:3, 1:3)
!
      do i=1,6
         do j=1,6
            s(i,   j)   = +k_glob(i,j)
            s(i,   j+6) = -k_glob(i,j)
            s(i+6, j)   = -k_glob(i,j)
            s(i+6, j+6) = +k_glob(i,j)
         enddo
      enddo
!
      return
      end
