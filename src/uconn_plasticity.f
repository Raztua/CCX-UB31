!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2025 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
!     ASCE 41-17 nonlinear plastic hinge constitutive model for UCONN6.
!
      subroutine uconn_asce41_eval(nelem, dof_idx, delta_u,
     &                            f_res, k_tang, perf_level)
      use uconn6_module
      implicit none
!
      integer nelem, dof_idx, perf_level
      real*8 delta_u, f_res, k_tang
      real*8 my, th_y, th_cap, c_res, th_u, th_fail, alpha_h
      real*8 ke, th_abs, sgn, khard, mpeak, mres, ksoft
!
      f_res = 0.0d0
      k_tang = 0.0d0
      perf_level = 0
!
      if(.not.allocated(uconn6_is_nonlinear)) return
      if(nelem.gt.size(uconn6_is_nonlinear)) return
      if(uconn6_is_nonlinear(nelem).ne.1) return
!
      if(delta_u.ge.0.0d0) then
         sgn = 1.0d0
         my       = uconn6_asce41(1, nelem)
         th_y     = uconn6_asce41(2, nelem)
         th_cap   = uconn6_asce41(3, nelem)
         c_res    = uconn6_asce41(4, nelem)
         th_u     = uconn6_asce41(5, nelem)
         th_fail  = uconn6_asce41(6, nelem)
         alpha_h  = uconn6_asce41(7, nelem)
      else
         sgn = -1.0d0
         my       = uconn6_asce41(9, nelem)
         th_y     = uconn6_asce41(10, nelem)
         th_cap   = uconn6_asce41(11, nelem)
         c_res    = uconn6_asce41(12, nelem)
         th_u     = uconn6_asce41(5, nelem)
         th_fail  = uconn6_asce41(6, nelem)
         alpha_h  = uconn6_asce41(7, nelem)
         if(my.le.0.0d0) my = uconn6_asce41(1, nelem)
         if(th_y.le.0.0d0) th_y = uconn6_asce41(2, nelem)
         if(th_cap.le.0.0d0) th_cap = uconn6_asce41(3, nelem)
         if(c_res.le.0.0d0) c_res = uconn6_asce41(4, nelem)
      endif
!
      if(th_y.le.1.0d-12) th_y = 1.0d-4
      if(th_cap.le.th_y) th_cap = th_y * 4.0d0
      if(th_u.le.th_cap) th_u = th_cap * 1.5d0
      if(th_fail.le.th_u) th_fail = th_u * 2.0d0
      if(c_res.le.0.0d0) c_res = 0.20d0
      if(alpha_h.le.0.0d0) alpha_h = 0.001d0
!
      ke = my / th_y
      th_abs = dabs(delta_u)
!
!     Branch A-B: Elastic range (0 <= |th| <= th_y)
!
      if(th_abs.le.th_y) then
         f_res = ke * delta_u
         k_tang = ke
         perf_level = 0
!
!     Branch B-C: Plastic plateau / Hardening (th_y < |th| <= th_cap)
!
      elseif(th_abs.le.th_cap) then
         khard = alpha_h * ke
         f_res = sgn * (my + khard * (th_abs - th_y))
         k_tang = khard
         if(th_abs.le.0.010d0) then
            perf_level = 1
         else
            perf_level = 2
         endif
!
!     Branch C-D: Softening descent (th_cap < |th| <= th_u)
!
      elseif(th_abs.le.th_u) then
         khard = alpha_h * ke
         mpeak = my + khard * (th_cap - th_y)
         mres = c_res * my
         ksoft = (mres - mpeak) / (th_u - th_cap)
         f_res = sgn * (mpeak + ksoft * (th_abs - th_cap))
         k_tang = ksoft
         if(th_abs.le.0.025d0) then
            perf_level = 2
         elseif(th_abs.le.0.040d0) then
            perf_level = 3
         else
            perf_level = 4
         endif
!
!     Branch D-E: Residual plateau (th_u < |th| <= th_fail)
!
      elseif(th_abs.le.th_fail) then
         mres = c_res * my
         f_res = sgn * mres
         k_tang = 1.0d-6 * ke
         if(th_abs.le.0.040d0) then
            perf_level = 3
         else
            perf_level = 4
         endif
!
!     Past E: Complete failure (|th| > th_fail)
!
      else
         mres = c_res * my
         f_res = sgn * (0.01d0 * mres)
         k_tang = 1.0d-8 * ke
         perf_level = 4
      endif
!
      return
      end
!
!     Update historical state variables for connector element nelem
!
      subroutine uconn_asce41_update_state(nelem, dof_idx, delta_u,
     &                                    f_res, k_tang, perf_level)
      use uconn6_module
      implicit none
!
      integer nelem, dof_idx, perf_level
      real*8 delta_u, f_res, k_tang, th_y, th_abs, th_max, th_p
!
      if(.not.allocated(uconn6_state)) return
      if(nelem.gt.size(uconn6_state, 2)) return
!
      th_abs = dabs(delta_u)
      th_y = 1.0d-4
      if(allocated(uconn6_asce41)) then
         if(nelem.le.size(uconn6_asce41, 2)) then
            th_y = uconn6_asce41(2, nelem)
         endif
      endif
!
      th_max = uconn6_state(3, nelem)
      if(th_abs.gt.th_max) th_max = th_abs
      th_p = 0.0d0
      if(th_max.gt.th_y) th_p = th_max - th_y
!
      uconn6_state(1, nelem) = delta_u
      uconn6_state(2, nelem) = th_p
      uconn6_state(3, nelem) = th_max
      uconn6_state(4, nelem) = f_res
      uconn6_state(5, nelem) = k_tang
      uconn6_state(6, nelem) = dble(perf_level)
!
      return
      end
