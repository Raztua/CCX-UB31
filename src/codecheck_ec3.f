!     ==================================================================
!     codecheck_ec3.f
!     Eurocode 3 (EN 1993-1-1:2005) Structural Steel Code Checking
!     ==================================================================
      subroutine calc_ec3_properties(sect_type, dims, E, nu, fy,
     &  A, Iyy, Izz, Wel_y, Wel_z, Wpl_y, Wpl_z, Av_y, Av_z,
     &  It, Iw, iy, iz, iclass)
      implicit none
      real*8, intent(in) :: sect_type, dims(6), E, nu, fy
      real*8, intent(out) :: A, Iyy, Izz, Wel_y, Wel_z, Wpl_y, Wpl_z
      real*8, intent(out) :: Av_y, Av_z, It, Iw, iy, iz
      integer, intent(out) :: iclass

      real*8 :: b, h, t_f, t_w, b_t, t_t, b_b, t_b, r_o, t, r_i
      real*8 :: pi, eps, c_f, c_w, cf_tf, cw_tw
      integer :: st

      pi = 4.d0 * datan(1.d0)
      st = nint(sect_type)
      eps = dsqrt(235.0d6 / max(fy, 1.0d6))

      ! Defaults
      A = 0.01d0
      Iyy = 1.0d-5
      Izz = 1.0d-5
      Wel_y = 1.0d-4
      Wel_z = 1.0d-4
      Wpl_y = 1.0d-4
      Wpl_z = 1.0d-4
      Av_y = 0.005d0
      Av_z = 0.005d0
      It = 1.0d-6
      Iw = 0.0d0
      iclass = 1

      if (st .eq. 1) then
!        Rectangular Solid (dims: b, h)
         b = dims(1)
         h = dims(2)
         A = b * h
         Iyy = (1.d0/12.d0) * b * (h**3)
         Izz = (1.d0/12.d0) * h * (b**3)
         Wel_y = (1.d0/6.d0) * b * (h**2)
         Wel_z = (1.d0/6.d0) * h * (b**2)
         Wpl_y = 0.25d0 * b * (h**2)
         Wpl_z = 0.25d0 * h * (b**2)
         Av_y = (5.d0/6.d0) * A
         Av_z = (5.d0/6.d0) * A
         if (b .le. h) then
            It = (1.d0/3.d0) * (b**3) * h * (1.d0 - 0.63d0*(b/h))
         else
            It = (1.d0/3.d0) * (h**3) * b * (1.d0 - 0.63d0*(h/b))
         endif
         Iw = 0.d0
         iclass = 1

      elseif (st .eq. 2) then
!        Circular / Pipe (dims: r_outer, thickness)
         r_o = dims(1)
         t   = dims(2)
         if (t .le. 0.d0) then
            A = pi * (r_o**2)
            Iyy = 0.25d0 * pi * (r_o**4)
            Izz = Iyy
            Wel_y = (pi / 4.d0) * (r_o**3)
            Wel_z = Wel_y
            Wpl_y = (4.d0 / 3.d0) * (r_o**3)
            Wpl_z = Wpl_y
            Av_y = 0.90d0 * A
            Av_z = Av_y
            It = 0.5d0 * pi * (r_o**4)
            Iw = 0.d0
            iclass = 1
         else
            r_i = max(0.d0, r_o - t)
            A = pi * (r_o**2 - r_i**2)
            Iyy = 0.25d0 * pi * (r_o**4 - r_i**4)
            Izz = Iyy
            Wel_y = Iyy / r_o
            Wel_z = Wel_y
            Wpl_y = (4.d0 / 3.d0) * (r_o**3 - r_i**3)
            Wpl_z = Wpl_y
            Av_y = (2.d0 / pi) * A
            Av_z = Av_y
            It = 2.d0 * Iyy
            Iw = 0.d0

            if ((2.d0 * r_o / t) .le. 50.d0 * (eps**2)) then
               iclass = 1
            elseif ((2.d0 * r_o / t) .le. 70.d0 * (eps**2)) then
               iclass = 2
            elseif ((2.d0 * r_o / t) .le. 90.d0 * (eps**2)) then
               iclass = 3
            else
               iclass = 3
            endif
         endif

      elseif (st .eq. 3) then
!        I-Beam / H-Section (dims: h, b_top, t_f_top, b_bot, t_f_bot, t_w)
         h   = dims(1)
         b_t = dims(2)
         t_t = dims(3)
         b_b = dims(4)
         t_b = dims(5)
         t_w = dims(6)

         b = 0.5d0 * (b_t + b_b)
         t_f = 0.5d0 * (t_t + t_b)

         A = 2.d0 * b * t_f + (h - 2.d0 * t_f) * t_w
         Iyy = (1.d0/12.d0) * t_w * ((h - 2.d0*t_f)**3) + 
     &         2.d0 * ((1.d0/12.d0)*b*(t_f**3) + 
     &                 b*t_f*((0.5d0*(h - t_f))**2))
         Izz = 2.d0 * ((1.d0/12.d0) * t_f * (b**3)) + 
     &         (1.d0/12.d0) * (h - 2.d0*t_f) * (t_w**3)

         Wel_y = Iyy / (0.5d0 * h)
         Wel_z = Izz / (0.5d0 * b)

         Wpl_y = b * t_f * (h - t_f) + 
     &           0.25d0 * t_w * ((h - 2.d0*t_f)**2)
         Wpl_z = 2.d0 * (0.25d0 * t_f * (b**2)) + 
     &           0.25d0 * (h - 2.d0*t_f) * (t_w**2)

         Av_z = max(A - 2.d0 * b * t_f + (t_w + 2.d0 * t_f) * t_f,
     &              (h - 2.d0*t_f) * t_w)
         Av_y = 2.d0 * b * t_f

         It = (1.d0/3.d0) * (2.d0*b*(t_f**3) + (h - 2.d0*t_f)*(t_w**3))
         Iw = (Izz * ((h - t_f)**2)) / 4.d0

         c_f = (b - t_w) / 2.d0
         cf_tf = c_f / max(t_f, 1.0d-6)
         c_w = h - 2.d0 * t_f
         cw_tw = c_w / max(t_w, 1.0d-6)

         if (cf_tf .le. 9.d0 * eps .and. cw_tw .le. 72.d0 * eps) then
            iclass = 1
         elseif (cf_tf .le. 10.d0 * eps .and. 
     &           cw_tw .le. 83.d0 * eps) then
            iclass = 2
         elseif (cf_tf .le. 14.d0 * eps .and. 
     &           cw_tw .le. 124.d0 * eps) then
            iclass = 3
         else
            iclass = 3
         endif

      elseif (st .eq. 7) then
!        Box / RHS (dims: h, b, t_bot, t_left, t_top, t_right)
         h = dims(1)
         b = dims(2)
         t_b = dims(3)
         t_w = dims(4)
         t_t = dims(5)
         t_f = 0.5d0 * (t_b + t_t)

         A = 2.d0 * b * t_f + 2.d0 * (h - 2.d0 * t_f) * t_w
         Iyy = (1.d0/12.d0) * b * (h**3) - 
     &         (1.d0/12.d0) * (b - 2.d0*t_w) * ((h - 2.d0*t_f)**3)
         Izz = (1.d0/12.d0) * h * (b**3) - 
     &         (1.d0/12.d0) * (h - 2.d0*t_f) * ((b - 2.d0*t_w)**3)

         Wel_y = Iyy / (0.5d0 * h)
         Wel_z = Izz / (0.5d0 * b)

         Wpl_y = 0.25d0 * b * (h**2) - 
     &           0.25d0 * (b - 2.d0*t_w) * ((h - 2.d0*t_f)**2)
         Wpl_z = 0.25d0 * h * (b**2) - 
     &           0.25d0 * (h - 2.d0*t_f) * ((b - 2.d0*t_w)**2)

         Av_z = A * (h / (b + h))
         Av_y = A * (b / (b + h))

         It = (4.d0 * ((h - t_f)**2) * ((b - t_w)**2)) / 
     &        (2.d0*(h - t_f)/t_w + 2.d0*(b - t_w)/t_f)
         Iw = 0.d0

         c_f = b - 2.d0 * t_w
         cf_tf = c_f / max(t_f, 1.0d-6)
         c_w = h - 2.d0 * t_f
         cw_tw = c_w / max(t_w, 1.0d-6)

         if (cf_tf .le. 33.d0 * eps .and. cw_tw .le. 72.d0 * eps) then
            iclass = 1
         elseif (cf_tf .le. 38.d0 * eps .and. 
     &           cw_tw .le. 83.d0 * eps) then
            iclass = 2
         elseif (cf_tf .le. 42.d0 * eps .and. 
     &           cw_tw .le. 124.d0 * eps) then
            iclass = 3
         else
            iclass = 3
         endif

      else
!        Other / Channel / Angle fallback
         b = dims(1)
         h = dims(2)
         t_f = dims(3)
         t_w = dims(4)
         A = b * t_f + (h - t_f) * t_w
         Iyy = (1.d0/12.d0) * b * (h**3)
         Izz = (1.d0/12.d0) * h * (b**3)
         Wel_y = (1.d0/6.d0) * b * (h**2)
         Wel_z = (1.d0/6.d0) * h * (b**2)
         Wpl_y = 1.25d0 * Wel_y
         Wpl_z = 1.25d0 * Wel_z
         Av_y = 0.5d0 * A
         Av_z = 0.5d0 * A
         It = (1.d0/3.d0) * (b*(t_f**3) + h*(t_w**3))
         Iw = 0.d0
         iclass = 2
      endif

      iy = dsqrt(max(1.0d-12, Iyy / max(A, 1.0d-12)))
      iz = dsqrt(max(1.0d-12, Izz / max(A, 1.0d-12)))

      end subroutine calc_ec3_properties


!     ==================================================================
!     calc_ec3_member_capacities: Precomputes EC3 member capacities
!     ==================================================================
      subroutine calc_ec3_member_capacities(L_elem, Lcr_y_in, Lcr_z_in,
     &  L_lt_in, c1_in, fy_in, E_in, nu_in, gm0_in, gm1_in, sect_type,
     &  dims, Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z, Nb_Rd_y, Nb_Rd_z,
     &  Nb_Rd, Mb_Rd, lbar_y, lbar_z, Cmy, Cmz, CmLT, iclass)
      implicit none

      real*8, intent(in) :: L_elem, Lcr_y_in, Lcr_z_in, L_lt_in, c1_in
      real*8, intent(in) :: fy_in, E_in, nu_in, gm0_in, gm1_in
      real*8, intent(in) :: sect_type, dims(6)
      real*8, intent(out) :: Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z
      real*8, intent(out) :: Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd
      real*8, intent(out) :: lbar_y, lbar_z, Cmy, Cmz, CmLT
      integer, intent(out) :: iclass

      real*8 :: A, Iyy, Izz, Wel_y, Wel_z, Wpl_y, Wpl_z, Av_y, Av_z
      real*8 :: It, Iw, iy, iz, Wy, Wz
      real*8 :: Lcr_y, Lcr_z, L_lt, C1, fy, E, nu, gm0, gm1
      real*8 :: pi, G, Mcr, lam_1, lbar_lt
      real*8 :: alpha_y, alpha_z, alpha_lt, phi_y, phi_z, phi_lt
      real*8 :: chi_y, chi_z, chi_lt

      pi = 4.d0 * datan(1.d0)

      Lcr_y = max(0.01d0, Lcr_y_in)
      Lcr_z = max(0.01d0, Lcr_z_in)
      L_lt  = max(0.01d0, L_lt_in)
      C1    = max(0.1d0, c1_in)
      fy    = max(1.0d6, fy_in)
      E     = max(1.0d9, E_in)
      nu    = max(0.01d0, nu_in)
      gm0   = max(0.1d0, gm0_in)
      gm1   = max(0.1d0, gm1_in)
      G     = E / (2.d0 * (1.d0 + nu))

      call calc_ec3_properties(sect_type, dims, E, nu, fy,
     &  A, Iyy, Izz, Wel_y, Wel_z, Wpl_y, Wpl_z, Av_y, Av_z,
     &  It, Iw, iy, iz, iclass)

      if (iclass .le. 2) then
         Wy = Wpl_y
         Wz = Wpl_z
      else
         Wy = Wel_y
         Wz = Wel_z
      endif

      Nt_Rd = A * fy / gm0
      Nc_Rd = A * fy / gm0
      Vpl_y = Av_y * (fy / dsqrt(3.d0)) / gm0
      Vpl_z = Av_z * (fy / dsqrt(3.d0)) / gm0
      Mc_y = Wy * fy / gm0
      Mc_z = Wz * fy / gm0

!     Column Buckling (EC3 6.3.1)
      lam_1 = pi * dsqrt(E / fy)
      lbar_y = (Lcr_y / max(iy, 1.0d-6)) / lam_1
      lbar_z = (Lcr_z / max(iz, 1.0d-6)) / lam_1

      alpha_y = 0.21d0
      alpha_z = 0.34d0

      phi_y = 0.5d0 * (1.d0 + alpha_y*(lbar_y - 0.2d0) + (lbar_y**2))
      chi_y = 1.d0 / (phi_y + 
     &        dsqrt(max(0.0d0, (phi_y**2) - (lbar_y**2))))
      chi_y = min(1.0d0, max(0.01d0, chi_y))

      phi_z = 0.5d0 * (1.d0 + alpha_z*(lbar_z - 0.2d0) + (lbar_z**2))
      chi_z = 1.d0 / (phi_z + 
     &        dsqrt(max(0.0d0, (phi_z**2) - (lbar_z**2))))
      chi_z = min(1.0d0, max(0.01d0, chi_z))

      Nb_Rd_y = chi_y * A * fy / gm1
      Nb_Rd_z = chi_z * A * fy / gm1
      Nb_Rd   = min(Nb_Rd_y, Nb_Rd_z)

!     Lateral-Torsional Buckling (EC3 6.3.2)
      Mcr = C1 * (pi**2 * E * Izz / (L_lt**2)) * 
     &      dsqrt(Iw / max(Izz, 1.0d-12) + 
     &            (L_lt**2 * G * It) / 
     &            max(pi**2 * E * Izz, 1.0d-12))
      Mcr = max(1.0d-3, Mcr)

      lbar_lt = dsqrt(Wy * fy / Mcr)
      alpha_lt = 0.34d0

      phi_lt = 0.5d0 * (1.d0 + alpha_lt*(lbar_lt - 0.2d0) + 
     &                  (lbar_lt**2))
      chi_lt = 1.d0 / (phi_lt + 
     &         dsqrt(max(0.0d0, (phi_lt**2) - (lbar_lt**2))))
      chi_lt = min(1.0d0, max(0.01d0, chi_lt))

      Mb_Rd = chi_lt * Wy * fy / gm1

      Cmy = 0.95d0
      Cmz = 0.95d0
      CmLT = 0.95d0

      end subroutine calc_ec3_member_capacities


!     ==================================================================
!     calc_ec3_station_check: Fast station check against precomputed caps
!     ==================================================================
      subroutine calc_ec3_station_check(fx, vy, vz, tx, my, mz,
     &  Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z, Nb_Rd_y, Nb_Rd_z,
     &  Nb_Rd, Mb_Rd, lbar_y, lbar_z, Cmy, Cmz, CmLT, iclass,
     &  uc_tot, uc_ax, uc_sh, uc_bnd, uc_buck, uc_ltb, uc_stab,
     &  gov_mode)
      implicit none

      real*8, intent(in) :: fx, vy, vz, tx, my, mz
      real*8, intent(in) :: Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z
      real*8, intent(in) :: Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd
      real*8, intent(in) :: lbar_y, lbar_z, Cmy, Cmz, CmLT
      integer, intent(in) :: iclass
      real*8, intent(out) :: uc_tot, uc_ax, uc_sh, uc_bnd, uc_buck
      real*8, intent(out) :: uc_ltb, uc_stab
      character*20, intent(out) :: gov_mode

      real*8 :: uc_ten, uc_comp, uc_sh_y, uc_sh_z, uc_bnd_y, uc_bnd_z
      real*8 :: rho_y, rho_z, My_V_Rd, Mz_V_Rd
      real*8 :: kyy, kyz, kzy, kzz, ny, nz, uc_661, uc_662

!     1. Tension & Compression
      uc_ten = 0.d0
      uc_comp = 0.d0
      if (fx .gt. 0.d0) then
         uc_ten = fx / max(Nt_Rd, 1.0d-6)
         uc_ax  = uc_ten
      else
         uc_comp = dabs(fx) / max(Nc_Rd, 1.0d-6)
         uc_ax   = uc_comp
      endif

!     2. Shear
      uc_sh_y = dabs(vy) / max(Vpl_y, 1.0d-6)
      uc_sh_z = dabs(vz) / max(Vpl_z, 1.0d-6)
      uc_sh   = max(uc_sh_y, uc_sh_z)

!     3. Bending & Shear Reduction
      if (dabs(vz) .gt. 0.5d0 * Vpl_z) then
         rho_z = ((2.d0 * dabs(vz) / Vpl_z) - 1.d0)**2
         My_V_Rd = max(0.1d0 * Mc_y, (1.d0 - rho_z) * Mc_y)
      else
         My_V_Rd = Mc_y
      endif

      if (dabs(vy) .gt. 0.5d0 * Vpl_y) then
         rho_y = ((2.d0 * dabs(vy) / Vpl_y) - 1.d0)**2
         Mz_V_Rd = max(0.1d0 * Mc_z, (1.d0 - rho_y) * Mc_z)
      else
         Mz_V_Rd = Mc_z
      endif

      uc_bnd_y = dabs(my) / max(My_V_Rd, 1.0d-6)
      uc_bnd_z = dabs(mz) / max(Mz_V_Rd, 1.0d-6)
      uc_bnd   = uc_bnd_y + uc_bnd_z

!     4. Column Buckling
      if (fx .lt. 0.d0) then
         uc_buck = dabs(fx) / max(Nb_Rd, 1.0d-6)
      else
         uc_buck = 0.d0
      endif

!     5. Lateral-Torsional Buckling
      uc_ltb = dabs(my) / max(Mb_Rd, 1.0d-6)

!     6. Stability Interaction (Annex B Eq. 6.61 & 6.62)
      if (fx .lt. 0.d0) then
         ny = dabs(fx) / max(Nb_Rd_y, 1.0d-6)
         nz = dabs(fx) / max(Nb_Rd_z, 1.0d-6)

         if (iclass .le. 2) then
            kyy = Cmy * (1.d0 + (lbar_y - 0.2d0) * ny)
            kyy = min(kyy, Cmy * (1.d0 + 0.8d0 * ny))

            kzz = Cmz * (1.d0 + (2.d0 * lbar_z - 0.6d0) * nz)
            kzz = min(kzz, Cmz * (1.d0 + 1.4d0 * nz))

            kyz = 0.6d0 * kzz
            kzy = max(1.d0 - (0.1d0 * lbar_z / (CmLT - 0.25d0)) * nz,
     &                1.d0 - (0.1d0 / (CmLT - 0.25d0)) * nz)
         else
            kyy = Cmy * (1.d0 + 0.6d0 * lbar_y * ny)
            kyy = min(kyy, Cmy * (1.d0 + 0.6d0 * ny))

            kzz = Cmz * (1.d0 + 0.6d0 * lbar_z * nz)
            kzz = min(kzz, Cmz * (1.d0 + 0.6d0 * nz))

            kyz = kzz
            kzy = max(1.d0 - (0.05d0*lbar_z / (CmLT - 0.25d0)) * nz,
     &                1.d0 - (0.05d0 / (CmLT - 0.25d0)) * nz)
         endif

         uc_661 = (dabs(fx) / Nb_Rd_y) + 
     &            kyy * (dabs(my) / Mb_Rd) + 
     &            kyz * (dabs(mz) / Mc_z)

         uc_662 = (dabs(fx) / Nb_Rd_z) + 
     &            kzy * (dabs(my) / Mb_Rd) + 
     &            kzz * (dabs(mz) / Mc_z)

         uc_stab = max(uc_661, uc_662)
      else
         uc_stab = uc_ten + uc_bnd
      endif

!     7. Governing Check Mode
      uc_tot = max(uc_ax, uc_sh, uc_bnd, uc_buck, uc_ltb, uc_stab)

      if (uc_tot .eq. uc_stab) then
         if (fx .lt. 0.d0) then
            if (uc_661 .ge. uc_662) then
               gov_mode = 'EC3-Eq6.61'
            else
               gov_mode = 'EC3-Eq6.62'
            endif
         else
            gov_mode = 'EC3-Tens+Bend'
         endif
      elseif (uc_tot .eq. uc_ltb) then
         gov_mode = 'EC3-LTB'
      elseif (uc_tot .eq. uc_buck) then
         gov_mode = 'EC3-Buckling'
      elseif (uc_tot .eq. uc_bnd) then
         gov_mode = 'EC3-Bending'
      elseif (uc_tot .eq. uc_sh) then
         gov_mode = 'EC3-Shear'
      elseif (uc_tot .eq. uc_ax) then
         if (fx .gt. 0.d0) then
            gov_mode = 'EC3-Tension'
         else
            gov_mode = 'EC3-Compression'
         endif
      else
         gov_mode = 'EC3-General'
      endif

      end subroutine calc_ec3_station_check


!     ==================================================================
!     calc_ec3_member_check: Wrapper for full evaluation
!     ==================================================================
      subroutine calc_ec3_member_check(fx, vy, vz, tx, my, mz,
     &  L_elem, Lcr_y_in, Lcr_z_in, L_lt_in, c1_in, fy_in, E_in, nu_in,
     &  gm0_in, gm1_in, sect_type, dims,
     &  uc_tot, uc_ax, uc_sh, uc_bnd, uc_buck, uc_ltb, uc_stab,
     &  gov_mode)
      implicit none

      real*8, intent(in) :: fx, vy, vz, tx, my, mz
      real*8, intent(in) :: L_elem, Lcr_y_in, Lcr_z_in, L_lt_in, c1_in
      real*8, intent(in) :: fy_in, E_in, nu_in, gm0_in, gm1_in
      real*8, intent(in) :: sect_type, dims(6)
      real*8, intent(out) :: uc_tot, uc_ax, uc_sh, uc_bnd, uc_buck
      real*8, intent(out) :: uc_ltb, uc_stab
      character*20, intent(out) :: gov_mode

      real*8 :: Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z
      real*8 :: Nb_Rd_y, Nb_Rd_z, Nb_Rd, Mb_Rd
      real*8 :: lbar_y, lbar_z, Cmy, Cmz, CmLT
      integer :: iclass

      call calc_ec3_member_capacities(L_elem, Lcr_y_in, Lcr_z_in,
     &  L_lt_in, c1_in, fy_in, E_in, nu_in, gm0_in, gm1_in, sect_type,
     &  dims, Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z, Nb_Rd_y, Nb_Rd_z,
     &  Nb_Rd, Mb_Rd, lbar_y, lbar_z, Cmy, Cmz, CmLT, iclass)

      call calc_ec3_station_check(fx, vy, vz, tx, my, mz,
     &  Nt_Rd, Nc_Rd, Vpl_y, Vpl_z, Mc_y, Mc_z, Nb_Rd_y, Nb_Rd_z,
     &  Nb_Rd, Mb_Rd, lbar_y, lbar_z, Cmy, Cmz, CmLT, iclass,
     &  uc_tot, uc_ax, uc_sh, uc_bnd, uc_buck, uc_ltb, uc_stab,
     &  gov_mode)

      end subroutine calc_ec3_member_check
