! This example demonstrates:
! * **Series 1:** Solid nominal model curve (`LINE_SOLID`).
! * **Series 2:** Experimental scatter points with **symmetric Y error
!   bars** and **asymmetric X error bars** (`yerr`, `xerr_low`, `xerr_high`).
! * **Series 3 & 4:** Upper/lower confidence bounds using
!   **`LINE_DASHED`** and **`LINE_DASHDOT`**.

program demo_errbars_and_linestyles
   use, intrinsic :: iso_fortran_env, only: wp => real64
   use mdraw_xy
   implicit none

   type(series_t) :: curves(4)
   type(plot_config_t) :: cfg
   integer :: i

   ! 1. Nominal curve (Solid)
   allocate(curves(1)%x(80), curves(1)%y(80))
   curves(1)%name       = "Nominal Fit"
   curves(1)%color      = 2   ! Green
   curves(1)%line_style = LINE_SOLID
   curves(1)%line_width = 25
   do i = 1, 80
      curves(1)%x(i) = 0.5_wp + 9.5_wp * real(i - 1, wp) / 79.0_wp
      curves(1)%y(i) = 18.0_wp * exp(-0.28_wp * curves(1)%x(i)) + 1.2_wp
   end do

   ! 2. Upper bound (Dashed)
   allocate(curves(2)%x(80), curves(2)%y(80))
   curves(2)%name       = "+2 Sigma (Dashed)"
   curves(2)%color      = 6   ! Cyan
   curves(2)%line_style = LINE_DASHED
   curves(2)%line_width = 18
   curves(2)%x = curves(1)%x
   curves(2)%y = curves(1)%y * 1.22_wp

   ! 3. Lower bound (Dash-Dot)
   allocate(curves(3)%x(80), curves(3)%y(80))
   curves(3)%name       = "-2 Sigma (Dash-Dot)"
   curves(3)%color      = 5   ! Magenta
   curves(3)%line_style = LINE_DASHDOT
   curves(3)%line_width = 18
   curves(3)%x = curves(1)%x
   curves(3)%y = curves(1)%y * 0.78_wp

   ! 4. Experimental Measurements (Scatter + Symmetric Y & Asymmetric X Error Bars)
   allocate(curves(4)%x(12), curves(4)%y(12), curves(4)%yerr(12))
   allocate(curves(4)%xerr_low(12), curves(4)%xerr_high(12))
   curves(4)%name        = "Measured Data"
   curves(4)%color       = 3   ! Yellow
   curves(4)%line_width  = 0   ! Scatter + error bars only
   curves(4)%marker      = MARKER_CIRCLE
   curves(4)%marker_size = 0.014
   do i = 1, 12
      curves(4)%x(i)         = 0.8_wp * real(i, wp)
      curves(4)%y(i)         = 18.0_wp * exp(-0.28_wp * curves(4)%x(i)) + 1.2_wp &
         + 0.4_wp * sin(real(i, wp))
      curves(4)%yerr(i)      = 0.15_wp * curves(4)%y(i)
      curves(4)%xerr_low(i)  = 0.15_wp
      curves(4)%xerr_high(i) = 0.30_wp
   end do

   cfg%title       = "Decay Calibration with Error Bars"
   cfg%xlabel      = "Elapsed Time (s)"
   cfg%ylabel      = "Signal Intensity (V)"
   cfg%y_log       = .true.
   cfg%show_grid   = .true.
   cfg%show_legend = .true.
   cfg%interactive = .true.

   call render_multi_xy(curves, cfg)
end program demo_errbars_and_linestyles
