module mdraw_xy
   use, intrinsic :: iso_fortran_env, only: wp => real64
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use M_draw
   implicit none
   private

   public :: axis_t, series_t, plot_config_t
   public :: compute_nice_axis, render_multi_xy

   ! Marker style constants
   integer, parameter, public :: MARKER_NONE     = 0
   integer, parameter, public :: MARKER_CIRCLE   = 1
   integer, parameter, public :: MARKER_SQUARE   = 2
   integer, parameter, public :: MARKER_TRIANGLE = 3
   integer, parameter, public :: MARKER_CROSS    = 4
   integer, parameter, public :: MARKER_PLUS     = 5

   ! Line style constants
   integer, parameter, public :: LINE_SOLID      = 0
   integer, parameter, public :: LINE_DASHED     = 1
   integer, parameter, public :: LINE_DOTTED     = 2
   integer, parameter, public :: LINE_DASHDOT    = 3

   type :: axis_t
      real(wp) :: vmin = 0.0_wp
      real(wp) :: vmax = 1.0_wp
      real(wp) :: step = 0.2_wp
      integer  :: nticks = 6
      logical  :: is_log = .false.
      logical  :: is_valid = .false.
      character(len=:), allocatable :: label
   end type axis_t

   type :: series_t
      real(wp), allocatable :: x(:)
      real(wp), allocatable :: y(:)

      ! Optional error bars:
      ! Provide xerr / yerr for symmetric (+/-) bars, OR
      ! xerr_low & xerr_high / yerr_low & yerr_high for asymmetric bars.
      real(wp), allocatable :: xerr(:)
      real(wp), allocatable :: xerr_low(:), xerr_high(:)
      real(wp), allocatable :: yerr(:)
      real(wp), allocatable :: yerr_low(:), yerr_high(:)

      character(len=:), allocatable :: name
      integer :: color         = 2            ! M_draw index (2=green, 3=yellow, 1=red, 5=magenta, 6=cyan)
      integer :: line_style    = LINE_SOLID
      integer :: line_width    = 25           ! 0 = no line (scatter/errorbar only)
      integer :: marker        = MARKER_NONE
      real    :: marker_size   = 0.015        ! Normalized viewport radius
      integer :: marker_step   = 1            ! Draw marker/errorbar every N points
      real    :: err_cap_size  = 0.010        ! Normalized viewport half-width of error bar caps
   end type series_t

   type :: plot_config_t
      character(len=:), allocatable :: title
      character(len=:), allocatable :: xlabel
      character(len=:), allocatable :: ylabel
      logical  :: x_log        = .false.
      logical  :: y_log        = .false.
      integer  :: target_ticks = 6
      real(wp) :: pad_ratio    = 0.05_wp
      logical  :: show_grid    = .true.
      logical  :: show_legend  = .true.
      logical  :: interactive  = .true.       ! Set .false. for svg/pdf/ps file drivers
      integer  :: bg_color     = 0            ! Black
      integer  :: axis_color   = 7            ! White
      integer  :: grid_color   = 4            ! Blue/dim accent
   end type plot_config_t

contains

   !> Computes clean axis bounds in linear space or log10 exponent space
   function compute_nice_axis(data, is_log, target_ticks, pad_ratio, label) result(ax)
      real(wp), intent(in) :: data(:)
      logical, intent(in)  :: is_log
      integer, intent(in), optional :: target_ticks
      real(wp), intent(in), optional :: pad_ratio
      character(len=*), intent(in), optional :: label
      type(axis_t) :: ax

      integer :: n_target, n_valid, i, k
      real(wp) :: dmin, dmax, span, nice_step, pad
      real(wp), allocatable :: work(:)
      logical, allocatable :: valid_mask(:)

      ax%is_log = is_log
      n_target = 6
      if (present(target_ticks)) n_target = max(2, target_ticks)

      pad = 0.0_wp
      if (present(pad_ratio) .and. .not. is_log) pad = max(0.0_wp, pad_ratio)

      if (present(label)) then
         ax%label = trim(label)
      else
         ax%label = ""
      end if

      if (size(data) == 0) return
      allocate(valid_mask(size(data)))

      if (is_log) then
         valid_mask = ieee_is_finite(data) .and. (data > 0.0_wp)
      else
         valid_mask = ieee_is_finite(data)
      end if

      n_valid = count(valid_mask)
      if (n_valid == 0) return

      allocate(work(n_valid))
      k = 0
      do i = 1, size(data)
         if (valid_mask(i)) then
            k = k + 1
            if (is_log) then
               work(k) = log10(data(i))
            else
               work(k) = data(i)
            end if
         end if
      end do

      dmin = minval(work)
      dmax = maxval(work)
      span = dmax - dmin

      if (is_log) then
         ax%vmin = floor(dmin)
         ax%vmax = ceiling(dmax)
         if (ax%vmax <= ax%vmin) ax%vmax = ax%vmin + 1.0_wp
         ax%step = max(1.0_wp, real(ceiling((ax%vmax - ax%vmin) / real(n_target, wp)),kind=wp))
         ax%nticks = nint((ax%vmax - ax%vmin) / ax%step) + 1
         ax%is_valid = .true.
         return
      end if

      if (span <= epsilon(1.0_wp) * max(1.0_wp, abs(dmax))) then
         if (abs(dmax) <= epsilon(1.0_wp)) then
            dmin = -1.0_wp
            dmax =  1.0_wp
         else
            dmin = dmin - 0.1_wp * abs(dmin)
            dmax = dmax + 0.1_wp * abs(dmax)
         end if
         span = dmax - dmin
      else if (pad > 0.0_wp) then
         dmin = dmin - pad * span
         dmax = dmax + pad * span
         span = dmax - dmin
      end if

      nice_step = nice_num(span / real(n_target - 1, wp), round=.true.)
      ax%vmin = floor(dmin / nice_step) * nice_step
      ax%vmax = ceiling(dmax / nice_step) * nice_step
      ax%step = nice_step
      ax%nticks = nint((ax%vmax - ax%vmin) / nice_step) + 1
      ax%is_valid = .true.
   end function compute_nice_axis

   pure real(wp) function nice_num(x, round) result(res)
      real(wp), intent(in) :: x
      logical, intent(in)  :: round
      real(wp) :: exponent, fraction, nice_fraction

      exponent = floor(log10(x))
      fraction = x / (10.0_wp ** exponent)

      if (round) then
         if (fraction < 1.5_wp) then
            nice_fraction = 1.0_wp
         else if (fraction < 3.0_wp) then
            nice_fraction = 2.0_wp
         else if (fraction < 7.0_wp) then
            nice_fraction = 5.0_wp
         else
            nice_fraction = 10.0_wp
         end if
      else
         if (fraction <= 1.0_wp) then
            nice_fraction = 1.0_wp
         else if (fraction <= 2.0_wp) then
            nice_fraction = 2.0_wp
         else if (fraction <= 5.0_wp) then
            nice_fraction = 5.0_wp
         else
            nice_fraction = 10.0_wp
         end if
      end if

      res = nice_fraction * (10.0_wp ** exponent)
   end function nice_num

   function format_tick(val, step, is_log) result(str)
      real(wp), intent(in) :: val, step
      logical, intent(in)  :: is_log
      character(len=32) :: str
      real(wp) :: clean_val
      integer :: decimals, exp_int
      character(len=16) :: fmt

      if (is_log) then
         exp_int = nint(val)
         if (abs(val - real(exp_int, wp)) < 1.0e-6_wp) then
            if (exp_int >= -2 .and. exp_int <= 3) then
               write(str, '(G0.4)') 10.0_wp ** exp_int
            else
               write(str, '("10^", I0)') exp_int
            end if
         else
            write(str, '(ES9.1)') 10.0_wp ** val
         end if
         str = adjustl(str)
         return
      end if

      clean_val = val
      if (abs(clean_val) < 1.0e-12_wp * max(1.0_wp, step)) clean_val = 0.0_wp

      if (abs(clean_val) >= 1.0e5_wp .or. (abs(clean_val) > 0.0_wp .and. abs(clean_val) < 1.0e-3_wp)) then
         write(str, '(ES10.2)') clean_val
      else
         decimals = max(0, -floor(log10(step)) + 1)
         if (decimals > 6) decimals = 6
         write(fmt, '("(F0.", I0, ")")') decimals
         write(str, fmt) clean_val
      end if
      str = adjustl(str)
   end function format_tick

   !> Resolves lower and upper error bounds for index i of a series
   subroutine get_err_bounds(val, i, sym, low, high, v_lo, v_hi, has_err)
      real(wp), intent(in) :: val
      integer, intent(in)  :: i
      real(wp), allocatable, intent(in) :: sym(:), low(:), high(:)
      real(wp), intent(out) :: v_lo, v_hi
      logical, intent(out)  :: has_err

      has_err = .false.
      v_lo = val
      v_hi = val

      if (allocated(low) .and. allocated(high)) then
         if (i <= size(low) .and. i <= size(high)) then
            if (ieee_is_finite(low(i)) .and. ieee_is_finite(high(i))) then
               v_lo = val - abs(low(i))
               v_hi = val + abs(high(i))
               has_err = .true.
            end if
         end if
      else if (allocated(sym)) then
         if (i <= size(sym)) then
            if (ieee_is_finite(sym(i))) then
               v_lo = val - abs(sym(i))
               v_hi = val + abs(sym(i))
               has_err = .true.
            end if
         end if
      end if
   end subroutine get_err_bounds

   !> Draws solid, dashed, dotted, or dash-dot segments with uniform screen-space dash length
   subroutine draw_styled_segment(x1, y1, x2, y2, style, xspan, yspan, vp_w, vp_h, phase)
      real, intent(in)    :: x1, y1, x2, y2, xspan, yspan, vp_w, vp_h
      integer, intent(in) :: style
      real, intent(inout) :: phase

      real :: dx_n, dy_n, seg_len, pos, step_len, t0, t1
      real :: pat(4)
      integer :: n_pat, idx

      if (style == LINE_SOLID) then
         call move2(x1, y1)
         call draw2(x2, y2)
         return
      end if

      ! Convert segment delta into normalized screen units so dash lengths stay uniform
      dx_n = ((x2 - x1) / xspan) * vp_w
      dy_n = ((y2 - y1) / yspan) * vp_h
      seg_len = hypot(dx_n, dy_n)
      if (seg_len <= 1.0e-7) return

      ! Pattern lengths in normalized screen units: [on, off, on, off]
      select case (style)
       case (LINE_DASHED)
         n_pat = 2
         pat(1:2) = [0.024, 0.014]
       case (LINE_DOTTED)
         n_pat = 2
         pat(1:2) = [0.004, 0.011]
       case (LINE_DASHDOT)
         n_pat = 4
         pat(1:4) = [0.024, 0.010, 0.004, 0.010]
       case default
         call move2(x1, y1)
         call draw2(x2, y2)
         return
      end select
      pos = 0.0
      do while (pos < seg_len)
! Determine which pattern element we are currently inside
         block
            real :: cycle_len, rem, acc
            cycle_len = sum(pat(1:n_pat))
            rem = modulo(phase, cycle_len)
            acc = 0.0
            idx = 1
            do while (idx < n_pat .and. rem >= acc + pat(idx))
               acc = acc + pat(idx)
               idx = idx + 1
            end do
            step_len = min(seg_len - pos, (acc + pat(idx)) - rem)
         end block
         if (mod(idx, 2) == 1) then
            t0 = pos / seg_len
            t1 = (pos + step_len) / seg_len
            call move2(x1 + t0 * (x2 - x1), y1 + t0 * (y2 - y1))
            call draw2(x1 + t1 * (x2 - x1), y1 + t1 * (y2 - y1))
         end if
         pos = pos + step_len
         phase = phase + step_len
      end do
   end subroutine draw_styled_segment
   subroutine draw_marker(wx, wy, mtype, rx, ry)
      real, intent(in) :: wx, wy, rx, ry
      integer, intent(in) :: mtype
      integer :: j
      real :: ang, pi2
      select case (mtype)
       case (MARKER_CIRCLE)
         pi2 = 6.2831853
         call move2(wx + rx, wy)
         do j = 1, 16
            ang = pi2 * real(j) / 16.0
            call draw2(wx + rx * cos(ang), wy + ry * sin(ang))
         end do
       case (MARKER_SQUARE)
         call move2(wx - rx, wy - ry)
         call draw2(wx + rx, wy - ry)
         call draw2(wx + rx, wy + ry)
         call draw2(wx - rx, wy + ry)
         call draw2(wx - rx, wy - ry)
       case (MARKER_TRIANGLE)
         call move2(wx, wy + 1.15 * ry)
         call draw2(wx + rx, wy - 0.85 * ry)
         call draw2(wx - rx, wy - 0.85 * ry)
         call draw2(wx, wy + 1.15 * ry)
       case (MARKER_CROSS)
         call move2(wx - rx, wy - ry)
         call draw2(wx + rx, wy + ry)
         call move2(wx - rx, wy + ry)
         call draw2(wx + rx, wy - ry)
       case (MARKER_PLUS)
         call move2(wx - rx, wy)
         call draw2(wx + rx, wy)
         call move2(wx, wy - ry)
         call draw2(wx, wy + ry)
      end select
   end subroutine draw_marker
!> Renders multiple XY series with linear/log axes, error bars, styled lines, markers, and legend
   subroutine render_multi_xy(series, cfg, device)
      type(series_t), intent(in) :: series(:)
      type(plot_config_t), intent(in), optional :: cfg
      character(len=*), intent(in), optional :: device
      type(plot_config_t) :: opt
      type(axis_t) :: x_ax, y_ax
      real(wp), allocatable :: all_x(:), all_y(:)
      real(wp) :: v_lo, v_hi
      integer :: s, i, k, cap_est, idum, mstep
      real :: xmin, xmax, ymin, ymax, xspan, yspan
      real :: tick_val, sub_val, sx, sy, prev_x, prev_y, phase
      real :: rx, ry, cap_x, cap_y, lo_w, hi_w, leg_y, leg_x
      real :: vp_l, vp_r, vp_b, vp_t, vp_w, vp_h
      logical :: pen_down, pt_valid, has_err
      if (size(series) == 0) error stop "render_multi_xy: At least one series is required."
      if (present(cfg)) opt = cfg
      if (.not. allocated(opt%title)) opt%title = ""
      if (.not. allocated(opt%xlabel)) opt%xlabel = ""
      if (.not. allocated(opt%ylabel)) opt%ylabel = ""
! Pool all points + error bar extrema so axis limits never clip error caps
      cap_est = 0
      do s = 1, size(series)
         if (size(series(s)%x) /= size(series(s)%y)) then
            error stop "render_multi_xy: Series X and Y sizes must match."
         end if
         cap_est = cap_est + 3 * size(series(s)%x)
      end do
      if (cap_est == 0) error stop "render_multi_xy: All series are empty."
      allocate(all_x(cap_est), all_y(cap_est))
      k = 0
      do s = 1, size(series)
         do i = 1, size(series(s)%x)
            k = k + 1
            all_x(k) = series(s)%x(i)
            all_y(k) = series(s)%y(i)
            call get_err_bounds(series(s)%x(i), i, series(s)%xerr, &
               series(s)%xerr_low, series(s)%xerr_high, v_lo, v_hi, has_err)
            if (has_err) then
               if (.not. opt%x_log .or. v_lo > 0.0_wp) then
                  k = k + 1; all_x(k) = v_lo
                  k = k + 1; all_y(k) = series(s)%y(i)
               end if
               if (.not. opt%x_log .or. v_hi > 0.0_wp) then
                  k = k + 1; all_x(k) = v_hi
                  k = k + 1; all_y(k) = series(s)%y(i)
               end if
            end if
            call get_err_bounds(series(s)%y(i), i, series(s)%yerr, &
               series(s)%yerr_low, series(s)%yerr_high, v_lo, v_hi, has_err)
            if (has_err) then
               if (.not. opt%y_log .or. v_lo > 0.0_wp) then
                  k = k + 1; all_x(k) = series(s)%x(i)
                  k = k + 1; all_y(k) = v_lo
               end if
               if (.not. opt%y_log .or. v_hi > 0.0_wp) then
                  k = k + 1; all_x(k) = series(s)%x(i)
                  k = k + 1; all_y(k) = v_hi
               end if
            end if
         end do
      end do
      x_ax = compute_nice_axis(all_x(1:k), opt%x_log, opt%target_ticks, opt%pad_ratio, opt%xlabel)
      y_ax = compute_nice_axis(all_y(1:k), opt%y_log, opt%target_ticks, opt%pad_ratio, opt%ylabel)
      if (.not. x_ax%is_valid .or. .not. y_ax%is_valid) then
         error stop "render_multi_xy: No valid positive/finite points for selected axis scale."
      end if
      if (present(device)) then
         call vinit(trim(device))
      else
         call vinit(' ')
      end if
      call color(opt%bg_color)
      call clear()
      call font('futura.l')
      vp_l = -0.72
      vp_b = -0.72
      vp_t = 0.80
      if (opt%show_legend) then
         vp_r = 0.56
      else
         vp_r = 0.88
      end if
      vp_w = vp_r - vp_l
      vp_h = vp_t - vp_b
      xmin = real(x_ax%vmin)
      xmax = real(x_ax%vmax)
      ymin = real(y_ax%vmin)
      ymax = real(y_ax%vmax)
      xspan = xmax - xmin
      yspan = ymax - ymin
! -------------------------------------------------------------------------
! 1. Outer Normalized Viewport [-1, 1]: Titles, Tick Labels, Legend
! -------------------------------------------------------------------------
      call viewport(-1.0, 1.0, -1.0, 1.0)
      call ortho2(-1.0, 1.0, -1.0, 1.0)
      call color(opt%axis_color)
      if (len_trim(opt%title) > 0) then
         call textsize(0.042, 0.062)
         call centertext(.true.)
         call move2(0.0, 0.90)
         call drawstr(trim(opt%title))
      end if
      if (len_trim(opt%xlabel) > 0) then
         call textsize(0.030, 0.045)
         call centertext(.true.)
         call move2(0.5 * (vp_l + vp_r), -0.92)
         call drawstr(trim(opt%xlabel))
      end if
      if (len_trim(opt%ylabel) > 0) then
         call textsize(0.030, 0.045)
         call centertext(.true.)
         call textang(90.0)
         call move2(-0.93, 0.5 * (vp_b + vp_t))
         call drawstr(trim(opt%ylabel))
         call textang(0.0)
      end if
! X tick labels
      call textsize(0.024, 0.034)
      call centertext(.true.)
      do i = 0, x_ax%nticks - 1
         tick_val = real(x_ax%vmin + real(i, wp) * x_ax%step)
         if (tick_val > xmax + 1.0e-5 * xspan) exit
         sx = vp_l + ((tick_val - xmin) / xspan) * vp_w
         call move2(sx, vp_b - 0.055)
         call drawstr(trim(format_tick(real(tick_val, wp), x_ax%step, opt%x_log)))
      end do
! Y tick labels
      call centertext(.false.)
      do i = 0, y_ax%nticks - 1
         tick_val = real(y_ax%vmin + real(i, wp) * y_ax%step)
         if (tick_val > ymax + 1.0e-5 * yspan) exit
         sy = vp_b + ((tick_val - ymin) / yspan) * vp_h
         block
            character(len=32) :: ylbl
            ylbl = format_tick(real(tick_val, wp), y_ax%step, opt%y_log)
            call move2(vp_l - 0.025 - len_trim(ylbl) * 0.022, sy - 0.012)
            call drawstr(trim(ylbl))
         end block
      end do
! Legend with styled line sample + marker sample
      if (opt%show_legend) then
         leg_x = vp_r + 0.04
         leg_y = vp_t - 0.05
         call textsize(0.022, 0.032)
         do s = 1, size(series)
            call color(series(s)%color)
            if (series(s)%line_width > 0) then
               call linewidth(series(s)%line_width)
               phase = 0.0
               call draw_styled_segment(leg_x, leg_y, leg_x + 0.09, leg_y, &
                  series(s)%line_style, 2.0, 2.0, 2.0, 2.0, phase)
               call linewidth(0)
            end if
            if (series(s)%marker /= MARKER_NONE) then
               call draw_marker(leg_x + 0.045, leg_y, series(s)%marker, 0.012, 0.012)
            end if
            call color(opt%axis_color)
            call move2(leg_x + 0.11, leg_y - 0.010)
            if (allocated(series(s)%name)) then
               call drawstr(trim(series(s)%name))
            else
               call drawstr("Series")
            end if
            leg_y = leg_y - 0.075
         end do
      end if
! -------------------------------------------------------------------------
! 2. Inner Data Viewport: Gridlines, Ticks, Error Bars, Curves & Markers
! -------------------------------------------------------------------------
      call viewport(vp_l, vp_r, vp_b, vp_t)
      call ortho2(xmin, xmax, ymin, ymax)
! X gridlines & ticks
      do i = 0, x_ax%nticks - 1
         tick_val = real(x_ax%vmin + real(i, wp) * x_ax%step)
         if (tick_val > xmax + 1.0e-5 * xspan) exit
         if (opt%show_grid .and. i > 0 .and. i < x_ax%nticks - 1) then
            call color(opt%grid_color)
            call move2(tick_val, ymin)
            call draw2(tick_val, ymax)
         end if
         call color(opt%axis_color)
         call move2(tick_val, ymin)
         call draw2(tick_val, ymin + 0.022 * yspan)
         if (opt%x_log .and. i < x_ax%nticks - 1) then
            do k = 2, 9
               sub_val = tick_val + log10(real(k))
               if (sub_val < xmax) then
                  call move2(sub_val, ymin)
                  call draw2(sub_val, ymin + 0.011 * yspan)
               end if
            end do
         end if
      end do
! Y gridlines & ticks
      do i = 0, y_ax%nticks - 1
         tick_val = real(y_ax%vmin + real(i, wp) * y_ax%step)
         if (tick_val > ymax + 1.0e-5 * yspan) exit
         if (opt%show_grid .and. i > 0 .and. i < y_ax%nticks - 1) then
            call color(opt%grid_color)
            call move2(xmin, tick_val)
            call draw2(xmax, tick_val)
         end if
         call color(opt%axis_color)
         call move2(xmin, tick_val)
         call draw2(xmin + 0.018 * xspan, tick_val)
         if (opt%y_log .and. i < y_ax%nticks - 1) then
            do k = 2, 9
               sub_val = tick_val + log10(real(k))
               if (sub_val < ymax) then
                  call move2(xmin, sub_val)
                  call draw2(xmin + 0.009 * xspan, sub_val)
               end if
            end do
         end if
      end do
! Bounding plot frame
      call color(opt%axis_color)
      call move2(xmin, ymin)
      call draw2(xmax, ymin)
      call draw2(xmax, ymax)
      call draw2(xmin, ymax)
      call draw2(xmin, ymin)
! Render each series: 1) Error bars, 2) Styled curve, 3) Scatter markers
      do s = 1, size(series)
         call color(series(s)%color)
         mstep = max(1, series(s)%marker_step)
         rx = series(s)%marker_size * xspan / vp_w
         ry = series(s)%marker_size * yspan / vp_h
         cap_x = series(s)%err_cap_size * xspan / vp_w
         cap_y = series(s)%err_cap_size * yspan / vp_h
! 1) Draw Error Bars
         call linewidth(12)
         do i = 1, size(series(s)%x), mstep
            if (.not. is_point_plottable(series(s)%x(i), series(s)%y(i), opt%x_log, opt%y_log)) cycle
            sx = map_coord(series(s)%x(i), opt%x_log)
            sy = map_coord(series(s)%y(i), opt%y_log)
! Horizontal X error bar
            call get_err_bounds(series(s)%x(i), i, series(s)%xerr, &
               series(s)%xerr_low, series(s)%xerr_high, v_lo, v_hi, has_err)
            if (has_err) then
               if (opt%x_log .and. v_lo <= 0.0_wp) then
                  lo_w = xmin ! Clamp lower cap to axis minimum if error crosses <= 0 on log scale
               else
                  lo_w = map_coord(v_lo, opt%x_log)
               end if
               hi_w = map_coord(v_hi, opt%x_log)
               call move2(lo_w, sy)
               call draw2(hi_w, sy)
               if (.not. opt%x_log .or. v_lo > 0.0_wp) then
                  call move2(lo_w, sy - cap_y)
                  call draw2(lo_w, sy + cap_y)
               end if
               call move2(hi_w, sy - cap_y)
               call draw2(hi_w, sy + cap_y)
            end if
! Vertical Y error bar
            call get_err_bounds(series(s)%y(i), i, series(s)%yerr, &
               series(s)%yerr_low, series(s)%yerr_high, v_lo, v_hi, has_err)
            if (has_err) then
               if (opt%y_log .and. v_lo <= 0.0_wp) then
                  lo_w = ymin ! Clamp lower cap to axis minimum if error crosses <= 0 on log scale
               else
                  lo_w = map_coord(v_lo, opt%y_log)
               end if
               hi_w = map_coord(v_hi, opt%y_log)
               call move2(sx, lo_w)
               call draw2(sx, hi_w)
               if (.not. opt%y_log .or. v_lo > 0.0_wp) then
                  call move2(sx - cap_x, lo_w)
                  call draw2(sx + cap_x, lo_w)
               end if
               call move2(sx - cap_x, hi_w)
               call draw2(sx + cap_x, hi_w)
            end if
         end do
         call linewidth(0)
! 2) Draw Styled Connecting Line
         if (series(s)%line_width > 0) then
            call linewidth(series(s)%line_width)
            pen_down = .false.
            phase = 0.0
            do i = 1, size(series(s)%x)
               pt_valid = is_point_plottable(series(s)%x(i), series(s)%y(i), opt%x_log, opt%y_log)
               if (pt_valid) then
                  sx = map_coord(series(s)%x(i), opt%x_log)
                  sy = map_coord(series(s)%y(i), opt%y_log)
                  if (.not. pen_down) then
                     prev_x = sx
                     prev_y = sy
                     pen_down = .true.
                  else
                     call draw_styled_segment(prev_x, prev_y, sx, sy, series(s)%line_style, &
                        xspan, yspan, vp_w, vp_h, phase)
                     prev_x = sx
                     prev_y = sy
                  end if
               else
                  pen_down = .false.
                  phase = 0.0
               end if
            end do
            call linewidth(0)
         end if
! 3) Draw Scatter Markers
         if (series(s)%marker /= MARKER_NONE) then
            do i = 1, size(series(s)%x), mstep
               if (is_point_plottable(series(s)%x(i), series(s)%y(i), opt%x_log, opt%y_log)) then
                  sx = map_coord(series(s)%x(i), opt%x_log)
                  sy = map_coord(series(s)%y(i), opt%y_log)
                  call draw_marker(sx, sy, series(s)%marker, rx, ry)
               end if
            end do
         end if
      end do
      call vflush()
      if (opt%interactive) idum = getkey()
      call vexit()
   end subroutine render_multi_xy

   pure logical function is_point_plottable(x, y, x_log, y_log)
      real(wp), intent(in) :: x, y
      logical, intent(in) :: x_log, y_log
      is_point_plottable = ieee_is_finite(x) .and. ieee_is_finite(y)
      if (x_log) is_point_plottable = is_point_plottable .and. (x > 0.0_wp)
      if (y_log) is_point_plottable = is_point_plottable .and. (y > 0.0_wp)
   end function is_point_plottable

   pure real function map_coord(val, is_log)
      real(wp), intent(in) :: val
      logical, intent(in) :: is_log
      if (is_log) then
         map_coord = real(log10(val))
      else
         map_coord = real(val)
      end if
   end function map_coord

end module mdraw_xy
