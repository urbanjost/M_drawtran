program frenchie_ball_chase
   use, intrinsic :: iso_c_binding, only: c_int, c_int32_t
   use M_draw
   implicit none

   interface
      function c_usleep(usec) bind(c, name='usleep')
         import :: c_int, c_int32_t
         integer(c_int32_t), value :: usec
         integer(c_int) :: c_usleep
      end function c_usleep
   end interface

   real, parameter :: PI = 3.141592653589793
   integer :: frame, key, ierr, tile_i
   integer(c_int) :: rc
   real :: t, stride, dog_x, dog_y, body_pitch
   real :: fl_ang, fr_ang, hl_ang, hr_ang
   real :: ear_flap, tongue_len, tail_wag
   real :: ball_x, ball_y, ball_bounce, ball_spin, ball_shadow_r
   real :: tile_x, scroll_offset
   logical :: is_blinking

   ! Custom realistic fawn, black-mask & toy-ball palette (indices >= 10)
   integer, parameter :: C_BG         = 10
   integer, parameter :: C_WALL_TRIM  = 11
   integer, parameter :: C_FLOOR      = 12
   integer, parameter :: C_FLOOR_LINE = 13
   integer, parameter :: C_SHADOW     = 14
   integer, parameter :: C_FAWN_BACK  = 15
   integer, parameter :: C_FAWN_DARK  = 16
   integer, parameter :: C_FAWN_BASE  = 17
   integer, parameter :: C_FAWN_LIGHT = 18
   integer, parameter :: C_FAWN_HIGH  = 19
   integer, parameter :: C_EAR_PINK   = 20
   integer, parameter :: C_EAR_INNER  = 21
   integer, parameter :: C_MASK_SOFT  = 22
   integer, parameter :: C_MASK_DARK  = 23
   integer, parameter :: C_NOSE_BLACK = 24
   integer, parameter :: C_TONGUE     = 25
   integer, parameter :: C_WHITE      = 26
   integer, parameter :: C_COLLAR     = 27
   integer, parameter :: C_GOLD       = 28
   integer, parameter :: C_BALL_RED   = 29
   integer, parameter :: C_BALL_DARK  = 30

   call prefsize(960, 620)
   call vinit(' ')

   ! Define warm studio, fawn coat & glossy red ball RGB palette (0 to 255)
   call mapcolor(C_BG,         38,  46,  58)   ! Warm slate wall
   call mapcolor(C_WALL_TRIM,  54,  64,  78)   ! Baseboard trim
   call mapcolor(C_FLOOR,      66,  58,  50)   ! Warm hardwood floor
   call mapcolor(C_FLOOR_LINE, 48,  42,  36)   ! Floorboard seams
   call mapcolor(C_SHADOW,     26,  24,  24)   ! Contact shadows
   call mapcolor(C_FAWN_BACK, 128,  88,  52)   ! Far-side legs & back ear
   call mapcolor(C_FAWN_DARK, 158, 112,  68)   ! Shading & neck rolls
   call mapcolor(C_FAWN_BASE, 202, 156, 104)   ! Warm fawn body & head
   call mapcolor(C_FAWN_LIGHT,224, 182, 132)   ! Lit near legs & cheek
   call mapcolor(C_FAWN_HIGH, 238, 204, 158)   ! Back & brow highlight
   call mapcolor(C_EAR_PINK,  216, 162, 154)   ! Inner bat-ear skin
   call mapcolor(C_EAR_INNER, 178, 122, 116)   ! Ear fold shadow
   call mapcolor(C_MASK_SOFT,  56,  50,  48)   ! Soft charcoal mask edge
   call mapcolor(C_MASK_DARK,  28,  26,  26)   ! Velvety black muzzle mask
   call mapcolor(C_NOSE_BLACK, 12,  12,  14)   ! Glossy black nose
   call mapcolor(C_TONGUE,    228, 106, 122)   ! Flopping pink tongue
   call mapcolor(C_WHITE,     246, 244, 238)   ! Chest blaze & highlights
   call mapcolor(C_COLLAR,    156,  34,  42)   ! Crimson leather collar
   call mapcolor(C_GOLD,      224, 182,  76)   ! Brass ID tag
   call mapcolor(C_BALL_RED,  218,  44,  48)   ! Bouncy red rubber ball
   call mapcolor(C_BALL_DARK, 148,  24,  28)   ! Ball lower shading

   call ortho2(-15.0, 15.0, -9.5, 10.5)
   ierr = backbuffer()

   frame = 0
   do
      t = real(frame) * 0.09
      stride = t * 2.4

      ! --- DOG BOUNDING / GALLOP KINEMATICS ---
      dog_x = -3.8 + 1.1 * sin(t * 0.55)
      dog_y = abs(sin(stride)) * 0.65
      body_pitch = 4.5 * cos(stride)

      ! Articulated leg swing angles (in degrees)
      fr_ang =  38.0 * sin(stride)
      fl_ang =  38.0 * sin(stride + 0.85)
      hr_ang = -42.0 * sin(stride + 0.35)
      hl_ang = -42.0 * sin(stride + 1.20)

      ! Secondary motion: flapping bat ears, flopping tongue, wagging tail
      ear_flap   = 10.0 * sin(stride - 0.5)
      tongue_len = 0.65 + 0.35 * sin(stride + 0.4)
      tail_wag   = 26.0 * sin(stride * 1.5)
      is_blinking = (mod(frame, 95) >= 90)

      ! --- BOUNCING BALL PHYSICS (AHEAD OF THE FRENCHIE) ---
      ball_x = 7.8 + 1.6 * cos(t * 0.55 + 0.4)
      ball_bounce = abs(sin(t * 1.45))
      ball_y = -5.7 + 4.8 * (ball_bounce ** 0.85)
      ball_spin = -t * 140.0
      ball_shadow_r = 1.15 - 0.55 * ball_bounce

      call color(C_BG)
      call clear()
      call polyfill(.true.)

      ! --- 1. SCROLLING FLOOR & BASEBOARD ---
      call color(C_WALL_TRIM)
      call rect(-15.0, -4.6, 15.0, -3.9)
      call color(C_FLOOR)
      call rect(-15.0, -9.5, 15.0, -4.6)

      scroll_offset = mod(real(frame) * 0.32, 4.0)
      call polyfill(.false.)
      call color(C_FLOOR_LINE)
      call linewidth(8)
      do tile_i = -5, 6
         tile_x = real(tile_i) * 4.0 - scroll_offset
         call move2(tile_x, -4.6)
         call draw2(tile_x - 2.2, -9.5)
      end do
      call linewidth(1)
      call polyfill(.true.)

      ! --- 2. DYNAMIC GROUND CONTACT SHADOWS ---
      call color(C_SHADOW)
      call ellipse_fill(dog_x - 0.5, -6.8, 5.6 - 0.7 * dog_y, 0.50)
      call ellipse_fill(ball_x, -6.8, ball_shadow_r, 0.32)

      ! --- 3. DRAW THE BOUNDING FRENCH BULLDOG ---
      call pushmatrix()
      call translate(dog_x, dog_y, 0.0)
      call rotate(body_pitch, 'z')

      call draw_far_legs(fl_ang, hl_ang)
      call draw_tail(tail_wag)
      call draw_torso()
      call draw_near_legs(fr_ang, hr_ang)
      call draw_far_ear(ear_flap * 0.6)
      call draw_profile_head(tongue_len, is_blinking)
      call draw_near_ear(ear_flap)

      call popmatrix()

      ! --- 4. DRAW THE BOUNCING RED RUBBER BALL ---
      call draw_bouncing_ball(ball_x, ball_y, ball_spin, ball_bounce)

      ! --- 5. CAPTION ---
      call polyfill(.false.)
      call color(C_FAWN_HIGH)
      call font('times.rb')
      call linewidth(14)
      call centertext(.true.)
      call textsize(0.55, 0.72)
      call move2(0.0, -8.5)
      call drawstr('FRENCHIE BALL CHASE  (PRESS ANY KEY)')
      call linewidth(1)

      call swapbuffers()
      rc = c_usleep(28000_c_int32_t)

      key = checkkey()
      if (key /= 0) exit
      frame = frame + 1
   end do

   call vexit()

contains

   ! =========================================================================
   ! GLOSSY RED RUBBER BALL WITH ROTATING STRIPE & SPECULAR HIGHLIGHT
   ! =========================================================================
   subroutine draw_bouncing_ball(bx, by, spin_deg, bounce_h)
      real, intent(in) :: bx, by, spin_deg, bounce_h
      real :: squash_x, squash_y

      if (bounce_h < 0.08) then
         squash_x = 1.12
         squash_y = 0.88
      else
         squash_x = 1.0
         squash_y = 1.0
      end if

      call pushmatrix()
      call translate(bx, by, 0.0)
      call scale(squash_x, squash_y, 1.0)

      call polyfill(.true.)
      call color(C_BALL_DARK)
      call circle(0.0, 0.0, 1.05)

      call color(C_BALL_RED)
      call circle(-0.10, 0.12, 0.94)

      call pushmatrix()
      call rotate(spin_deg, 'z')
      call polyfill(.false.)
      call color(C_WHITE)
      call linewidth(16)
      call arc(0.0, 0.0, 0.65, 25.0, 155.0)
      call linewidth(1)
      call popmatrix()

      call polyfill(.true.)
      call color(C_WHITE)
      call ellipse_fill(-0.36, 0.42, 0.24, 0.14)

      call popmatrix()
   end subroutine draw_bouncing_ball

   ! =========================================================================
   ! ARTICULATED FAR-SIDE LEGS (PIVOTING AT SHOULDER & HIP)
   ! =========================================================================
   subroutine draw_far_legs(front_deg, hind_deg)
      real, intent(in) :: front_deg, hind_deg
      real :: lx(8), ly(8)

      call pushmatrix()
      call translate(2.4, -1.8, 0.0)
      call rotate(front_deg, 'z')
      lx = [-0.6,  0.6,  0.7,  1.2,  1.1,  0.0, -0.3, -0.6]
      ly = [ 0.2,  0.2, -3.5, -4.0, -4.4, -4.4, -3.4, -1.4]
      call smooth_poly(C_FAWN_BACK, lx, ly, 8, .true.)
      call popmatrix()

      call pushmatrix()
      call translate(-4.1, -1.4, 0.0)
      call rotate(hind_deg, 'z')
      lx = [-0.7,  0.6,  0.1, -0.6, -0.1,  0.8,  0.9,  0.4]
      ly = [ 0.2,  0.2, -2.6, -4.2, -4.6, -4.6, -3.8, -2.0]
      call smooth_poly(C_FAWN_BACK, lx, ly, 8, .true.)
      call popmatrix()
   end subroutine draw_far_legs

   ! =========================================================================
   ! WAGGING SHORT LOW-SET FRENCHIE TAIL
   ! =========================================================================
   subroutine draw_tail(wag_deg)
      real, intent(in) :: wag_deg
      real :: tx(6), ty(6)

      call pushmatrix()
      call translate(-6.2, -0.8, 0.0)
      call rotate(wag_deg, 'z')

      tx = [0.0, -0.9, -1.3, -1.1, -0.4,  0.1]
      ty = [0.3,  0.1, -0.4, -0.8, -0.6, -0.2]
      call smooth_poly(C_FAWN_DARK, tx, ty, 6, .true.)

      call popmatrix()
   end subroutine draw_tail

   ! =========================================================================
   ! COMPACT, MUSCULAR BODY WITH ROACHED BACK, DEEP CHEST & WHITE BLAZE
   ! =========================================================================
   subroutine draw_torso()
      real :: bx(14), by(14), hlx(8), hly(8), wx(7), wy(7)

      bx = [ 2.4,  0.6, -1.8, -4.2, -5.8, -6.3, -5.8, &
         -4.2, -2.2, -0.4,  1.6,  3.5,  4.4,  3.8]
      by = [ 2.4,  1.6,  1.8,  1.5,  0.4, -1.0, -2.4, &
         -3.0, -2.6, -2.8, -3.6, -3.5, -1.8,  0.6]
      call smooth_poly(C_FAWN_BASE, bx, by, 14, .true.)

      hlx = [ 1.6, -0.2, -2.2, -4.2, -5.2, -4.2, -2.0,  0.2]
      hly = [ 1.8,  1.4,  1.6,  1.3,  0.4,  0.6,  0.9,  1.1]
      call smooth_poly(C_FAWN_LIGHT, hlx, hly, 8, .true.)

      wx = [ 3.7,  4.35, 4.3,  3.5,  2.7,  2.8,  3.2]
      wy = [ 0.2, -1.3, -2.7, -3.4, -3.1, -1.8, -0.5]
      call smooth_poly(C_WHITE, wx, wy, 7, .true.)

      call polyfill(.false.)
      call color(C_COLLAR)
      call linewidth(34)
      call move2(1.4, 2.35)
      call draw2(3.85, 0.25)
      call linewidth(1)
      call polyfill(.true.)
      call color(C_GOLD)
      call circle(4.05, -0.35, 0.34)
   end subroutine draw_torso
! =========================================================================
! ARTICULATED NEAR-SIDE LEGS (PIVOTING AT SHOULDER & HIP)
! =========================================================================
   subroutine draw_near_legs(front_deg, hind_deg)
      real, intent(in) :: front_deg, hind_deg
      real :: fx(10), fy(10), hx(11), hy(11)
      call pushmatrix()
      call translate(2.1, -1.5, 0.0)
      call rotate(front_deg, 'z')
      fx = [-0.7,  0.8,  0.7,  0.6,  1.2,  1.1, -0.1, -0.4, -0.6, -0.7]
      fy = [ 0.2,  0.2, -2.2, -4.1, -4.6, -5.0, -5.0, -4.5, -2.5, -1.0]
      call smooth_poly(C_FAWN_LIGHT, fx, fy, 10, .true.)
      call polyfill(.false.)
      call color(C_FAWN_DARK)
      call linewidth(10)
      call arc(0.0, 0.1, 1.0, 195.0, 330.0)
      call linewidth(1)
      call polyfill(.true.)
      call popmatrix()
      call pushmatrix()
      call translate(-4.3, -1.1, 0.0)
      call rotate(hind_deg, 'z')
      hx = [-1.1,  0.8,  1.1,  0.5, -0.1, -0.5,  0.1,  0.0, -1.0, -1.3, -1.5]
      hy = [ 0.4,  0.2, -1.5, -3.0, -4.5, -5.1, -5.4, -5.4, -5.3, -4.2, -2.0]
      call smooth_poly(C_FAWN_LIGHT, hx, hy, 11, .true.)
      call polyfill(.false.)
      call color(C_FAWN_DARK)
      call linewidth(10)
      call arc(0.0, -0.6, 1.2, 280.0, 355.0)
      call linewidth(1)
      call polyfill(.true.)
      call popmatrix()
   end subroutine draw_near_legs
! =========================================================================
! FAR BAT EAR
! =========================================================================
   subroutine draw_far_ear(tilt_deg)
      real, intent(in) :: tilt_deg
      real :: ex(8), ey(8)
      call pushmatrix()
      call translate(3.6, 4.5, 0.0)
      call rotate(tilt_deg, 'z')
      ex = [-0.4,  0.1,  1.1,  1.8,  1.7,  1.1,  0.4, -0.2]
      ey = [ 0.0,  2.2,  3.5,  3.3,  2.1,  0.8,  0.0, -0.2]
      call smooth_poly(C_FAWN_BACK, ex, ey, 8, .true.)
      call popmatrix()
   end subroutine draw_far_ear
! =========================================================================
! BRACHYCEPHALIC PROFILE HEAD, BLACK MASK, PANTING TONGUE & EYE
! =========================================================================
   subroutine draw_profile_head(t_len, blink)
      real, intent(in)    :: t_len
      logical, intent(in) :: blink
      real :: hx(14), hy(14), hlx(8), hly(8), mx(12), my(12), jx(8), jy(8), tx(7), ty(7)
      hx = [ 1.3,  1.8,  3.2,  4.6,  5.2,  5.3,  5.8, &
         6.0,  5.6,  4.8,  3.8,  2.8,  1.8,  1.2]
      hy = [ 2.2,  4.4,  5.1,  4.8,  3.8,  2.8,  2.4, &
         1.4,  0.5,  0.2,  0.4,  0.6,  1.0,  1.6]
      call smooth_poly(C_FAWN_BASE, hx, hy, 14, .true.)
      hlx = [ 2.2,  3.2,  4.4,  4.7,  4.2,  3.1,  2.3,  2.0]
      hly = [ 4.5,  4.8,  4.4,  3.3,  2.0,  1.5,  2.2,  3.5]
      call smooth_poly(C_FAWN_LIGHT, hlx, hly, 8, .true.)
      call polyfill(.false.)
      call color(C_FAWN_DARK)
      call linewidth(14)
      call arc(1.4, 2.2, 1.6, 285.0, 355.0)
      call linewidth(10)
      call arc(0.8, 2.0, 1.8, 285.0, 350.0)
      call linewidth(1)
      call polyfill(.true.)
! Happy panting pink tongue bouncing as he runs
      tx = [ 4.6,  5.4,  6.0,  5.8,  5.1,  4.5,  4.4]
      ty = [ 0.9,  0.8,  0.4 - t_len, 0.1 - t_len, 0.1 - t_len*0.8, 0.5, 0.8]
      call smooth_poly(C_TONGUE, tx, ty, 7, .true.)
      jx = [ 3.8,  4.8,  5.5,  5.3,  4.5,  3.6,  3.4,  3.5]
      jy = [ 0.7,  0.7,  0.4,  0.0, -0.1,  0.1,  0.4,  0.6]
      call smooth_poly(C_MASK_DARK, jx, jy, 8, .true.)
      mx = [ 3.8,  4.6,  5.4,  5.9,  6.1,  5.8, &
         5.1,  4.2,  3.5,  3.4,  3.5,  3.6]
      my = [ 3.4,  3.2,  2.7,  2.3,  1.4,  0.5, &
         0.3,  0.5,  1.1,  1.9,  2.6,  3.1]
      call smooth_poly(C_MASK_SOFT, mx, my, 12, .true.)
      mx = [ 4.2,  4.9,  5.5,  5.9,  6.0,  5.7, &
         5.0,  4.3,  3.8,  3.8,  3.9,  4.0]
      my = [ 3.1,  2.9,  2.5,  2.2,  1.4,  0.6, &
         0.4,  0.6,  1.1,  1.8,  2.4,  2.8]
      call smooth_poly(C_MASK_DARK, mx, my, 12, .true.)
      call color(C_MASK_SOFT)
      call ellipse_fill(5.45, 2.55, 0.45, 0.25)
      call color(C_NOSE_BLACK)
      call ellipse_fill(5.82, 2.22, 0.28, 0.36)
      call color(C_MASK_SOFT)
      call circle(5.90, 2.15, 0.09)
      call color(C_WHITE)
      call circle(5.82, 2.42, 0.05)
      if (.not. blink) then
         call color(C_NOSE_BLACK)
         call circle(4.35, 2.95, 0.38)
         call color(C_WHITE)
         call circle(4.48, 3.05, 0.11)
      else
         call polyfill(.false.)
         call color(C_NOSE_BLACK)
         call linewidth(16)
         call move2(4.00, 2.95); call draw2(4.70, 2.95)
         call linewidth(1)
         call polyfill(.true.)
      end if
      call polyfill(.false.)
      call color(C_FAWN_DARK)
      call linewidth(12)
      call arc(3.8, 3.2, 1.1, 25.0, 95.0)
      call linewidth(9)
      call arc(3.5, 3.5, 1.2, 30.0, 90.0)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_profile_head
! =========================================================================
! FOREGROUND ROUNDED "BAT EAR"
! =========================================================================
   subroutine draw_near_ear(tilt_deg)
      real, intent(in) :: tilt_deg
      real :: ox(9), oy(9), ix(8), iy(8), rx(6), ry(6)
      call pushmatrix()
      call translate(2.3, 4.1, 0.0)
      call rotate(tilt_deg, 'z')
      ox = [-0.9, -1.1, -0.5,  0.4,  1.2,  1.5,  1.3,  0.8, -0.1]
      oy = [-0.2,  1.6,  3.4,  3.8,  3.2,  1.8,  0.4, -0.4, -0.5]
      call smooth_poly(C_FAWN_DARK, ox, oy, 9, .true.)
      ox = [-0.7, -0.9, -0.4,  0.4,  1.1,  1.35, 1.15, 0.7, -0.1]
      oy = [-0.1,  1.5,  3.2,  3.55, 3.0,  1.7,  0.4, -0.3, -0.4]
      call smooth_poly(C_FAWN_BASE, ox, oy, 9, .true.)
      ix = [-0.1, -0.2,  0.1,  0.6,  1.0,  1.1,  0.8,  0.3]
      iy = [ 0.1,  1.4,  2.8,  3.0,  2.4,  1.3,  0.3, -0.1]
      call smooth_poly(C_EAR_PINK, ix, iy, 8, .true.)
      rx = [ 0.1,  0.1,  0.4,  0.6,  0.5,  0.2]
      ry = [ 0.2,  1.2,  2.1,  1.8,  0.9,  0.2]
      call smooth_poly(C_EAR_INNER, rx, ry, 6, .true.)
      call popmatrix()
   end subroutine draw_near_ear
! =========================================================================
! SMOOTH CLOSED CATMULL-ROM SPLINE POLYGON GENERATOR
! =========================================================================
   subroutine smooth_poly(col, px, py, n, closed)
      integer, intent(in) :: col, n
      real, intent(in)    :: px(n), py(n)
      logical, intent(in) :: closed
      integer :: i, s, i0, i1, i2, i3
      integer, parameter :: STEPS = 12
      real :: t, t2, t3, x, y
      call color(col)
      call makepoly()
      do i = 1, n
         i0 = mod(i - 2 + n, n) + 1
         i1 = i
         i2 = mod(i, n) + 1
         i3 = mod(i + 1, n) + 1
         do s = 0, STEPS - 1
            t  = real(s) / real(STEPS)
            t2 = t * t
            t3 = t2 * t
            x = 0.5 * ((2.0 * px(i1)) + &
               (-px(i0) + px(i2)) * t + &
               (2.0 * px(i0) - 5.0 * px(i1) + 4.0 * px(i2) - px(i3)) * t2 + &
               (-px(i0) + 3.0 * px(i1) - 3.0 * px(i2) + px(i3)) * t3)
            y = 0.5 * ((2.0 * py(i1)) + &
               (-py(i0) + py(i2)) * t + &
               (2.0 * py(i0) - 5.0 * py(i1) + 4.0 * py(i2) - py(i3)) * t2 + &
               (-py(i0) + 3.0 * py(i1) - 3.0 * py(i2) + py(i3)) * t3)
            if (i == 1 .and. s == 0) then
               call move2(x, y)
            else
               call draw2(x, y)
            end if
         end do
      end do
      if (closed) call closepoly()
   end subroutine smooth_poly
   subroutine ellipse_fill(cx, cy, rx, ry)
      real, intent(in) :: cx, cy, rx, ry
      integer :: k
      real :: ang
      call makepoly()
      do k = 0, 35
         ang = 2.0 * PI * real(k) / 36.0
         if (k == 0) then
            call move2(cx + rx * cos(ang), cy + ry * sin(ang))
         else
            call draw2(cx + rx * cos(ang), cy + ry * sin(ang))
         end if
      end do
      call closepoly()
   end subroutine ellipse_fill
end program frenchie_ball_chase
