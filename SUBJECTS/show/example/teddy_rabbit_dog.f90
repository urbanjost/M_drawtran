program frenchie_fawn
   use M_draw
   implicit none

   integer :: key

   ! Custom realistic palette (indices >= 10 for VOGLE compatibility)
   integer, parameter :: C_BG         = 10
   integer, parameter :: C_BG_HALO    = 11
   integer, parameter :: C_FAWN_DARK  = 12
   integer, parameter :: C_FAWN_BASE  = 13
   integer, parameter :: C_FAWN_LIGHT = 14
   integer, parameter :: C_FAWN_HIGH  = 15
   integer, parameter :: C_EAR_PINK   = 16
   integer, parameter :: C_EAR_SHADOW = 17
   integer, parameter :: C_MASK_SOFT  = 18
   integer, parameter :: C_MASK_DARK  = 19
   integer, parameter :: C_NOSE_BLACK = 20
   integer, parameter :: C_NOSE_SHINE = 21
   integer, parameter :: C_EYE_IRIS   = 22
   integer, parameter :: C_WHITE      = 23
   integer, parameter :: C_COLLAR     = 24
   integer, parameter :: C_GOLD       = 25

   call prefsize(720, 760)
   call vinit(' ')

   ! Define rich, warm fawn & charcoal-mask RGB palette (0 to 255)
   call mapcolor(C_BG,         34,  42,  52)   ! Deep slate studio background
   call mapcolor(C_BG_HALO,    48,  58,  70)   ! Soft studio backdrop glow
   call mapcolor(C_FAWN_DARK, 152, 108,  66)   ! Deep fawn shading & wrinkles
   call mapcolor(C_FAWN_BASE, 198, 152, 102)   ! Rich warm fawn coat
   call mapcolor(C_FAWN_LIGHT,222, 180, 130)   ! Lit fawn cheeks & brow
   call mapcolor(C_FAWN_HIGH, 238, 204, 158)   ! Soft highlight fur
   call mapcolor(C_EAR_PINK,  214, 156, 148)   ! Warm inner ear skin
   call mapcolor(C_EAR_SHADOW,168, 114, 108)   ! Inner ear fold shadow
   call mapcolor(C_MASK_SOFT,  58,  52,  50)   ! Soft charcoal mask blend
   call mapcolor(C_MASK_DARK,  30,  28,  28)   ! Velvety black muzzle mask
   call mapcolor(C_NOSE_BLACK, 14,  14,  15)   ! Glossy black nose leather
   call mapcolor(C_NOSE_SHINE,115, 118, 125)   ! Moist nose specular highlight
   call mapcolor(C_EYE_IRIS,   46,  30,  20)   ! Deep soulful brown iris
   call mapcolor(C_WHITE,     246, 244, 240)   ! Chest blaze & eye catchlights
   call mapcolor(C_COLLAR,    142,  36,  42)   ! Classic crimson leather collar
   call mapcolor(C_GOLD,      218, 178,  78)   ! Brass ID tag

   call ortho2(-10.0, 10.0, -10.5, 10.5)
   call polyfill(.true.)

   ! --- 1. STUDIO BACKDROP ---
   call color(C_BG)
   call clear()
   call color(C_BG_HALO)
   call circle(0.0, 0.2, 8.8)

   ! --- 2. MUSCULAR NECK, SHOULDERS & WHITE CHEST BLAZE ---
   call draw_body_and_collar()

   ! --- 3. SIGNATURE ROUNDED "BAT EARS" ---
   call draw_bat_ear(-1.0)  ! Left ear
   call draw_bat_ear( 1.0)  ! Right ear

   ! --- 4. BROAD SQUARE SKULL, CHEEKS & FOREHEAD WRINKLES ---
   call draw_head_and_wrinkles()

   ! --- 5. BLACK MUZZLE MASK, FLEWS (JOWLS) & NOSE ---
   call draw_black_mask_and_nose()

   ! --- 6. EXPRESSIVE DARK EYES WITH CATCHLIGHTS ---
   call draw_frenchie_eyes()

   ! --- 7. CAPTION ---
   call polyfill(.false.)
   call color(C_FAWN_HIGH)
   call font('times.rb')
   call linewidth(14)
   call centertext(.true.)
   call textsize(0.58, 0.78)
   call move2(0.0, -9.4)
   call drawstr('FAWN FRENCH BULLDOG')
   call linewidth(1)

   call vflush()
   key = getkey()
   call vexit()

contains

   ! =========================================================================
   ! BROAD CHEST, SHOULDERS, WHITE BLAZE & COLLAR
   ! =========================================================================
   subroutine draw_body_and_collar()
      real :: bx(10), by(10), wx(8), wy(8)

      ! Broad muscular shoulders & chest
      bx = [-6.2, -5.6, -3.8, -1.8,  0.0,  1.8,  3.8,  5.6,  6.2,  0.0]
      by = [-8.5, -5.4, -3.6, -3.0, -2.9, -3.0, -3.6, -5.4, -8.5, -8.5]
      call smooth_poly(C_FAWN_DARK, bx, by, 10, .true.)

      ! Lit upper chest area
      bx = [-4.6, -4.0, -2.2,  0.0,  2.2,  4.0,  4.6,  0.0, -2.2, -4.0]
      by = [-8.5, -5.6, -4.0, -3.8, -4.0, -5.6, -8.5, -8.5, -8.5, -8.5]
      call smooth_poly(C_FAWN_BASE, bx, by, 10, .true.)

      ! White chest tuxedo blaze
      wx = [-1.7, -1.9, -1.1,  0.0,  1.1,  1.9,  1.7,  0.0]
      wy = [-8.5, -6.2, -4.5, -4.3, -4.5, -6.2, -8.5, -8.5]
      call smooth_poly(C_WHITE, wx, wy, 8, .true.)

      ! Crimson collar band & brass tag
      call polyfill(.false.)
      call color(C_COLLAR)
      call linewidth(42)
      call arc(0.0, 1.5, 6.2, 242.0, 298.0)
      call linewidth(1)
      call polyfill(.true.)

      call color(C_GOLD)
      call circle(0.0, -5.15, 0.42)
      call color(C_FAWN_DARK)
      call circle(0.0, -5.15, 0.14)
   end subroutine draw_body_and_collar

   ! =========================================================================
   ! ROUNDED, BROAD-BASED "BAT EAR" (side = -1.0 for left, +1.0 for right)
   ! =========================================================================
   subroutine draw_bat_ear(side)
      real, intent(in) :: side
      real :: ox(9), oy(9), ix(8), iy(8), rx(6), ry(6)

      ! Outer fawn ear contour (wide base, tapering smoothly to rounded tip)
      ox = side * [1.5, 2.2, 3.6, 5.2, 5.6, 5.1, 4.3, 3.2, 2.2]
      oy =        [3.4, 5.8, 8.2, 8.7, 7.6, 5.5, 3.6, 2.5, 2.6]
      call smooth_poly(C_FAWN_DARK, ox, oy, 9, .true.)

      ! Slightly inset warm fawn ear rim
      ox = side * [1.7, 2.4, 3.6, 5.0, 5.3, 4.9, 4.1, 3.1, 2.3]
      oy =        [3.5, 5.7, 7.9, 8.4, 7.4, 5.5, 3.7, 2.7, 2.8]
      call smooth_poly(C_FAWN_BASE, ox, oy, 9, .true.)

      ! Soft pink inner ear pinna
      ix = side * [2.1, 2.7, 3.7, 4.7, 4.8, 4.3, 3.5, 2.6]
      iy =        [3.5, 5.4, 7.3, 7.6, 6.6, 4.8, 3.4, 3.1]
      call smooth_poly(C_EAR_PINK, ix, iy, 8, .true.)

      ! Inner ear vertical cartilage ridges for realistic depth
      rx = side * [2.7, 3.1, 3.6, 3.9, 3.5, 2.9]
      ry =        [3.6, 5.0, 6.3, 5.8, 4.5, 3.5]
      call smooth_poly(C_EAR_SHADOW, rx, ry, 6, .true.)
   end subroutine draw_bat_ear

   ! =========================================================================
   ! BROAD DOME SKULL, FULL CHEEKS & FOREHEAD WRINKLE FOLDS
   ! =========================================================================
   subroutine draw_head_and_wrinkles()
      real :: hx(14), hy(14), cx(10), cy(10)

      ! Outer head contour (flat-topped broad dome + wide rounded cheeks)
      hx = [ 0.0, -1.8, -3.3, -4.1, -4.5, -4.1, -2.6, &
         0.0,  2.6,  4.1,  4.5,  4.1,  3.3,  1.8]
      hy = [ 4.5,  4.4,  3.6,  2.1,  0.3, -1.6, -2.8, &
         -3.2, -2.8, -1.6,  0.3,  2.1,  3.6,  4.4]
      call smooth_poly(C_FAWN_DARK, hx, hy, 14, .true.)

      ! Main warm fawn head coat
      hx = [ 0.0, -1.7, -3.1, -3.9, -4.25,-3.8, -2.4, &
         0.0,  2.4,  3.8,  4.25, 3.9,  3.1,  1.7]
      hy = [ 4.3,  4.2,  3.4,  2.0,  0.3, -1.5, -2.6, &
         -3.0, -2.6, -1.5,  0.3,  2.0,  3.4,  4.2]
      call smooth_poly(C_FAWN_BASE, hx, hy, 14, .true.)

      ! Lit fawn cheek pads (left & right)
      cx = [-1.6, -2.9, -3.8, -3.9, -3.2, -2.0, -1.3, -1.2, -1.3, -1.4]
      cy = [ 1.2,  1.1,  0.2, -1.0, -1.9, -2.1, -1.4, -0.4,  0.4,  0.9]
      call smooth_poly(C_FAWN_LIGHT, cx, cy, 10, .true.)

      cx = -cx
      call smooth_poly(C_FAWN_LIGHT, cx, cy, 10, .true.)

      ! Highlighted brow arches above eyes
      call color(C_FAWN_HIGH)
      call ellipse_fill(-1.85, 2.25, 0.95, 0.45)
      call ellipse_fill( 1.85, 2.25, 0.95, 0.45)

      ! Subtle forehead wrinkle furrows (characteristic Frenchie frown lines)
      call polyfill(.false.)
      call color(C_FAWN_DARK)
      call linewidth(14)
      call arc(0.0, 1.4, 2.2, 62.0, 118.0)
      call linewidth(10)
      call arc(0.0, 1.9, 2.3, 66.0, 114.0)
      ! Central vertical furrow between brows
      call move2(0.0, 1.6); call draw2(0.0, 2.9)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_head_and_wrinkles

   ! =========================================================================
   ! VELVETY BLACK MASK, OVER-NOSE WRINKLE ROPE, FLEWS & GLOSSY NOSE
   ! =========================================================================
   subroutine draw_black_mask_and_nose()
      real :: mx(14), my(14), nx(10), ny(10)

      ! 1. Soft charcoal outer halo of the black mask (blends mask into fawn fur)
      mx = [ 0.0, -1.1, -2.4, -2.9, -2.7, -2.3, -1.2, &
         0.0,  1.2,  2.3,  2.7,  2.9,  2.4,  1.1]
      my = [ 1.6,  1.9,  1.6,  0.7, -0.5, -1.9, -2.7, &
         -2.9, -2.7, -1.9, -0.5,  0.7,  1.6,  1.9]
      call smooth_poly(C_MASK_SOFT, mx, my, 14, .true.)

      ! 2. Deep velvety black inner muzzle & eye-mask
      mx = [ 0.0, -0.9, -2.1, -2.5, -2.4, -2.0, -1.0, &
         0.0,  1.0,  2.0,  2.4,  2.5,  2.1,  0.9]
      my = [ 1.3,  1.6,  1.3,  0.5, -0.6, -1.8, -2.5, &
         -2.7, -2.5, -1.8, -0.6,  0.5,  1.3,  1.6]
      call smooth_poly(C_MASK_DARK, mx, my, 14, .true.)

      ! 3. Heavy padded flews (pendulous upper lip jowls)
      call color(C_MASK_SOFT)
      call ellipse_fill(-1.15, -1.45, 1.15, 0.85)
      call ellipse_fill( 1.15, -1.45, 1.15, 0.85)

      call color(C_MASK_DARK)
      call ellipse_fill(-1.10, -1.40, 1.02, 0.74)
      call ellipse_fill( 1.10, -1.40, 1.02, 0.74)

      ! Subtle chin pad below mouth
      call color(C_MASK_SOFT)
      call ellipse_fill(0.0, -2.35, 0.85, 0.38)

      ! Lip crease line (inverted Y mouth line)
      call polyfill(.false.)
      call color(C_NOSE_BLACK)
      call linewidth(16)
      call move2(0.0, -0.95); call draw2(0.0, -1.55)
      call arc(-1.05, -1.25, 1.08, 255.0, 345.0)
      call arc( 1.05, -1.25, 1.08, 195.0, 285.0)
      call linewidth(1)
      call polyfill(.true.)

      ! 4. Characteristic "nose rope" (fawn-to-charcoal skin fold above the nose)
      call color(C_MASK_SOFT)
      call ellipse_fill(0.0, 0.42, 1.35, 0.42)
      call color(C_MASK_DARK)
      call ellipse_fill(0.0, 0.34, 1.25, 0.32)

      ! 5. Broad, flat, upturned black nose
      nx = [ 0.0, -0.65, -1.05, -0.95, -0.45, 0.0, 0.45, 0.95, 1.05, 0.65]
      ny = [ 0.25, 0.28,  0.02, -0.45, -0.78,-0.85,-0.78,-0.45, 0.02, 0.28]
      call smooth_poly(C_NOSE_BLACK, nx, ny, 10, .true.)

! Moist specular highlight along top ridge of nose
      call color(C_NOSE_SHINE)
      call ellipse_fill(-0.35, 0.10, 0.32, 0.08)
      call ellipse_fill( 0.35, 0.10, 0.32, 0.08)
! Open comma-shaped nostrils
      call color(C_BG)
      call ellipse_fill(-0.48, -0.25, 0.20, 0.13)
      call ellipse_fill( 0.48, -0.25, 0.20, 0.13)
! Philtrum vertical groove
      call polyfill(.false.)
      call color(C_MASK_SOFT)
      call linewidth(10)
      call move2(0.0, -0.35); call draw2(0.0, -0.85)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_black_mask_and_nose
! =========================================================================
! LARGE, ROUND, EXPRESSIVE DARK FRENCHIE EYES WITH DOUBLE CATCHLIGHTS
! =========================================================================
   subroutine draw_frenchie_eyes()
      real :: ex(2)
      integer :: i
      ex = [-1.75, 1.75]
      do i = 1, 2
! Dark eye-rim eyeliner
         call color(C_NOSE_BLACK)
         call circle(ex(i), 1.05, 0.54)
! Deep soulful brown iris
         call color(C_EYE_IRIS)
         call circle(ex(i), 1.05, 0.46)
! Large dark pupil
         call color(C_NOSE_BLACK)
         call circle(ex(i), 1.05, 0.30)
! Primary studio window catchlight (upper-left)
         call color(C_WHITE)
         call circle(ex(i) - 0.15, 1.22, 0.13)
! Secondary lower-right corneal glint
         call circle(ex(i) + 0.16, 0.90, 0.06)
      end do
   end subroutine draw_frenchie_eyes
! =========================================================================
! SMOOTH CLOSED CATMULL-ROM SPLINE POLYGON GENERATOR
! =========================================================================
   subroutine smooth_poly(col, px, py, n, closed)
      integer, intent(in) :: col, n
      real, intent(in) :: px(n), py(n)
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
            t = real(s) / real(STEPS)
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
! Helper for smooth filled ellipses
   subroutine ellipse_fill(cx, cy, rx, ry)
      real, intent(in) :: cx, cy, rx, ry
      real, parameter :: PI = 3.141592653589793
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
end program frenchie_fawn
