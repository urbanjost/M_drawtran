! multi-scene silent-film featurette in M_draw / VOGLE that alternates
! between the animated Little Tramp and classic 1920s silent-film intertitle
! cards with ornate Art Deco corner filigree, iris-in/iris-out transitions,
! and authentic projector flicker.
!
program chaplin_featurette
   use M_draw
   implicit none

   real, parameter :: PI = 3.141592653589793
   integer :: frame, scene_frame, scene, key, ierr, idum
   real :: t, hat_lift, hat_tilt, cane_angle, iris_r
   logical :: is_blinking

   ! Custom silent-film palette (indices >= 10 for VOGLE compatibility)
   integer, parameter :: C_BG       = 10
   integer, parameter :: C_SPOT_A   = 11
   integer, parameter :: C_SPOT_B   = 12
   integer, parameter :: C_BLACK    = 13
   integer, parameter :: C_CHARCOAL = 14
   integer, parameter :: C_GRAY     = 15
   integer, parameter :: C_SKIN_SHD = 16
   integer, parameter :: C_SKIN     = 17
   integer, parameter :: C_WHITE    = 18
   integer, parameter :: C_CANE     = 19
   integer, parameter :: C_GOLD_DIM = 20

   call prefsize(650, 780)
   call vinit(' ')

   ! Define custom RGB palette (0 to 255)
   call mapcolor(C_BG,        22,  20,  18)   ! Deep theater black-sepia
   call mapcolor(C_SPOT_A,    58,  54,  48)   ! Spotlight frame A
   call mapcolor(C_SPOT_B,    52,  48,  43)   ! Spotlight frame B (flicker)
   call mapcolor(C_BLACK,     14,  14,  15)   ! Suit, hat, hair, mustache
   call mapcolor(C_CHARCOAL,  38,  38,  42)   ! Coat highlights & folds
   call mapcolor(C_GRAY,     125, 122, 116)   ! Hat band & film scratches
   call mapcolor(C_SKIN_SHD, 198, 178, 156)   ! Face shadow facet
   call mapcolor(C_SKIN,     232, 214, 194)   ! Silent-film pale tone
   call mapcolor(C_WHITE,    248, 244, 232)   ! Parchment white text & borders
   call mapcolor(C_CANE,     176, 138,  84)   ! Bamboo cane
   call mapcolor(C_GOLD_DIM, 165, 148, 112)   ! Vintage gilt filigree

   call ortho2(-10.0, 10.0, -12.0, 12.0)
   ierr = backbuffer()

   frame = 0
   do
      ! 4-scene cycle (~420 frames total = ~14 seconds per full loop)
      ! Scene 0 (0..99):    Opening Intertitle Card
      ! Scene 1 (100..239): Chaplin tips hat & twirls cane (with iris-in)
      ! Scene 2 (240..339): Dialogue Intertitle Card
      ! Scene 3 (340..439): Chaplin tips hat & winks (with iris-out)
      scene_frame = mod(frame, 440)
      if (scene_frame < 100) then
         scene = 0
      else if (scene_frame < 240) then
         scene = 1
         scene_frame = scene_frame - 100
      else if (scene_frame < 340) then
         scene = 2
         scene_frame = scene_frame - 240
      else
         scene = 3
         scene_frame = scene_frame - 340
      end if

      call color(C_BG)
      call clear()

      select case (scene)
       case (0)
         ! --- OPENING TITLE CARD ---
         call draw_intertitle_card( &
            'A  FORTRAN  SILENT  PICTURE', &
            '"THE  LITTLE  TRAMP"', &
            'STARRING  CHARLIE  CHAPLIN', &
            'ACT  I', frame)

       case (1)
         ! --- LIVE ACTION SCENE 1: HAT TIP & CANE TWIRL ---
         t = real(scene_frame) * 0.065
         hat_lift   = max(0.0, sin(t)) ** 2 * 1.45
         hat_tilt   = -max(0.0, sin(t)) ** 2 * 16.0
         cane_angle = -t * 180.0 / PI
         is_blinking = (mod(scene_frame, 70) >= 64)

         ! Iris-in during the first 22 frames of the scene
         iris_r = min(8.6, 1.0 + real(scene_frame) * 0.36)
         call draw_chaplin_scene(frame, iris_r, hat_lift, hat_tilt, cane_angle, is_blinking, .false.)

       case (2)
         ! --- DIALOGUE INTERTITLE CARD ---
         call draw_intertitle_card( &
            '"A  day  without  laughter', &
            'is  a  day  wasted."', &
            '---  CHARLIE  CHAPLIN', &
            'SCENE  II', frame)

       case (3)
         ! --- LIVE ACTION SCENE 2: CHEERFUL WINK & IRIS-OUT ---
         t = real(scene_frame) * 0.075
         hat_lift   = max(0.0, sin(t)) ** 2 * 1.35
         hat_tilt   = max(0.0, sin(t)) ** 2 * 15.0
         cane_angle = t * 180.0 / PI
         ! Deliberate cheeky wink mid-scene
         is_blinking = (scene_frame >= 40 .and. scene_frame <= 58)

         ! Iris-out during the final 25 frames
         if (scene_frame > 75) then
            iris_r = max(0.2, 8.6 - real(scene_frame - 75) * 0.35)
         else
            iris_r = 8.6
         end if
         call draw_chaplin_scene(frame, iris_r, hat_lift, hat_tilt, cane_angle, is_blinking, .true.)
      end select

      idum = swapbuffers()
      call call_usleep(32000)

      key = checkkey()
      if (key /= 0) exit
      frame = frame + 1
   end do

   call vexit()

contains

   ! =========================================================================
   ! SILENT-FILM INTERTITLE CARD WITH ART DECO CORNERS & PROJECTOR ARTIFACTS
   ! =========================================================================
   subroutine draw_intertitle_card(line1, line2, line3, badge, gframe)
      character(len=*), intent(in) :: line1, line2, line3, badge
      integer, intent(in) :: gframe
      real :: jitter_x, jitter_y, scratch_x

      ! Subtle film-gate weave (micro-jitter)
      jitter_x = 0.03 * sin(real(gframe) * 1.7)
      jitter_y = 0.03 * cos(real(gframe) * 2.3)

      call pushmatrix()
      call translate(jitter_x, jitter_y, 0.0)

      ! Outer & inner Art Deco borders
      call polyfill(.false.)
      call color(C_WHITE)
      call linewidth(16)
      call rect(-9.0, -11.0, 9.0, 11.0)

      call color(C_GOLD_DIM)
      call linewidth(8)
      call rect(-8.5, -10.5, 8.5, 10.5)

      ! Ornate Art Deco corner brackets
      call draw_corner_ornament(-8.5,  10.5,  1.0, -1.0)
      call draw_corner_ornament( 8.5,  10.5, -1.0, -1.0)
      call draw_corner_ornament(-8.5, -10.5,  1.0,  1.0)
      call draw_corner_ornament( 8.5, -10.5, -1.0,  1.0)

      ! Top & Bottom decorative dividers
      call color(C_GOLD_DIM)
      call linewidth(10)
      call move2(-5.5,  6.2); call draw2(5.5,  6.2)
      call move2(-5.5, -6.2); call draw2(5.5, -6.2)
      call polyfill(.true.)
      call circle(0.0,  6.2, 0.22)
      call circle(0.0, -6.2, 0.22)

      ! Badge header (e.g., ACT I / SCENE II)
      call color(C_GOLD_DIM)
      call font('times.rb')
      call centertext(.true.)
      call textsize(0.36, 0.48)
      call move2(0.0, 7.6)
      call drawstr(trim(badge))

      ! Main silent-film typography
      call color(C_WHITE)
      call textsize(0.46, 0.62)
      call move2(0.0, 2.2)
      call drawstr(trim(line1))

      call textsize(0.52, 0.70)
      call move2(0.0, 0.0)
      call drawstr(trim(line2))

      call color(C_GOLD_DIM)
      call textsize(0.38, 0.50)
      call move2(0.0, -3.2)
      call drawstr(trim(line3))

      ! Footer cue
      call color(C_GRAY)
      call textsize(0.28, 0.38)
      call move2(0.0, -8.8)
      call drawstr('(PRESS ANY KEY TO EXIT)')

      call popmatrix()

      ! Occasional vertical film scratch
      if (mod(gframe, 5) == 0) then
         scratch_x = -8.0 + mod(real(gframe * 41), 16.0)
         call polyfill(.false.)
         call color(C_GRAY)
         call linewidth(4)
         call move2(scratch_x, -10.4)
         call draw2(scratch_x - 0.05, 10.4)
      end if
      call linewidth(1)
   end subroutine draw_intertitle_card

   subroutine draw_corner_ornament(cx, cy, sx, sy)
      real, intent(in) :: cx, cy, sx, sy
      call polyfill(.false.)
      call color(C_WHITE)
      call linewidth(10)
      call move2(cx, cy + sy * 1.6)
      call draw2(cx + sx * 1.6, cy + sy * 1.6)
      call draw2(cx + sx * 1.6, cy)
      call move2(cx + sx * 0.6, cy + sy * 0.6)
      call draw2(cx + sx * 2.2, cy + sy * 2.2)
   end subroutine draw_corner_ornament

   ! =========================================================================
   ! ANIMATED CHAPLIN SCENE WITH IRIS TRANSITION & FILM GRAIN
   ! =========================================================================
   subroutine draw_chaplin_scene(gframe, spot_r, h_lift, h_tilt, c_ang, blink, wink_only)
      integer, intent(in) :: gframe
      real, intent(in)    :: spot_r, h_lift, h_tilt, c_ang
      logical, intent(in) :: blink, wink_only
      real :: scratch_x

      call polyfill(.true.)
      if (mod(gframe, 4) < 2) then
         call color(C_SPOT_A)
      else
         call color(C_SPOT_B)
      end if
      call circle(0.0, 0.5, spot_r)

      call draw_torso()
      call draw_head(blink, wink_only, h_lift > 0.25)

      call pushmatrix()
      call translate(0.25 * h_lift, h_lift, 0.0)
      call rotate(h_tilt, 'z')
      call draw_bowler_hat()
      call popmatrix()

      call draw_cane_and_hand(c_ang)

      ! Film scratch & outer frame
      call polyfill(.false.)
      if (mod(gframe, 3) == 0) then
         scratch_x = -7.5 + mod(real(gframe * 37), 15.0)
         call color(C_GRAY)
         call linewidth(4)
         call move2(scratch_x, -10.8)
         call draw2(scratch_x + 0.08, 10.8)
      end if

      call color(C_WHITE)
      call linewidth(14)
      call rect(-9.2, -11.2, 9.2, 11.2)
      call linewidth(6)
      call rect(-8.9, -10.9, 8.9, 10.9)
      call linewidth(1)
   end subroutine draw_chaplin_scene

   subroutine draw_torso()
      call quad(C_BLACK,   -5.2, -10.0,  -4.4, -3.4,   4.4, -3.4,   5.2, -10.0)
      call tri(C_BLACK,    -4.4,  -3.4,  -2.2, -2.5,  -5.2, -10.0)
      call tri(C_BLACK,     4.4,  -3.4,   2.2, -2.5,   5.2, -10.0)
      call tri(C_CHARCOAL, -2.6,  -2.6,  -1.0, -7.2,  -4.2,  -6.5)
      call tri(C_CHARCOAL,  2.6,  -2.6,   1.0, -7.2,   4.2,  -6.5)
      call tri(C_WHITE,    -1.6,  -2.6,   1.6, -2.6,   0.0,  -6.8)
      call tri(C_WHITE,    -1.8,  -2.4,  -0.4, -3.2,  -1.9,  -3.9)
      call tri(C_WHITE,     1.8,  -2.4,   0.4, -3.2,   1.9,  -3.9)
      call quad(C_BLACK,   -0.35, -3.2,   0.35,-3.2,   0.55, -6.5,  -0.55, -6.5)
      call tri(C_CHARCOAL, -0.4,  -3.0,   0.4, -3.0,   0.0,  -3.6)
   end subroutine draw_torso

   subroutine draw_head(blink, wink_only, raised_brows)
      logical, intent(in) :: blink, wink_only, raised_brows
      real :: brow_dy

      brow_dy = merge(0.22, 0.0, raised_brows)

      call quad(C_SKIN_SHD,-1.1,  -2.7,  -1.1, -1.0,   1.1,  -1.0,   1.1,  -2.7)

      call color(C_SKIN_SHD)
      call circle(-2.7, 1.3, 0.55)
      call circle( 2.7, 1.3, 0.55)
      call tri(C_SKIN, 0.0, 4.2, -2.6, 3.2, -2.7, 0.6)
      call tri(C_SKIN_SHD, 0.0, 4.2, 2.6, 3.2, 2.7, 0.6)
      call tri(C_SKIN, 0.0, 4.2, -2.7, 0.6, 0.0, -0.2)
      call tri(C_SKIN_SHD, 0.0, 4.2, 2.7, 0.6, 0.0, -0.2)
      call tri(C_SKIN, -2.7, 0.6, -1.9, -1.5, 0.0, -0.2)
      call tri(C_SKIN_SHD, 2.7, 0.6, 1.9, -1.5, 0.0, -0.2)
      call tri(C_SKIN, -1.9, -1.5, 0.0, -2.2, 0.0, -0.2)
      call tri(C_SKIN_SHD, 1.9, -1.5, 0.0, -2.2, 0.0, -0.2)
      call color(C_BLACK)
      call circle(-2.6, 2.7, 0.65)
      call circle(-2.8, 2.0, 0.50)
      call circle( 2.6, 2.7, 0.65)
      call circle( 2.8, 2.0, 0.50)
      call tri(C_BLACK, -2.6, 3.2, 0.0, 4.4, 2.6, 3.2)
      call quad(C_BLACK, -1.85, 2.15+brow_dy, -0.55, 2.45+brow_dy, &
         -0.50, 2.25+brow_dy, -1.80, 1.95+brow_dy)
      call quad(C_BLACK, 1.85, 2.15+brow_dy, 0.55, 2.45+brow_dy, &
         0.50, 2.25+brow_dy, 1.80, 1.95+brow_dy)
! Left Eye (always open during a wink)
      if (.not. blink .or. wink_only) then
         call color(C_BLACK)
         call circle(-1.15, 1.55, 0.34)
         call color(C_WHITE)
         call circle(-1.24, 1.64, 0.10)
      else
         call polyfill(.false.)
         call color(C_BLACK)
         call linewidth(18)
         call move2(-1.50, 1.52); call draw2(-0.80, 1.52)
         call linewidth(1)
         call polyfill(.true.)
      end if
! Right Eye (closes for both a blink and a wink)
      if (.not. blink) then
         call color(C_BLACK)
         call circle( 1.15, 1.55, 0.34)
         call color(C_WHITE)
         call circle( 1.06, 1.64, 0.10)
      else
         call polyfill(.false.)
         call color(C_BLACK)
         call linewidth(18)
         call move2( 0.80, 1.52); call draw2( 1.50, 1.52)
         call linewidth(1)
         call polyfill(.true.)
      end if
      call tri(C_SKIN_SHD, 0.0, 1.6, 0.35, 0.45, -0.15, 0.45)
      call quad(C_BLACK, -0.52, -0.45, -0.45, 0.15, 0.45, 0.15, 0.52, -0.45)
      call polyfill(.false.)
      call color(C_BLACK)
      call linewidth(16)
      call arc(0.0, -0.55, 1.05, 215.0, 325.0)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_head
   subroutine draw_bowler_hat()
      call color(C_BLACK)
      call circle(0.0, 4.9, 2.55)
      call quad(C_BLACK, -2.55, 3.7, -2.55, 4.9, 2.55, 4.9, 2.55, 3.7)
      call polyfill(.false.)
      call color(C_CHARCOAL)
      call linewidth(14)
      call arc(-0.3, 5.1, 1.8, 95.0, 165.0)
      call linewidth(1)
      call polyfill(.true.)
      call quad(C_GRAY, -2.58, 3.7, -2.58, 4.15, 2.58, 4.15, 2.58, 3.7)
      call polyfill(.false.)
      call color(C_BLACK)
      call linewidth(45)
      call arc(0.0, 4.7, 3.65, 205.0, 335.0)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_bowler_hat
   subroutine draw_cane_and_hand(angle_deg)
      real, intent(in) :: angle_deg
      call pushmatrix()
      call translate(3.2, -5.2, 0.0)
      call rotate(angle_deg, 'z')
      call quad(C_CANE, -0.14, -4.5, 0.14, -4.5, 0.14, 1.2, -0.14, 1.2)
      call polyfill(.false.)
      call color(C_CANE)
      call linewidth(26)
      call arc(-0.55, 1.2, 0.55, 0.0, 180.0)
      call linewidth(1)
      call polyfill(.true.)
      call popmatrix()
      call color(C_SKIN)
      call circle(3.2, -5.2, 0.58)
   end subroutine draw_cane_and_hand
   subroutine tri(col, x1, y1, x2, y2, x3, y3)
      integer, intent(in) :: col
      real, intent(in) :: x1, y1, x2, y2, x3, y3
      call color(col)
      call makepoly()
      call move2(x1, y1)
      call draw2(x2, y2)
      call draw2(x3, y3)
      call closepoly()
   end subroutine tri
   subroutine quad(col, x1, y1, x2, y2, x3, y3, x4, y4)
      integer, intent(in) :: col
      real, intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
      call color(col)
      call makepoly()
      call move2(x1, y1)
      call draw2(x2, y2)
      call draw2(x3, y3)
      call draw2(x4, y4)
      call closepoly()
   end subroutine quad

subroutine system_sleep(seconds)
   use,intrinsic                 :: iso_c_binding, only: c_int

   class(*),intent(in)           :: seconds
   integer(kind=c_int)           :: cint
   select type(seconds)
    type is (integer);             cint=seconds               ; call call_sleep(cint)
    type is (real);                cint=nint(seconds*1000000) ; call call_usleep(cint)
    type is (doubleprecision);     cint=nint(seconds*1000000) ; call call_usleep(cint)
   end select
end SUBROUTINE system_sleep

subroutine call_sleep(wait_seconds)
   use,intrinsic                   :: iso_c_binding, only: c_int

   integer(kind=c_int),intent(in)  :: wait_seconds
   integer(kind=c_int)             :: how_long
   interface
      function c_sleep(seconds) bind (C,name="sleep")
         import
         integer(c_int)       :: c_sleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
         integer(c_int), intent(in), VALUE :: seconds
      end function c_sleep
   end interface
   if(wait_seconds>0)then
      how_long=c_sleep(wait_seconds)
   endif
end subroutine call_sleep

subroutine call_usleep(microseconds)

   use,intrinsic                   :: iso_c_binding, only: c_int
   integer(kind=c_int),intent(in)  :: microseconds
   integer(kind=c_int)             :: status
   interface
      function c_usleep(mseconds) bind (C,name="usleep")
         import
         integer(c_int)       :: c_usleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
         integer(c_int), intent(in), VALUE :: mseconds
      end function c_usleep
   end interface
   if(microseconds>0)then
      status=c_usleep(microseconds)
   endif
end subroutine call_usleep
end program chaplin_featurette


! ### What's Happening in Each Scene
!
! 1. **Act I Title Card**: Renders a classic 1920s title card with double
! borders, gilt Art Deco corner ornaments (`draw_corner_ornament`), serif
! typography (`times.rb`), and subtle projector gate-weave jitter.
!
! 2. **Scene 1 (Iris-In)**: Opens with an expanding circular spotlight
! iris as Chaplin tips his bowler hat and spins his bamboo cane.
!
! 3. **Scene II Dialogue Card**: Cuts to a quoted Chaplin intertitle card
! with vintage film scratches.
!
! 4. **Scene 2 (Wink & Iris-Out)**: Chaplin tips his hat in the opposite
! direction, gives a deliberate single-eye wink (`wink_only = .true.`),
! and closes out with a classic silent-film iris-out.
!
