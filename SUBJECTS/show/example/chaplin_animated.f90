! An animated Charlie Chaplin ("The Little Tramp") program using M_draw /
! VOGLE's double-buffering (backbuffer and swapbuffers) and hierarchical
! matrix transforms (pushmatrix, translate, rotate, popmatrix).
! 
! In this animation, Chaplin tips his bowler hat, blinks, twirls his bamboo
! cane, and has subtle vintage film flicker and vertical scratches—giving
! it the feel of an authentic 1920s silent-film reel.

program chaplin_animated
   use M_draw
   implicit none

   real, parameter :: PI = 3.141592653589793
   integer :: frame, key, ierr
   real :: t, hat_lift, hat_tilt, cane_angle, scratch_x
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

   call prefsize(650, 780)
   call vinit(' ')

   ! Define custom RGB palette (0 to 255)
   call mapcolor(C_BG,        24,  22,  20)   ! Theater backdrop
   call mapcolor(C_SPOT_A,    58,  54,  48)   ! Spotlight frame A
   call mapcolor(C_SPOT_B,    52,  48,  43)   ! Spotlight frame B (film flicker)
   call mapcolor(C_BLACK,     14,  14,  15)   ! Suit, hat, hair, mustache
   call mapcolor(C_CHARCOAL,  38,  38,  42)   ! Coat highlights & folds
   call mapcolor(C_GRAY,     125, 122, 116)   ! Hat band & film scratches
   call mapcolor(C_SKIN_SHD, 198, 178, 156)   ! Face shadow facet
   call mapcolor(C_SKIN,     232, 214, 194)   ! Silent-film pale tone
   call mapcolor(C_WHITE,    248, 245, 238)   ! Wingtip collar & title card
   call mapcolor(C_CANE,     176, 138,  84)   ! Bamboo cane

   call ortho2(-10.0, 10.0, -12.0, 12.0)

   ! Enable double-buffering for flicker-free animation
   ierr = backbuffer()

   frame = 0
   do
      t = real(frame) * 0.06

      ! Smooth periodic hat-tip motion (pauses down, lifts & tilts up)
      hat_lift = max(0.0, sin(t)) ** 2 * 1.45
      hat_tilt = -max(0.0, sin(t)) ** 2 * 16.0

      ! Continuous cane twirl (360-degree spin synchronized with hat tip)
      cane_angle = -t * 180.0 / PI

      ! Quick periodic blink every ~90 frames
      is_blinking = (mod(frame, 95) >= 88 .and. mod(frame, 95) <= 92)

      ! --- 1. CLEAR & VINTAGE SPOTLIGHT WITH PROJECTOR FLICKER ---
      call color(C_BG)
      call clear()
      call polyfill(.true.)

      if (mod(frame, 4) < 2) then
         call color(C_SPOT_A)
      else
         call color(C_SPOT_B)
      end if
      call circle(0.0, 0.5, 8.6)

      ! --- 2. JACKET, COLLAR & TIE ---
      call draw_torso()

      ! --- 3. HEAD, FACE, EYES & MUSTACHE ---
      call draw_head(is_blinking, hat_lift > 0.25)

      ! --- 4. ANIMATED BOWLER HAT (TRANSLATE + ROTATE) ---
      call pushmatrix()
      call translate(0.25 * hat_lift, hat_lift,0.0 )
      call rotate(hat_tilt, 'z')
      call draw_bowler_hat()
      call popmatrix()

      ! --- 5. TWIRLING BAMBOO CANE & HAND ---
      call draw_cane_and_hand(cane_angle)

      ! --- 6. SILENT-FILM SCRATCHES & TITLE CARD FRAME ---
      call polyfill(.false.)
      if (mod(frame, 3) == 0) then
         scratch_x = -7.5 + mod(real(frame * 37), 15.0)
         call color(C_GRAY)
         call linewidth(4)
         call move2(scratch_x, -10.8)
         call draw2(scratch_x + 0.08, 10.8)
      end if

      call draw_title_card()

      ! Swap back buffer to screen and pace animation (~30 FPS)
      call swapbuffers()
      call call_usleep(32000)

      ! Exit cleanly if the user presses any key (e.g., 'q' or Esc)
      key = checkkey()
      if (key /= 0) exit

      frame = frame + 1
   end do

   call vexit()

contains

   subroutine draw_torso()
      ! Main coat silhouette
      call quad(C_BLACK,   -5.2, -10.0,  -4.4, -3.4,   4.4, -3.4,   5.2, -10.0)
      call tri(C_BLACK,    -4.4,  -3.4,  -2.2, -2.5,  -5.2, -10.0)
      call tri(C_BLACK,     4.4,  -3.4,   2.2, -2.5,   5.2, -10.0)
      ! Lapel highlights
      call tri(C_CHARCOAL, -2.6,  -2.6,  -1.0, -7.2,  -4.2,  -6.5)
      call tri(C_CHARCOAL,  2.6,  -2.6,   1.0, -7.2,   4.2,  -6.5)
      ! White shirt & wingtip collar
      call tri(C_WHITE,    -1.6,  -2.6,   1.6, -2.6,   0.0,  -6.8)
      call tri(C_WHITE,    -1.8,  -2.4,  -0.4, -3.2,  -1.9,  -3.9)
      call tri(C_WHITE,     1.8,  -2.4,   0.4, -3.2,   1.9,  -3.9)
      ! Necktie
      call quad(C_BLACK,   -0.35, -3.2,   0.35,-3.2,   0.55, -6.5,  -0.55, -6.5)
      call tri(C_CHARCOAL, -0.4,  -3.0,   0.4, -3.0,   0.0,  -3.6)
   end subroutine draw_torso

   subroutine draw_head(blink, raised_brows)
      logical, intent(in) :: blink, raised_brows
      real :: brow_dy

      brow_dy = merge(0.22, 0.0, raised_brows)

      ! Neck & Ears
      call quad(C_SKIN_SHD,-1.1,  -2.7,  -1.1, -1.0,   1.1,  -1.0,   1.1,  -2.7)
      call color(C_SKIN_SHD)
      call circle(-2.7, 1.3, 0.55)
      call circle( 2.7, 1.3, 0.55)

      ! Faceted Face
      call tri(C_SKIN,      0.0,   4.2,  -2.6,  3.2,  -2.7,   0.6)
      call tri(C_SKIN_SHD,  0.0,   4.2,   2.6,  3.2,   2.7,   0.6)
      call tri(C_SKIN,      0.0,   4.2,  -2.7,  0.6,   0.0,  -0.2)
      call tri(C_SKIN_SHD,  0.0,   4.2,   2.7,  0.6,   0.0,  -0.2)
      call tri(C_SKIN,     -2.7,   0.6,  -1.9, -1.5,   0.0,  -0.2)
      call tri(C_SKIN_SHD,  2.7,   0.6,   1.9, -1.5,   0.0,  -0.2)
      call tri(C_SKIN,     -1.9,  -1.5,   0.0, -2.2,   0.0,  -0.2)
      call tri(C_SKIN_SHD,  1.9,  -1.5,   0.0, -2.2,   0.0,  -0.2)

      ! Hair temples & top of head (visible when hat lifts!)
      call color(C_BLACK)
      call circle(-2.6, 2.7, 0.65)
      call circle(-2.8, 2.0, 0.50)
      call circle( 2.6, 2.7, 0.65)
      call circle( 2.8, 2.0, 0.50)
      call tri(C_BLACK, -2.6, 3.2,  0.0, 4.4,  2.6, 3.2)

      ! Expressive Eyebrows
      call quad(C_BLACK, -1.85, 2.15+brow_dy, -0.55, 2.45+brow_dy, &
                         -0.50, 2.25+brow_dy, -1.80, 1.95+brow_dy)
      call quad(C_BLACK,  1.85, 2.15+brow_dy,  0.55, 2.45+brow_dy, &
                          0.50, 2.25+brow_dy,  1.80, 1.95+brow_dy)

      ! Eyes (open with catchlights, or closed eyelid lines when blinking)
      if (.not. blink) then
         call color(C_BLACK)
         call circle(-1.15, 1.55, 0.34)
         call circle( 1.15, 1.55, 0.34)
         call color(C_WHITE)
         call circle(-1.24, 1.64, 0.10)
         call circle( 1.06, 1.64, 0.10)
      else
         call polyfill(.false.)
         call color(C_BLACK)
         call linewidth(18)
         call move2(-1.50, 1.52); call draw2(-0.80, 1.52)
         call move2( 0.80, 1.52); call draw2( 1.50, 1.52)
         call linewidth(1)
         call polyfill(.true.)
      end if

      ! Nose shadow & Toothbrush Mustache
      call tri(C_SKIN_SHD, 0.0,   1.6,   0.35, 0.45, -0.15,  0.45)
      call quad(C_BLACK,  -0.52, -0.45, -0.45, 0.15,  0.45,  0.15,  0.52, -0.45)

      ! Smile
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

      ! Crown highlight
      call polyfill(.false.)
      call color(C_CHARCOAL)
      call linewidth(14)
      call arc(-0.3, 5.1, 1.8, 95.0, 165.0)
      call linewidth(1)
      call polyfill(.true.)

      ! Hat ribbon band
      call quad(C_GRAY, -2.58, 3.7, -2.58, 4.15, 2.58, 4.15, 2.58, 3.7)

      ! Curved brim
      call polyfill(.false.)
      call color(C_BLACK)
      call linewidth(45)
      call arc(0.0, 4.7, 3.65, 205.0, 335.0)
      call linewidth(1)
      call polyfill(.true.)
   end subroutine draw_bowler_hat

   subroutine draw_cane_and_hand(angle_deg)
      real, intent(in) :: angle_deg

      ! Rotate the bamboo cane around Chaplin's right hand at (3.2, -5.2)
      call pushmatrix()
      call translate(3.2, -5.2, 0.0)
      call rotate(angle_deg, 'z')

      ! Cane shaft centered near grip
      call quad(C_CANE, -0.14, -4.5,  0.14, -4.5,  0.14, 1.2, -0.14, 1.2)

      ! Curved bamboo crook handle
      call polyfill(.false.)
      call color(C_CANE)
      call linewidth(26)
      call arc(-0.55, 1.2, 0.55, 0.0, 180.0)
      call linewidth(1)
      call polyfill(.true.)

      call popmatrix()

      ! Hand in front of spinning cane shaft
      call color(C_SKIN)
      call circle(3.2, -5.2, 0.58)
   end subroutine draw_cane_and_hand

   subroutine draw_title_card()
      call polyfill(.false.)
      call color(C_WHITE)
      call linewidth(14)
      call rect(-9.2, -11.2, 9.2, 11.2)
      call linewidth(6)
      call rect(-8.9, -10.9, 8.9, 10.9)
      call linewidth(1)

      call font('times.rb')
      call textsize(0.42, 0.55)
      call centertext(.true.)
      call move2(0.0, -9.8)
      call drawstr('"THE LITTLE TRAMP"  (PRESS ANY KEY)')
   end subroutine draw_title_card

   subroutine tri(col, x1, y1, x2, y2, x3, y3)
      integer, intent(in) :: col
      real, intent(in)    :: x1, y1, x2, y2, x3, y3

      call color(col)
      call makepoly()
      call move2(x1, y1)
      call draw2(x2, y2)
      call draw2(x3, y3)
      call closepoly()
   end subroutine tri

   subroutine quad(col, x1, y1, x2, y2, x3, y3, x4, y4)
      integer, intent(in) :: col
      real, intent(in)    :: x1, y1, x2, y2, x3, y3, x4, y4

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

end program chaplin_animated

! Key Animation Techniques Demonstrated
!
!    Double Buffering (backbuffer + swapbuffers): Every frame is drawn
!    completely off-screen first and swapped to the window in one instant
!    step, preventing any tearing or flickering.
!
!    Hierarchical Transforms (pushmatrix / translate / rotate / popmatrix):
!    Instead of manually recalculating polygon coordinates every frame,
!    the bowler hat and cane are drawn in their local coordinates and
!    rotated/translated smoothly via the graphics transformation stack.
!
!    Non-Blocking Keyboard Exit (checkkey): Unlike getkey(), which pauses
!    execution, checkkey() lets the animation loop run continuously until
!    you press any key to close the window.
!
! (Note: If compiling against classic VOGLE on a system without usleep
! in M_draw, add use iso_c_binding or replace call usleep(32000) with a
! short Fortran cpu_time delay loop.)
