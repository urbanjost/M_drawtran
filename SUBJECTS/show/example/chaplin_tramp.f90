program chaplin_tramp
   use M_draw
   implicit none

   integer :: key

   ! Custom silent-film sepia/monochrome palette (indices >= 10 for VOGLE safety)
   integer, parameter :: C_BG       = 10
   integer, parameter :: C_SPOTLIGHT= 11
   integer, parameter :: C_BLACK    = 12
   integer, parameter :: C_CHARCOAL = 13
   integer, parameter :: C_GRAY     = 14
   integer, parameter :: C_SKIN_SHD = 15
   integer, parameter :: C_SKIN     = 16
   integer, parameter :: C_WHITE    = 17
   integer, parameter :: C_CANE     = 18

   call vinit(' ')
   call prefsize(650, 780)

   ! Define custom RGB palette (0 to 255)
   call mapcolor(C_BG,        28,  26,  24)   ! Dark theater backdrop
   call mapcolor(C_SPOTLIGHT, 58,  54,  48)   ! Warm vintage vignette
   call mapcolor(C_BLACK,     14,  14,  15)   ! Suit, hat, hair, mustache
   call mapcolor(C_CHARCOAL,  38,  38,  42)   ! Coat highlights & folds
   call mapcolor(C_GRAY,     125, 122, 116)   ! Hat band & shading
   call mapcolor(C_SKIN_SHD, 198, 178, 156)   ! Face shadow facet
   call mapcolor(C_SKIN,     232, 214, 194)   ! Silent-film pale makeup tone
   call mapcolor(C_WHITE,    248, 245, 238)   ! Wingtip collar & shirt
   call mapcolor(C_CANE,     176, 138,  84)   ! Bamboo cane

   call color(C_BG)
   call clear()
   call ortho2(-10.0, 10.0, -12.0, 12.0)
   call polyfill(.true.)

   ! --- 1. VINTAGE FILM SPOTLIGHT BACKDROP ---
   call color(C_SPOTLIGHT)
   call circle(0.0, 0.5, 8.6)

   ! --- 2. TIGHT-FITTING JACKET & SHOULDERS ---
   ! Main torso silhouette
   call quad(C_BLACK,   -5.2, -10.0,  -4.4, -3.4,   4.4, -3.4,   5.2, -10.0)
   ! Shoulder curves
   call tri(C_BLACK,    -4.4,  -3.4,  -2.2, -2.5,  -5.2, -10.0)
   call tri(C_BLACK,     4.4,  -3.4,   2.2, -2.5,   5.2, -10.0)
   ! Coat lapels & chest shading
   call tri(C_CHARCOAL, -2.6,  -2.6,  -1.0, -7.2,  -4.2,  -6.5)
   call tri(C_CHARCOAL,  2.6,  -2.6,   1.0, -7.2,   4.2,  -6.5)

   ! --- 3. HIGH WHITE WINGTIP COLLAR & TIE ---
   ! Shirt front
   call tri(C_WHITE,    -1.6,  -2.6,   1.6, -2.6,   0.0,  -6.8)
   ! Left & right collar flaps
   call tri(C_WHITE,    -1.8,  -2.4,  -0.4, -3.2,  -1.9,  -3.9)
   call tri(C_WHITE,     1.8,  -2.4,   0.4, -3.2,   1.9,  -3.9)
   ! Narrow dark necktie
   call quad(C_BLACK,   -0.35, -3.2,   0.35,-3.2,   0.55, -6.5,  -0.55, -6.5)
   call tri(C_CHARCOAL, -0.4,  -3.0,   0.4, -3.0,   0.0,  -3.6) ! Tie knot

   ! --- 4. NECK & FACE ---
   ! Neck
   call quad(C_SKIN_SHD,-1.1,  -2.7,  -1.1, -1.0,   1.1,  -1.0,   1.1,  -2.7)

   ! Ears
   call color(C_SKIN_SHD)
   call circle(-2.7, 1.3, 0.55)
   call circle( 2.7, 1.3, 0.55)

   ! Faceted Face (Left lit side, Right slightly shaded for depth)
   call tri(C_SKIN,      0.0,   4.2,  -2.6,  3.2,  -2.7,   0.6) ! Upper left cheek
   call tri(C_SKIN_SHD,  0.0,   4.2,   2.6,  3.2,   2.7,   0.6) ! Upper right cheek
   call tri(C_SKIN,      0.0,   4.2,  -2.7,  0.6,   0.0,  -0.2) ! Center forehead/nose
   call tri(C_SKIN_SHD,  0.0,   4.2,   2.7,  0.6,   0.0,  -0.2)
   call tri(C_SKIN,     -2.7,   0.6,  -1.9, -1.5,   0.0,  -0.2) ! Lower left jaw
   call tri(C_SKIN_SHD,  2.7,   0.6,   1.9, -1.5,   0.0,  -0.2) ! Lower right jaw
   call tri(C_SKIN,     -1.9,  -1.5,   0.0, -2.2,   0.0,  -0.2) ! Left chin
   call tri(C_SKIN_SHD,  1.9,  -1.5,   0.0, -2.2,   0.0,  -0.2) ! Right chin

   ! --- 5. CURLY HAIR TEMPLES ---
   call color(C_BLACK)
   call circle(-2.6, 2.7, 0.65)
   call circle(-2.8, 2.0, 0.50)
   call circle( 2.6, 2.7, 0.65)
   call circle( 2.8, 2.0, 0.50)

   ! --- 6. ICONIC BOWLER (DERBY) HAT ---
   ! Rounded crown
   call color(C_BLACK)
   call circle(0.0, 4.9, 2.55)
   call quad(C_BLACK,   -2.55,  3.7,  -2.55, 4.9,   2.55,  4.9,   2.55,  3.7)
   ! Subtle sheen on crown
   call color(C_CHARCOAL)
   call arc(-0.3, 5.1, 1.8, 95.0, 165.0)
   ! Hat ribbon band
   call quad(C_GRAY,    -2.58,  3.7,  -2.58, 4.15,  2.58,  4.15,  2.58,  3.7)
   ! Curved bowler brim
   call polyfill(.false.)
   call color(C_BLACK)
   call linewidth(45)
   call arc(0.0, 4.7, 3.65, 205.0, 335.0)
   call linewidth(1)
   call polyfill(.true.)

   ! --- 7. EXPRESSIVE EYES, BROWS, MUSTACHE & SMILE ---
   ! Expressive slanted silent-film eyebrows
   call quad(C_BLACK,   -1.85,  2.15, -0.55, 2.45, -0.50,  2.25, -1.80,  1.95)
   call quad(C_BLACK,    1.85,  2.15,  0.55, 2.45,  0.50,  2.25,  1.80,  1.95)

   ! Dark expressive eyes with white catchlights
   call color(C_BLACK)
   call circle(-1.15, 1.55, 0.34)
   call circle( 1.15, 1.55, 0.34)
   call color(C_WHITE)
   call circle(-1.24, 1.64, 0.10)
   call circle( 1.06, 1.64, 0.10)

   ! Nose shadow
   call tri(C_SKIN_SHD,  0.0,   1.6,   0.35, 0.45, -0.15,  0.45)

   ! Iconic Toothbrush Mustache
   call quad(C_BLACK,   -0.52, -0.45, -0.45, 0.15,  0.45,  0.15,  0.52, -0.45)

   ! Gentle wistful smile
   call polyfill(.false.)
   call color(C_BLACK)
   call linewidth(16)
   call arc(0.0, -0.55, 1.05, 215.0, 325.0)
   call linewidth(1)
   call polyfill(.true.)

   ! --- 8. BAMBOO CANE & GLOVED HAND ---
   ! Cane shaft
   call quad(C_CANE,     3.25, -10.0,  3.55,-10.0,  3.15, -4.8,   2.85, -4.8)
   ! Cane curved handle
   call polyfill(.false.)
   call color(C_CANE)
   call linewidth(28)
   call arc(2.45, -4.8, 0.55, 0.0, 180.0)
   call linewidth(1)
   call polyfill(.true.)
   ! Hand resting on cane top
   call color(C_SKIN)
   call circle(2.95, -4.45, 0.58)

   ! --- 9. SILENT FILM TITLE CARD BORDER & CAPTION ---
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
   call drawstr('"THE LITTLE TRAMP"')

   call vflush()
   key = getkey()
   call vexit()

contains

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

end program chaplin_tramp
!    Silent-Film Aesthetic: Uses a warm sepia-and-charcoal palette
!    (mapcolor) inside a double-lined silent-film title-card frame (rect).
!
!    Mixed Primitives: Combines filled triangles (tri) and quadrilaterals
!    (quad) for facial planes, lapels, and the toothbrush mustache with
!    circle and thick arc strokes for the bowler hat brim, cane handle,
!    and wistful smile.
!
!    VOGLE & M_draw Compatible: Uses color indices 10–18 so it runs
!    cleanly on both M_draw and classic libvogle without overwriting
!    default indices 0–7.
