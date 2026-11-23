! A low-poly geometric fox built from shaded filled triangles
! and polygons in M_draw / VOGLE. Using custom RGB colors (mapcolor) and
! polyfill(.true.), it creates a faceted, stained-glass or origami-style
! illustration.

program lowpoly_fox
   use M_draw
   implicit none

   integer :: key
   ! Custom color indices
   integer, parameter :: C_BG       = 10
   integer, parameter :: C_OR_DARK  = 11
   integer, parameter :: C_OR_MID   = 12
   integer, parameter :: C_OR_LIGHT = 13
   integer, parameter :: C_OR_BRIGHT= 14
   integer, parameter :: C_CREAM    = 15
   integer, parameter :: C_WHITE    = 16
   integer, parameter :: C_CHARCOAL = 17
   integer, parameter :: C_GOLD     = 18

   call vinit(' ')
   call prefsize(700, 700)

   ! Define custom RGB palette (0 to 255)
   call mapcolor(C_BG,        26,  32,  44)   ! Deep slate background
   call mapcolor(C_OR_DARK,  175,  55,  15)   ! Deep burnt orange
   call mapcolor(C_OR_MID,   218,  82,  22)   ! Classic fox orange
   call mapcolor(C_OR_LIGHT, 242, 118,  34)   ! Warm tangerine
   call mapcolor(C_OR_BRIGHT,252, 155,  55)   ! Highlight orange
   call mapcolor(C_CREAM,    235, 228, 215)   ! Soft off-white fur
   call mapcolor(C_WHITE,    252, 250, 245)   ! Bright white highlight
   call mapcolor(C_CHARCOAL,  22,  24,  30)   ! Nose, eyes, ear tips
   call mapcolor(C_GOLD,     245, 195,  45)   ! Eye iris

   ! Clear canvas and set symmetric 2D coordinate space
   call color(C_BG)
   call clear()
   call ortho2(-10.0, 10.0, -10.0, 10.0)

   ! Enable solid polygon filling
   call polyfill(.true.)

   ! --- 1. EARS (Outer & Inner Facets) ---
   ! Left Ear
   call tri(C_OR_DARK,   -6.2,  8.5,  -6.8,  2.8,  -4.2,  4.8)
   call tri(C_OR_MID,    -6.2,  8.5,  -4.2,  4.8,  -2.2,  4.5)
   call tri(C_CHARCOAL,  -6.2,  8.5,  -5.6,  6.2,  -4.8,  6.6) ! Dark ear tip
   call tri(C_CREAM,     -5.6,  6.2,  -5.4,  3.5,  -3.5,  4.4) ! Inner ear

   ! Right Ear
   call tri(C_OR_DARK,    6.2,  8.5,   6.8,  2.8,   4.2,  4.8)
   call tri(C_OR_MID,     6.2,  8.5,   4.2,  4.8,   2.2,  4.5)
   call tri(C_CHARCOAL,   6.2,  8.5,   5.6,  6.2,   4.8,  6.6) ! Dark ear tip
   call tri(C_CREAM,      5.6,  6.2,   5.4,  3.5,   3.5,  4.4) ! Inner ear

   ! --- 2. FOREHEAD & CROWN ---
   call tri(C_OR_LIGHT,   0.0,  5.5,  -2.2,  4.5,   2.2,  4.5)
   call tri(C_OR_MID,    -2.2,  4.5,  -4.2,  4.8,  -3.5,  1.8)
   call tri(C_OR_MID,     2.2,  4.5,   4.2,  4.8,   3.5,  1.8)
   call tri(C_OR_BRIGHT,  0.0,  5.5,  -2.2,  4.5,   0.0,  2.0)
   call tri(C_OR_LIGHT,   0.0,  5.5,   2.2,  4.5,   0.0,  2.0)
   call tri(C_OR_MID,    -2.2,  4.5,  -3.5,  1.8,   0.0,  2.0)
   call tri(C_OR_DARK,    2.2,  4.5,   3.5,  1.8,   0.0,  2.0)

   ! --- 3. UPPER CHEEKS & BRIDGE OF NOSE ---
   call tri(C_OR_DARK,   -4.2,  4.8,  -6.8,  2.8,  -3.5,  1.8)
   call tri(C_OR_DARK,    4.2,  4.8,   6.8,  2.8,   3.5,  1.8)
   call tri(C_OR_MID,    -6.8,  2.8,  -7.5, -0.5,  -3.5,  1.8)
   call tri(C_OR_MID,     6.8,  2.8,   7.5, -0.5,   3.5,  1.8)

   ! Nose bridge facets
   call tri(C_OR_BRIGHT,  0.0,  2.0,  -1.4, -0.5,   0.0, -3.2)
   call tri(C_OR_LIGHT,   0.0,  2.0,   1.4, -0.5,   0.0, -3.2)
   call tri(C_OR_MID,     0.0,  2.0,  -3.5,  1.8,  -1.4, -0.5)
   call tri(C_OR_DARK,    0.0,  2.0,   3.5,  1.8,   1.4, -0.5)

   ! --- 4. WHITE CHEEK RUFFS & MUZZLE ---
   ! Left Cheek
   call tri(C_CREAM,     -7.5, -0.5,  -5.8, -3.6,  -3.5,  1.8)
   call tri(C_WHITE,     -3.5,  1.8,  -5.8, -3.6,  -1.4, -0.5)
   call tri(C_CREAM,     -5.8, -3.6,  -2.5, -4.8,  -1.4, -0.5)
   call tri(C_WHITE,     -1.4, -0.5,  -2.5, -4.8,   0.0, -3.2)

   ! Right Cheek
   call tri(C_CREAM,      7.5, -0.5,   5.8, -3.6,   3.5,  1.8)
   call tri(C_WHITE,      3.5,  1.8,   5.8, -3.6,   1.4, -0.5)
   call tri(C_CREAM,      5.8, -3.6,   2.5, -4.8,   1.4, -0.5)
   call tri(C_WHITE,      1.4, -0.5,   2.5, -4.8,   0.0, -3.2)

   ! Chin
   call tri(C_CREAM,     -2.5, -4.8,   0.0, -5.8,   0.0, -3.2)
   call tri(C_WHITE,      2.5, -4.8,   0.0, -5.8,   0.0, -3.2)

   ! --- 5. EYES & NOSE ---
   ! Left Eye
   call tri(C_CHARCOAL,  -3.6,  1.2,  -1.8,  0.6,  -2.6,  1.5)
   call tri(C_GOLD,      -3.4,  1.1,  -2.0,  0.7,  -2.6,  1.3)

   ! Right Eye
   call tri(C_CHARCOAL,   3.6,  1.2,   1.8,  0.6,   2.6,  1.5)
   call tri(C_GOLD,       3.4,  1.1,   2.0,  0.7,   2.6,  1.3)

   ! Nose
   call tri(C_CHARCOAL,  -1.1, -3.0,   1.1, -3.0,   0.0, -4.4)
   call tri(C_WHITE,     -0.6, -3.2,   0.2, -3.2,  -0.2, -3.5) ! Nose glint

   ! --- 6. SUBTLE FACET WIREFRAME OVERLAY ---
   call polyfill(.false.)
   call linewidth(8)
   call tri(C_BG, -6.2,  8.5,  -4.2,  4.8,  -2.2,  4.5)
   call tri(C_BG,  6.2,  8.5,   4.2,  4.8,   2.2,  4.5)
   call tri(C_BG,  0.0,  5.5,  -2.2,  4.5,   0.0,  2.0)
   call tri(C_BG,  0.0,  5.5,   2.2,  4.5,   0.0,  2.0)
   call tri(C_BG,  0.0,  2.0,  -1.4, -0.5,   0.0, -3.2)
   call tri(C_BG,  0.0,  2.0,   1.4, -0.5,   0.0, -3.2)

   ! --- CAPTION ---
   call color(C_CREAM)
   call font('futura.m')
   call textsize(0.35, 0.45)
   call centertext(.true.)
   call move2(0.0, -8.2)
   call drawstr('LOW-POLY FOX  |  M_draw / VOGLE')

   call vflush()
   key = getkey()
   call vexit()

contains

   ! Helper subroutine to draw a 2D triangle from three (x, y) vertices
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

end program lowpoly_fox

!    Custom Palette (mapcolor): Replaces the default 8 primary colors
!    with warm oranges, cream, charcoal, and gold so adjacent triangles
!    create a 3D lighting effect.
!
!    Polygon Construction (makepoly / closepoly): Every facet is defined
!    as a closed 3-vertex polygon via the tri helper subroutine while
!    polyfill(.true.) is active.
!
!    Crisp Edge Accents: Toggling polyfill(.false.) at the end lets you
!    redraw selected triangles as thin outlines to sharpen the low-poly
!    aesthetic.
