program vogle_demo
   use M_draw
   implicit none

   real, parameter :: PI = 3.141592653589793
   character(len=20) :: device
   integer :: key

   ! Initialize graphics device (blank prompts or uses M_DRAW_DEVICE, e.g., 'X11', 'svg', 'pdf')
   device = ' '
   call vinit(device)

   ! Set up a dark background
   call color(D_BLACK)
   call clear()

   ! --- LEFT VIEWPORT: 2D Parametric "Maurer Rose" ---
   call viewport(-1.0, 0.0, -1.0, 1.0)
   call ortho2(-1.2, 1.2, -1.2, 1.2)
   call draw_maurer_rose(n=6.0, d=71.0)

   ! --- RIGHT VIEWPORT: 3D Wireframe Torus ---
   call viewport(0.0, 1.0, -1.0, 1.0)
   call perspective(45.0, 1.0, 0.1, 100.0)
   call lookat(0.0, 3.2, 2.2, 0.0, 0.0, 0.0, 0.0)
   call draw_torus(major_r=1.0, minor_r=0.4, rings=36, sides=20)

   ! --- TITLE OVERLAY ---
   call viewport(-1.0, 1.0, -1.0, 1.0)
   call ortho2(-1.0, 1.0, -1.0, 1.0)
   call color(D_WHITE)
   call font('times.rb')
   call textsize(0.04, 0.06)
   call centertext(.true.)
   call move2(0.0, 0.88)
   call drawstr('M_drawtran / M_draw / VOGLE Vector Graphics in Fortran')

   ! Flush graphics and wait for a key press or mouse click before closing
   call vflush()
   key = getkey()
   call vexit()

contains

   subroutine draw_maurer_rose(n, d)
      real, intent(in) :: n, d
      integer :: k
      real :: theta, r, x, y

      call color(D_CYAN)
      call move2(0.0, 0.0)
      do k = 0, 360
         theta = real(k) * d * PI / 180.0
         r = sin(n * theta)
         x = r * cos(theta)
         y = r * sin(theta)
         call draw2(x, y)
      end do

      ! Overlay the smooth rose curve in magenta
      call color(D_MAGENTA)
      call linewidth(20)
      call move2(0.0, 0.0)
      do k = 0, 360
         theta = real(k) * PI / 180.0
         r = sin(n * theta)
         x = r * cos(theta)
         y = r * sin(theta)
         call draw2(x, y)
      end do
      call linewidth(1)
   end subroutine draw_maurer_rose

   subroutine draw_torus(major_r, minor_r, rings, sides)
      real, intent(in) :: major_r, minor_r
      integer, intent(in) :: rings, sides
      integer :: i, j
      real :: u, v, du, dv, x, y, z

      du = 2.0 * PI / real(rings)
      dv = 2.0 * PI / real(sides)

      ! Draw longitudinal rings
      call color(D_YELLOW)
      do i = 0, rings - 1
         u = real(i) * du
         do j = 0, sides
            v = real(j) * dv
            x = (major_r + minor_r * cos(v)) * cos(u)
            y = (major_r + minor_r * cos(v)) * sin(u)
            z = minor_r * sin(v)
            if (j == 0) then
               call move(x, y, z)
            else
               call draw(x, y, z)
            end if
         end do
      end do

      ! Draw latitudinal circles
      call color(D_GREEN)
      do j = 0, sides - 1
         v = real(j) * dv
         do i = 0, rings
            u = real(i) * du
            x = (major_r + minor_r * cos(v)) * cos(u)
            y = (major_r + minor_r * cos(v)) * sin(u)
            z = minor_r * sin(v)
            if (i == 0) then
               call move(x, y, z)
            else
               call draw(x, y, z)
            end if
         end do
      end do
   end subroutine draw_torus

end program vogle_demo
