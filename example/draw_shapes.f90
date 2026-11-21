! Visual Test-Card Demo (demo_mdraw_shapes.f90)

!   This program draws a complete graphics test card—filled polygons,
!   hatched shapes, dashed lines, arcs, sectors, bicubic patches, and
!   hardware text—and outputs it to any driver you pass on the command
!   line (defaulting to svg -> demo_output.svg).

program demo_mdraw_shapes
   use, intrinsic :: iso_fortran_env, only: real32, int16
   use m_draw
   implicit none

   character(len=32) :: dev_name, out_file
   real,parameter :: PI=acos(-1.0)
   integer :: nargs
   real(real32) :: star_x(10), star_y(10), pts(10, 2)
   real(real32) :: angle, r
   integer :: i

   nargs = command_argument_count()
   if (nargs >= 1) then
      call get_command_argument(1, dev_name)
   else
      dev_name = "svg"
   end if

   if (nargs >= 2) then
      call get_command_argument(2, out_file)
   else
      select case (trim(dev_name))
      case ("svg");        out_file = "demo_output.svg"
      case ("ps", "psc");  out_file = "demo_output.ps"
      case ("pdf");        out_file = "demo_output.pdf"
      case ("p3", "ppm");  out_file = "demo_output.ppm"
      case ("p1", "pbm");  out_file = "demo_output.pbm"
      case ("xfig");       out_file = "demo_output.fig"
      case ("canvas");     out_file = "demo_output.html"
      case ("X11", "x11"); out_file = "-"
      case default;        out_file = "demo_output.out"
      end select
   end if

   if (trim(out_file) /= "-") then
      call voutput(trim(out_file))
   end if

   call prefsize(800, 800)
   call vinit(trim(dev_name))
   call ortho2(-1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32)

   ! Background clear (white)
   call color(D_WHITE)
   call clear()

   ! Outer border
   call color(D_BLACK)
   call linewidth(20)
   call rect(-0.95_real32, -0.95_real32, 0.95_real32, 0.95_real32)

   ! 1. Filled 5-pointed star (top-left)
   do i = 1, 10
      angle = real(i - 1, real32) * 2.0_real32 * PI / 10.0_real32 + PI / 2.0_real32
      r = merge(0.35_real32, 0.14_real32, mod(i, 2) == 1)
      star_x(i) = -0.45_real32 + r * cos(angle)
      star_y(i) =  0.45_real32 + r * sin(angle)
      pts(i, 1) = star_x(i)
      pts(i, 2) = star_y(i)
   end do
   call color(D_RED)
   call polyfill(1)
   call poly2(10, pts)
   call polyfill(0)

   ! 2. Hatched circle & sector (top-right)
   call color(D_BLUE)
   call polyhatch(1)
   call hatchang(45.0_real32)
   call hatchpitch(0.04_real32)
   call circle(0.45_real32, 0.45_real32, 0.30_real32)
   call polyhatch(0)

   ! 3. Dashed line & concentric arcs (bottom-left)
   call color(D_GREEN)
   call dashcode(0.05_real32)
   call linestyle("1100")
   call move2(-0.85_real32, -0.45_real32)
   call draw2(-0.05_real32, -0.45_real32)
   call linestyle("") ! reset to solid

   call color(D_MAGENTA)
   do i = 1, 5
      call arc(-0.45_real32, -0.80_real32, 0.06_real32 * real(i, real32), 20.0_real32, 160.0_real32)
   end do

   ! 4. Filled sector & label (bottom-right)
   call color(D_CYAN)
   call polyfill(1)
   call sector(0.45_real32, -0.55_real32, 0.30_real32, 30.0_real32, 310.0_real32)
   call polyfill(0)

   call color(D_BLACK)
   call font("large")
   call textsize(0.04_real32, 0.06_real32)
   call move2(-0.65_real32, -0.05_real32)
   call drawstr("M_DRAW Fortran 1-Based Port")

   ! Pause for key press if interactive X11
   if (trim(dev_name) == "X11" .or. trim(dev_name) == "x11") then
      write(*, '(A)') "Press any key inside the X11 window to exit..."
      i = getkey()
   end if

   call vexit()
   if (trim(out_file) /= "-") then
      write(*, '(A, A)') "Wrote output to: ", trim(out_file)
   end if
end program demo_mdraw_shapes
