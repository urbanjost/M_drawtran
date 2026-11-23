module m_draw_drv_sixel
   use, intrinsic :: iso_fortran_env, only: real32, output_unit
   use, intrinsic :: iso_c_binding
   use m_draw_types
   use m_draw_pref
   use m_draw_verror
   implicit none
   private
   public :: SIXEL_draw_devcpy, sixel_low_draw_devcpy

   integer, parameter :: CMAPSIZE        = 256
   integer, parameter :: DEV_SIXEL_UPPER = 0
   integer, parameter :: DEV_SIXEL_LOWER = 1

   integer, parameter :: MODE_BRAILLE    = 0
   integer, parameter :: MODE_SIXEL      = 1

   character(len=1), parameter :: ESC = achar(27)

   integer, parameter :: BRAILLE_BIT(0:1, 0:3) = reshape([ &
      1,   8, &
      2,  16, &
      4,  32, &
      64, 128 ], [2, 4])

   type :: rgb_t
      integer :: r = 0, g = 0, b = 0
   end type rgb_t

   type(rgb_t), save      :: carray(CMAPSIZE)
   integer, save          :: cur_color     = 7
   integer, save          :: bg_color      = 0
   integer, save          :: GLOBAL_device = DEV_SIXEL_UPPER
   integer, save          :: render_mode   = MODE_BRAILLE

   ! Terminal grid & pixel dimensions
   integer, save          :: term_cols     = 80
   integer, save          :: term_rows     = 24
   integer, save          :: cell_px_w     = 8
   integer, save          :: cell_px_h     = 16
   integer, save          :: pix_w         = 160
   integer, save          :: pix_h         = 96

   ! Braille framebuffers
   integer(1), allocatable, save :: dot_mask(:,:)
   integer, allocatable, save    :: cell_fg(:,:)
   character(len=1), allocatable, save :: text_ovl(:,:)

   ! Sixel 8-bit indexed pixel framebuffer (pix_w, pix_h)
   integer(1), allocatable, save :: sixel_fb(:,:)

   ! Backbuffer support
   logical, save          :: back_used = .false.
   integer(1), allocatable, save :: back_mask(:,:)
   integer, allocatable, save    :: back_fg(:,:)
   character(len=1), allocatable, save :: back_ovl(:,:)
   integer(1), allocatable, save :: back_sixel(:,:)

   ! Mouse state
   integer, save          :: last_mx = 0, last_my = 0, last_mbtn = 0
   logical, save          :: raw_enabled = .false.

#ifdef _WIN32
   ! -------------------------------------------------------------------------
   ! Native Windows Console API bindings
   ! -------------------------------------------------------------------------
   integer(c_int), parameter :: STD_INPUT_HANDLE  = -10
   integer(c_int), parameter :: STD_OUTPUT_HANDLE = -11
   integer(c_int), parameter :: ENABLE_PROCESSED_INPUT           = 1
   integer(c_int), parameter :: ENABLE_LINE_INPUT                = 2
   integer(c_int), parameter :: ENABLE_ECHO_INPUT                = 4
   integer(c_int), parameter :: ENABLE_WINDOW_INPUT              = 8
   integer(c_int), parameter :: ENABLE_MOUSE_INPUT               = 16
   integer(c_int), parameter :: ENABLE_VIRTUAL_TERMINAL_INPUT    = 512
   integer(c_int), parameter :: ENABLE_PROCESSED_OUTPUT          = 1
   integer(c_int), parameter :: ENABLE_WRAP_AT_EOL_OUTPUT        = 2
   integer(c_int), parameter :: ENABLE_VIRTUAL_TERMINAL_PROCESSING = 4

   type, bind(C) :: COORD
      integer(c_short) :: x, y
   end type COORD

   type, bind(C) :: SMALL_RECT
      integer(c_short) :: Left, Top, Right, Bottom
   end type SMALL_RECT

   type, bind(C) :: CONSOLE_SCREEN_BUFFER_INFO
      type(COORD)      :: dwSize
      type(COORD)      :: dwCursorPosition
      integer(c_short) :: wAttributes
      type(SMALL_RECT) :: srWindow
      type(COORD)      :: dwMaximumWindowSize
   end type CONSOLE_SCREEN_BUFFER_INFO

   type(c_ptr), save    :: hStdin  = c_null_ptr
   type(c_ptr), save    :: hStdout = c_null_ptr
   integer(c_int), save :: orig_in_mode  = 0
   integer(c_int), save :: orig_out_mode = 0

   interface
      type(c_ptr) function GetStdHandle(nStdHandle) bind(C, name="GetStdHandle")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int
         integer(c_int), value :: nStdHandle
      end function GetStdHandle

      integer(c_int) function GetConsoleScreenBufferInfo(hConsoleOutput, lpConsoleScreenBufferInfo) &
         bind(C, name="GetConsoleScreenBufferInfo")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int
         import :: CONSOLE_SCREEN_BUFFER_INFO
         type(c_ptr), value :: hConsoleOutput
         type(CONSOLE_SCREEN_BUFFER_INFO), intent(out) :: lpConsoleScreenBufferInfo
      end function GetConsoleScreenBufferInfo

      integer(c_int) function GetConsoleMode(hConsoleHandle, lpMode) bind(C, name="GetConsoleMode")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int
         type(c_ptr), value :: hConsoleHandle
         integer(c_int), intent(out) :: lpMode
      end function GetConsoleMode

      integer(c_int) function SetConsoleMode(hConsoleHandle, dwMode) bind(C, name="SetConsoleMode")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int
         type(c_ptr), value :: hConsoleHandle
         integer(c_int), value :: dwMode
      end function SetConsoleMode

      integer(c_int) function SetConsoleOutputCP(wCodePageID) bind(C, name="SetConsoleOutputCP")
         use, intrinsic :: iso_c_binding, only: c_int
         integer(c_int), value :: wCodePageID
      end function SetConsoleOutputCP

      integer(c_int) function WaitForSingleObject(hHandle, dwMilliseconds) bind(C, name="WaitForSingleObject")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int
         type(c_ptr), value :: hHandle
         integer(c_int), value :: dwMilliseconds
      end function WaitForSingleObject

      integer(c_int) function ReadFile(hFile, lpBuffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped) &
         bind(C, name="ReadFile")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
         type(c_ptr), value :: hFile
         character(kind=c_char), intent(out) :: lpBuffer(*)
         integer(c_int), value :: nNumberOfBytesToRead
         integer(c_int), intent(out) :: lpNumberOfBytesRead
         type(c_ptr), value :: lpOverlapped
      end function ReadFile

   end interface
#else
   ! -------------------------------------------------------------------------
   ! POSIX termios / ioctl / poll bindings (Linux, macOS, Cygwin, WSL, BSD)
   ! -------------------------------------------------------------------------
   integer(c_short), parameter :: POLLIN    = 1_c_short
   integer(c_int), parameter   :: TCSANOW   = 0_c_int
   integer(c_int), parameter   :: STDIN_FD  = 0_c_int
   integer(c_int), parameter   :: STDOUT_FD = 1_c_int

   ! Over-sized opaque buffer for struct termios across Linux/macOS/Cygwin/BSD
   integer(c_int64_t), save :: orig_termios(16) = 0

   type, bind(C) :: winsize_t
      integer(c_short) :: ws_row
      integer(c_short) :: ws_col
      integer(c_short) :: ws_xpixel
      integer(c_short) :: ws_ypixel
   end type winsize_t

   type, bind(C) :: pollfd_t
      integer(c_int)   :: fd
      integer(c_short) :: events
      integer(c_short) :: revents
   end type pollfd_t

   interface
      integer(c_int) function isatty(fd) bind(C, name="isatty")
         use, intrinsic :: iso_c_binding, only: c_int
         integer(c_int), value :: fd
      end function isatty

      integer(c_int) function tcgetattr(fd, termios_p) bind(C, name="tcgetattr")
         use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
         integer(c_int), value :: fd
         integer(c_int64_t), intent(out) :: termios_p(*)
      end function tcgetattr

      integer(c_int) function tcsetattr(fd, optional_actions, termios_p) bind(C, name="tcsetattr")
         use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
         integer(c_int), value :: fd, optional_actions
         integer(c_int64_t), intent(in) :: termios_p(*)
      end function tcsetattr

      subroutine cfmakeraw(termios_p) bind(C, name="cfmakeraw")
         use, intrinsic :: iso_c_binding, only: c_int64_t
         integer(c_int64_t), intent(inout) :: termios_p(*)
      end subroutine cfmakeraw

      integer(c_int) function c_ioctl_winsz(fd, request, argp) bind(C, name="ioctl")
         use, intrinsic :: iso_c_binding, only: c_int, c_long
         import :: winsize_t
         integer(c_int), value :: fd
         integer(c_long), value :: request
         type(winsize_t), intent(out) :: argp
      end function c_ioctl_winsz

      integer(c_int) function c_poll(fds, nfds, timeout) bind(C, name="poll")
         use, intrinsic :: iso_c_binding, only: c_int, c_long
         import :: pollfd_t
         type(pollfd_t), intent(inout) :: fds(*)
         integer(c_long), value :: nfds
         integer(c_int), value :: timeout
      end function c_poll

      integer(c_intptr_t) function c_read(fd, buf, count) bind(C, name="read")
         use, intrinsic :: iso_c_binding, only: c_int, c_intptr_t, c_size_t, c_char
         integer(c_int), value :: fd
         character(kind=c_char), intent(out) :: buf(*)
         integer(c_size_t), value :: count
      end function c_read
   end interface
#endif

contains

   ! -------------------------------------------------------------------------
   ! Cross-Platform Raw Mode & Non-Blocking Byte Reader
   ! -------------------------------------------------------------------------
   subroutine enable_raw_mode()
#ifdef _WIN32
      integer(c_int) :: in_mode, out_mode, ok
      if (raw_enabled) return
      hStdin  = GetStdHandle(STD_INPUT_HANDLE)
      hStdout = GetStdHandle(STD_OUTPUT_HANDLE)
      ok = SetConsoleOutputCP(65001)
      if (GetConsoleMode(hStdin, orig_in_mode) /= 0 .and. &
         GetConsoleMode(hStdout, orig_out_mode) /= 0) then
         in_mode = ior(iand(orig_in_mode, not(ior(ENABLE_LINE_INPUT, ior(ENABLE_ECHO_INPUT, ENABLE_PROCESSED_INPUT)))), &
            ENABLE_VIRTUAL_TERMINAL_INPUT)
         out_mode = ior(orig_out_mode, ENABLE_VIRTUAL_TERMINAL_PROCESSING)
         ok = SetConsoleMode(hStdin, in_mode)
         ok = SetConsoleMode(hStdout, out_mode)
         raw_enabled = .true.
      end if
#else
      integer(c_int64_t) :: raw_termios(16)
      integer(c_int) :: ok

      if (raw_enabled) return
      if (isatty(STDIN_FD) == 0) return
      if (tcgetattr(STDIN_FD, orig_termios) == 0) then
         raw_termios = orig_termios
         call cfmakeraw(raw_termios)
         ok = tcsetattr(STDIN_FD, TCSANOW, raw_termios)
         if (ok == 0) raw_enabled = .true.
      end if
#endif
   end subroutine enable_raw_mode
   subroutine disable_raw_mode()
#ifdef _WIN32
      integer(c_int) :: ok
      if (.not. raw_enabled) return
      ok = SetConsoleMode(hStdin, orig_in_mode)
      ok = SetConsoleMode(hStdout, orig_out_mode)
      raw_enabled = .false.
#else
      integer(c_int) :: ok
      if (.not. raw_enabled) return
      ok = tcsetattr(STDIN_FD, TCSANOW, orig_termios)
      raw_enabled = .false.
#endif
   end subroutine disable_raw_mode
   logical function read_byte_timeout(ch, timeout_ms)
      character(len=1), intent(out) :: ch
      integer, intent(in) :: timeout_ms
#ifdef _WIN32
      integer(c_int) :: wait_rc, nread
      character(kind=c_char) :: buf(1)
      ch = achar(0)
      read_byte_timeout = .false.
      if (.not. raw_enabled) return
      wait_rc = WaitForSingleObject(hStdin, int(timeout_ms, c_int))
      if (wait_rc == 0) then
         if (ReadFile(hStdin, buf, 1_c_int, nread, c_null_ptr) /= 0 .and. nread > 0) then
            ch = buf(1)
            read_byte_timeout = .true.
         end if
      end if
#else
      type(pollfd_t) :: pfd(1)
      character(kind=c_char) :: buf(1)
      integer(c_int) :: prc
      ch = achar(0)
      read_byte_timeout = .false.
      if (.not. raw_enabled) return
      pfd(1)%fd = STDIN_FD
      pfd(1)%events = POLLIN
      pfd(1)%revents = 0_c_short
      prc = c_poll(pfd, 1_c_long, int(timeout_ms, c_int))
      if (prc > 0 .and. iand(pfd(1)%revents, POLLIN) /= 0) then
         if (c_read(STDIN_FD, buf, 1_c_size_t) > 0_c_intptr_t) then
            ch = buf(1)
            read_byte_timeout = .true.
         end if
      end if
#endif
   end function read_byte_timeout

! -------------------------------------------------------------------------
! Terminal Capability & Size Detection
! -------------------------------------------------------------------------
   subroutine detect_terminal_capabilities()
      character(len=64) :: env_val, resp
      character(len=1) :: ch
      integer :: stat, i, n
      logical :: has_sixel
        call get_environment_variable("TERM_PROGRAM", env_val, status=stat)
        if (stat == 0 .and. trim(env_val) == "mintty") then
           render_mode = MODE_SIXEL
           return
        end if
      render_mode = MODE_BRAILLE
      call get_environment_variable("M_DRAW_SIXEL_MODE", env_val, status=stat)
      if (stat == 0) then
         select case (trim(adjustl(env_val)))
          case ("sixel", "SIXEL", "1")
            render_mode = MODE_SIXEL
            return
          case ("braille", "BRAILLE", "ansi", "ANSI", "0")
            render_mode = MODE_BRAILLE
            return
         end select
      end if
      if (.not. raw_enabled) return
! Query Primary Device Attributes (DA1): ESC [ c
      write(output_unit, '(A)', advance='no') ESC // '[c'
      flush(output_unit)
      resp = ' '
      n = 0
      do i = 1, len(resp)
         if (.not. read_byte_timeout(ch, 60)) exit
         n = n + 1
         resp(n:n) = ch
         if (ch == 'c') exit
      end do
! DA1 response containing ";4;" or ";4c" indicates Sixel graphics support
      has_sixel = (index(resp(1:n), ';4;') > 0 .or. index(resp(1:n), ';4c') > 0)
      if (has_sixel) render_mode = MODE_SIXEL
   end subroutine detect_terminal_capabilities

   subroutine query_terminal_size()
      integer :: prefx, prefy, prefxs, prefys
#ifdef _WIN32
      type(CONSOLE_SCREEN_BUFFER_INFO) :: csbi
      term_cols = 80
      term_rows = 24
      if (raw_enabled) then
         if (GetConsoleScreenBufferInfo(hStdout, csbi) /= 0) then
            term_cols = max(20, int(csbi%srWindow%Right - csbi%srWindow%Left + 1))
            term_rows = max(10, int(csbi%srWindow%Bottom - csbi%srWindow%Top + 1))
         end if
      end if
      cell_px_w = 10
      cell_px_h = 20
#else
      type(winsize_t) :: ws
! Try common TIOCGWINSZ ioctl numbers (Linux x86_64: 0x5413, macOS/BSD: 0x40087468, Cygwin: 0x5401)
      integer(c_long), parameter :: TIOCGWINSZ_LINUX = int(z'5413', c_long)
      integer(c_long), parameter :: TIOCGWINSZ_MAC = int(z'40087468', c_long)
      integer(c_long), parameter :: TIOCGWINSZ_CYGWIN = int(z'5401', c_long)
      integer(c_int) :: rc
      term_cols = 80
      term_rows = 24
      cell_px_w = 10
      cell_px_h = 20
      ws = winsize_t(0_c_short, 0_c_short, 0_c_short, 0_c_short)
      rc = c_ioctl_winsz(STDOUT_FD, TIOCGWINSZ_LINUX, ws)
      if (rc /= 0) rc = c_ioctl_winsz(STDOUT_FD, TIOCGWINSZ_MAC, ws)
      if (rc /= 0) rc = c_ioctl_winsz(STDOUT_FD, TIOCGWINSZ_CYGWIN, ws)
      if (rc == 0 .and. ws%ws_col > 0 .and. ws%ws_row > 0) then
         term_cols = int(ws%ws_col)
         term_rows = int(ws%ws_row)
         if (ws%ws_xpixel > 0 .and. ws%ws_ypixel > 0) then
            cell_px_w = max(4, int(ws%ws_xpixel) / term_cols)
            cell_px_h = max(8, int(ws%ws_ypixel) / term_rows)
         end if
      end if
#endif
      prefx = -1; prefy = -1; prefxs = -1; prefys = -1
      call draw_getprefposandsize(prefx, prefy, prefxs, prefys)

      if (render_mode == MODE_SIXEL) then
         if (prefxs > 0) then
            pix_w = prefxs
            term_cols = max(10, pix_w / cell_px_w)
         else
            pix_w = term_cols * cell_px_w
         end if
         if (prefys > 0) then
            pix_h = prefys
            term_rows = max(5, pix_h / cell_px_h)
         else
            pix_h = term_rows * cell_px_h
         end if
      else
         if (prefxs > 0) term_cols = max(10, prefxs / 2)
         if (prefys > 0) term_rows = max(5, prefys / 4)
         pix_w = term_cols * 2
         pix_h = term_rows * 4
      end if
   end subroutine query_terminal_size
! -------------------------------------------------------------------------
! Pixel & Line Rasterization (Shared by Braille and Sixel Modes)
! -------------------------------------------------------------------------
   subroutine set_pixel(px, py)
      integer, intent(in) :: px, py
      integer :: cx, cy, dx, dy, bit
      if (px < 0 .or. px >= pix_w .or. py < 0 .or. py >= pix_h) return
      if (render_mode == MODE_SIXEL) then
         if (back_used) then
            back_sixel(px + 1, py + 1) = int(cur_color, 1)
         else
            sixel_fb(px + 1, py + 1) = int(cur_color, 1)
         end if
      else
         cx = px / 2 + 1
         cy = py / 4 + 1
         dx = mod(px, 2)
         dy = mod(py, 4)
         bit = BRAILLE_BIT(dx, dy)
         if (back_used) then
            back_mask(cx, cy) = int(ior(int(back_mask(cx, cy)), bit), 1)
            back_fg(cx, cy) = cur_color
         else
            dot_mask(cx, cy) = int(ior(int(dot_mask(cx, cy)), bit), 1)
            cell_fg(cx, cy) = cur_color
         end if
      end if
   end subroutine set_pixel
   subroutine draw_line_bresenham(x0_in, y0_in, x1_in, y1_in)
      integer, intent(in) :: x0_in, y0_in, x1_in, y1_in
      integer :: x0, y0, x1, y1, dx, dy, sx, sy, err, e2
      x0 = x0_in; y0 = y0_in; x1 = x1_in; y1 = y1_in
      dx = abs(x1 - x0)
      sx = merge(1, -1, x0 < x1)
      dy = -abs(y1 - y0)
      sy = merge(1, -1, y0 < y1)
      err = dx + dy
      do
         call set_pixel(x0, y0)
         if (x0 == x1 .and. y0 == y1) exit
         e2 = 2 * err
         if (e2 >= dy) then
            err = err + dy
            x0 = x0 + sx
         end if
         if (e2 <= dx) then
            err = err + dx
            y0 = y0 + sy
         end if
      end do
   end subroutine draw_line_bresenham
! -------------------------------------------------------------------------
! Sixel & UTF-8 Braille Output Encoders
! -------------------------------------------------------------------------
   subroutine utf8_braille(mask, br, nb)
      integer, intent(in) :: mask
      character(len=3), intent(out) :: br
      integer, intent(out) :: nb
      integer :: cp
! Unicode Braille Patterns block starts at U+2800 (10240)
      cp = 10240 + iand(mask, 255)
      br(1:1) = achar(224 + cp / 4096)
      br(2:2) = achar(128 + mod(cp / 64, 64))
      br(3:3) = achar(128 + mod(cp, 64))
      nb = 3
   end subroutine utf8_braille

   subroutine flush_sixel_frame()
      integer :: y0, b, x, idx, ci, r_pct, g_pct, b_pct, run_len, pos, n_active
      integer :: row_bits(pix_w), active_colors(CMAPSIZE)
      logical :: color_used(0:CMAPSIZE-1), first_pass
      character(len=64) :: hdr
      character(len=1) :: s_ch, prev_ch
      character(len=:), allocatable :: frame_buf

      pos = 0
      write(hdr, '(A,I0,A,I0,A)') ESC // '[H' // ESC // 'P0;1;0q"1;1;', pix_w, ';', pix_h
      call append_str(frame_buf, pos, trim(hdr))

      color_used = .false.
      do y0 = 1, pix_h
         do x = 1, pix_w
            ci = iand(int(sixel_fb(x, y0)), 255)
            color_used(ci) = .true.
         end do
      end do

      n_active = 0
      do ci = 0, CMAPSIZE - 1
         if (color_used(ci)) then
            n_active = n_active + 1
            active_colors(n_active) = ci
            r_pct = nint(carray(ci + 1)%r * 100.0 / 255.0)
            g_pct = nint(carray(ci + 1)%g * 100.0 / 255.0)
            b_pct = nint(carray(ci + 1)%b * 100.0 / 255.0)
            write(hdr, '(A,I0,A,I0,A,I0,A,I0)') '#', ci, ';2;', r_pct, ';', g_pct, ';', b_pct
            call append_str(frame_buf, pos, trim(hdr))
         end if
      end do

      do y0 = 1, pix_h, 6
         first_pass = .true.
         do idx = 1, n_active
            ci = active_colors(idx)
            row_bits = 0
            do b = 0, 5
               if (y0 + b <= pix_h) then
                  do x = 1, pix_w
                     if (iand(int(sixel_fb(x, y0 + b)), 255) == ci) then
                        row_bits(x) = ior(row_bits(x), ishft(1, b))
                     end if
                  end do
               end if
            end do

            if (all(row_bits == 0)) cycle

            if (.not. first_pass) call append_str(frame_buf, pos, '$')
            first_pass = .false.
            write(hdr, '(A,I0)') '#', ci
            call append_str(frame_buf, pos, trim(hdr))

            prev_ch = achar(63 + row_bits(1))
            run_len = 1
            do x = 2, pix_w
               s_ch = achar(63 + row_bits(x))
               if (s_ch == prev_ch) then
                  run_len = run_len + 1
               else
                  call append_sixel_run(frame_buf, pos, prev_ch, run_len)
                  prev_ch = s_ch
                  run_len = 1
               end if
            end do
            call append_sixel_run(frame_buf, pos, prev_ch, run_len)
         end do
         if (y0 + 6 <= pix_h) call append_str(frame_buf, pos, '-')
      end do

      call append_str(frame_buf, pos, ESC // '\')
      if (pos > 0) write(output_unit, '(A)', advance='no') frame_buf(1:pos)
   end subroutine flush_sixel_frame

   subroutine emit_sixel_run(ch, count)
      character(len=1), intent(in) :: ch
      integer, intent(in) :: count
      character(len=24) :: rle
      if (count <= 3) then
         write(output_unit, '(A)', advance='no') repeat(ch, count)
      else
         write(rle, '(A,I0,A)') '!', count, ch
         write(output_unit, '(A)', advance='no') trim(rle)
      end if
   end subroutine emit_sixel_run
! -------------------------------------------------------------------------
! M_draw Driver Entry Points
! -------------------------------------------------------------------------
   integer function SIXEL_init()
      integer :: idum
      call enable_raw_mode()
      call detect_terminal_capabilities()
      call query_terminal_size()
      vdevice%depth = 24
      vdevice%sizeSx = pix_w
      vdevice%sizeSy = pix_h
      vdevice%sizeX = min(pix_w, pix_h)
      vdevice%sizeY = vdevice%sizeX
      vdevice%hheight = real(merge(cell_px_h, 4, render_mode == MODE_SIXEL), real32)
      vdevice%hwidth = real(merge(cell_px_w, 2, render_mode == MODE_SIXEL), real32)
      if (allocated(dot_mask)) deallocate(dot_mask, cell_fg, text_ovl)
      if (allocated(sixel_fb)) deallocate(sixel_fb)
      allocate(dot_mask(term_cols, term_rows), source=0_1)
      allocate(cell_fg(term_cols, term_rows), source=7)
      allocate(text_ovl(term_cols, term_rows), source=' ')
      if (render_mode == MODE_SIXEL) then
         allocate(sixel_fb(pix_w, pix_h), source=0_1)
      end if
      if (GLOBAL_device == DEV_SIXEL_UPPER) then
         idum = SIXEL_mapcolor(0, 0, 0, 0)
         idum = SIXEL_mapcolor(7, 255, 255, 255)
      else
         idum = SIXEL_mapcolor(7, 0, 0, 0)
         idum = SIXEL_mapcolor(0, 255, 255, 255)
      end if
      idum = SIXEL_mapcolor(1, 255, 60, 60)
      idum = SIXEL_mapcolor(2, 60, 255, 60)
      idum = SIXEL_mapcolor(3, 255, 255, 60)
      idum = SIXEL_mapcolor(4, 80, 140, 255)
      idum = SIXEL_mapcolor(5, 255, 80, 255)
      idum = SIXEL_mapcolor(6, 60, 255, 255)
      bg_color = 0
      cur_color = 7
      back_used = .false.
      write(output_unit, '(A)', advance='no') ESC // '[?25l' // ESC // '[2J' // ESC // '[?1000;1006h'
      flush(output_unit)
      SIXEL_init = 1
   end function SIXEL_init

   integer function SIXEL_exit()
      integer :: idum
      idum = SIXEL_sync()
      write(output_unit, '(A,I0,A)') ESC // '[?1000;1006l' // ESC // '[0m' // ESC // '[?25h' // ESC // '[', term_rows + 1, ';1H'
      flush(output_unit)
      call disable_raw_mode()
      if (allocated(dot_mask)) deallocate(dot_mask, cell_fg, text_ovl)
      if (allocated(sixel_fb)) deallocate(sixel_fb)
      if (allocated(back_mask)) deallocate(back_mask, back_fg, back_ovl)
      if (allocated(back_sixel)) deallocate(back_sixel)
      SIXEL_exit = 1
   end function SIXEL_exit

   integer function SIXEL_draw(x, y)
      integer :: idum
      integer, intent(in) :: x, y
      call draw_line_bresenham(vdevice%cpVx, vdevice%sizeSy - 1 - vdevice%cpVy, &
         x, vdevice%sizeSy - 1 - y)
      if (vdevice%sync /= 0 .and. .not. back_used) idum = SIXEL_sync()
      SIXEL_draw = UNUSED
   end function SIXEL_draw

   integer function SIXEL_clear()
      integer :: idum
      bg_color = cur_color
      if (back_used) then
         if (render_mode == MODE_SIXEL) then
            back_sixel = int(cur_color, 1)
         else
            back_mask = 0_1; back_fg = cur_color
         end if
         back_ovl = ' '
      else
         if (render_mode == MODE_SIXEL) then
            sixel_fb = int(cur_color, 1)
         else
            dot_mask = 0_1; cell_fg = cur_color
         end if
         text_ovl = ' '
         idum = SIXEL_sync()
      end if
      SIXEL_clear = UNUSED
   end function SIXEL_clear

   integer function SIXEL_color(ind_in)
      integer, intent(in) :: ind_in
      if (ind_in >= 0) cur_color = mod(ind_in, CMAPSIZE)
      SIXEL_color = UNUSED
   end function SIXEL_color

   integer function SIXEL_setlw(ind_in)
      integer, intent(in) :: ind_in
      SIXEL_setlw = UNUSED
   end function SIXEL_setlw

   integer function SIXEL_mapcolor(i, r, g, b)
      integer, intent(in) :: i, r, g, b
      if (i < 0 .or. i >= CMAPSIZE) then
         SIXEL_mapcolor = -1; return
      end if
      carray(i + 1) = rgb_t(r, g, b)
      SIXEL_mapcolor = 0
   end function SIXEL_mapcolor

   integer function SIXEL_font(fontfile)
      character(len=*), intent(in) :: fontfile
      SIXEL_font = 1
   end function SIXEL_font

   integer function SIXEL_char(c)
      character(len=1), intent(in) :: c
      integer :: cx, cy, idum
      cx = vdevice%cpVx / merge(cell_px_w, 2, render_mode == MODE_SIXEL) + 1
      cy = (vdevice%sizeSy - 1 - vdevice%cpVy) / merge(cell_px_h, 4, render_mode == MODE_SIXEL) + 1
      if (cx >= 1 .and. cx <= term_cols .and. cy >= 1 .and. cy <= term_rows) then
         if (back_used) then
            back_ovl(cx, cy) = c; back_fg(cx, cy) = cur_color
         else
            text_ovl(cx, cy) = c; cell_fg(cx, cy) = cur_color
         end if
      end if
      if (vdevice%sync /= 0 .and. .not. back_used) idum = SIXEL_sync()
      SIXEL_char = UNUSED
   end function SIXEL_char

   integer function SIXEL_string(s)
      character(len=*), intent(in) :: s
      integer :: i, cx, cy, cw, ch, idum
      cw = merge(cell_px_w, 2, render_mode == MODE_SIXEL)
      ch = merge(cell_px_h, 4, render_mode == MODE_SIXEL)
      cy = (vdevice%sizeSy - 1 - vdevice%cpVy) / ch + 1
      do i = 1, len(s)
         cx = vdevice%cpVx / cw + i
         if (cx >= 1 .and. cx <= term_cols .and. cy >= 1 .and. cy <= term_rows) then
            if (back_used) then
               back_ovl(cx, cy) = s(i:i); back_fg(cx, cy) = cur_color
            else
               text_ovl(cx, cy) = s(i:i); cell_fg(cx, cy) = cur_color
            end if
         end if
      end do
      if (vdevice%sync /= 0 .and. .not. back_used) idum = SIXEL_sync()
      SIXEL_string = UNUSED
   end function SIXEL_string

   integer function SIXEL_fill(n, x, y)
      integer, intent(in) :: n
      integer, intent(in) :: x(n), y(n)
      integer :: py(n), ymin, ymax, scan_y, i, j, nodes, node_x(n), tmp, px, idum
      character(len=128) :: buf
      if (n > MAXVERTS) then
         write(buf, '(A, I0, A)') "M_draw: more than ", MAXVERTS, " points in a polygon"
         call draw_verror(buf)
      end if
      do i = 1, n
         py(i) = vdevice%sizeSy - 1 - y(i)
      end do
      ymin = max(0, minval(py))
      ymax = min(vdevice%sizeSy - 1, maxval(py))
      do scan_y = ymin, ymax
         nodes = 0; j = n
         do i = 1, n
            if ((py(i) < scan_y .and. py(j) >= scan_y) .or. (py(j) < scan_y .and. py(i) >= scan_y)) then
               nodes = nodes + 1
               node_x(nodes) = x(i) + nint(real(scan_y - py(i)) / real(py(j) - py(i)) * real(x(j) - x(i)))
            end if
            j = i
         end do
         do i = 1, nodes - 1
            do j = i + 1, nodes
               if (node_x(i) > node_x(j)) then
                  tmp = node_x(i); node_x(i) = node_x(j); node_x(j) = tmp
               end if
            end do
         end do
         do i = 1, nodes - 1, 2
            do px = max(0, node_x(i)), min(pix_w - 1, node_x(i + 1))
               call set_pixel(px, scan_y)
            end do
         end do
      end do
      if (vdevice%sync /= 0 .and. .not. back_used) idum = SIXEL_sync()
      SIXEL_fill = UNUSED
   end function SIXEL_fill

   integer function SIXEL_backbuf()
      if (.not. allocated(back_mask)) then
         allocate(back_mask(term_cols, term_rows), source=0_1)
         allocate(back_fg(term_cols, term_rows), source=7)
         allocate(back_ovl(term_cols, term_rows), source=' ')
      end if
      back_mask = dot_mask
      back_fg = cell_fg
      back_ovl = text_ovl
      if (render_mode == MODE_SIXEL) then
         if (.not. allocated(back_sixel)) allocate(back_sixel(pix_w, pix_h), source=0_1)
         back_sixel = sixel_fb
      end if
      back_used = .true.
      SIXEL_backbuf = 1
   end function SIXEL_backbuf

   integer function SIXEL_frontbuf()
      back_used = .false.
      SIXEL_frontbuf = UNUSED
   end function SIXEL_frontbuf

   integer function SIXEL_swapbuf()
      integer :: idum
      if (back_used) then
         dot_mask = back_mask
         cell_fg = back_fg
         text_ovl = back_ovl
         if (render_mode == MODE_SIXEL .and. allocated(back_sixel)) sixel_fb = back_sixel
         idum = SIXEL_sync()
      end if
      SIXEL_swapbuf = UNUSED
   end function SIXEL_swapbuf

   integer function SIXEL_checkkey()
      character(len=1) :: ch
      if (read_byte_timeout(ch, 0)) then
         SIXEL_checkkey = ichar(ch)
      else
         SIXEL_checkkey = 0
      end if
   end function SIXEL_checkkey

   integer function SIXEL_getkey()
      character(len=1) :: ch
      integer :: btn, cx, cy, cw, ch_h, idum
      cw = merge(cell_px_w, 2, render_mode == MODE_SIXEL)
      ch_h = merge(cell_px_h, 4, render_mode == MODE_SIXEL)
      do
         if (.not. read_byte_timeout(ch, 50)) cycle
         ! Allow immediate exit on Ctrl+C (3) or Ctrl+D (4) in raw mode
         if (ichar(ch) == 3 .or. ichar(ch) == 4) then
            idum = SIXEL_exit()
            stop
         end if
         if (ch == ESC) then
            if (read_byte_timeout(ch, 20) .and. ch == '[') then
               if (read_byte_timeout(ch, 20) .and. ch == '<') then
                  call parse_sgr_mouse(btn, cx, cy)
                  last_mx = min(pix_w - 1, max(0, (cx - 1) * cw + cw / 2))
                  last_my = min(pix_h - 1, max(0, pix_h - 1 - ((cy - 1) * ch_h + ch_h / 2)))
                  last_mbtn = btn
                  cycle
               end if
            end if
            SIXEL_getkey = 27
            return
         end if
         SIXEL_getkey = ichar(ch)
         return
      end do
   end function SIXEL_getkey

   subroutine parse_sgr_mouse(btn, cx, cy)
      integer, intent(out) :: btn, cx, cy
      character(len=1) :: ch
      integer :: vals(3), idx
      vals = [0, 1, 1]
      idx = 1
      do
         if (.not. read_byte_timeout(ch, 20)) exit
         if (ch >= '0' .and. ch <= '9') then
            vals(idx) = vals(idx) * 10 + (ichar(ch) - ichar('0'))
         else if (ch == ';') then
            idx = min(3, idx + 1)
         else if (ch == 'M') then
            btn = vals(1) + 1
            exit
         else if (ch == 'm') then
            btn = 0
            exit
         else
            exit
         end if
      end do
      cx = vals(2)
      cy = vals(3)
   end subroutine parse_sgr_mouse

   integer function SIXEL_locator(xaddr, yaddr)
      integer, intent(out) :: xaddr, yaddr
      character(len=1) :: ch
      integer :: btn, cx, cy, cw, ch_h
      cw = merge(cell_px_w, 2, render_mode == MODE_SIXEL)
      ch_h = merge(cell_px_h, 4, render_mode == MODE_SIXEL)
      do while (read_byte_timeout(ch, 0))
         if (ch == ESC) then
            if (read_byte_timeout(ch, 10) .and. ch == '[') then
               if (read_byte_timeout(ch, 10) .and. ch == '<') then
                  call parse_sgr_mouse(btn, cx, cy)
                  last_mx = min(pix_w - 1, max(0, (cx - 1) * cw + cw / 2))
                  last_my = min(pix_h - 1, max(0, pix_h - 1 - ((cy - 1) * ch_h + ch_h / 2)))
                  last_mbtn = btn
               end if
            end if
         end if
      end do
      xaddr = last_mx
      yaddr = last_my
      SIXEL_locator = last_mbtn
   end function SIXEL_locator

   integer function SIXEL_sync()
      integer :: r, c, fg, last_fg, nb
      character(len=3) :: br
      character(len=64) :: esc_seq
      type(rgb_t) :: bgc, fgc
      if (render_mode == MODE_SIXEL .and. allocated(sixel_fb)) then
         call flush_sixel_frame()
! Overlay crisp text labels on top of Sixel canvas
         do r = 1, term_rows
            do c = 1, term_cols
               if (text_ovl(c, r) /= ' ') then
                  fgc = carray(cell_fg(c, r) + 1)
                  write(esc_seq, '(A,I0,A,I0,A,I0,A,I0,A,I0,A)') ESC // '[', r, ';', c, 'H' // &
                     ESC // '[38;2;', fgc%r, ';', fgc%g, ';', fgc%b, 'm'
                  write(output_unit, '(A)', advance='no') trim(esc_seq) // text_ovl(c, r)
               end if
            end do
         end do
         flush(output_unit)
         SIXEL_sync = UNUSED
         return
      end if
      if (.not. allocated(dot_mask)) then
         SIXEL_sync = UNUSED; return
      end if
      bgc = carray(bg_color + 1)
      write(esc_seq, '(A,I0,A,I0,A,I0,A)') ESC // '[H' // ESC // '[48;2;', bgc%r, ';', bgc%g, ';', bgc%b, 'm'
      write(output_unit, '(A)', advance='no') trim(esc_seq)
      last_fg = -1
      do r = 1, term_rows
         if (r > 1) write(output_unit, '(A)', advance='no') achar(13) // achar(10)
         do c = 1, term_cols
            fg = cell_fg(c, r)
            if (fg /= last_fg) then
               fgc = carray(fg + 1)
               write(esc_seq, '(A,I0,A,I0,A,I0,A)') ESC // '[38;2;', fgc%r, ';', fgc%g, ';', fgc%b, 'm'
               write(output_unit, '(A)', advance='no') trim(esc_seq)
               last_fg = fg
            end if
            if (text_ovl(c, r) /= ' ') then
               write(output_unit, '(A)', advance='no') text_ovl(c, r)
            else
               call utf8_braille(int(dot_mask(c, r)), br, nb)
               write(output_unit, '(A)', advance='no') br(1:nb)
            end if
         end do
      end do
      flush(output_unit)
      SIXEL_sync = UNUSED
   end function SIXEL_sync

   subroutine setup_sixeldev()
      vdevice%dev%large = "large"
      vdevice%dev%small = "small"
      vdevice%dev%Vbackb => SIXEL_backbuf
      vdevice%dev%Vchar => SIXEL_char
      vdevice%dev%Vcheckkey => SIXEL_checkkey
      vdevice%dev%Vclear => SIXEL_clear
      vdevice%dev%Vcolor => SIXEL_color
      vdevice%dev%Vdraw => SIXEL_draw
      vdevice%dev%Vexit => SIXEL_exit
      vdevice%dev%Vfill => SIXEL_fill
      vdevice%dev%Vfont => SIXEL_font
      vdevice%dev%Vfrontb => SIXEL_frontbuf
      vdevice%dev%Vgetkey => SIXEL_getkey
      vdevice%dev%Vinit => SIXEL_init
      vdevice%dev%Vlocator => SIXEL_locator
      vdevice%dev%Vmapcolor => SIXEL_mapcolor
      vdevice%dev%Vsetlw => SIXEL_setlw
      vdevice%dev%Vstring => SIXEL_string
      vdevice%dev%Vswapb => SIXEL_swapbuf
      vdevice%dev%Vsync => SIXEL_sync
   end subroutine setup_sixeldev

   integer function SIXEL_draw_devcpy()
      call setup_sixeldev()
      vdevice%dev%devname = "SIXEL"
      GLOBAL_device = DEV_SIXEL_UPPER
      SIXEL_draw_devcpy = UNUSED
   end function SIXEL_draw_devcpy


   integer function sixel_low_draw_devcpy()
      call setup_sixeldev()
      vdevice%dev%devname = "sixel"
      GLOBAL_device = DEV_SIXEL_LOWER
      sixel_low_draw_devcpy = UNUSED
   end function sixel_low_draw_devcpy

   subroutine append_str(buf, pos, str)
      character(len=:), allocatable, intent(inout) :: buf
      integer, intent(inout) :: pos
      character(len=*), intent(in) :: str
      character(len=:), allocatable :: tmp
      integer :: n, need
      n = len(str)
      if (n == 0) return
      need = pos + n
      if (.not. allocated(buf)) then
         allocate(character(len=max(65536, need * 2)) :: buf)
      else if (need > len(buf)) then
         allocate(character(len=max(len(buf) * 2, need * 2)) :: tmp)
         tmp(1:pos) = buf(1:pos)
         call move_alloc(tmp, buf)
      end if
      buf(pos + 1:need) = str
      pos = need
   end subroutine append_str

   subroutine append_sixel_run(buf, pos, ch, count)
      character(len=:), allocatable, intent(inout) :: buf
      integer, intent(inout) :: pos
      character(len=1), intent(in) :: ch
      integer, intent(in) :: count
      character(len=24) :: rle
      if (count <= 3) then
         call append_str(buf, pos, repeat(ch, count))
      else
         write(rle, '(A,I0,A)') '!', count, ch
         call append_str(buf, pos, trim(rle))
      end if
   end subroutine append_sixel_run

end module m_draw_drv_sixel
