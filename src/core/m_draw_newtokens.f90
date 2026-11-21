module m_draw_newtokens
   use m_draw_types
   use m_draw_verror
   implicit none
   private
   public :: draw_newtokens

   type(TokList), pointer, save :: current => null()

contains

   function draw_newtokens(num) result(addr)
      integer, intent(in) :: num
      type(Token), pointer :: addr(:)
      type(TokList), pointer :: tl
      integer :: sz, istat, start_idx
      logical :: need_alloc

      need_alloc = .not. associated(vdevice%tokens)
      if (.not. need_alloc) then
         if (num > MAXTOKS - current%count) need_alloc = .true.
      end if

      if (need_alloc) then
         allocate(tl, stat=istat)
         if (istat /= 0) call draw_verror("newtokens: allocate returns error")

         if (associated(vdevice%tokens)) then
            current%next => tl
         else
            vdevice%tokens => tl
         end if

         tl%count = 0
         tl%next => null()
         sz = max(num, MAXTOKS)

         allocate(tl%toks(sz), stat=istat)
         if (istat /= 0) call draw_verror("newtokens: allocate returns error")

         current => tl
      end if

      start_idx = current%count + 1
      addr => current%toks(start_idx : start_idx + num - 1)
      current%count = current%count + num
   end function draw_newtokens

end module m_draw_newtokens
