! Core library external bridges (resolves cyclic module interface blocks)

function draw_newtokens(num) result(toks)
   use m_draw_types, only: Token
   use m_draw_newtokens, only: mod_newtokens => draw_newtokens
   integer, intent(in) :: num
   type(Token), pointer :: toks(:)
   toks => mod_newtokens(num)
end function draw_newtokens

subroutine draw_makepoly()
   use m_draw_polygons, only: mod_makepoly => draw_makepoly
   call mod_makepoly()
end subroutine draw_makepoly

subroutine draw_closepoly()
   use m_draw_polygons, only: mod_closepoly => draw_closepoly
   call mod_closepoly()
end subroutine draw_closepoly

subroutine draw_font(name)
   use m_draw_text, only: mod_font => draw_font
   character(len=*), intent(in) :: name
   call mod_font(name)
end subroutine draw_font

subroutine draw_color(i)
   use m_draw_drivers, only: mod_color => draw_color
   integer, intent(in) :: i
   call mod_color(i)
end subroutine draw_color

subroutine draw_textsize(width, height)
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_text, only: mod_textsize => draw_textsize
   real(real32), intent(in) :: width, height
   call mod_textsize(width, height)
end subroutine draw_textsize

subroutine draw_linewidth(w)
   use m_draw_drivers, only: mod_linewidth => draw_linewidth
   integer, intent(in) :: w
   call mod_linewidth(w)
end subroutine draw_linewidth

subroutine draw_mapcolor(i, r, g, b)
   use, intrinsic :: iso_fortran_env, only: int16
   use m_draw_drivers, only: mod_mapcolor => draw_mapcolor
   integer, intent(in) :: i
   integer(int16), intent(in) :: r, g, b
   call mod_mapcolor(i, r, g, b)
end subroutine draw_mapcolor

integer function draw_getkey()
   use m_draw_drivers, only: mod_getkey => draw_getkey
   draw_getkey = mod_getkey()
end function draw_getkey

! Driver registration external bridges
integer function drv_NIL_devcpy()
       use m_draw_drv_nil
      drv_NIL_devcpy       = NIL_draw_devcpy()
       end function
integer function drv_VOG_devcpy()
       use m_draw_drv_vog
      drv_VOG_devcpy       = VOG_draw_devcpy()
       end function
integer function drv_FTI_devcpy()
       use m_draw_drv_fti
      drv_FTI_devcpy       = FTI_draw_devcpy()
       end function
integer function drv_CGMT_devcpy()
      use m_draw_drv_cgmt
     drv_CGMT_devcpy      = CGMT_draw_devcpy()
      end function
integer function drv_GNU_devcpy()
       use m_draw_drv_gnu
      drv_GNU_devcpy       = GNU_draw_devcpy()
       end function
integer function drv_XFIG_devcpy()
      use m_draw_drv_xfig
     drv_XFIG_devcpy      = XFIG_draw_devcpy()
      end function
integer function drv_PS_devcpy()
        use m_draw_drv_ps
       drv_PS_devcpy        = PS_draw_devcpy()
        end function
integer function drv_PSP_devcpy()
       use m_draw_drv_ps
       drv_PSP_devcpy       = PSP_draw_devcpy()
       end function
integer function drv_PSC_devcpy()
       use m_draw_drv_ps
       drv_PSC_devcpy       = PSC_draw_devcpy()
       end function
integer function drv_PPSC_devcpy()
      use m_draw_drv_ps
       drv_PPSC_devcpy      = PPSC_draw_devcpy()
      end function
integer function drv_PSG_devcpy()
       use m_draw_drv_ps
       drv_PSG_devcpy       = PSG_draw_devcpy()
       end function
integer function drv_PPSG_devcpy()
      use m_draw_drv_ps
       drv_PPSG_devcpy      = PPSG_draw_devcpy()
      end function
integer function drv_SVG_devcpy()
       use m_draw_drv_svg
      drv_SVG_devcpy       = SVG_draw_devcpy()
       end function
integer function drv_PDF_devcpy()
       use m_draw_drv_pdf
      drv_PDF_devcpy       = PDF_draw_devcpy()
       end function
integer function drv_HPGL_LAND_devcpy()
 use m_draw_drv_hpdxy
    drv_HPGL_LAND_devcpy = HPGL_LAND_draw_devcpy()
 end function
integer function drv_HPGL_PORT_devcpy()
 use m_draw_drv_hpdxy
    drv_HPGL_PORT_devcpy = HPGL_PORT_draw_devcpy()
 end function
integer function drv_PCL_LAND_devcpy()
  use m_draw_drv_hpdxy
    drv_PCL_LAND_devcpy  = PCL_LAND_draw_devcpy()
  end function
integer function drv_PCL_PORT_devcpy()
  use m_draw_drv_hpdxy
    drv_PCL_PORT_devcpy  = PCL_PORT_draw_devcpy()
  end function
integer function drv_DXY_devcpy()
       use m_draw_drv_hpdxy
    drv_DXY_devcpy       = DXY_draw_devcpy()
       end function
integer function drv_PCL5_LAND_devcpy()
 use m_draw_drv_pcl5
     drv_PCL5_LAND_devcpy = PCL5_LAND_draw_devcpy()
 end function
integer function drv_PCL5_PORT_devcpy()
 use m_draw_drv_pcl5
     drv_PCL5_PORT_devcpy = PCL5_PORT_draw_devcpy()
 end function
integer function drv_HPGL2_LAND_devcpy()
use m_draw_drv_pcl5
     drv_HPGL2_LAND_devcpy= HPGL2_LAND_draw_devcpy()
end function
integer function drv_HPGL2_PORT_devcpy()
use m_draw_drv_pcl5
     drv_HPGL2_PORT_devcpy= HPGL2_PORT_draw_devcpy()
end function
integer function drv_CANVAS_devcpy()
    use m_draw_drv_canvas
   drv_CANVAS_devcpy    = CANVAS_draw_devcpy()
    end function
integer function drv_MIF_devcpy()
       use m_draw_drv_mif
      drv_MIF_devcpy       = MIF_draw_devcpy()
       end function
integer function drv_MIF4_devcpy()
      use m_draw_drv_mif
      drv_MIF4_devcpy      = MIF4_draw_devcpy()
      end function
integer function drv_RTF_devcpy()
       use m_draw_drv_rtf
      drv_RTF_devcpy       = RTF_draw_devcpy()
       end function
integer function drv_VML_devcpy()
       use m_draw_drv_vml
      drv_VML_devcpy       = VML_draw_devcpy()
       end function
integer function drv_USEMAP_devcpy()
    use m_draw_drv_usemap
   drv_USEMAP_devcpy    = USEMAP_draw_devcpy()
    end function
integer function drv_PBM_devcpy()
       use m_draw_drv_pbm
      drv_PBM_devcpy       = PBM_draw_devcpy()
       end function
integer function drv_P1_devcpy()
        use m_draw_drv_pbm
      drv_P1_devcpy        = P1_draw_devcpy()
        end function
integer function drv_P4_devcpy()
        use m_draw_drv_pbm
      drv_P4_devcpy        = P4_draw_devcpy()
        end function
integer function drv_XBM_devcpy()
       use m_draw_drv_pbm
      drv_XBM_devcpy       = XBM_draw_devcpy()
       end function
integer function drv_BM_devcpy()
        use m_draw_drv_pbm
      drv_BM_devcpy        = BM_draw_devcpy()
        end function
integer function drv_PPM_devcpy()
       use m_draw_drv_ppm
      drv_PPM_devcpy       = PPM_draw_devcpy()
       end function
integer function drv_P3_devcpy()
        use m_draw_drv_ppm
      drv_P3_devcpy        = P3_draw_devcpy()
        end function
integer function drv_P6_devcpy()
        use m_draw_drv_ppm
      drv_P6_devcpy        = P6_draw_devcpy()
        end function
integer function drv_CHAR_devcpy()
      use m_draw_drv_ppm
      drv_CHAR_devcpy      = CHAR_draw_devcpy()
      end function
integer function drv_UNIXPLOT_devcpy()
  use m_draw_drv_unixplot
 drv_UNIXPLOT_devcpy  = UNIXPLOT_draw_devcpy()
  end function
integer function drv_TEK_devcpy()
       use m_draw_drv_tek
      drv_TEK_devcpy       = TEK_draw_devcpy()
       end function
integer function drv_XTEK_devcpy()
      use m_draw_drv_tek
      drv_XTEK_devcpy      = XTEK_draw_devcpy()
      end function
integer function drv_HPGT_devcpy()
      use m_draw_drv_hpgt
     drv_HPGT_devcpy      = HPGT_draw_devcpy()
      end function
integer function drv_X11_devcpy()
       use m_draw_drv_x11
      drv_X11_devcpy       = X11_draw_devcpy()
       end function
integer function drv_x11_low_devcpy()
   use m_draw_drv_x11
      drv_x11_low_devcpy   = x11_low_draw_devcpy()
   end function

subroutine draw_vexit()
   use m_draw_drivers, only: mod_vexit => draw_vexit
   call mod_vexit()
end subroutine draw_vexit

subroutine draw_dashline(p0, p1)
   use, intrinsic :: iso_fortran_env, only: real32
   use m_draw_draw, only: mod_dashline => draw_dashline
   real(real32), intent(inout) :: p0(4), p1(4)
   call mod_dashline(p0, p1)
end subroutine draw_dashline
