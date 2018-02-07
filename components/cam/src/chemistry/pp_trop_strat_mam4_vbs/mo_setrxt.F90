
      module mo_setrxt

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: setrxt
      public :: setrxt_hrates

      contains

      subroutine setrxt( rate, temp, m, ncol )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      real(r8)  :: itemp(ncol*pver)
      real(r8)  :: exp_fac(ncol*pver)
      real(r8)  :: ko(ncol*pver)
      real(r8)  :: kinf(ncol*pver)

      rate(:,126) = 1.2e-10_r8
      rate(:,130) = 1.2e-10_r8
      rate(:,136) = 6.9e-12_r8
      rate(:,137) = 7.2e-11_r8
      rate(:,138) = 1.6e-12_r8
      rate(:,144) = 1.8e-12_r8
      rate(:,148) = 1.8e-12_r8
      rate(:,160) = 3.5e-12_r8
      rate(:,162) = 1e-11_r8
      rate(:,163) = 2.2e-11_r8
      rate(:,164) = 5e-11_r8
      rate(:,199) = 1.7e-13_r8
      rate(:,201) = 2.607e-10_r8
      rate(:,202) = 9.75e-11_r8
      rate(:,203) = 2.07e-10_r8
      rate(:,204) = 2.088e-10_r8
      rate(:,205) = 1.17e-10_r8
      rate(:,206) = 4.644e-11_r8
      rate(:,207) = 1.204e-10_r8
      rate(:,208) = 9.9e-11_r8
      rate(:,209) = 3.3e-12_r8
      rate(:,228) = 4.5e-11_r8
      rate(:,229) = 4.62e-10_r8
      rate(:,230) = 1.2e-10_r8
      rate(:,231) = 9e-11_r8
      rate(:,232) = 3e-11_r8
      rate(:,237) = 2.14e-11_r8
      rate(:,238) = 1.9e-10_r8
      rate(:,251) = 2.57e-10_r8
      rate(:,252) = 1.8e-10_r8
      rate(:,253) = 1.794e-10_r8
      rate(:,254) = 1.3e-10_r8
      rate(:,255) = 7.65e-11_r8
      rate(:,269) = 4e-13_r8
      rate(:,273) = 1.31e-10_r8
      rate(:,274) = 3.5e-11_r8
      rate(:,275) = 9e-12_r8
      rate(:,282) = 6.8e-14_r8
      rate(:,283) = 2e-13_r8
      rate(:,297) = 7e-13_r8
      rate(:,298) = 1e-12_r8
      rate(:,302) = 1e-14_r8
      rate(:,303) = 1e-11_r8
      rate(:,304) = 1.15e-11_r8
      rate(:,305) = 4e-14_r8
      rate(:,318) = 3e-12_r8
      rate(:,319) = 6.7e-13_r8
      rate(:,329) = 3.5e-13_r8
      rate(:,330) = 5.4e-11_r8
      rate(:,333) = 2e-12_r8
      rate(:,334) = 1.4e-11_r8
      rate(:,337) = 2.4e-12_r8
      rate(:,348) = 5e-12_r8
      rate(:,358) = 1.6e-12_r8
      rate(:,360) = 6.7e-12_r8
      rate(:,363) = 3.5e-12_r8
      rate(:,366) = 1.3e-11_r8
      rate(:,367) = 1.4e-11_r8
      rate(:,371) = 2.4e-12_r8
      rate(:,372) = 1.4e-11_r8
      rate(:,377) = 2.4e-12_r8
      rate(:,378) = 4e-11_r8
      rate(:,379) = 4e-11_r8
      rate(:,381) = 1.4e-11_r8
      rate(:,385) = 2.4e-12_r8
      rate(:,386) = 4e-11_r8
      rate(:,390) = 7e-11_r8
      rate(:,391) = 1e-10_r8
      rate(:,396) = 2.4e-12_r8
      rate(:,411) = 4.7e-11_r8
      rate(:,424) = 2.1e-12_r8
      rate(:,425) = 2.8e-13_r8
      rate(:,433) = 1.7e-11_r8
      rate(:,439) = 8.4e-11_r8
      rate(:,441) = 1.9e-11_r8
      rate(:,442) = 1.2e-14_r8
      rate(:,443) = 2e-10_r8
      rate(:,450) = 2.4e-12_r8
      rate(:,451) = 2e-11_r8
      rate(:,455) = 2.3e-11_r8
      rate(:,456) = 2e-11_r8
      rate(:,460) = 3.3e-11_r8
      rate(:,461) = 1e-12_r8
      rate(:,462) = 5.7e-11_r8
      rate(:,463) = 3.4e-11_r8
      rate(:,466) = 2.3e-12_r8
      rate(:,467) = 1.2e-11_r8
      rate(:,468) = 5.7e-11_r8
      rate(:,469) = 2.8e-11_r8
      rate(:,470) = 6.6e-11_r8
      rate(:,471) = 1.4e-11_r8
      rate(:,474) = 1.9e-12_r8
      rate(:,490) = 6.34e-08_r8
      rate(:,496) = 1.9e-11_r8
      rate(:,497) = 1.2e-14_r8
      rate(:,498) = 2e-10_r8
      rate(:,503) = 1.34e-11_r8
      rate(:,507) = 1.34e-11_r8
      rate(:,509) = 1.7e-11_r8
      rate(:,527) = 1.2e-14_r8
      rate(:,530) = 1.29e-07_r8
      rate(:,539) = 1.2e-10_r8
      rate(:,542) = 2.8e-13_r8
 
      do n = 1,pver
        offset = (n-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,n)
      end do

      rate(:,127) = 1.63e-10_r8 * exp( 60._r8 * itemp(:) )
      rate(:,128) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      rate(:,129) = 3.3e-11_r8 * exp( 55._r8 * itemp(:) )
      exp_fac(:) = exp( -2060._r8 * itemp(:) )
      rate(:,131) = 8e-12_r8 * exp_fac(:)
      rate(:,541) = 8e-12_r8 * exp_fac(:)
      rate(:,134) = 1.6e-11_r8 * exp( -4570._r8 * itemp(:) )
      exp_fac(:) = exp( -2000._r8 * itemp(:) )
      rate(:,135) = 1.4e-12_r8 * exp_fac(:)
      rate(:,387) = 1.05e-14_r8 * exp_fac(:)
      rate(:,501) = 1.05e-14_r8 * exp_fac(:)
      rate(:,533) = 1.05e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 200._r8 * itemp(:) )
      rate(:,140) = 3e-11_r8 * exp_fac(:)
      rate(:,226) = 5.5e-12_r8 * exp_fac(:)
      rate(:,265) = 3.8e-12_r8 * exp_fac(:)
      rate(:,287) = 3.8e-12_r8 * exp_fac(:)
      rate(:,314) = 3.8e-12_r8 * exp_fac(:)
      rate(:,322) = 3.8e-12_r8 * exp_fac(:)
      rate(:,326) = 3.8e-12_r8 * exp_fac(:)
      rate(:,342) = 2.3e-11_r8 * exp_fac(:)
      rate(:,352) = 3.8e-12_r8 * exp_fac(:)
      rate(:,362) = 3.8e-12_r8 * exp_fac(:)
      rate(:,389) = 1.52e-11_r8 * exp_fac(:)
      rate(:,397) = 1.52e-12_r8 * exp_fac(:)
      rate(:,403) = 3.8e-12_r8 * exp_fac(:)
      rate(:,406) = 3.8e-12_r8 * exp_fac(:)
      rate(:,410) = 3.8e-12_r8 * exp_fac(:)
      rate(:,426) = 3.8e-12_r8 * exp_fac(:)
      rate(:,430) = 3.8e-12_r8 * exp_fac(:)
      rate(:,436) = 3.8e-12_r8 * exp_fac(:)
      rate(:,440) = 3.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -490._r8 * itemp(:) )
      rate(:,141) = 1e-14_r8 * exp_fac(:)
      rate(:,531) = 1e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( -470._r8 * itemp(:) )
      rate(:,142) = 1.4e-10_r8 * exp_fac(:)
      rate(:,532) = 1.4e-10_r8 * exp_fac(:)
      rate(:,143) = 2.8e-12_r8 * exp( -1800._r8 * itemp(:) )
      exp_fac(:) = exp( 250._r8 * itemp(:) )
      rate(:,145) = 4.8e-11_r8 * exp_fac(:)
      rate(:,224) = 1.7e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 180._r8 * itemp(:) )
      rate(:,146) = 1.8e-11_r8 * exp_fac(:)
      rate(:,300) = 4.2e-12_r8 * exp_fac(:)
      rate(:,313) = 4.2e-12_r8 * exp_fac(:)
      rate(:,321) = 4.2e-12_r8 * exp_fac(:)
      rate(:,350) = 4.2e-12_r8 * exp_fac(:)
      rate(:,370) = 4.4e-12_r8 * exp_fac(:)
      rate(:,376) = 4.4e-12_r8 * exp_fac(:)
      rate(:,449) = 4.2e-12_r8 * exp_fac(:)
      rate(:,454) = 4.2e-12_r8 * exp_fac(:)
      rate(:,459) = 4.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -940._r8 * itemp(:) )
      rate(:,147) = 1.7e-12_r8 * exp_fac(:)
      rate(:,540) = 1.7e-12_r8 * exp_fac(:)
      rate(:,151) = 1.3e-12_r8 * exp( 380._r8 * itemp(:) )
      rate(:,152) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      exp_fac(:) = exp( 220._r8 * itemp(:) )
      rate(:,153) = 2.9e-12_r8 * exp_fac(:)
      rate(:,154) = 1.45e-12_r8 * exp_fac(:)
      rate(:,155) = 1.45e-12_r8 * exp_fac(:)
      rate(:,156) = 1.5e-11_r8 * exp( -3600._r8 * itemp(:) )
      rate(:,157) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      exp_fac(:) = exp( -2450._r8 * itemp(:) )
      rate(:,158) = 1.2e-13_r8 * exp_fac(:)
      rate(:,184) = 3e-11_r8 * exp_fac(:)
      rate(:,537) = 1.2e-13_r8 * exp_fac(:)
      rate(:,161) = 1.5e-11_r8 * exp( 170._r8 * itemp(:) )
      exp_fac(:) = exp( 270._r8 * itemp(:) )
      rate(:,165) = 3.3e-12_r8 * exp_fac(:)
      rate(:,180) = 1.4e-11_r8 * exp_fac(:)
      rate(:,194) = 7.4e-12_r8 * exp_fac(:)
      rate(:,296) = 8.1e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1500._r8 * itemp(:) )
      rate(:,166) = 3e-12_r8 * exp_fac(:)
      rate(:,225) = 5.8e-12_r8 * exp_fac(:)
      rate(:,538) = 3e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 20._r8 * itemp(:) )
      rate(:,168) = 7.26e-11_r8 * exp_fac(:)
      rate(:,169) = 4.64e-11_r8 * exp_fac(:)
      rate(:,176) = 8.1e-11_r8 * exp( -30._r8 * itemp(:) )
      rate(:,177) = 7.1e-12_r8 * exp( -1270._r8 * itemp(:) )
      rate(:,178) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:) )
      rate(:,179) = 1.1e-11_r8 * exp( -980._r8 * itemp(:) )
      rate(:,181) = 3.6e-11_r8 * exp( -375._r8 * itemp(:) )
      rate(:,182) = 2.3e-11_r8 * exp( -200._r8 * itemp(:) )
      rate(:,183) = 3.3e-12_r8 * exp( -115._r8 * itemp(:) )
      rate(:,185) = 1e-12_r8 * exp( -1590._r8 * itemp(:) )
      rate(:,186) = 3.5e-13_r8 * exp( -1370._r8 * itemp(:) )
      exp_fac(:) = exp( 290._r8 * itemp(:) )
      rate(:,187) = 2.6e-12_r8 * exp_fac(:)
      rate(:,188) = 6.4e-12_r8 * exp_fac(:)
      rate(:,218) = 4.1e-13_r8 * exp_fac(:)
      rate(:,399) = 7.5e-12_r8 * exp_fac(:)
      rate(:,413) = 7.5e-12_r8 * exp_fac(:)
      rate(:,416) = 7.5e-12_r8 * exp_fac(:)
      rate(:,419) = 7.5e-12_r8 * exp_fac(:)
      rate(:,189) = 6.5e-12_r8 * exp( 135._r8 * itemp(:) )
      exp_fac(:) = exp( -840._r8 * itemp(:) )
      rate(:,191) = 3.6e-12_r8 * exp_fac(:)
      rate(:,240) = 2e-12_r8 * exp_fac(:)
      rate(:,192) = 1.2e-12_r8 * exp( -330._r8 * itemp(:) )
      rate(:,193) = 2.8e-11_r8 * exp( 85._r8 * itemp(:) )
      exp_fac(:) = exp( 230._r8 * itemp(:) )
      rate(:,195) = 6e-13_r8 * exp_fac(:)
      rate(:,215) = 1.5e-12_r8 * exp_fac(:)
      rate(:,223) = 1.9e-11_r8 * exp_fac(:)
      rate(:,196) = 1e-11_r8 * exp( -3300._r8 * itemp(:) )
      rate(:,197) = 1.8e-12_r8 * exp( -250._r8 * itemp(:) )
      rate(:,198) = 3.4e-12_r8 * exp( -130._r8 * itemp(:) )
      exp_fac(:) = exp( -500._r8 * itemp(:) )
      rate(:,200) = 3e-12_r8 * exp_fac(:)
      rate(:,234) = 1.4e-10_r8 * exp_fac(:)
      exp_fac(:) = exp( -800._r8 * itemp(:) )
      rate(:,212) = 1.7e-11_r8 * exp_fac(:)
      rate(:,239) = 6.3e-12_r8 * exp_fac(:)
      rate(:,213) = 4.8e-12_r8 * exp( -310._r8 * itemp(:) )
      rate(:,214) = 1.6e-11_r8 * exp( -780._r8 * itemp(:) )
      rate(:,216) = 9.5e-13_r8 * exp( 550._r8 * itemp(:) )
      exp_fac(:) = exp( 260._r8 * itemp(:) )
      rate(:,217) = 2.3e-12_r8 * exp_fac(:)
      rate(:,220) = 8.8e-12_r8 * exp_fac(:)
      rate(:,219) = 4.5e-12_r8 * exp( 460._r8 * itemp(:) )
      rate(:,222) = 1.9e-11_r8 * exp( 215._r8 * itemp(:) )
      rate(:,227) = 1.2e-10_r8 * exp( -430._r8 * itemp(:) )
      rate(:,233) = 1.6e-10_r8 * exp( -260._r8 * itemp(:) )
      exp_fac(:) = exp( 0._r8 * itemp(:) )
      rate(:,235) = 1.4e-11_r8 * exp_fac(:)
      rate(:,237) = 2.14e-11_r8 * exp_fac(:)
      rate(:,238) = 1.9e-10_r8 * exp_fac(:)
      rate(:,251) = 2.57e-10_r8 * exp_fac(:)
      rate(:,252) = 1.8e-10_r8 * exp_fac(:)
      rate(:,253) = 1.794e-10_r8 * exp_fac(:)
      rate(:,254) = 1.3e-10_r8 * exp_fac(:)
      rate(:,255) = 7.65e-11_r8 * exp_fac(:)
      rate(:,269) = 4e-13_r8 * exp_fac(:)
      rate(:,273) = 1.31e-10_r8 * exp_fac(:)
      rate(:,274) = 3.5e-11_r8 * exp_fac(:)
      rate(:,275) = 9e-12_r8 * exp_fac(:)
      rate(:,282) = 6.8e-14_r8 * exp_fac(:)
      rate(:,283) = 2e-13_r8 * exp_fac(:)
      rate(:,297) = 7e-13_r8 * exp_fac(:)
      rate(:,298) = 1e-12_r8 * exp_fac(:)
      rate(:,302) = 1e-14_r8 * exp_fac(:)
      rate(:,303) = 1e-11_r8 * exp_fac(:)
      rate(:,304) = 1.15e-11_r8 * exp_fac(:)
      rate(:,305) = 4e-14_r8 * exp_fac(:)
      rate(:,318) = 3e-12_r8 * exp_fac(:)
      rate(:,319) = 6.7e-13_r8 * exp_fac(:)
      rate(:,329) = 3.5e-13_r8 * exp_fac(:)
      rate(:,330) = 5.4e-11_r8 * exp_fac(:)
      rate(:,333) = 2e-12_r8 * exp_fac(:)
      rate(:,334) = 1.4e-11_r8 * exp_fac(:)
      rate(:,337) = 2.4e-12_r8 * exp_fac(:)
      rate(:,348) = 5e-12_r8 * exp_fac(:)
      rate(:,358) = 1.6e-12_r8 * exp_fac(:)
      rate(:,360) = 6.7e-12_r8 * exp_fac(:)
      rate(:,363) = 3.5e-12_r8 * exp_fac(:)
      rate(:,366) = 1.3e-11_r8 * exp_fac(:)
      rate(:,367) = 1.4e-11_r8 * exp_fac(:)
      rate(:,371) = 2.4e-12_r8 * exp_fac(:)
      rate(:,372) = 1.4e-11_r8 * exp_fac(:)
      rate(:,377) = 2.4e-12_r8 * exp_fac(:)
      rate(:,378) = 4e-11_r8 * exp_fac(:)
      rate(:,379) = 4e-11_r8 * exp_fac(:)
      rate(:,381) = 1.4e-11_r8 * exp_fac(:)
      rate(:,385) = 2.4e-12_r8 * exp_fac(:)
      rate(:,386) = 4e-11_r8 * exp_fac(:)
      rate(:,390) = 7e-11_r8 * exp_fac(:)
      rate(:,391) = 1e-10_r8 * exp_fac(:)
      rate(:,396) = 2.4e-12_r8 * exp_fac(:)
      rate(:,411) = 4.7e-11_r8 * exp_fac(:)
      rate(:,424) = 2.1e-12_r8 * exp_fac(:)
      rate(:,425) = 2.8e-13_r8 * exp_fac(:)
      rate(:,433) = 1.7e-11_r8 * exp_fac(:)
      rate(:,439) = 8.4e-11_r8 * exp_fac(:)
      rate(:,441) = 1.9e-11_r8 * exp_fac(:)
      rate(:,442) = 1.2e-14_r8 * exp_fac(:)
      rate(:,443) = 2e-10_r8 * exp_fac(:)
      rate(:,450) = 2.4e-12_r8 * exp_fac(:)
      rate(:,451) = 2e-11_r8 * exp_fac(:)
      rate(:,455) = 2.3e-11_r8 * exp_fac(:)
      rate(:,456) = 2e-11_r8 * exp_fac(:)
      rate(:,460) = 3.3e-11_r8 * exp_fac(:)
      rate(:,461) = 1e-12_r8 * exp_fac(:)
      rate(:,462) = 5.7e-11_r8 * exp_fac(:)
      rate(:,463) = 3.4e-11_r8 * exp_fac(:)
      rate(:,466) = 2.3e-12_r8 * exp_fac(:)
      rate(:,467) = 1.2e-11_r8 * exp_fac(:)
      rate(:,468) = 5.7e-11_r8 * exp_fac(:)
      rate(:,469) = 2.8e-11_r8 * exp_fac(:)
      rate(:,470) = 6.6e-11_r8 * exp_fac(:)
      rate(:,471) = 1.4e-11_r8 * exp_fac(:)
      rate(:,474) = 1.9e-12_r8 * exp_fac(:)
      rate(:,490) = 6.34e-08_r8 * exp_fac(:)
      rate(:,496) = 1.9e-11_r8 * exp_fac(:)
      rate(:,497) = 1.2e-14_r8 * exp_fac(:)
      rate(:,498) = 2e-10_r8 * exp_fac(:)
      rate(:,503) = 1.34e-11_r8 * exp_fac(:)
      rate(:,507) = 1.34e-11_r8 * exp_fac(:)
      rate(:,509) = 1.7e-11_r8 * exp_fac(:)
      rate(:,527) = 1.2e-14_r8 * exp_fac(:)
      rate(:,530) = 1.29e-07_r8 * exp_fac(:)
      rate(:,539) = 1.2e-10_r8 * exp_fac(:)
      rate(:,542) = 2.8e-13_r8 * exp_fac(:)
      exp_fac(:) = exp( 400._r8 * itemp(:) )
      rate(:,236) = 6e-12_r8 * exp_fac(:)
      rate(:,335) = 5e-13_r8 * exp_fac(:)
      rate(:,368) = 5e-13_r8 * exp_fac(:)
      rate(:,373) = 5e-13_r8 * exp_fac(:)
      rate(:,382) = 5e-13_r8 * exp_fac(:)
      rate(:,393) = 5e-13_r8 * exp_fac(:)
      rate(:,241) = 1.46e-11_r8 * exp( -1040._r8 * itemp(:) )
      rate(:,242) = 1.42e-12_r8 * exp( -1150._r8 * itemp(:) )
      exp_fac(:) = exp( -1520._r8 * itemp(:) )
      rate(:,243) = 1.64e-12_r8 * exp_fac(:)
      rate(:,354) = 8.5e-16_r8 * exp_fac(:)
      rate(:,536) = 8.5e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( -1100._r8 * itemp(:) )
      rate(:,244) = 2.03e-11_r8 * exp_fac(:)
      rate(:,473) = 3.4e-12_r8 * exp_fac(:)
      rate(:,245) = 1.96e-12_r8 * exp( -1200._r8 * itemp(:) )
      rate(:,246) = 4.85e-12_r8 * exp( -850._r8 * itemp(:) )
      rate(:,247) = 9e-13_r8 * exp( -360._r8 * itemp(:) )
      exp_fac(:) = exp( -1600._r8 * itemp(:) )
      rate(:,248) = 1.25e-12_r8 * exp_fac(:)
      rate(:,258) = 3.4e-11_r8 * exp_fac(:)
      rate(:,249) = 1.3e-12_r8 * exp( -1770._r8 * itemp(:) )
      rate(:,250) = 9.2e-13_r8 * exp( -1560._r8 * itemp(:) )
      rate(:,256) = 9.7e-15_r8 * exp( 625._r8 * itemp(:) )
      rate(:,257) = 6e-13_r8 * exp( -2058._r8 * itemp(:) )
      rate(:,259) = 5.5e-12_r8 * exp( 125._r8 * itemp(:) )
      rate(:,260) = 5e-13_r8 * exp( -424._r8 * itemp(:) )
      rate(:,261) = 1.9e-14_r8 * exp( 706._r8 * itemp(:) )
      rate(:,262) = 4.1e-13_r8 * exp( 750._r8 * itemp(:) )
      exp_fac(:) = exp( 300._r8 * itemp(:) )
      rate(:,263) = 2.8e-12_r8 * exp_fac(:)
      rate(:,325) = 2.9e-12_r8 * exp_fac(:)
      rate(:,264) = 2.9e-12_r8 * exp( -345._r8 * itemp(:) )
      rate(:,266) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:) )
      exp_fac(:) = exp( 700._r8 * itemp(:) )
      rate(:,270) = 7.5e-13_r8 * exp_fac(:)
      rate(:,284) = 7.5e-13_r8 * exp_fac(:)
      rate(:,299) = 7.5e-13_r8 * exp_fac(:)
      rate(:,312) = 7.5e-13_r8 * exp_fac(:)
      rate(:,320) = 7.5e-13_r8 * exp_fac(:)
      rate(:,324) = 8.6e-13_r8 * exp_fac(:)
      rate(:,336) = 8e-13_r8 * exp_fac(:)
      rate(:,349) = 7.5e-13_r8 * exp_fac(:)
      rate(:,359) = 7.5e-13_r8 * exp_fac(:)
      rate(:,369) = 8e-13_r8 * exp_fac(:)
      rate(:,374) = 8e-13_r8 * exp_fac(:)
      rate(:,383) = 8e-13_r8 * exp_fac(:)
      rate(:,394) = 8e-13_r8 * exp_fac(:)
      rate(:,401) = 7.5e-13_r8 * exp_fac(:)
      rate(:,405) = 7.5e-13_r8 * exp_fac(:)
      rate(:,408) = 7.5e-13_r8 * exp_fac(:)
      rate(:,421) = 7.5e-13_r8 * exp_fac(:)
      rate(:,428) = 7.5e-13_r8 * exp_fac(:)
      rate(:,434) = 7.5e-13_r8 * exp_fac(:)
      rate(:,437) = 7.5e-13_r8 * exp_fac(:)
      rate(:,448) = 7.5e-13_r8 * exp_fac(:)
      rate(:,453) = 7.5e-13_r8 * exp_fac(:)
      rate(:,458) = 7.5e-13_r8 * exp_fac(:)
      rate(:,271) = 2.4e+12_r8 * exp( -7000._r8 * itemp(:) )
      rate(:,272) = 2.6e-12_r8 * exp( 265._r8 * itemp(:) )
      rate(:,276) = 1.08e-10_r8 * exp( 105._r8 * itemp(:) )
      exp_fac(:) = exp( -2630._r8 * itemp(:) )
      rate(:,281) = 1.2e-14_r8 * exp_fac(:)
      rate(:,528) = 1.2e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 365._r8 * itemp(:) )
      rate(:,285) = 2.6e-12_r8 * exp_fac(:)
      rate(:,402) = 2.6e-12_r8 * exp_fac(:)
      rate(:,407) = 2.6e-12_r8 * exp_fac(:)
      rate(:,409) = 2.6e-12_r8 * exp_fac(:)
      rate(:,422) = 2.6e-12_r8 * exp_fac(:)
      rate(:,429) = 2.6e-12_r8 * exp_fac(:)
      rate(:,435) = 2.6e-12_r8 * exp_fac(:)
      rate(:,438) = 2.6e-12_r8 * exp_fac(:)
      rate(:,286) = 6.9e-12_r8 * exp( -230._r8 * itemp(:) )
      rate(:,288) = 7.2e-11_r8 * exp( -70._r8 * itemp(:) )
      rate(:,289) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:) )
      exp_fac(:) = exp( -1900._r8 * itemp(:) )
      rate(:,290) = 1.4e-12_r8 * exp_fac(:)
      rate(:,310) = 6.5e-15_r8 * exp_fac(:)
      rate(:,529) = 6.5e-15_r8 * exp_fac(:)
      rate(:,291) = 4.63e-12_r8 * exp( 350._r8 * itemp(:) )
      rate(:,292) = 7.8e-13_r8 * exp( -1050._r8 * itemp(:) )
      exp_fac(:) = exp( 500._r8 * itemp(:) )
      rate(:,293) = 2.9e-12_r8 * exp_fac(:)
      rate(:,294) = 2e-12_r8 * exp_fac(:)
      rate(:,323) = 7.1e-13_r8 * exp_fac(:)
      rate(:,344) = 2e-12_r8 * exp_fac(:)
      rate(:,447) = 2e-12_r8 * exp_fac(:)
      rate(:,452) = 2e-12_r8 * exp_fac(:)
      rate(:,457) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 1040._r8 * itemp(:) )
      rate(:,295) = 4.3e-13_r8 * exp_fac(:)
      rate(:,345) = 4.3e-13_r8 * exp_fac(:)
      rate(:,398) = 4.3e-13_r8 * exp_fac(:)
      rate(:,412) = 4.3e-13_r8 * exp_fac(:)
      rate(:,415) = 4.3e-13_r8 * exp_fac(:)
      rate(:,418) = 4.3e-13_r8 * exp_fac(:)
      rate(:,301) = 1.6e+11_r8 * exp( -4150._r8 * itemp(:) )
      rate(:,309) = 4.6e-13_r8 * exp( -1156._r8 * itemp(:) )
      rate(:,311) = 3.75e-13_r8 * exp( -40._r8 * itemp(:) )
      rate(:,315) = 8.7e-12_r8 * exp( -615._r8 * itemp(:) )
      rate(:,316) = 1.4e-12_r8 * exp( -1860._r8 * itemp(:) )
      rate(:,317) = 8.4e-13_r8 * exp( 830._r8 * itemp(:) )
      rate(:,331) = 4.8e-12_r8 * exp( 120._r8 * itemp(:) )
      rate(:,332) = 5.1e-14_r8 * exp( 693._r8 * itemp(:) )
      exp_fac(:) = exp( 360._r8 * itemp(:) )
      rate(:,338) = 2.7e-12_r8 * exp_fac(:)
      rate(:,339) = 1.3e-13_r8 * exp_fac(:)
      rate(:,341) = 9.6e-12_r8 * exp_fac(:)
      rate(:,347) = 5.3e-12_r8 * exp_fac(:)
      rate(:,384) = 2.7e-12_r8 * exp_fac(:)
      rate(:,395) = 2.7e-12_r8 * exp_fac(:)
      rate(:,340) = 1.5e-15_r8 * exp( -2100._r8 * itemp(:) )
      exp_fac(:) = exp( 530._r8 * itemp(:) )
      rate(:,343) = 4.6e-12_r8 * exp_fac(:)
      rate(:,346) = 2.3e-12_r8 * exp_fac(:)
      rate(:,351) = 2.3e-12_r8 * exp( -170._r8 * itemp(:) )
      rate(:,355) = 4.13e-12_r8 * exp( 452._r8 * itemp(:) )
      rate(:,361) = 5.4e-14_r8 * exp( 870._r8 * itemp(:) )
      exp_fac(:) = exp( 175._r8 * itemp(:) )
      rate(:,364) = 1.86e-11_r8 * exp_fac(:)
      rate(:,365) = 1.86e-11_r8 * exp_fac(:)
      rate(:,375) = 1.6e+09_r8 * exp( -8300._r8 * itemp(:) )
      exp_fac(:) = exp( -446._r8 * itemp(:) )
      rate(:,380) = 3.03e-12_r8 * exp_fac(:)
      rate(:,500) = 3.03e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 410._r8 * itemp(:) )
      rate(:,388) = 2.54e-11_r8 * exp_fac(:)
      rate(:,502) = 2.54e-11_r8 * exp_fac(:)
      rate(:,392) = 1.3e-12_r8 * exp( 640._r8 * itemp(:) )
      exp_fac(:) = exp( -193._r8 * itemp(:) )
      rate(:,400) = 2.3e-12_r8 * exp_fac(:)
      rate(:,499) = 2.3e-12_r8 * exp_fac(:)
      rate(:,404) = 5.9e-12_r8 * exp( 225._r8 * itemp(:) )
      rate(:,423) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:) )
      exp_fac(:) = exp( 352._r8 * itemp(:) )
      rate(:,431) = 1.7e-12_r8 * exp_fac(:)
      rate(:,508) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 490._r8 * itemp(:) )
      rate(:,444) = 1.2e-12_r8 * exp_fac(:)
      rate(:,504) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -580._r8 * itemp(:) )
      rate(:,445) = 6.3e-16_r8 * exp_fac(:)
      rate(:,505) = 6.3e-16_r8 * exp_fac(:)
      rate(:,535) = 6.3e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( 440._r8 * itemp(:) )
      rate(:,446) = 1.2e-11_r8 * exp_fac(:)
      rate(:,506) = 1.2e-11_r8 * exp_fac(:)
      rate(:,464) = 2.1e-11_r8 * exp( -2200._r8 * itemp(:) )
      rate(:,465) = 7.2e-14_r8 * exp( -1070._r8 * itemp(:) )
      rate(:,472) = 1.6e-13_r8 * exp( -2280._r8 * itemp(:) )
      rate(:,475) = 2.7e-11_r8 * exp( 335._r8 * itemp(:) )
      rate(:,478) = 1.9e-13_r8 * exp( 520._r8 * itemp(:) )
      rate(:,479) = 9.6e-12_r8 * exp( -234._r8 * itemp(:) )
      rate(:,480) = 1.7e-12_r8 * exp( -710._r8 * itemp(:) )
      rate(:,534) = 4.4e-15_r8 * exp( -2500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)
 
      n = ncol*pver

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( rate(:,139), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.9e-31_r8 * itemp(:)**1._r8
      kinf(:) = 2.6e-11_r8
      call jpl( rate(:,149), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,159), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,167), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,170), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,171), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,172), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,190), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-32_r8 * itemp(:)**3.6_r8
      kinf(:) = 3.7e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,210), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,221), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,267), m, 0.6_r8, ko, kinf, n )

      ko(:) = 4.28e-33_r8
      kinf(:) = 9.3e-15_r8 * itemp(:)**(-4.42_r8)
      call jpl( rate(:,268), m, 0.8_r8, ko, kinf, n )

      ko(:) = 5.2e-30_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.2e-10_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,278), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.5e-30_r8
      kinf(:) = 8.3e-13_r8 * itemp(:)**(-2._r8)
      call jpl( rate(:,279), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.6e-29_r8 * itemp(:)**3.3_r8
      kinf(:) = 3.1e-10_r8 * itemp(:)
      call jpl( rate(:,280), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8.6e-29_r8 * itemp(:)**3.1_r8
      kinf(:) = 9e-12_r8 * itemp(:)**0.85_r8
      call jpl( rate(:,306), m, 0.48_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,307), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,327), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,353), m, 0.5_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,414), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,417), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,420), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,427), m, 0.6_r8, ko, kinf, n )

      end subroutine setrxt


      subroutine setrxt_hrates( rate, temp, m, ncol, kbot )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      integer   ::  k
      real(r8)  :: itemp(ncol*kbot)
      real(r8)  :: exp_fac(ncol*kbot)
      real(r8)  :: ko(ncol*kbot)
      real(r8)  :: kinf(ncol*kbot)
      real(r8)  :: wrk(ncol*kbot)
 
      n = ncol*kbot

      rate(:n,136) = 6.9e-12_r8
 
      do k = 1,kbot
        offset = (k-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,k)
      end do

      rate(:n,128) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      rate(:n,131) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:n,140) = 3e-11_r8 * exp( 200._r8 * itemp(:) )
      rate(:n,141) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:n,142) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:n,145) = 4.8e-11_r8 * exp( 250._r8 * itemp(:) )
      rate(:n,146) = 1.8e-11_r8 * exp( 180._r8 * itemp(:) )
      rate(:n,147) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:n,152) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      rate(:n,156) = 1.5e-11_r8 * exp( -3600._r8 * itemp(:) )
      rate(:n,157) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      rate(:n,165) = 3.3e-12_r8 * exp( 270._r8 * itemp(:) )
      rate(:n,166) = 3e-12_r8 * exp( -1500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( wrk, m, 0.6_r8, ko, kinf, n )
      rate(:n,139) = wrk(:)























      end subroutine setrxt_hrates

      end module mo_setrxt
