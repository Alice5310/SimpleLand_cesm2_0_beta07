
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

      rate(:,153) = 0.000258_r8
      rate(:,154) = 0.085_r8
      rate(:,155) = 1.2e-10_r8
      rate(:,160) = 1.2e-10_r8
      rate(:,161) = 1e-20_r8
      rate(:,162) = 1.3e-16_r8
      rate(:,164) = 4.2e-13_r8
      rate(:,166) = 8e-14_r8
      rate(:,167) = 3.9e-17_r8
      rate(:,174) = 6.9e-12_r8
      rate(:,175) = 7.2e-11_r8
      rate(:,176) = 1.6e-12_r8
      rate(:,182) = 1.8e-12_r8
      rate(:,186) = 1.8e-12_r8
      rate(:,190) = 7e-13_r8
      rate(:,191) = 5e-12_r8
      rate(:,200) = 3.5e-12_r8
      rate(:,202) = 1e-11_r8
      rate(:,203) = 2.2e-11_r8
      rate(:,204) = 5e-11_r8
      rate(:,239) = 1.7e-13_r8
      rate(:,241) = 2.607e-10_r8
      rate(:,242) = 9.75e-11_r8
      rate(:,243) = 2.07e-10_r8
      rate(:,244) = 2.088e-10_r8
      rate(:,245) = 1.17e-10_r8
      rate(:,246) = 4.644e-11_r8
      rate(:,247) = 1.204e-10_r8
      rate(:,248) = 9.9e-11_r8
      rate(:,249) = 3.3e-12_r8
      rate(:,268) = 4.5e-11_r8
      rate(:,269) = 4.62e-10_r8
      rate(:,270) = 1.2e-10_r8
      rate(:,271) = 9e-11_r8
      rate(:,272) = 3e-11_r8
      rate(:,277) = 2.14e-11_r8
      rate(:,278) = 1.9e-10_r8
      rate(:,291) = 2.57e-10_r8
      rate(:,292) = 1.8e-10_r8
      rate(:,293) = 1.794e-10_r8
      rate(:,294) = 1.3e-10_r8
      rate(:,295) = 7.65e-11_r8
      rate(:,309) = 4e-13_r8
      rate(:,313) = 1.31e-10_r8
      rate(:,314) = 3.5e-11_r8
      rate(:,315) = 9e-12_r8
      rate(:,322) = 6.8e-14_r8
      rate(:,323) = 2e-13_r8
      rate(:,337) = 7e-13_r8
      rate(:,338) = 1e-12_r8
      rate(:,342) = 1e-14_r8
      rate(:,343) = 1e-11_r8
      rate(:,344) = 1.15e-11_r8
      rate(:,345) = 4e-14_r8
      rate(:,358) = 3e-12_r8
      rate(:,359) = 6.7e-13_r8
      rate(:,369) = 3.5e-13_r8
      rate(:,370) = 5.4e-11_r8
      rate(:,373) = 2e-12_r8
      rate(:,374) = 1.4e-11_r8
      rate(:,377) = 2.4e-12_r8
      rate(:,388) = 5e-12_r8
      rate(:,398) = 1.6e-12_r8
      rate(:,400) = 6.7e-12_r8
      rate(:,403) = 3.5e-12_r8
      rate(:,406) = 1.3e-11_r8
      rate(:,407) = 1.4e-11_r8
      rate(:,411) = 2.4e-12_r8
      rate(:,412) = 1.4e-11_r8
      rate(:,417) = 2.4e-12_r8
      rate(:,418) = 4e-11_r8
      rate(:,419) = 4e-11_r8
      rate(:,421) = 1.4e-11_r8
      rate(:,425) = 2.4e-12_r8
      rate(:,426) = 4e-11_r8
      rate(:,430) = 7e-11_r8
      rate(:,431) = 1e-10_r8
      rate(:,436) = 2.4e-12_r8
      rate(:,451) = 4.7e-11_r8
      rate(:,464) = 2.1e-12_r8
      rate(:,465) = 2.8e-13_r8
      rate(:,473) = 1.7e-11_r8
      rate(:,479) = 8.4e-11_r8
      rate(:,481) = 1.9e-11_r8
      rate(:,482) = 1.2e-14_r8
      rate(:,483) = 2e-10_r8
      rate(:,490) = 2.4e-12_r8
      rate(:,491) = 2e-11_r8
      rate(:,495) = 2.3e-11_r8
      rate(:,496) = 2e-11_r8
      rate(:,500) = 3.3e-11_r8
      rate(:,501) = 1e-12_r8
      rate(:,502) = 5.7e-11_r8
      rate(:,503) = 3.4e-11_r8
      rate(:,506) = 2.3e-12_r8
      rate(:,507) = 1.2e-11_r8
      rate(:,508) = 5.7e-11_r8
      rate(:,509) = 2.8e-11_r8
      rate(:,510) = 6.6e-11_r8
      rate(:,511) = 1.4e-11_r8
      rate(:,514) = 1.9e-12_r8
      rate(:,535) = 6.34e-08_r8
      rate(:,536) = 1.9e-11_r8
      rate(:,537) = 1.2e-14_r8
      rate(:,538) = 2e-10_r8
      rate(:,543) = 1.34e-11_r8
      rate(:,547) = 1.34e-11_r8
      rate(:,549) = 1.7e-11_r8
      rate(:,570) = 6e-11_r8
      rate(:,573) = 1e-12_r8
      rate(:,574) = 4e-10_r8
      rate(:,575) = 2e-10_r8
      rate(:,576) = 1e-10_r8
      rate(:,577) = 5e-16_r8
      rate(:,578) = 4.4e-10_r8
      rate(:,579) = 9e-10_r8
      rate(:,582) = 1.2e-14_r8
      rate(:,593) = 1.2e-10_r8
      rate(:,596) = 2.8e-13_r8
      rate(:,597) = 1.29e-07_r8
 
      do n = 1,pver
        offset = (n-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,n)
      end do

      rate(:,156) = 1.63e-10_r8 * exp( 60._r8 * itemp(:) )
      rate(:,157) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      exp_fac(:) = exp( 55._r8 * itemp(:) )
      rate(:,158) = 2.64e-11_r8 * exp_fac(:)
      rate(:,159) = 6.6e-12_r8 * exp_fac(:)
      rate(:,163) = 3.6e-18_r8 * exp( -220._r8 * itemp(:) )
      rate(:,165) = 1.8e-15_r8 * exp( 45._r8 * itemp(:) )
      rate(:,168) = 3.5e-11_r8 * exp( -135._r8 * itemp(:) )
      exp_fac(:) = exp( -2060._r8 * itemp(:) )
      rate(:,169) = 8e-12_r8 * exp_fac(:)
      rate(:,595) = 8e-12_r8 * exp_fac(:)
      rate(:,172) = 1.6e-11_r8 * exp( -4570._r8 * itemp(:) )
      exp_fac(:) = exp( -2000._r8 * itemp(:) )
      rate(:,173) = 1.4e-12_r8 * exp_fac(:)
      rate(:,427) = 1.05e-14_r8 * exp_fac(:)
      rate(:,541) = 1.05e-14_r8 * exp_fac(:)
      rate(:,587) = 1.05e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 200._r8 * itemp(:) )
      rate(:,178) = 3e-11_r8 * exp_fac(:)
      rate(:,266) = 5.5e-12_r8 * exp_fac(:)
      rate(:,305) = 3.8e-12_r8 * exp_fac(:)
      rate(:,327) = 3.8e-12_r8 * exp_fac(:)
      rate(:,354) = 3.8e-12_r8 * exp_fac(:)
      rate(:,362) = 3.8e-12_r8 * exp_fac(:)
      rate(:,366) = 3.8e-12_r8 * exp_fac(:)
      rate(:,382) = 2.3e-11_r8 * exp_fac(:)
      rate(:,392) = 3.8e-12_r8 * exp_fac(:)
      rate(:,402) = 3.8e-12_r8 * exp_fac(:)
      rate(:,429) = 1.52e-11_r8 * exp_fac(:)
      rate(:,437) = 1.52e-12_r8 * exp_fac(:)
      rate(:,443) = 3.8e-12_r8 * exp_fac(:)
      rate(:,446) = 3.8e-12_r8 * exp_fac(:)
      rate(:,450) = 3.8e-12_r8 * exp_fac(:)
      rate(:,466) = 3.8e-12_r8 * exp_fac(:)
      rate(:,470) = 3.8e-12_r8 * exp_fac(:)
      rate(:,476) = 3.8e-12_r8 * exp_fac(:)
      rate(:,480) = 3.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -490._r8 * itemp(:) )
      rate(:,179) = 1e-14_r8 * exp_fac(:)
      rate(:,585) = 1e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( -470._r8 * itemp(:) )
      rate(:,180) = 1.4e-10_r8 * exp_fac(:)
      rate(:,586) = 1.4e-10_r8 * exp_fac(:)
      rate(:,181) = 2.8e-12_r8 * exp( -1800._r8 * itemp(:) )
      exp_fac(:) = exp( 250._r8 * itemp(:) )
      rate(:,183) = 4.8e-11_r8 * exp_fac(:)
      rate(:,264) = 1.7e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 180._r8 * itemp(:) )
      rate(:,184) = 1.8e-11_r8 * exp_fac(:)
      rate(:,340) = 4.2e-12_r8 * exp_fac(:)
      rate(:,353) = 4.2e-12_r8 * exp_fac(:)
      rate(:,361) = 4.2e-12_r8 * exp_fac(:)
      rate(:,390) = 4.2e-12_r8 * exp_fac(:)
      rate(:,410) = 4.4e-12_r8 * exp_fac(:)
      rate(:,416) = 4.4e-12_r8 * exp_fac(:)
      rate(:,489) = 4.2e-12_r8 * exp_fac(:)
      rate(:,494) = 4.2e-12_r8 * exp_fac(:)
      rate(:,499) = 4.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -940._r8 * itemp(:) )
      rate(:,185) = 1.7e-12_r8 * exp_fac(:)
      rate(:,594) = 1.7e-12_r8 * exp_fac(:)
      rate(:,189) = 1.3e-12_r8 * exp( 380._r8 * itemp(:) )
      rate(:,192) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      exp_fac(:) = exp( 220._r8 * itemp(:) )
      rate(:,193) = 2.9e-12_r8 * exp_fac(:)
      rate(:,194) = 1.45e-12_r8 * exp_fac(:)
      rate(:,195) = 1.45e-12_r8 * exp_fac(:)
      rate(:,196) = 1.5e-11_r8 * exp( -3600._r8 * itemp(:) )
      rate(:,197) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      exp_fac(:) = exp( -2450._r8 * itemp(:) )
      rate(:,198) = 1.2e-13_r8 * exp_fac(:)
      rate(:,224) = 3e-11_r8 * exp_fac(:)
      rate(:,591) = 1.2e-13_r8 * exp_fac(:)
      rate(:,201) = 1.5e-11_r8 * exp( 170._r8 * itemp(:) )
      exp_fac(:) = exp( 270._r8 * itemp(:) )
      rate(:,205) = 3.3e-12_r8 * exp_fac(:)
      rate(:,220) = 1.4e-11_r8 * exp_fac(:)
      rate(:,234) = 7.4e-12_r8 * exp_fac(:)
      rate(:,336) = 8.1e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1500._r8 * itemp(:) )
      rate(:,206) = 3e-12_r8 * exp_fac(:)
      rate(:,265) = 5.8e-12_r8 * exp_fac(:)
      rate(:,592) = 3e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 20._r8 * itemp(:) )
      rate(:,208) = 7.26e-11_r8 * exp_fac(:)
      rate(:,209) = 4.64e-11_r8 * exp_fac(:)
      rate(:,216) = 8.1e-11_r8 * exp( -30._r8 * itemp(:) )
      rate(:,217) = 7.1e-12_r8 * exp( -1270._r8 * itemp(:) )
      rate(:,218) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:) )
      rate(:,219) = 1.1e-11_r8 * exp( -980._r8 * itemp(:) )
      rate(:,221) = 3.6e-11_r8 * exp( -375._r8 * itemp(:) )
      rate(:,222) = 2.3e-11_r8 * exp( -200._r8 * itemp(:) )
      rate(:,223) = 3.3e-12_r8 * exp( -115._r8 * itemp(:) )
      rate(:,225) = 1e-12_r8 * exp( -1590._r8 * itemp(:) )
      rate(:,226) = 3.5e-13_r8 * exp( -1370._r8 * itemp(:) )
      exp_fac(:) = exp( 290._r8 * itemp(:) )
      rate(:,227) = 2.6e-12_r8 * exp_fac(:)
      rate(:,228) = 6.4e-12_r8 * exp_fac(:)
      rate(:,258) = 4.1e-13_r8 * exp_fac(:)
      rate(:,439) = 7.5e-12_r8 * exp_fac(:)
      rate(:,453) = 7.5e-12_r8 * exp_fac(:)
      rate(:,456) = 7.5e-12_r8 * exp_fac(:)
      rate(:,459) = 7.5e-12_r8 * exp_fac(:)
      rate(:,229) = 6.5e-12_r8 * exp( 135._r8 * itemp(:) )
      exp_fac(:) = exp( -840._r8 * itemp(:) )
      rate(:,231) = 3.6e-12_r8 * exp_fac(:)
      rate(:,280) = 2e-12_r8 * exp_fac(:)
      rate(:,232) = 1.2e-12_r8 * exp( -330._r8 * itemp(:) )
      rate(:,233) = 2.8e-11_r8 * exp( 85._r8 * itemp(:) )
      exp_fac(:) = exp( 230._r8 * itemp(:) )
      rate(:,235) = 6e-13_r8 * exp_fac(:)
      rate(:,255) = 1.5e-12_r8 * exp_fac(:)
      rate(:,263) = 1.9e-11_r8 * exp_fac(:)
      rate(:,236) = 1e-11_r8 * exp( -3300._r8 * itemp(:) )
      rate(:,237) = 1.8e-12_r8 * exp( -250._r8 * itemp(:) )
      rate(:,238) = 3.4e-12_r8 * exp( -130._r8 * itemp(:) )
      exp_fac(:) = exp( -500._r8 * itemp(:) )
      rate(:,240) = 3e-12_r8 * exp_fac(:)
      rate(:,274) = 1.4e-10_r8 * exp_fac(:)
      exp_fac(:) = exp( -800._r8 * itemp(:) )
      rate(:,252) = 1.7e-11_r8 * exp_fac(:)
      rate(:,279) = 6.3e-12_r8 * exp_fac(:)
      rate(:,253) = 4.8e-12_r8 * exp( -310._r8 * itemp(:) )
      rate(:,254) = 1.6e-11_r8 * exp( -780._r8 * itemp(:) )
      rate(:,256) = 9.5e-13_r8 * exp( 550._r8 * itemp(:) )
      exp_fac(:) = exp( 260._r8 * itemp(:) )
      rate(:,257) = 2.3e-12_r8 * exp_fac(:)
      rate(:,260) = 8.8e-12_r8 * exp_fac(:)
      rate(:,259) = 4.5e-12_r8 * exp( 460._r8 * itemp(:) )
      rate(:,262) = 1.9e-11_r8 * exp( 215._r8 * itemp(:) )
      rate(:,267) = 1.2e-10_r8 * exp( -430._r8 * itemp(:) )
      rate(:,273) = 1.6e-10_r8 * exp( -260._r8 * itemp(:) )
      exp_fac(:) = exp( 0._r8 * itemp(:) )
      rate(:,275) = 1.4e-11_r8 * exp_fac(:)
      rate(:,277) = 2.14e-11_r8 * exp_fac(:)
      rate(:,278) = 1.9e-10_r8 * exp_fac(:)
      rate(:,291) = 2.57e-10_r8 * exp_fac(:)
      rate(:,292) = 1.8e-10_r8 * exp_fac(:)
      rate(:,293) = 1.794e-10_r8 * exp_fac(:)
      rate(:,294) = 1.3e-10_r8 * exp_fac(:)
      rate(:,295) = 7.65e-11_r8 * exp_fac(:)
      rate(:,309) = 4e-13_r8 * exp_fac(:)
      rate(:,313) = 1.31e-10_r8 * exp_fac(:)
      rate(:,314) = 3.5e-11_r8 * exp_fac(:)
      rate(:,315) = 9e-12_r8 * exp_fac(:)
      rate(:,322) = 6.8e-14_r8 * exp_fac(:)
      rate(:,323) = 2e-13_r8 * exp_fac(:)
      rate(:,337) = 7e-13_r8 * exp_fac(:)
      rate(:,338) = 1e-12_r8 * exp_fac(:)
      rate(:,342) = 1e-14_r8 * exp_fac(:)
      rate(:,343) = 1e-11_r8 * exp_fac(:)
      rate(:,344) = 1.15e-11_r8 * exp_fac(:)
      rate(:,345) = 4e-14_r8 * exp_fac(:)
      rate(:,358) = 3e-12_r8 * exp_fac(:)
      rate(:,359) = 6.7e-13_r8 * exp_fac(:)
      rate(:,369) = 3.5e-13_r8 * exp_fac(:)
      rate(:,370) = 5.4e-11_r8 * exp_fac(:)
      rate(:,373) = 2e-12_r8 * exp_fac(:)
      rate(:,374) = 1.4e-11_r8 * exp_fac(:)
      rate(:,377) = 2.4e-12_r8 * exp_fac(:)
      rate(:,388) = 5e-12_r8 * exp_fac(:)
      rate(:,398) = 1.6e-12_r8 * exp_fac(:)
      rate(:,400) = 6.7e-12_r8 * exp_fac(:)
      rate(:,403) = 3.5e-12_r8 * exp_fac(:)
      rate(:,406) = 1.3e-11_r8 * exp_fac(:)
      rate(:,407) = 1.4e-11_r8 * exp_fac(:)
      rate(:,411) = 2.4e-12_r8 * exp_fac(:)
      rate(:,412) = 1.4e-11_r8 * exp_fac(:)
      rate(:,417) = 2.4e-12_r8 * exp_fac(:)
      rate(:,418) = 4e-11_r8 * exp_fac(:)
      rate(:,419) = 4e-11_r8 * exp_fac(:)
      rate(:,421) = 1.4e-11_r8 * exp_fac(:)
      rate(:,425) = 2.4e-12_r8 * exp_fac(:)
      rate(:,426) = 4e-11_r8 * exp_fac(:)
      rate(:,430) = 7e-11_r8 * exp_fac(:)
      rate(:,431) = 1e-10_r8 * exp_fac(:)
      rate(:,436) = 2.4e-12_r8 * exp_fac(:)
      rate(:,451) = 4.7e-11_r8 * exp_fac(:)
      rate(:,464) = 2.1e-12_r8 * exp_fac(:)
      rate(:,465) = 2.8e-13_r8 * exp_fac(:)
      rate(:,473) = 1.7e-11_r8 * exp_fac(:)
      rate(:,479) = 8.4e-11_r8 * exp_fac(:)
      rate(:,481) = 1.9e-11_r8 * exp_fac(:)
      rate(:,482) = 1.2e-14_r8 * exp_fac(:)
      rate(:,483) = 2e-10_r8 * exp_fac(:)
      rate(:,490) = 2.4e-12_r8 * exp_fac(:)
      rate(:,491) = 2e-11_r8 * exp_fac(:)
      rate(:,495) = 2.3e-11_r8 * exp_fac(:)
      rate(:,496) = 2e-11_r8 * exp_fac(:)
      rate(:,500) = 3.3e-11_r8 * exp_fac(:)
      rate(:,501) = 1e-12_r8 * exp_fac(:)
      rate(:,502) = 5.7e-11_r8 * exp_fac(:)
      rate(:,503) = 3.4e-11_r8 * exp_fac(:)
      rate(:,506) = 2.3e-12_r8 * exp_fac(:)
      rate(:,507) = 1.2e-11_r8 * exp_fac(:)
      rate(:,508) = 5.7e-11_r8 * exp_fac(:)
      rate(:,509) = 2.8e-11_r8 * exp_fac(:)
      rate(:,510) = 6.6e-11_r8 * exp_fac(:)
      rate(:,511) = 1.4e-11_r8 * exp_fac(:)
      rate(:,514) = 1.9e-12_r8 * exp_fac(:)
      rate(:,535) = 6.34e-08_r8 * exp_fac(:)
      rate(:,536) = 1.9e-11_r8 * exp_fac(:)
      rate(:,537) = 1.2e-14_r8 * exp_fac(:)
      rate(:,538) = 2e-10_r8 * exp_fac(:)
      rate(:,543) = 1.34e-11_r8 * exp_fac(:)
      rate(:,547) = 1.34e-11_r8 * exp_fac(:)
      rate(:,549) = 1.7e-11_r8 * exp_fac(:)
      rate(:,570) = 6e-11_r8 * exp_fac(:)
      rate(:,573) = 1e-12_r8 * exp_fac(:)
      rate(:,574) = 4e-10_r8 * exp_fac(:)
      rate(:,575) = 2e-10_r8 * exp_fac(:)
      rate(:,576) = 1e-10_r8 * exp_fac(:)
      rate(:,577) = 5e-16_r8 * exp_fac(:)
      rate(:,578) = 4.4e-10_r8 * exp_fac(:)
      rate(:,579) = 9e-10_r8 * exp_fac(:)
      rate(:,582) = 1.2e-14_r8 * exp_fac(:)
      rate(:,593) = 1.2e-10_r8 * exp_fac(:)
      rate(:,596) = 2.8e-13_r8 * exp_fac(:)
      rate(:,597) = 1.29e-07_r8 * exp_fac(:)
      exp_fac(:) = exp( 400._r8 * itemp(:) )
      rate(:,276) = 6e-12_r8 * exp_fac(:)
      rate(:,375) = 5e-13_r8 * exp_fac(:)
      rate(:,408) = 5e-13_r8 * exp_fac(:)
      rate(:,413) = 5e-13_r8 * exp_fac(:)
      rate(:,422) = 5e-13_r8 * exp_fac(:)
      rate(:,433) = 5e-13_r8 * exp_fac(:)
      rate(:,281) = 1.46e-11_r8 * exp( -1040._r8 * itemp(:) )
      rate(:,282) = 1.42e-12_r8 * exp( -1150._r8 * itemp(:) )
      exp_fac(:) = exp( -1520._r8 * itemp(:) )
      rate(:,283) = 1.64e-12_r8 * exp_fac(:)
      rate(:,394) = 8.5e-16_r8 * exp_fac(:)
      rate(:,590) = 8.5e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( -1100._r8 * itemp(:) )
      rate(:,284) = 2.03e-11_r8 * exp_fac(:)
      rate(:,513) = 3.4e-12_r8 * exp_fac(:)
      rate(:,285) = 1.96e-12_r8 * exp( -1200._r8 * itemp(:) )
      rate(:,286) = 4.85e-12_r8 * exp( -850._r8 * itemp(:) )
      rate(:,287) = 9e-13_r8 * exp( -360._r8 * itemp(:) )
      exp_fac(:) = exp( -1600._r8 * itemp(:) )
      rate(:,288) = 1.25e-12_r8 * exp_fac(:)
      rate(:,298) = 3.4e-11_r8 * exp_fac(:)
      rate(:,289) = 1.3e-12_r8 * exp( -1770._r8 * itemp(:) )
      rate(:,290) = 9.2e-13_r8 * exp( -1560._r8 * itemp(:) )
      rate(:,296) = 9.7e-15_r8 * exp( 625._r8 * itemp(:) )
      rate(:,297) = 6e-13_r8 * exp( -2058._r8 * itemp(:) )
      rate(:,299) = 5.5e-12_r8 * exp( 125._r8 * itemp(:) )
      rate(:,300) = 5e-13_r8 * exp( -424._r8 * itemp(:) )
      rate(:,301) = 1.9e-14_r8 * exp( 706._r8 * itemp(:) )
      rate(:,302) = 4.1e-13_r8 * exp( 750._r8 * itemp(:) )
      exp_fac(:) = exp( 300._r8 * itemp(:) )
      rate(:,303) = 2.8e-12_r8 * exp_fac(:)
      rate(:,365) = 2.9e-12_r8 * exp_fac(:)
      rate(:,304) = 2.9e-12_r8 * exp( -345._r8 * itemp(:) )
      rate(:,306) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:) )
      exp_fac(:) = exp( 700._r8 * itemp(:) )
      rate(:,310) = 7.5e-13_r8 * exp_fac(:)
      rate(:,324) = 7.5e-13_r8 * exp_fac(:)
      rate(:,339) = 7.5e-13_r8 * exp_fac(:)
      rate(:,352) = 7.5e-13_r8 * exp_fac(:)
      rate(:,360) = 7.5e-13_r8 * exp_fac(:)
      rate(:,364) = 8.6e-13_r8 * exp_fac(:)
      rate(:,376) = 8e-13_r8 * exp_fac(:)
      rate(:,389) = 7.5e-13_r8 * exp_fac(:)
      rate(:,399) = 7.5e-13_r8 * exp_fac(:)
      rate(:,409) = 8e-13_r8 * exp_fac(:)
      rate(:,414) = 8e-13_r8 * exp_fac(:)
      rate(:,423) = 8e-13_r8 * exp_fac(:)
      rate(:,434) = 8e-13_r8 * exp_fac(:)
      rate(:,441) = 7.5e-13_r8 * exp_fac(:)
      rate(:,445) = 7.5e-13_r8 * exp_fac(:)
      rate(:,448) = 7.5e-13_r8 * exp_fac(:)
      rate(:,461) = 7.5e-13_r8 * exp_fac(:)
      rate(:,468) = 7.5e-13_r8 * exp_fac(:)
      rate(:,474) = 7.5e-13_r8 * exp_fac(:)
      rate(:,477) = 7.5e-13_r8 * exp_fac(:)
      rate(:,488) = 7.5e-13_r8 * exp_fac(:)
      rate(:,493) = 7.5e-13_r8 * exp_fac(:)
      rate(:,498) = 7.5e-13_r8 * exp_fac(:)
      rate(:,311) = 2.4e+12_r8 * exp( -7000._r8 * itemp(:) )
      rate(:,312) = 2.6e-12_r8 * exp( 265._r8 * itemp(:) )
      rate(:,316) = 1.08e-10_r8 * exp( 105._r8 * itemp(:) )
      exp_fac(:) = exp( -2630._r8 * itemp(:) )
      rate(:,321) = 1.2e-14_r8 * exp_fac(:)
      rate(:,583) = 1.2e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 365._r8 * itemp(:) )
      rate(:,325) = 2.6e-12_r8 * exp_fac(:)
      rate(:,442) = 2.6e-12_r8 * exp_fac(:)
      rate(:,447) = 2.6e-12_r8 * exp_fac(:)
      rate(:,449) = 2.6e-12_r8 * exp_fac(:)
      rate(:,462) = 2.6e-12_r8 * exp_fac(:)
      rate(:,469) = 2.6e-12_r8 * exp_fac(:)
      rate(:,475) = 2.6e-12_r8 * exp_fac(:)
      rate(:,478) = 2.6e-12_r8 * exp_fac(:)
      rate(:,326) = 6.9e-12_r8 * exp( -230._r8 * itemp(:) )
      rate(:,328) = 7.2e-11_r8 * exp( -70._r8 * itemp(:) )
      rate(:,329) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:) )
      exp_fac(:) = exp( -1900._r8 * itemp(:) )
      rate(:,330) = 1.4e-12_r8 * exp_fac(:)
      rate(:,350) = 6.5e-15_r8 * exp_fac(:)
      rate(:,584) = 6.5e-15_r8 * exp_fac(:)
      rate(:,331) = 4.63e-12_r8 * exp( 350._r8 * itemp(:) )
      rate(:,332) = 7.8e-13_r8 * exp( -1050._r8 * itemp(:) )
      exp_fac(:) = exp( 500._r8 * itemp(:) )
      rate(:,333) = 2.9e-12_r8 * exp_fac(:)
      rate(:,334) = 2e-12_r8 * exp_fac(:)
      rate(:,363) = 7.1e-13_r8 * exp_fac(:)
      rate(:,384) = 2e-12_r8 * exp_fac(:)
      rate(:,487) = 2e-12_r8 * exp_fac(:)
      rate(:,492) = 2e-12_r8 * exp_fac(:)
      rate(:,497) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 1040._r8 * itemp(:) )
      rate(:,335) = 4.3e-13_r8 * exp_fac(:)
      rate(:,385) = 4.3e-13_r8 * exp_fac(:)
      rate(:,438) = 4.3e-13_r8 * exp_fac(:)
      rate(:,452) = 4.3e-13_r8 * exp_fac(:)
      rate(:,455) = 4.3e-13_r8 * exp_fac(:)
      rate(:,458) = 4.3e-13_r8 * exp_fac(:)
      rate(:,341) = 1.6e+11_r8 * exp( -4150._r8 * itemp(:) )
      rate(:,349) = 4.6e-13_r8 * exp( -1156._r8 * itemp(:) )
      rate(:,351) = 3.75e-13_r8 * exp( -40._r8 * itemp(:) )
      rate(:,355) = 8.7e-12_r8 * exp( -615._r8 * itemp(:) )
      rate(:,356) = 1.4e-12_r8 * exp( -1860._r8 * itemp(:) )
      rate(:,357) = 8.4e-13_r8 * exp( 830._r8 * itemp(:) )
      rate(:,371) = 4.8e-12_r8 * exp( 120._r8 * itemp(:) )
      rate(:,372) = 5.1e-14_r8 * exp( 693._r8 * itemp(:) )
      exp_fac(:) = exp( 360._r8 * itemp(:) )
      rate(:,378) = 2.7e-12_r8 * exp_fac(:)
      rate(:,379) = 1.3e-13_r8 * exp_fac(:)
      rate(:,381) = 9.6e-12_r8 * exp_fac(:)
      rate(:,387) = 5.3e-12_r8 * exp_fac(:)
      rate(:,424) = 2.7e-12_r8 * exp_fac(:)
      rate(:,435) = 2.7e-12_r8 * exp_fac(:)
      rate(:,380) = 1.5e-15_r8 * exp( -2100._r8 * itemp(:) )
      exp_fac(:) = exp( 530._r8 * itemp(:) )
      rate(:,383) = 4.6e-12_r8 * exp_fac(:)
      rate(:,386) = 2.3e-12_r8 * exp_fac(:)
      rate(:,391) = 2.3e-12_r8 * exp( -170._r8 * itemp(:) )
      rate(:,395) = 4.13e-12_r8 * exp( 452._r8 * itemp(:) )
      rate(:,401) = 5.4e-14_r8 * exp( 870._r8 * itemp(:) )
      exp_fac(:) = exp( 175._r8 * itemp(:) )
      rate(:,404) = 1.86e-11_r8 * exp_fac(:)
      rate(:,405) = 1.86e-11_r8 * exp_fac(:)
      rate(:,415) = 1.6e+09_r8 * exp( -8300._r8 * itemp(:) )
      exp_fac(:) = exp( -446._r8 * itemp(:) )
      rate(:,420) = 3.03e-12_r8 * exp_fac(:)
      rate(:,540) = 3.03e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 410._r8 * itemp(:) )
      rate(:,428) = 2.54e-11_r8 * exp_fac(:)
      rate(:,542) = 2.54e-11_r8 * exp_fac(:)
      rate(:,432) = 1.3e-12_r8 * exp( 640._r8 * itemp(:) )
      exp_fac(:) = exp( -193._r8 * itemp(:) )
      rate(:,440) = 2.3e-12_r8 * exp_fac(:)
      rate(:,539) = 2.3e-12_r8 * exp_fac(:)
      rate(:,444) = 5.9e-12_r8 * exp( 225._r8 * itemp(:) )
      rate(:,463) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:) )
      exp_fac(:) = exp( 352._r8 * itemp(:) )
      rate(:,471) = 1.7e-12_r8 * exp_fac(:)
      rate(:,548) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 490._r8 * itemp(:) )
      rate(:,484) = 1.2e-12_r8 * exp_fac(:)
      rate(:,544) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -580._r8 * itemp(:) )
      rate(:,485) = 6.3e-16_r8 * exp_fac(:)
      rate(:,545) = 6.3e-16_r8 * exp_fac(:)
      rate(:,589) = 6.3e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( 440._r8 * itemp(:) )
      rate(:,486) = 1.2e-11_r8 * exp_fac(:)
      rate(:,546) = 1.2e-11_r8 * exp_fac(:)
      rate(:,504) = 2.1e-11_r8 * exp( -2200._r8 * itemp(:) )
      rate(:,505) = 7.2e-14_r8 * exp( -1070._r8 * itemp(:) )
      rate(:,512) = 1.6e-13_r8 * exp( -2280._r8 * itemp(:) )
      rate(:,515) = 2.7e-11_r8 * exp( 335._r8 * itemp(:) )
      rate(:,518) = 1.9e-13_r8 * exp( 520._r8 * itemp(:) )
      rate(:,519) = 9.6e-12_r8 * exp( -234._r8 * itemp(:) )
      rate(:,520) = 1.7e-12_r8 * exp( -710._r8 * itemp(:) )
      rate(:,588) = 4.4e-15_r8 * exp( -2500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)
 
      n = ncol*pver

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( rate(:,177), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.9e-31_r8 * itemp(:)**1._r8
      kinf(:) = 2.6e-11_r8
      call jpl( rate(:,187), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,199), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,207), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,210), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,211), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,212), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,230), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-32_r8 * itemp(:)**3.6_r8
      kinf(:) = 3.7e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,250), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,261), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,307), m, 0.6_r8, ko, kinf, n )

      ko(:) = 4.28e-33_r8
      kinf(:) = 9.3e-15_r8 * itemp(:)**(-4.42_r8)
      call jpl( rate(:,308), m, 0.8_r8, ko, kinf, n )

      ko(:) = 5.2e-30_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.2e-10_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,318), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.5e-30_r8
      kinf(:) = 8.3e-13_r8 * itemp(:)**(-2._r8)
      call jpl( rate(:,319), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.6e-29_r8 * itemp(:)**3.3_r8
      kinf(:) = 3.1e-10_r8 * itemp(:)
      call jpl( rate(:,320), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8.6e-29_r8 * itemp(:)**3.1_r8
      kinf(:) = 9e-12_r8 * itemp(:)**0.85_r8
      call jpl( rate(:,346), m, 0.48_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,347), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,367), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,393), m, 0.5_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,454), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,457), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,460), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,467), m, 0.6_r8, ko, kinf, n )

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

      rate(:n,161) = 1e-20_r8
      rate(:n,162) = 1.3e-16_r8
      rate(:n,166) = 8e-14_r8
      rate(:n,167) = 3.9e-17_r8
      rate(:n,174) = 6.9e-12_r8
      rate(:n,190) = 7e-13_r8
      rate(:n,191) = 5e-12_r8
      rate(:n,570) = 6e-11_r8
      rate(:n,573) = 1e-12_r8
      rate(:n,574) = 4e-10_r8
      rate(:n,575) = 2e-10_r8
      rate(:n,576) = 1e-10_r8
      rate(:n,578) = 4.4e-10_r8
 
      do k = 1,kbot
        offset = (k-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,k)
      end do

      rate(:n,157) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      exp_fac(:) = exp( 55._r8 * itemp(:) )
      rate(:n,158) = 2.64e-11_r8 * exp_fac(:)
      rate(:n,159) = 6.6e-12_r8 * exp_fac(:)
      rate(:n,163) = 3.6e-18_r8 * exp( -220._r8 * itemp(:) )
      rate(:n,165) = 1.8e-15_r8 * exp( 45._r8 * itemp(:) )
      rate(:n,168) = 3.5e-11_r8 * exp( -135._r8 * itemp(:) )
      rate(:n,169) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:n,178) = 3e-11_r8 * exp( 200._r8 * itemp(:) )
      rate(:n,179) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:n,180) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:n,183) = 4.8e-11_r8 * exp( 250._r8 * itemp(:) )
      rate(:n,184) = 1.8e-11_r8 * exp( 180._r8 * itemp(:) )
      rate(:n,185) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:n,192) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      rate(:n,196) = 1.5e-11_r8 * exp( -3600._r8 * itemp(:) )
      rate(:n,197) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      rate(:n,205) = 3.3e-12_r8 * exp( 270._r8 * itemp(:) )
      rate(:n,206) = 3e-12_r8 * exp( -1500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( wrk, m, 0.6_r8, ko, kinf, n )
      rate(:n,177) = wrk(:)























      end subroutine setrxt_hrates

      end module mo_setrxt
