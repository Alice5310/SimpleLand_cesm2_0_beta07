      module mo_nln_matrix
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only: veclen
      private
      public :: nlnmat
      contains
      subroutine nlnmat01( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,536) = -(rxt(k,358)*y(k,208))
         mat(k,1468) = -rxt(k,358)*y(k,1)
         mat(k,1942) = rxt(k,361)*y(k,185)
         mat(k,809) = rxt(k,361)*y(k,121)
         mat(k,512) = -(rxt(k,362)*y(k,208))
         mat(k,1466) = -rxt(k,362)*y(k,2)
         mat(k,808) = rxt(k,359)*y(k,197)
         mat(k,1836) = rxt(k,359)*y(k,185)
         mat(k,764) = -(rxt(k,441)*y(k,123) + rxt(k,442)*y(k,131) + rxt(k,443) &
                      *y(k,208))
         mat(k,1657) = -rxt(k,441)*y(k,5)
         mat(k,1761) = -rxt(k,442)*y(k,5)
         mat(k,1490) = -rxt(k,443)*y(k,5)
         mat(k,82) = -(rxt(k,400)*y(k,208))
         mat(k,1403) = -rxt(k,400)*y(k,6)
         mat(k,285) = -(rxt(k,403)*y(k,208))
         mat(k,1435) = -rxt(k,403)*y(k,7)
         mat(k,375) = rxt(k,401)*y(k,197)
         mat(k,1814) = rxt(k,401)*y(k,186)
         mat(k,83) = .120_r8*rxt(k,400)*y(k,208)
         mat(k,1404) = .120_r8*rxt(k,400)*y(k,6)
         mat(k,761) = .100_r8*rxt(k,442)*y(k,131)
         mat(k,787) = .100_r8*rxt(k,445)*y(k,131)
         mat(k,1750) = .100_r8*rxt(k,442)*y(k,5) + .100_r8*rxt(k,445)*y(k,109)
         mat(k,1930) = .500_r8*rxt(k,402)*y(k,186) + .200_r8*rxt(k,429)*y(k,214) &
                      + .060_r8*rxt(k,435)*y(k,216)
         mat(k,376) = .500_r8*rxt(k,402)*y(k,121)
         mat(k,593) = .200_r8*rxt(k,429)*y(k,121)
         mat(k,609) = .060_r8*rxt(k,435)*y(k,121)
         mat(k,1923) = .200_r8*rxt(k,429)*y(k,214) + .200_r8*rxt(k,435)*y(k,216)
         mat(k,592) = .200_r8*rxt(k,429)*y(k,121)
         mat(k,607) = .200_r8*rxt(k,435)*y(k,121)
         mat(k,1939) = .200_r8*rxt(k,429)*y(k,214) + .150_r8*rxt(k,435)*y(k,216)
         mat(k,594) = .200_r8*rxt(k,429)*y(k,121)
         mat(k,610) = .150_r8*rxt(k,435)*y(k,121)
         mat(k,1925) = .210_r8*rxt(k,435)*y(k,216)
         mat(k,608) = .210_r8*rxt(k,435)*y(k,121)
         mat(k,156) = -(rxt(k,363)*y(k,208))
         mat(k,1415) = -rxt(k,363)*y(k,14)
         mat(k,760) = .050_r8*rxt(k,442)*y(k,131)
         mat(k,786) = .050_r8*rxt(k,445)*y(k,131)
         mat(k,1749) = .050_r8*rxt(k,442)*y(k,5) + .050_r8*rxt(k,445)*y(k,109)
         mat(k,259) = -(rxt(k,329)*y(k,123) + rxt(k,330)*y(k,208))
         mat(k,1651) = -rxt(k,329)*y(k,15)
         mat(k,1431) = -rxt(k,330)*y(k,15)
         mat(k,1251) = -(rxt(k,212)*y(k,41) + rxt(k,213)*y(k,197) + rxt(k,214) &
                      *y(k,131))
         mat(k,1294) = -rxt(k,212)*y(k,16)
         mat(k,1879) = -rxt(k,213)*y(k,16)
         mat(k,1786) = -rxt(k,214)*y(k,16)
         mat(k,1544) = 4.000_r8*rxt(k,215)*y(k,18) + (rxt(k,216)+rxt(k,217))*y(k,58) &
                      + rxt(k,220)*y(k,121) + rxt(k,223)*y(k,130) + rxt(k,468) &
                      *y(k,147) + rxt(k,224)*y(k,208)
         mat(k,1319) = (rxt(k,216)+rxt(k,217))*y(k,18)
         mat(k,699) = rxt(k,225)*y(k,130) + rxt(k,231)*y(k,207) + rxt(k,226)*y(k,208)
         mat(k,1980) = rxt(k,220)*y(k,18)
         mat(k,1349) = rxt(k,223)*y(k,18) + rxt(k,225)*y(k,80)
         mat(k,1088) = rxt(k,468)*y(k,18)
         mat(k,1373) = rxt(k,231)*y(k,80)
         mat(k,1520) = rxt(k,224)*y(k,18) + rxt(k,226)*y(k,80)
         mat(k,1538) = rxt(k,218)*y(k,58)
         mat(k,1313) = rxt(k,218)*y(k,18)
         mat(k,1898) = (rxt(k,518)+rxt(k,523))*y(k,90)
         mat(k,642) = (rxt(k,518)+rxt(k,523))*y(k,84)
         mat(k,1552) = -(4._r8*rxt(k,215)*y(k,18) + (rxt(k,216) + rxt(k,217) + rxt(k,218) &
                      ) * y(k,58) + rxt(k,219)*y(k,197) + rxt(k,220)*y(k,121) &
                      + rxt(k,221)*y(k,122) + rxt(k,223)*y(k,130) + rxt(k,224) &
                      *y(k,208) + rxt(k,468)*y(k,147))
         mat(k,1327) = -(rxt(k,216) + rxt(k,217) + rxt(k,218)) * y(k,18)
         mat(k,1887) = -rxt(k,219)*y(k,18)
         mat(k,1988) = -rxt(k,220)*y(k,18)
         mat(k,1734) = -rxt(k,221)*y(k,18)
         mat(k,1357) = -rxt(k,223)*y(k,18)
         mat(k,1528) = -rxt(k,224)*y(k,18)
         mat(k,1093) = -rxt(k,468)*y(k,18)
         mat(k,1257) = rxt(k,214)*y(k,131)
         mat(k,437) = rxt(k,222)*y(k,130)
         mat(k,704) = rxt(k,232)*y(k,207)
         mat(k,647) = rxt(k,227)*y(k,130)
         mat(k,1357) = mat(k,1357) + rxt(k,222)*y(k,19) + rxt(k,227)*y(k,90)
         mat(k,1794) = rxt(k,214)*y(k,16)
         mat(k,1381) = rxt(k,232)*y(k,80)
         mat(k,432) = -(rxt(k,222)*y(k,130))
         mat(k,1339) = -rxt(k,222)*y(k,19)
         mat(k,1540) = rxt(k,221)*y(k,122)
         mat(k,1710) = rxt(k,221)*y(k,18)
         mat(k,165) = -(rxt(k,404)*y(k,208))
         mat(k,1417) = -rxt(k,404)*y(k,21)
         mat(k,1921) = rxt(k,407)*y(k,187)
         mat(k,333) = rxt(k,407)*y(k,121)
         mat(k,241) = -(rxt(k,406)*y(k,208))
         mat(k,1428) = -rxt(k,406)*y(k,22)
         mat(k,334) = rxt(k,405)*y(k,197)
         mat(k,1811) = rxt(k,405)*y(k,187)
         mat(k,200) = -(rxt(k,278)*y(k,55) + rxt(k,279)*y(k,208))
         mat(k,1613) = -rxt(k,278)*y(k,23)
         mat(k,1422) = -rxt(k,279)*y(k,23)
         mat(k,440) = -(rxt(k,280)*y(k,55) + rxt(k,281)*y(k,131) + rxt(k,306)*y(k,208))
         mat(k,1615) = -rxt(k,280)*y(k,24)
         mat(k,1753) = -rxt(k,281)*y(k,24)
         mat(k,1456) = -rxt(k,306)*y(k,24)
         mat(k,173) = -(rxt(k,286)*y(k,208))
         mat(k,1419) = -rxt(k,286)*y(k,25)
         mat(k,687) = .800_r8*rxt(k,282)*y(k,188) + .200_r8*rxt(k,283)*y(k,192)
         mat(k,1562) = .200_r8*rxt(k,283)*y(k,188)
         mat(k,246) = -(rxt(k,287)*y(k,208))
         mat(k,1429) = -rxt(k,287)*y(k,26)
         mat(k,688) = rxt(k,284)*y(k,197)
         mat(k,1812) = rxt(k,284)*y(k,188)
         mat(k,206) = -(rxt(k,288)*y(k,55) + rxt(k,289)*y(k,208))
         mat(k,1614) = -rxt(k,288)*y(k,27)
         mat(k,1423) = -rxt(k,289)*y(k,27)
         mat(k,852) = -(rxt(k,309)*y(k,123) + rxt(k,310)*y(k,131) + rxt(k,327) &
                      *y(k,208))
         mat(k,1662) = -rxt(k,309)*y(k,28)
         mat(k,1766) = -rxt(k,310)*y(k,28)
         mat(k,1496) = -rxt(k,327)*y(k,28)
         mat(k,720) = .130_r8*rxt(k,387)*y(k,131)
         mat(k,1766) = mat(k,1766) + .130_r8*rxt(k,387)*y(k,97)
         mat(k,315) = -(rxt(k,314)*y(k,208))
         mat(k,1439) = -rxt(k,314)*y(k,29)
         mat(k,664) = rxt(k,312)*y(k,197)
         mat(k,1818) = rxt(k,312)*y(k,189)
         mat(k,56) = -(rxt(k,315)*y(k,208))
         mat(k,1400) = -rxt(k,315)*y(k,30)
         mat(k,177) = -(rxt(k,410)*y(k,208))
         mat(k,1420) = -rxt(k,410)*y(k,31)
         mat(k,503) = rxt(k,408)*y(k,197)
         mat(k,1806) = rxt(k,408)*y(k,190)
         mat(k,1297) = -(rxt(k,176)*y(k,55) + rxt(k,212)*y(k,16) + rxt(k,256)*y(k,197) &
                      + rxt(k,257)*y(k,123) + rxt(k,258)*y(k,130) + rxt(k,259) &
                      *y(k,208))
         mat(k,1631) = -rxt(k,176)*y(k,41)
         mat(k,1253) = -rxt(k,212)*y(k,41)
         mat(k,1882) = -rxt(k,256)*y(k,41)
         mat(k,1688) = -rxt(k,257)*y(k,41)
         mat(k,1352) = -rxt(k,258)*y(k,41)
         mat(k,1523) = -rxt(k,259)*y(k,41)
         mat(k,542) = .400_r8*rxt(k,358)*y(k,208)
         mat(k,775) = .340_r8*rxt(k,442)*y(k,131)
         mat(k,263) = .500_r8*rxt(k,329)*y(k,123)
         mat(k,444) = rxt(k,281)*y(k,131)
         mat(k,858) = .500_r8*rxt(k,310)*y(k,131)
         mat(k,405) = .500_r8*rxt(k,298)*y(k,208)
         mat(k,676) = rxt(k,264)*y(k,208)
         mat(k,323) = .300_r8*rxt(k,265)*y(k,208)
         mat(k,1322) = rxt(k,183)*y(k,192)
         mat(k,870) = .800_r8*rxt(k,303)*y(k,208)
         mat(k,728) = .910_r8*rxt(k,387)*y(k,131)
         mat(k,480) = .300_r8*rxt(k,378)*y(k,208)
         mat(k,1055) = .800_r8*rxt(k,382)*y(k,192)
         mat(k,1070) = .120_r8*rxt(k,340)*y(k,131)
         mat(k,463) = .500_r8*rxt(k,353)*y(k,208)
         mat(k,801) = .340_r8*rxt(k,445)*y(k,131)
         mat(k,1138) = .600_r8*rxt(k,354)*y(k,131)
         mat(k,1983) = .100_r8*rxt(k,360)*y(k,185) + rxt(k,263)*y(k,192) &
                      + .500_r8*rxt(k,331)*y(k,194) + .500_r8*rxt(k,300)*y(k,196) &
                      + .920_r8*rxt(k,370)*y(k,199) + .250_r8*rxt(k,338)*y(k,201) &
                      + rxt(k,347)*y(k,203) + rxt(k,321)*y(k,210) + rxt(k,325) &
                      *y(k,211) + .340_r8*rxt(k,454)*y(k,212) + .320_r8*rxt(k,459) &
                      *y(k,213) + .250_r8*rxt(k,395)*y(k,215)
         mat(k,1688) = mat(k,1688) + .500_r8*rxt(k,329)*y(k,15) + rxt(k,371)*y(k,199) &
                      + .250_r8*rxt(k,337)*y(k,201) + rxt(k,348)*y(k,203)
         mat(k,1789) = .340_r8*rxt(k,442)*y(k,5) + rxt(k,281)*y(k,24) &
                      + .500_r8*rxt(k,310)*y(k,28) + .910_r8*rxt(k,387)*y(k,97) &
                      + .120_r8*rxt(k,340)*y(k,104) + .340_r8*rxt(k,445)*y(k,109) &
                      + .600_r8*rxt(k,354)*y(k,110)
         mat(k,359) = rxt(k,305)*y(k,208)
         mat(k,902) = .680_r8*rxt(k,463)*y(k,208)
         mat(k,816) = .100_r8*rxt(k,360)*y(k,121)
         mat(k,692) = .700_r8*rxt(k,283)*y(k,192)
         mat(k,668) = rxt(k,311)*y(k,192)
         mat(k,1240) = rxt(k,294)*y(k,192) + rxt(k,367)*y(k,199) + .250_r8*rxt(k,334) &
                      *y(k,201) + rxt(k,343)*y(k,203) + .250_r8*rxt(k,392)*y(k,215)
         mat(k,1597) = rxt(k,183)*y(k,58) + .800_r8*rxt(k,382)*y(k,100) + rxt(k,263) &
                      *y(k,121) + .700_r8*rxt(k,283)*y(k,188) + rxt(k,311)*y(k,189) &
                      + rxt(k,294)*y(k,191) + (4.000_r8*rxt(k,260)+2.000_r8*rxt(k,261)) &
                      *y(k,192) + 1.500_r8*rxt(k,368)*y(k,199) + .750_r8*rxt(k,373) &
                      *y(k,200) + .880_r8*rxt(k,335)*y(k,201) + 2.000_r8*rxt(k,344) &
                      *y(k,203) + .750_r8*rxt(k,447)*y(k,206) + .800_r8*rxt(k,323) &
                      *y(k,211) + .930_r8*rxt(k,452)*y(k,212) + .950_r8*rxt(k,457) &
                      *y(k,213) + .800_r8*rxt(k,393)*y(k,215)
         mat(k,456) = .500_r8*rxt(k,331)*y(k,121)
         mat(k,581) = .500_r8*rxt(k,300)*y(k,121)
         mat(k,1882) = mat(k,1882) + .450_r8*rxt(k,345)*y(k,203) + .150_r8*rxt(k,324) &
                      *y(k,211)
         mat(k,1118) = .920_r8*rxt(k,370)*y(k,121) + rxt(k,371)*y(k,123) + rxt(k,367) &
                      *y(k,191) + 1.500_r8*rxt(k,368)*y(k,192)
         mat(k,1192) = .750_r8*rxt(k,373)*y(k,192)
         mat(k,1160) = .250_r8*rxt(k,338)*y(k,121) + .250_r8*rxt(k,337)*y(k,123) &
                      + .250_r8*rxt(k,334)*y(k,191) + .880_r8*rxt(k,335)*y(k,192)
         mat(k,1210) = rxt(k,347)*y(k,121) + rxt(k,348)*y(k,123) + rxt(k,343)*y(k,191) &
                      + 2.000_r8*rxt(k,344)*y(k,192) + .450_r8*rxt(k,345)*y(k,197) &
                      + 4.000_r8*rxt(k,346)*y(k,203)
         mat(k,988) = .750_r8*rxt(k,447)*y(k,192)
         mat(k,1523) = mat(k,1523) + .400_r8*rxt(k,358)*y(k,1) + .500_r8*rxt(k,298) &
                      *y(k,50) + rxt(k,264)*y(k,51) + .300_r8*rxt(k,265)*y(k,52) &
                      + .800_r8*rxt(k,303)*y(k,73) + .300_r8*rxt(k,378)*y(k,98) &
                      + .500_r8*rxt(k,353)*y(k,108) + rxt(k,305)*y(k,136) &
                      + .680_r8*rxt(k,463)*y(k,174)
         mat(k,636) = rxt(k,321)*y(k,121)
         mat(k,1002) = rxt(k,325)*y(k,121) + .800_r8*rxt(k,323)*y(k,192) &
                      + .150_r8*rxt(k,324)*y(k,197)
         mat(k,969) = .340_r8*rxt(k,454)*y(k,121) + .930_r8*rxt(k,452)*y(k,192)
         mat(k,949) = .320_r8*rxt(k,459)*y(k,121) + .950_r8*rxt(k,457)*y(k,192)
         mat(k,1032) = .250_r8*rxt(k,395)*y(k,121) + .250_r8*rxt(k,392)*y(k,191) &
                      + .800_r8*rxt(k,393)*y(k,192)
      end do
      end subroutine nlnmat01
      subroutine nlnmat02( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,888) = -(rxt(k,290)*y(k,123) + rxt(k,291)*y(k,208))
         mat(k,1665) = -rxt(k,290)*y(k,44)
         mat(k,1499) = -rxt(k,291)*y(k,44)
         mat(k,540) = .800_r8*rxt(k,358)*y(k,208)
         mat(k,262) = rxt(k,329)*y(k,123)
         mat(k,174) = rxt(k,286)*y(k,208)
         mat(k,248) = .500_r8*rxt(k,287)*y(k,208)
         mat(k,853) = .500_r8*rxt(k,310)*y(k,131)
         mat(k,1129) = .100_r8*rxt(k,354)*y(k,131)
         mat(k,1961) = .400_r8*rxt(k,360)*y(k,185) + rxt(k,285)*y(k,188) &
                      + .270_r8*rxt(k,313)*y(k,189) + rxt(k,331)*y(k,194) + rxt(k,350) &
                      *y(k,205) + rxt(k,321)*y(k,210)
         mat(k,1665) = mat(k,1665) + rxt(k,329)*y(k,15)
         mat(k,1768) = .500_r8*rxt(k,310)*y(k,28) + .100_r8*rxt(k,354)*y(k,110)
         mat(k,814) = .400_r8*rxt(k,360)*y(k,121)
         mat(k,691) = rxt(k,285)*y(k,121) + 3.200_r8*rxt(k,282)*y(k,188) &
                      + .800_r8*rxt(k,283)*y(k,192)
         mat(k,667) = .270_r8*rxt(k,313)*y(k,121)
         mat(k,1577) = .800_r8*rxt(k,283)*y(k,188)
         mat(k,454) = rxt(k,331)*y(k,121)
         mat(k,1860) = .200_r8*rxt(k,349)*y(k,205)
         mat(k,548) = rxt(k,350)*y(k,121) + .200_r8*rxt(k,349)*y(k,197)
         mat(k,1499) = mat(k,1499) + .800_r8*rxt(k,358)*y(k,1) + rxt(k,286)*y(k,25) &
                      + .500_r8*rxt(k,287)*y(k,26)
         mat(k,634) = rxt(k,321)*y(k,121)
         mat(k,53) = -(rxt(k,292)*y(k,208))
         mat(k,1399) = -rxt(k,292)*y(k,46)
         mat(k,822) = -(rxt(k,328)*y(k,208))
         mat(k,1493) = -rxt(k,328)*y(k,47)
         mat(k,539) = .800_r8*rxt(k,358)*y(k,208)
         mat(k,766) = .520_r8*rxt(k,442)*y(k,131)
         mat(k,261) = .500_r8*rxt(k,329)*y(k,123)
         mat(k,792) = .520_r8*rxt(k,445)*y(k,131)
         mat(k,1957) = .250_r8*rxt(k,360)*y(k,185) + .820_r8*rxt(k,313)*y(k,189) &
                      + .500_r8*rxt(k,331)*y(k,194) + .270_r8*rxt(k,454)*y(k,212) &
                      + .040_r8*rxt(k,459)*y(k,213)
         mat(k,1660) = .500_r8*rxt(k,329)*y(k,15)
         mat(k,1764) = .520_r8*rxt(k,442)*y(k,5) + .520_r8*rxt(k,445)*y(k,109)
         mat(k,897) = .500_r8*rxt(k,463)*y(k,208)
         mat(k,813) = .250_r8*rxt(k,360)*y(k,121)
         mat(k,666) = .820_r8*rxt(k,313)*y(k,121) + .820_r8*rxt(k,311)*y(k,192)
         mat(k,1573) = .820_r8*rxt(k,311)*y(k,189) + .150_r8*rxt(k,452)*y(k,212) &
                      + .025_r8*rxt(k,457)*y(k,213)
         mat(k,453) = .500_r8*rxt(k,331)*y(k,121)
         mat(k,1493) = mat(k,1493) + .800_r8*rxt(k,358)*y(k,1) + .500_r8*rxt(k,463) &
                      *y(k,174)
         mat(k,960) = .270_r8*rxt(k,454)*y(k,121) + .150_r8*rxt(k,452)*y(k,192)
         mat(k,938) = .040_r8*rxt(k,459)*y(k,121) + .025_r8*rxt(k,457)*y(k,192)
         mat(k,1076) = -(rxt(k,316)*y(k,123) + rxt(k,317)*y(k,208))
         mat(k,1677) = -rxt(k,316)*y(k,48)
         mat(k,1512) = -rxt(k,317)*y(k,48)
         mat(k,930) = rxt(k,318)*y(k,208)
         mat(k,1065) = .880_r8*rxt(k,340)*y(k,131)
         mat(k,1132) = .500_r8*rxt(k,354)*y(k,131)
         mat(k,1973) = .170_r8*rxt(k,413)*y(k,193) + .050_r8*rxt(k,376)*y(k,200) &
                      + .250_r8*rxt(k,338)*y(k,201) + .170_r8*rxt(k,419)*y(k,204) &
                      + .400_r8*rxt(k,429)*y(k,214) + .250_r8*rxt(k,395)*y(k,215) &
                      + .540_r8*rxt(k,435)*y(k,216) + .510_r8*rxt(k,438)*y(k,217)
         mat(k,1677) = mat(k,1677) + .050_r8*rxt(k,377)*y(k,200) + .250_r8*rxt(k,337) &
                      *y(k,201) + .250_r8*rxt(k,396)*y(k,215)
         mat(k,736) = rxt(k,319)*y(k,208)
         mat(k,1778) = .880_r8*rxt(k,340)*y(k,104) + .500_r8*rxt(k,354)*y(k,110)
         mat(k,1231) = .250_r8*rxt(k,334)*y(k,201) + .250_r8*rxt(k,392)*y(k,215)
         mat(k,1588) = .240_r8*rxt(k,335)*y(k,201) + .500_r8*rxt(k,323)*y(k,211) &
                      + .100_r8*rxt(k,393)*y(k,215)
         mat(k,626) = .170_r8*rxt(k,413)*y(k,121) + .070_r8*rxt(k,412)*y(k,197)
         mat(k,1872) = .070_r8*rxt(k,412)*y(k,193) + .070_r8*rxt(k,418)*y(k,204)
         mat(k,1184) = .050_r8*rxt(k,376)*y(k,121) + .050_r8*rxt(k,377)*y(k,123)
         mat(k,1154) = .250_r8*rxt(k,338)*y(k,121) + .250_r8*rxt(k,337)*y(k,123) &
                      + .250_r8*rxt(k,334)*y(k,191) + .240_r8*rxt(k,335)*y(k,192)
         mat(k,749) = .170_r8*rxt(k,419)*y(k,121) + .070_r8*rxt(k,418)*y(k,197)
         mat(k,1512) = mat(k,1512) + rxt(k,318)*y(k,94) + rxt(k,319)*y(k,124)
         mat(k,1000) = .500_r8*rxt(k,323)*y(k,192)
         mat(k,602) = .400_r8*rxt(k,429)*y(k,121)
         mat(k,1029) = .250_r8*rxt(k,395)*y(k,121) + .250_r8*rxt(k,396)*y(k,123) &
                      + .250_r8*rxt(k,392)*y(k,191) + .100_r8*rxt(k,393)*y(k,192)
         mat(k,618) = .540_r8*rxt(k,435)*y(k,121)
         mat(k,387) = .510_r8*rxt(k,438)*y(k,121)
         mat(k,448) = -(rxt(k,297)*y(k,208))
         mat(k,1457) = -rxt(k,297)*y(k,49)
         mat(k,848) = .120_r8*rxt(k,310)*y(k,131)
         mat(k,1754) = .120_r8*rxt(k,310)*y(k,28)
         mat(k,1222) = .100_r8*rxt(k,294)*y(k,192) + .150_r8*rxt(k,295)*y(k,197)
         mat(k,1566) = .100_r8*rxt(k,294)*y(k,191)
         mat(k,1832) = .150_r8*rxt(k,295)*y(k,191) + .150_r8*rxt(k,345)*y(k,203)
         mat(k,1202) = .150_r8*rxt(k,345)*y(k,197)
         mat(k,403) = -(rxt(k,298)*y(k,208))
         mat(k,1452) = -rxt(k,298)*y(k,50)
         mat(k,1221) = .400_r8*rxt(k,295)*y(k,197)
         mat(k,1829) = .400_r8*rxt(k,295)*y(k,191) + .400_r8*rxt(k,345)*y(k,203)
         mat(k,1201) = .400_r8*rxt(k,345)*y(k,197)
         mat(k,675) = -(rxt(k,264)*y(k,208))
         mat(k,1481) = -rxt(k,264)*y(k,51)
         mat(k,1042) = .200_r8*rxt(k,382)*y(k,192)
         mat(k,689) = .300_r8*rxt(k,283)*y(k,192)
         mat(k,1569) = .200_r8*rxt(k,382)*y(k,100) + .300_r8*rxt(k,283)*y(k,188) &
                      + 2.000_r8*rxt(k,261)*y(k,192) + .250_r8*rxt(k,368)*y(k,199) &
                      + .250_r8*rxt(k,373)*y(k,200) + .250_r8*rxt(k,335)*y(k,201) &
                      + .250_r8*rxt(k,447)*y(k,206) + .500_r8*rxt(k,323)*y(k,211) &
                      + .250_r8*rxt(k,452)*y(k,212) + .250_r8*rxt(k,457)*y(k,213) &
                      + .300_r8*rxt(k,393)*y(k,215)
         mat(k,1102) = .250_r8*rxt(k,368)*y(k,192)
         mat(k,1173) = .250_r8*rxt(k,373)*y(k,192)
         mat(k,1148) = .250_r8*rxt(k,335)*y(k,192)
         mat(k,978) = .250_r8*rxt(k,447)*y(k,192)
         mat(k,997) = .500_r8*rxt(k,323)*y(k,192)
         mat(k,959) = .250_r8*rxt(k,452)*y(k,192)
         mat(k,937) = .250_r8*rxt(k,457)*y(k,192)
         mat(k,1023) = .300_r8*rxt(k,393)*y(k,192)
         mat(k,321) = -(rxt(k,265)*y(k,208))
         mat(k,1440) = -rxt(k,265)*y(k,52)
         mat(k,1565) = rxt(k,262)*y(k,197)
         mat(k,1819) = rxt(k,262)*y(k,192)
         mat(k,1638) = -(rxt(k,176)*y(k,41) + rxt(k,178)*y(k,76) + rxt(k,179)*y(k,78) &
                      + (rxt(k,180) + rxt(k,181)) * y(k,197) + rxt(k,182)*y(k,131) &
                      + rxt(k,189)*y(k,59) + rxt(k,198)*y(k,91) + rxt(k,288)*y(k,27))
         mat(k,1303) = -rxt(k,176)*y(k,55)
         mat(k,1018) = -rxt(k,178)*y(k,55)
         mat(k,471) = -rxt(k,179)*y(k,55)
         mat(k,1889) = -(rxt(k,180) + rxt(k,181)) * y(k,55)
         mat(k,1796) = -rxt(k,182)*y(k,55)
         mat(k,835) = -rxt(k,189)*y(k,55)
         mat(k,684) = -rxt(k,198)*y(k,55)
         mat(k,209) = -rxt(k,288)*y(k,55)
         mat(k,1554) = rxt(k,217)*y(k,58)
         mat(k,1329) = rxt(k,217)*y(k,18) + (4.000_r8*rxt(k,184)+2.000_r8*rxt(k,186)) &
                      *y(k,58) + rxt(k,188)*y(k,121) + rxt(k,193)*y(k,130) &
                      + rxt(k,469)*y(k,147) + rxt(k,183)*y(k,192) + rxt(k,194) &
                      *y(k,208)
         mat(k,100) = rxt(k,238)*y(k,207)
         mat(k,1912) = rxt(k,196)*y(k,130) + rxt(k,208)*y(k,207) + rxt(k,197)*y(k,208)
         mat(k,1990) = rxt(k,188)*y(k,58)
         mat(k,1359) = rxt(k,193)*y(k,58) + rxt(k,196)*y(k,84)
         mat(k,1094) = rxt(k,469)*y(k,58)
         mat(k,1604) = rxt(k,183)*y(k,58)
         mat(k,1383) = rxt(k,238)*y(k,64) + rxt(k,208)*y(k,84)
         mat(k,1530) = rxt(k,194)*y(k,58) + rxt(k,197)*y(k,84)
         mat(k,1612) = rxt(k,189)*y(k,59)
         mat(k,1312) = 2.000_r8*rxt(k,185)*y(k,58)
         mat(k,828) = rxt(k,189)*y(k,55) + (rxt(k,516)+rxt(k,521)+rxt(k,526))*y(k,84)
         mat(k,1897) = (rxt(k,516)+rxt(k,521)+rxt(k,526))*y(k,59) + (rxt(k,511) &
                       +rxt(k,517)+rxt(k,522))*y(k,91)
         mat(k,679) = (rxt(k,511)+rxt(k,517)+rxt(k,522))*y(k,84)
         mat(k,1311) = 2.000_r8*rxt(k,210)*y(k,58)
         mat(k,1323) = -(rxt(k,183)*y(k,192) + (4._r8*rxt(k,184) + 4._r8*rxt(k,185) &
                      + 4._r8*rxt(k,186) + 4._r8*rxt(k,210)) * y(k,58) + rxt(k,187) &
                      *y(k,197) + rxt(k,188)*y(k,121) + rxt(k,190)*y(k,122) + rxt(k,193) &
                      *y(k,130) + (rxt(k,194) + rxt(k,195)) * y(k,208) + (rxt(k,216) &
                      + rxt(k,217) + rxt(k,218)) * y(k,18) + rxt(k,469)*y(k,147))
         mat(k,1598) = -rxt(k,183)*y(k,58)
         mat(k,1883) = -rxt(k,187)*y(k,58)
         mat(k,1984) = -rxt(k,188)*y(k,58)
         mat(k,1730) = -rxt(k,190)*y(k,58)
         mat(k,1353) = -rxt(k,193)*y(k,58)
         mat(k,1524) = -(rxt(k,194) + rxt(k,195)) * y(k,58)
         mat(k,1548) = -(rxt(k,216) + rxt(k,217) + rxt(k,218)) * y(k,58)
         mat(k,1090) = -rxt(k,469)*y(k,58)
         mat(k,1632) = rxt(k,198)*y(k,91) + rxt(k,182)*y(k,131) + rxt(k,181)*y(k,197)
         mat(k,832) = rxt(k,191)*y(k,130)
         mat(k,1906) = rxt(k,209)*y(k,207)
         mat(k,681) = rxt(k,198)*y(k,55) + rxt(k,199)*y(k,130) + rxt(k,200)*y(k,208)
         mat(k,1353) = mat(k,1353) + rxt(k,191)*y(k,59) + rxt(k,199)*y(k,91)
         mat(k,1790) = rxt(k,182)*y(k,55)
         mat(k,233) = rxt(k,474)*y(k,147)
         mat(k,1090) = mat(k,1090) + rxt(k,474)*y(k,133)
         mat(k,1883) = mat(k,1883) + rxt(k,181)*y(k,55)
         mat(k,1377) = rxt(k,209)*y(k,84)
         mat(k,1524) = mat(k,1524) + rxt(k,200)*y(k,91)
         mat(k,830) = -(rxt(k,189)*y(k,55) + rxt(k,191)*y(k,130) + rxt(k,192)*y(k,208) &
                      + (rxt(k,516) + rxt(k,521) + rxt(k,526)) * y(k,84))
         mat(k,1622) = -rxt(k,189)*y(k,59)
         mat(k,1345) = -rxt(k,191)*y(k,59)
         mat(k,1494) = -rxt(k,192)*y(k,59)
         mat(k,1901) = -(rxt(k,516) + rxt(k,521) + rxt(k,526)) * y(k,59)
         mat(k,1317) = rxt(k,190)*y(k,122)
         mat(k,1719) = rxt(k,190)*y(k,58)
         mat(k,907) = -((rxt(k,267) + rxt(k,277)) * y(k,208))
         mat(k,1501) = -(rxt(k,267) + rxt(k,277)) * y(k,61)
         mat(k,769) = .230_r8*rxt(k,442)*y(k,131)
         mat(k,1250) = rxt(k,212)*y(k,41)
         mat(k,203) = .350_r8*rxt(k,279)*y(k,208)
         mat(k,443) = .630_r8*rxt(k,281)*y(k,131)
         mat(k,854) = .560_r8*rxt(k,310)*y(k,131)
         mat(k,1292) = rxt(k,212)*y(k,16) + rxt(k,176)*y(k,55) + rxt(k,257)*y(k,123) &
                      + rxt(k,258)*y(k,130) + rxt(k,259)*y(k,208)
         mat(k,1075) = rxt(k,316)*y(k,123) + rxt(k,317)*y(k,208)
         mat(k,1625) = rxt(k,176)*y(k,41)
         mat(k,743) = rxt(k,304)*y(k,208)
         mat(k,721) = .620_r8*rxt(k,387)*y(k,131)
         mat(k,1063) = .650_r8*rxt(k,340)*y(k,131)
         mat(k,795) = .230_r8*rxt(k,445)*y(k,131)
         mat(k,1130) = .560_r8*rxt(k,354)*y(k,131)
         mat(k,1963) = .170_r8*rxt(k,413)*y(k,193) + .220_r8*rxt(k,338)*y(k,201) &
                      + .400_r8*rxt(k,416)*y(k,202) + .350_r8*rxt(k,419)*y(k,204) &
                      + .225_r8*rxt(k,454)*y(k,212) + .250_r8*rxt(k,395)*y(k,215)
         mat(k,1667) = rxt(k,257)*y(k,41) + rxt(k,316)*y(k,48) + .220_r8*rxt(k,337) &
                      *y(k,201) + .500_r8*rxt(k,396)*y(k,215)
         mat(k,1346) = rxt(k,258)*y(k,41) + rxt(k,464)*y(k,134)
         mat(k,1770) = .230_r8*rxt(k,442)*y(k,5) + .630_r8*rxt(k,281)*y(k,24) &
                      + .560_r8*rxt(k,310)*y(k,28) + .620_r8*rxt(k,387)*y(k,97) &
                      + .650_r8*rxt(k,340)*y(k,104) + .230_r8*rxt(k,445)*y(k,109) &
                      + .560_r8*rxt(k,354)*y(k,110)
         mat(k,254) = rxt(k,464)*y(k,130) + rxt(k,465)*y(k,208)
         mat(k,899) = .700_r8*rxt(k,463)*y(k,208)
         mat(k,1226) = .220_r8*rxt(k,334)*y(k,201) + .250_r8*rxt(k,392)*y(k,215)
         mat(k,1579) = .110_r8*rxt(k,335)*y(k,201) + .125_r8*rxt(k,452)*y(k,212) &
                      + .200_r8*rxt(k,393)*y(k,215)
         mat(k,625) = .170_r8*rxt(k,413)*y(k,121) + .070_r8*rxt(k,412)*y(k,197)
         mat(k,1862) = .070_r8*rxt(k,412)*y(k,193) + .160_r8*rxt(k,415)*y(k,202) &
                      + .140_r8*rxt(k,418)*y(k,204)
         mat(k,1150) = .220_r8*rxt(k,338)*y(k,121) + .220_r8*rxt(k,337)*y(k,123) &
                      + .220_r8*rxt(k,334)*y(k,191) + .110_r8*rxt(k,335)*y(k,192)
         mat(k,588) = .400_r8*rxt(k,416)*y(k,121) + .160_r8*rxt(k,415)*y(k,197)
         mat(k,748) = .350_r8*rxt(k,419)*y(k,121) + .140_r8*rxt(k,418)*y(k,197)
         mat(k,1501) = mat(k,1501) + .350_r8*rxt(k,279)*y(k,23) + rxt(k,259)*y(k,41) &
                      + rxt(k,317)*y(k,48) + rxt(k,304)*y(k,74) + rxt(k,465)*y(k,134) &
                      + .700_r8*rxt(k,463)*y(k,174)
         mat(k,963) = .225_r8*rxt(k,454)*y(k,121) + .125_r8*rxt(k,452)*y(k,192)
         mat(k,1026) = .250_r8*rxt(k,395)*y(k,121) + .500_r8*rxt(k,396)*y(k,123) &
                      + .250_r8*rxt(k,392)*y(k,191) + .200_r8*rxt(k,393)*y(k,192)
      end do
      end subroutine nlnmat02
      subroutine nlnmat03( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,60) = -(rxt(k,237)*y(k,207))
         mat(k,1367) = -rxt(k,237)*y(k,63)
         mat(k,97) = -(rxt(k,238)*y(k,207))
         mat(k,1368) = -rxt(k,238)*y(k,64)
         mat(k,117) = -(rxt(k,411)*y(k,208))
         mat(k,1409) = -rxt(k,411)*y(k,65)
         mat(k,111) = .180_r8*rxt(k,431)*y(k,208)
         mat(k,1409) = mat(k,1409) + .180_r8*rxt(k,431)*y(k,176)
         mat(k,194) = -(rxt(k,478)*y(k,123) + (rxt(k,479) + rxt(k,481)) * y(k,208))
         mat(k,1649) = -rxt(k,478)*y(k,66)
         mat(k,1421) = -(rxt(k,479) + rxt(k,481)) * y(k,66)
         mat(k,577) = rxt(k,299)*y(k,197)
         mat(k,1804) = rxt(k,299)*y(k,196)
         mat(k,652) = -(rxt(k,234)*y(k,76) + rxt(k,235)*y(k,218) + rxt(k,236)*y(k,88))
         mat(k,1010) = -rxt(k,234)*y(k,72)
         mat(k,2001) = -rxt(k,235)*y(k,72)
         mat(k,1262) = -rxt(k,236)*y(k,72)
         mat(k,61) = 2.000_r8*rxt(k,237)*y(k,207)
         mat(k,98) = rxt(k,238)*y(k,207)
         mat(k,1370) = 2.000_r8*rxt(k,237)*y(k,63) + rxt(k,238)*y(k,64)
         mat(k,868) = -(rxt(k,303)*y(k,208))
         mat(k,1497) = -rxt(k,303)*y(k,73)
         mat(k,476) = .700_r8*rxt(k,378)*y(k,208)
         mat(k,418) = .500_r8*rxt(k,379)*y(k,208)
         mat(k,281) = rxt(k,390)*y(k,208)
         mat(k,1959) = .050_r8*rxt(k,376)*y(k,200) + .530_r8*rxt(k,338)*y(k,201) &
                      + .225_r8*rxt(k,454)*y(k,212) + .250_r8*rxt(k,395)*y(k,215)
         mat(k,1663) = .050_r8*rxt(k,377)*y(k,200) + .530_r8*rxt(k,337)*y(k,201) &
                      + .250_r8*rxt(k,396)*y(k,215)
         mat(k,1224) = .530_r8*rxt(k,334)*y(k,201) + .250_r8*rxt(k,392)*y(k,215)
         mat(k,1575) = .260_r8*rxt(k,335)*y(k,201) + .125_r8*rxt(k,452)*y(k,212) &
                      + .100_r8*rxt(k,393)*y(k,215)
         mat(k,1177) = .050_r8*rxt(k,376)*y(k,121) + .050_r8*rxt(k,377)*y(k,123)
         mat(k,1149) = .530_r8*rxt(k,338)*y(k,121) + .530_r8*rxt(k,337)*y(k,123) &
                      + .530_r8*rxt(k,334)*y(k,191) + .260_r8*rxt(k,335)*y(k,192)
         mat(k,1497) = mat(k,1497) + .700_r8*rxt(k,378)*y(k,98) + .500_r8*rxt(k,379) &
                      *y(k,99) + rxt(k,390)*y(k,114)
         mat(k,961) = .225_r8*rxt(k,454)*y(k,121) + .125_r8*rxt(k,452)*y(k,192)
         mat(k,1025) = .250_r8*rxt(k,395)*y(k,121) + .250_r8*rxt(k,396)*y(k,123) &
                      + .250_r8*rxt(k,392)*y(k,191) + .100_r8*rxt(k,393)*y(k,192)
         mat(k,742) = -(rxt(k,304)*y(k,208))
         mat(k,1488) = -rxt(k,304)*y(k,74)
         mat(k,202) = .650_r8*rxt(k,279)*y(k,208)
         mat(k,867) = .200_r8*rxt(k,303)*y(k,208)
         mat(k,875) = rxt(k,391)*y(k,208)
         mat(k,1954) = rxt(k,402)*y(k,186) + .050_r8*rxt(k,376)*y(k,200) &
                      + .400_r8*rxt(k,416)*y(k,202) + .170_r8*rxt(k,419)*y(k,204) &
                      + .700_r8*rxt(k,422)*y(k,209) + .600_r8*rxt(k,429)*y(k,214) &
                      + .250_r8*rxt(k,395)*y(k,215) + .340_r8*rxt(k,435)*y(k,216) &
                      + .170_r8*rxt(k,438)*y(k,217)
         mat(k,1656) = .050_r8*rxt(k,377)*y(k,200) + .250_r8*rxt(k,396)*y(k,215)
         mat(k,379) = rxt(k,402)*y(k,121)
         mat(k,1223) = .250_r8*rxt(k,392)*y(k,215)
         mat(k,1572) = .100_r8*rxt(k,393)*y(k,215)
         mat(k,1854) = .160_r8*rxt(k,415)*y(k,202) + .070_r8*rxt(k,418)*y(k,204)
         mat(k,1175) = .050_r8*rxt(k,376)*y(k,121) + .050_r8*rxt(k,377)*y(k,123)
         mat(k,587) = .400_r8*rxt(k,416)*y(k,121) + .160_r8*rxt(k,415)*y(k,197)
         mat(k,746) = .170_r8*rxt(k,419)*y(k,121) + .070_r8*rxt(k,418)*y(k,197)
         mat(k,1488) = mat(k,1488) + .650_r8*rxt(k,279)*y(k,23) + .200_r8*rxt(k,303) &
                      *y(k,73) + rxt(k,391)*y(k,115)
         mat(k,349) = .700_r8*rxt(k,422)*y(k,121)
         mat(k,599) = .600_r8*rxt(k,429)*y(k,121)
         mat(k,1024) = .250_r8*rxt(k,395)*y(k,121) + .250_r8*rxt(k,396)*y(k,123) &
                      + .250_r8*rxt(k,392)*y(k,191) + .100_r8*rxt(k,393)*y(k,192)
         mat(k,615) = .340_r8*rxt(k,435)*y(k,121)
         mat(k,386) = .170_r8*rxt(k,438)*y(k,121)
         mat(k,1277) = -((rxt(k,136) + rxt(k,137) + rxt(k,138)) * y(k,197) + rxt(k,142) &
                      *y(k,131))
         mat(k,1881) = -(rxt(k,136) + rxt(k,137) + rxt(k,138)) * y(k,75)
         mat(k,1788) = -rxt(k,142)*y(k,75)
         mat(k,1296) = rxt(k,259)*y(k,208)
         mat(k,1630) = rxt(k,178)*y(k,76)
         mat(k,908) = rxt(k,277)*y(k,208)
         mat(k,655) = rxt(k,234)*y(k,76)
         mat(k,1013) = rxt(k,178)*y(k,55) + rxt(k,234)*y(k,72) + rxt(k,134)*y(k,130) &
                      + rxt(k,126)*y(k,207) + rxt(k,143)*y(k,208)
         mat(k,700) = rxt(k,232)*y(k,207)
         mat(k,1904) = rxt(k,209)*y(k,207)
         mat(k,268) = rxt(k,164)*y(k,208)
         mat(k,1351) = rxt(k,134)*y(k,76) + rxt(k,146)*y(k,208)
         mat(k,256) = rxt(k,465)*y(k,208)
         mat(k,394) = rxt(k,470)*y(k,208)
         mat(k,1089) = rxt(k,475)*y(k,208)
         mat(k,1375) = rxt(k,126)*y(k,76) + rxt(k,232)*y(k,80) + rxt(k,209)*y(k,84)
         mat(k,1522) = rxt(k,259)*y(k,41) + rxt(k,277)*y(k,61) + rxt(k,143)*y(k,76) &
                      + rxt(k,164)*y(k,111) + rxt(k,146)*y(k,130) + rxt(k,465) &
                      *y(k,134) + rxt(k,470)*y(k,145) + rxt(k,475)*y(k,147)
         mat(k,1011) = -(rxt(k,126)*y(k,207) + rxt(k,134)*y(k,130) + rxt(k,143) &
                      *y(k,208) + rxt(k,178)*y(k,55) + rxt(k,234)*y(k,72))
         mat(k,1372) = -rxt(k,126)*y(k,76)
         mat(k,1347) = -rxt(k,134)*y(k,76)
         mat(k,1508) = -rxt(k,143)*y(k,76)
         mat(k,1626) = -rxt(k,178)*y(k,76)
         mat(k,653) = -rxt(k,234)*y(k,76)
         mat(k,1275) = rxt(k,136)*y(k,197)
         mat(k,1868) = rxt(k,136)*y(k,75)
         mat(k,468) = -(rxt(k,135)*y(k,130) + rxt(k,144)*y(k,208) + rxt(k,179)*y(k,55))
         mat(k,1340) = -rxt(k,135)*y(k,78)
         mat(k,1460) = -rxt(k,144)*y(k,78)
         mat(k,1616) = -rxt(k,179)*y(k,78)
         mat(k,1833) = 2.000_r8*rxt(k,150)*y(k,197)
         mat(k,1460) = mat(k,1460) + 2.000_r8*rxt(k,149)*y(k,208)
         mat(k,168) = rxt(k,477)*y(k,218)
         mat(k,1998) = rxt(k,477)*y(k,149)
         mat(k,698) = -(rxt(k,225)*y(k,130) + rxt(k,226)*y(k,208) + (rxt(k,231) &
                      + rxt(k,232)) * y(k,207))
         mat(k,1343) = -rxt(k,225)*y(k,80)
         mat(k,1484) = -rxt(k,226)*y(k,80)
         mat(k,1371) = -(rxt(k,231) + rxt(k,232)) * y(k,80)
         mat(k,1249) = rxt(k,212)*y(k,41) + rxt(k,213)*y(k,197)
         mat(k,1291) = rxt(k,212)*y(k,16)
         mat(k,1852) = rxt(k,213)*y(k,16)
         mat(k,1917) = -(rxt(k,196)*y(k,130) + rxt(k,197)*y(k,208) + (rxt(k,208) &
                      + rxt(k,209)) * y(k,207) + (rxt(k,511) + rxt(k,517) + rxt(k,522) &
                      ) * y(k,91) + (rxt(k,516) + rxt(k,521) + rxt(k,526)) * y(k,59) &
                      + (rxt(k,518) + rxt(k,523)) * y(k,90))
         mat(k,1364) = -rxt(k,196)*y(k,84)
         mat(k,1535) = -rxt(k,197)*y(k,84)
         mat(k,1388) = -(rxt(k,208) + rxt(k,209)) * y(k,84)
         mat(k,685) = -(rxt(k,511) + rxt(k,517) + rxt(k,522)) * y(k,84)
         mat(k,838) = -(rxt(k,516) + rxt(k,521) + rxt(k,526)) * y(k,84)
         mat(k,649) = -(rxt(k,518) + rxt(k,523)) * y(k,84)
         mat(k,210) = rxt(k,288)*y(k,55)
         mat(k,1308) = rxt(k,176)*y(k,55)
         mat(k,1643) = rxt(k,288)*y(k,27) + rxt(k,176)*y(k,41) + rxt(k,178)*y(k,76) &
                      + rxt(k,179)*y(k,78) + rxt(k,198)*y(k,91) + rxt(k,180)*y(k,197)
         mat(k,1334) = rxt(k,195)*y(k,208)
         mat(k,1020) = rxt(k,178)*y(k,55)
         mat(k,473) = rxt(k,179)*y(k,55)
         mat(k,685) = mat(k,685) + rxt(k,198)*y(k,55)
         mat(k,1894) = rxt(k,180)*y(k,55)
         mat(k,1535) = mat(k,1535) + rxt(k,195)*y(k,58)
         mat(k,101) = -(rxt(k,268)*y(k,208) + rxt(k,276)*y(k,207))
         mat(k,1407) = -rxt(k,268)*y(k,85)
         mat(k,1369) = -rxt(k,276)*y(k,85)
         mat(k,660) = -(rxt(k,269)*y(k,208))
         mat(k,1479) = -rxt(k,269)*y(k,86)
         mat(k,762) = .050_r8*rxt(k,442)*y(k,131)
         mat(k,201) = .350_r8*rxt(k,279)*y(k,208)
         mat(k,442) = .370_r8*rxt(k,281)*y(k,131)
         mat(k,850) = .120_r8*rxt(k,310)*y(k,131)
         mat(k,718) = .110_r8*rxt(k,387)*y(k,131)
         mat(k,1062) = .330_r8*rxt(k,340)*y(k,131)
         mat(k,788) = .050_r8*rxt(k,445)*y(k,131)
         mat(k,1127) = .120_r8*rxt(k,354)*y(k,131)
         mat(k,1950) = rxt(k,272)*y(k,198)
         mat(k,1757) = .050_r8*rxt(k,442)*y(k,5) + .370_r8*rxt(k,281)*y(k,24) &
                      + .120_r8*rxt(k,310)*y(k,28) + .110_r8*rxt(k,387)*y(k,97) &
                      + .330_r8*rxt(k,340)*y(k,104) + .050_r8*rxt(k,445)*y(k,109) &
                      + .120_r8*rxt(k,354)*y(k,110)
         mat(k,1848) = rxt(k,270)*y(k,198)
         mat(k,342) = rxt(k,272)*y(k,121) + rxt(k,270)*y(k,197)
         mat(k,1479) = mat(k,1479) + .350_r8*rxt(k,279)*y(k,23)
         mat(k,651) = rxt(k,234)*y(k,76) + rxt(k,236)*y(k,88) + rxt(k,235)*y(k,218)
         mat(k,1009) = rxt(k,234)*y(k,72)
         mat(k,1261) = rxt(k,236)*y(k,72)
         mat(k,1999) = rxt(k,235)*y(k,72)
         mat(k,1264) = -(rxt(k,173)*y(k,208) + rxt(k,236)*y(k,72))
         mat(k,1521) = -rxt(k,173)*y(k,88)
         mat(k,654) = -rxt(k,236)*y(k,88)
         mat(k,1295) = rxt(k,257)*y(k,123)
         mat(k,891) = rxt(k,290)*y(k,123)
         mat(k,1078) = rxt(k,316)*y(k,123)
         mat(k,831) = (rxt(k,516)+rxt(k,521)+rxt(k,526))*y(k,84)
         mat(k,196) = rxt(k,478)*y(k,123)
         mat(k,1903) = (rxt(k,516)+rxt(k,521)+rxt(k,526))*y(k,59)
         mat(k,1727) = rxt(k,172)*y(k,208)
         mat(k,1686) = rxt(k,257)*y(k,41) + rxt(k,290)*y(k,44) + rxt(k,316)*y(k,48) &
                      + rxt(k,478)*y(k,66)
         mat(k,1521) = mat(k,1521) + rxt(k,172)*y(k,122)
         mat(k,273) = -(rxt(k,151)*y(k,208))
         mat(k,1433) = -rxt(k,151)*y(k,89)
         mat(k,1706) = rxt(k,170)*y(k,197)
         mat(k,1813) = rxt(k,170)*y(k,122)
         mat(k,643) = -(rxt(k,227)*y(k,130) + (rxt(k,518) + rxt(k,523)) * y(k,84))
         mat(k,1341) = -rxt(k,227)*y(k,90)
         mat(k,1899) = -(rxt(k,518) + rxt(k,523)) * y(k,90)
         mat(k,1541) = rxt(k,219)*y(k,197)
         mat(k,1847) = rxt(k,219)*y(k,18)
         mat(k,680) = -(rxt(k,198)*y(k,55) + rxt(k,199)*y(k,130) + rxt(k,200)*y(k,208) &
                      + (rxt(k,511) + rxt(k,517) + rxt(k,522)) * y(k,84))
         mat(k,1619) = -rxt(k,198)*y(k,91)
         mat(k,1342) = -rxt(k,199)*y(k,91)
         mat(k,1482) = -rxt(k,200)*y(k,91)
         mat(k,1900) = -(rxt(k,511) + rxt(k,517) + rxt(k,522)) * y(k,91)
         mat(k,1315) = rxt(k,187)*y(k,197)
         mat(k,829) = rxt(k,192)*y(k,208)
         mat(k,1850) = rxt(k,187)*y(k,58)
         mat(k,1482) = mat(k,1482) + rxt(k,192)*y(k,59)
         mat(k,916) = -(rxt(k,333)*y(k,208))
         mat(k,1502) = -rxt(k,333)*y(k,92)
         mat(k,477) = .300_r8*rxt(k,378)*y(k,208)
         mat(k,419) = .500_r8*rxt(k,379)*y(k,208)
         mat(k,1964) = rxt(k,332)*y(k,194) + rxt(k,339)*y(k,201)
         mat(k,455) = rxt(k,332)*y(k,121)
         mat(k,1151) = rxt(k,339)*y(k,121)
         mat(k,1502) = mat(k,1502) + .300_r8*rxt(k,378)*y(k,98) + .500_r8*rxt(k,379) &
                      *y(k,99)
         mat(k,151) = -(rxt(k,364)*y(k,208))
         mat(k,1414) = -rxt(k,364)*y(k,93)
         mat(k,929) = -(rxt(k,318)*y(k,208))
         mat(k,1503) = -rxt(k,318)*y(k,94)
         mat(k,478) = .700_r8*rxt(k,378)*y(k,208)
         mat(k,420) = .500_r8*rxt(k,379)*y(k,208)
         mat(k,461) = .500_r8*rxt(k,353)*y(k,208)
         mat(k,1965) = .050_r8*rxt(k,376)*y(k,200) + .220_r8*rxt(k,338)*y(k,201) &
                      + .250_r8*rxt(k,395)*y(k,215)
         mat(k,1669) = .050_r8*rxt(k,377)*y(k,200) + .220_r8*rxt(k,337)*y(k,201) &
                      + .250_r8*rxt(k,396)*y(k,215)
         mat(k,427) = .500_r8*rxt(k,322)*y(k,208)
         mat(k,1227) = .220_r8*rxt(k,334)*y(k,201) + .250_r8*rxt(k,392)*y(k,215)
         mat(k,1580) = .230_r8*rxt(k,335)*y(k,201) + .200_r8*rxt(k,323)*y(k,211) &
                      + .100_r8*rxt(k,393)*y(k,215)
         mat(k,1180) = .050_r8*rxt(k,376)*y(k,121) + .050_r8*rxt(k,377)*y(k,123)
         mat(k,1152) = .220_r8*rxt(k,338)*y(k,121) + .220_r8*rxt(k,337)*y(k,123) &
                      + .220_r8*rxt(k,334)*y(k,191) + .230_r8*rxt(k,335)*y(k,192)
         mat(k,1503) = mat(k,1503) + .700_r8*rxt(k,378)*y(k,98) + .500_r8*rxt(k,379) &
                      *y(k,99) + .500_r8*rxt(k,353)*y(k,108) + .500_r8*rxt(k,322) &
                      *y(k,143)
         mat(k,998) = .200_r8*rxt(k,323)*y(k,192)
         mat(k,1027) = .250_r8*rxt(k,395)*y(k,121) + .250_r8*rxt(k,396)*y(k,123) &
                      + .250_r8*rxt(k,392)*y(k,191) + .100_r8*rxt(k,393)*y(k,192)
      end do
      end subroutine nlnmat03
      subroutine nlnmat04( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,212) = -(rxt(k,365)*y(k,208))
         mat(k,1424) = -rxt(k,365)*y(k,95)
         mat(k,1924) = .870_r8*rxt(k,376)*y(k,200)
         mat(k,1650) = .950_r8*rxt(k,377)*y(k,200)
         mat(k,1219) = rxt(k,372)*y(k,200)
         mat(k,1563) = .750_r8*rxt(k,373)*y(k,200)
         mat(k,1169) = .870_r8*rxt(k,376)*y(k,121) + .950_r8*rxt(k,377)*y(k,123) &
                      + rxt(k,372)*y(k,191) + .750_r8*rxt(k,373)*y(k,192)
         mat(k,69) = -(rxt(k,366)*y(k,208))
         mat(k,1402) = -rxt(k,366)*y(k,96)
         mat(k,554) = .600_r8*rxt(k,389)*y(k,208)
         mat(k,1402) = mat(k,1402) + .600_r8*rxt(k,389)*y(k,102)
         mat(k,719) = -(rxt(k,380)*y(k,123) + rxt(k,387)*y(k,131) + rxt(k,388) &
                      *y(k,208))
         mat(k,1654) = -rxt(k,380)*y(k,97)
         mat(k,1759) = -rxt(k,387)*y(k,97)
         mat(k,1486) = -rxt(k,388)*y(k,97)
         mat(k,475) = -(rxt(k,378)*y(k,208))
         mat(k,1461) = -rxt(k,378)*y(k,98)
         mat(k,1938) = .080_r8*rxt(k,370)*y(k,199)
         mat(k,1100) = .080_r8*rxt(k,370)*y(k,121)
         mat(k,416) = -(rxt(k,379)*y(k,208))
         mat(k,1454) = -rxt(k,379)*y(k,99)
         mat(k,1936) = .080_r8*rxt(k,376)*y(k,200)
         mat(k,1170) = .080_r8*rxt(k,376)*y(k,121)
         mat(k,1048) = -(rxt(k,381)*y(k,191) + rxt(k,382)*y(k,192) + rxt(k,383) &
                      *y(k,197) + rxt(k,384)*y(k,121) + rxt(k,385)*y(k,123))
         mat(k,1229) = -rxt(k,381)*y(k,100)
         mat(k,1586) = -rxt(k,382)*y(k,100)
         mat(k,1870) = -rxt(k,383)*y(k,100)
         mat(k,1971) = -rxt(k,384)*y(k,100)
         mat(k,1675) = -rxt(k,385)*y(k,100)
         mat(k,722) = rxt(k,380)*y(k,123)
         mat(k,1675) = mat(k,1675) + rxt(k,380)*y(k,97)
         mat(k,309) = -(rxt(k,386)*y(k,208))
         mat(k,1438) = -rxt(k,386)*y(k,101)
         mat(k,1040) = rxt(k,383)*y(k,197)
         mat(k,1817) = rxt(k,383)*y(k,100)
         mat(k,555) = -(rxt(k,389)*y(k,208))
         mat(k,1470) = -rxt(k,389)*y(k,102)
         mat(k,1839) = rxt(k,369)*y(k,199) + rxt(k,374)*y(k,200)
         mat(k,1101) = rxt(k,369)*y(k,197)
         mat(k,1172) = rxt(k,374)*y(k,197)
         mat(k,40) = -(rxt(k,503)*y(k,208))
         mat(k,1396) = -rxt(k,503)*y(k,103)
         mat(k,1064) = -(rxt(k,340)*y(k,131) + rxt(k,341)*y(k,208))
         mat(k,1777) = -rxt(k,340)*y(k,104)
         mat(k,1511) = -rxt(k,341)*y(k,104)
         mat(k,723) = .300_r8*rxt(k,387)*y(k,131)
         mat(k,1972) = .360_r8*rxt(k,370)*y(k,199)
         mat(k,1676) = .400_r8*rxt(k,371)*y(k,199)
         mat(k,1777) = mat(k,1777) + .300_r8*rxt(k,387)*y(k,97)
         mat(k,1230) = .390_r8*rxt(k,367)*y(k,199)
         mat(k,1587) = .310_r8*rxt(k,368)*y(k,199)
         mat(k,1109) = .360_r8*rxt(k,370)*y(k,121) + .400_r8*rxt(k,371)*y(k,123) &
                      + .390_r8*rxt(k,367)*y(k,191) + .310_r8*rxt(k,368)*y(k,192)
         mat(k,215) = -(rxt(k,342)*y(k,208))
         mat(k,1425) = -rxt(k,342)*y(k,105)
         mat(k,1808) = rxt(k,336)*y(k,201)
         mat(k,1147) = rxt(k,336)*y(k,197)
         mat(k,398) = -(rxt(k,351)*y(k,208))
         mat(k,1451) = -rxt(k,351)*y(k,106)
         mat(k,1934) = .800_r8*rxt(k,360)*y(k,185)
         mat(k,807) = .800_r8*rxt(k,360)*y(k,121)
         mat(k,220) = -(rxt(k,352)*y(k,208))
         mat(k,1426) = -rxt(k,352)*y(k,107)
         mat(k,1809) = .800_r8*rxt(k,349)*y(k,205)
         mat(k,546) = .800_r8*rxt(k,349)*y(k,197)
         mat(k,460) = -(rxt(k,353)*y(k,208))
         mat(k,1459) = -rxt(k,353)*y(k,108)
         mat(k,1711) = rxt(k,356)*y(k,203)
         mat(k,1203) = rxt(k,356)*y(k,122)
         mat(k,790) = -(rxt(k,444)*y(k,123) + rxt(k,445)*y(k,131) + rxt(k,446) &
                      *y(k,208))
         mat(k,1658) = -rxt(k,444)*y(k,109)
         mat(k,1762) = -rxt(k,445)*y(k,109)
         mat(k,1491) = -rxt(k,446)*y(k,109)
         mat(k,1133) = -(rxt(k,354)*y(k,131) + rxt(k,355)*y(k,208))
         mat(k,1781) = -rxt(k,354)*y(k,110)
         mat(k,1515) = -rxt(k,355)*y(k,110)
         mat(k,725) = .200_r8*rxt(k,387)*y(k,131)
         mat(k,1975) = .560_r8*rxt(k,370)*y(k,199)
         mat(k,1680) = .600_r8*rxt(k,371)*y(k,199)
         mat(k,1781) = mat(k,1781) + .200_r8*rxt(k,387)*y(k,97)
         mat(k,1233) = .610_r8*rxt(k,367)*y(k,199)
         mat(k,1590) = .440_r8*rxt(k,368)*y(k,199)
         mat(k,1112) = .560_r8*rxt(k,370)*y(k,121) + .600_r8*rxt(k,371)*y(k,123) &
                      + .610_r8*rxt(k,367)*y(k,191) + .440_r8*rxt(k,368)*y(k,192)
         mat(k,267) = -(rxt(k,152)*y(k,121) + (rxt(k,153) + rxt(k,154) + rxt(k,155) &
                      ) * y(k,122) + rxt(k,164)*y(k,208))
         mat(k,1926) = -rxt(k,152)*y(k,111)
         mat(k,1705) = -(rxt(k,153) + rxt(k,154) + rxt(k,155)) * y(k,111)
         mat(k,1432) = -rxt(k,164)*y(k,111)
         mat(k,1704) = rxt(k,171)*y(k,123)
         mat(k,1648) = rxt(k,171)*y(k,122)
         mat(k,279) = -(rxt(k,390)*y(k,208))
         mat(k,1434) = -rxt(k,390)*y(k,114)
         mat(k,1039) = .200_r8*rxt(k,382)*y(k,192)
         mat(k,1564) = .200_r8*rxt(k,382)*y(k,100)
         mat(k,877) = -(rxt(k,391)*y(k,208))
         mat(k,1498) = -rxt(k,391)*y(k,115)
         mat(k,1045) = rxt(k,384)*y(k,121) + rxt(k,385)*y(k,123) + rxt(k,381)*y(k,191) &
                      + .800_r8*rxt(k,382)*y(k,192)
         mat(k,1960) = rxt(k,384)*y(k,100)
         mat(k,1664) = rxt(k,385)*y(k,100)
         mat(k,1225) = rxt(k,381)*y(k,100)
         mat(k,1576) = .800_r8*rxt(k,382)*y(k,100)
         mat(k,50) = -(rxt(k,480)*y(k,208))
         mat(k,1398) = -rxt(k,480)*y(k,119)
         mat(k,1996) = -(rxt(k,152)*y(k,111) + rxt(k,161)*y(k,123) + rxt(k,165) &
                      *y(k,197) + rxt(k,166)*y(k,131) + rxt(k,167)*y(k,130) + rxt(k,188) &
                      *y(k,58) + rxt(k,220)*y(k,18) + rxt(k,263)*y(k,192) + rxt(k,272) &
                      *y(k,198) + rxt(k,285)*y(k,188) + rxt(k,296)*y(k,191) + rxt(k,300) &
                      *y(k,196) + rxt(k,313)*y(k,189) + rxt(k,321)*y(k,210) + rxt(k,325) &
                      *y(k,211) + (rxt(k,331) + rxt(k,332)) * y(k,194) + (rxt(k,338) &
                      + rxt(k,339)) * y(k,201) + rxt(k,347)*y(k,203) + rxt(k,350) &
                      *y(k,205) + (rxt(k,360) + rxt(k,361)) * y(k,185) + rxt(k,370) &
                      *y(k,199) + rxt(k,376)*y(k,200) + rxt(k,384)*y(k,100) + rxt(k,395) &
                      *y(k,215) + rxt(k,399)*y(k,184) + rxt(k,402)*y(k,186) + rxt(k,407) &
                      *y(k,187) + rxt(k,409)*y(k,190) + rxt(k,413)*y(k,193) + rxt(k,416) &
                      *y(k,202) + rxt(k,419)*y(k,204) + rxt(k,422)*y(k,209) + rxt(k,429) &
                      *y(k,214) + rxt(k,435)*y(k,216) + rxt(k,438)*y(k,217) + rxt(k,449) &
                      *y(k,206) + rxt(k,454)*y(k,212) + rxt(k,459)*y(k,213))
         mat(k,272) = -rxt(k,152)*y(k,121)
         mat(k,1701) = -rxt(k,161)*y(k,121)
         mat(k,1895) = -rxt(k,165)*y(k,121)
         mat(k,1802) = -rxt(k,166)*y(k,121)
         mat(k,1365) = -rxt(k,167)*y(k,121)
         mat(k,1335) = -rxt(k,188)*y(k,121)
         mat(k,1560) = -rxt(k,220)*y(k,121)
         mat(k,1610) = -rxt(k,263)*y(k,121)
         mat(k,346) = -rxt(k,272)*y(k,121)
         mat(k,697) = -rxt(k,285)*y(k,121)
         mat(k,1247) = -rxt(k,296)*y(k,121)
         mat(k,585) = -rxt(k,300)*y(k,121)
         mat(k,673) = -rxt(k,313)*y(k,121)
         mat(k,640) = -rxt(k,321)*y(k,121)
         mat(k,1007) = -rxt(k,325)*y(k,121)
         mat(k,459) = -(rxt(k,331) + rxt(k,332)) * y(k,121)
         mat(k,1166) = -(rxt(k,338) + rxt(k,339)) * y(k,121)
         mat(k,1217) = -rxt(k,347)*y(k,121)
         mat(k,553) = -rxt(k,350)*y(k,121)
         mat(k,821) = -(rxt(k,360) + rxt(k,361)) * y(k,121)
         mat(k,1125) = -rxt(k,370)*y(k,121)
         mat(k,1199) = -rxt(k,376)*y(k,121)
         mat(k,1061) = -rxt(k,384)*y(k,121)
         mat(k,1038) = -rxt(k,395)*y(k,121)
         mat(k,415) = -rxt(k,399)*y(k,121)
         mat(k,383) = -rxt(k,402)*y(k,121)
         mat(k,340) = -rxt(k,407)*y(k,121)
         mat(k,510) = -rxt(k,409)*y(k,121)
         mat(k,631) = -rxt(k,413)*y(k,121)
         mat(k,591) = -rxt(k,416)*y(k,121)
         mat(k,754) = -rxt(k,419)*y(k,121)
         mat(k,353) = -rxt(k,422)*y(k,121)
         mat(k,606) = -rxt(k,429)*y(k,121)
         mat(k,623) = -rxt(k,435)*y(k,121)
         mat(k,391) = -rxt(k,438)*y(k,121)
         mat(k,994) = -rxt(k,449)*y(k,121)
         mat(k,974) = -rxt(k,454)*y(k,121)
         mat(k,955) = -rxt(k,459)*y(k,121)
         mat(k,272) = mat(k,272) + 2.000_r8*rxt(k,154)*y(k,122) + rxt(k,164)*y(k,208)
         mat(k,1742) = 2.000_r8*rxt(k,154)*y(k,111) + rxt(k,157)*y(k,130) + rxt(k,471) &
                      *y(k,147)
         mat(k,1365) = mat(k,1365) + rxt(k,157)*y(k,122)
         mat(k,1098) = rxt(k,471)*y(k,122)
         mat(k,1536) = rxt(k,164)*y(k,111)
         mat(k,1738) = -((rxt(k,153) + rxt(k,154) + rxt(k,155)) * y(k,111) + (rxt(k,157) &
                      + rxt(k,159)) * y(k,130) + rxt(k,158)*y(k,131) + rxt(k,170) &
                      *y(k,197) + rxt(k,171)*y(k,123) + rxt(k,172)*y(k,208) + rxt(k,190) &
                      *y(k,58) + rxt(k,221)*y(k,18) + rxt(k,307)*y(k,191) + rxt(k,356) &
                      *y(k,203) + rxt(k,414)*y(k,193) + rxt(k,417)*y(k,202) + rxt(k,420) &
                      *y(k,204) + rxt(k,424)*y(k,138) + rxt(k,427)*y(k,184) + rxt(k,471) &
                      *y(k,147))
         mat(k,271) = -(rxt(k,153) + rxt(k,154) + rxt(k,155)) * y(k,122)
         mat(k,1361) = -(rxt(k,157) + rxt(k,159)) * y(k,122)
         mat(k,1798) = -rxt(k,158)*y(k,122)
         mat(k,1891) = -rxt(k,170)*y(k,122)
         mat(k,1697) = -rxt(k,171)*y(k,122)
         mat(k,1532) = -rxt(k,172)*y(k,122)
         mat(k,1331) = -rxt(k,190)*y(k,122)
         mat(k,1556) = -rxt(k,221)*y(k,122)
         mat(k,1244) = -rxt(k,307)*y(k,122)
         mat(k,1214) = -rxt(k,356)*y(k,122)
         mat(k,629) = -rxt(k,414)*y(k,122)
         mat(k,589) = -rxt(k,417)*y(k,122)
         mat(k,752) = -rxt(k,420)*y(k,122)
         mat(k,366) = -rxt(k,424)*y(k,122)
         mat(k,413) = -rxt(k,427)*y(k,122)
         mat(k,1095) = -rxt(k,471)*y(k,122)
         mat(k,544) = rxt(k,358)*y(k,208)
         mat(k,266) = rxt(k,329)*y(k,123)
         mat(k,1556) = mat(k,1556) + rxt(k,220)*y(k,121)
         mat(k,1331) = mat(k,1331) + rxt(k,188)*y(k,121)
         mat(k,276) = rxt(k,151)*y(k,208)
         mat(k,482) = .700_r8*rxt(k,378)*y(k,208)
         mat(k,1059) = rxt(k,384)*y(k,121) + rxt(k,385)*y(k,123)
         mat(k,1992) = rxt(k,220)*y(k,18) + rxt(k,188)*y(k,58) + rxt(k,384)*y(k,100) &
                      + 2.000_r8*rxt(k,161)*y(k,123) + rxt(k,167)*y(k,130) &
                      + rxt(k,166)*y(k,131) + rxt(k,399)*y(k,184) + rxt(k,360) &
                      *y(k,185) + rxt(k,402)*y(k,186) + rxt(k,407)*y(k,187) &
                      + rxt(k,285)*y(k,188) + rxt(k,313)*y(k,189) + rxt(k,409) &
                      *y(k,190) + rxt(k,296)*y(k,191) + rxt(k,263)*y(k,192) &
                      + rxt(k,413)*y(k,193) + rxt(k,331)*y(k,194) + rxt(k,300) &
                      *y(k,196) + rxt(k,165)*y(k,197) + rxt(k,272)*y(k,198) &
                      + .920_r8*rxt(k,370)*y(k,199) + .920_r8*rxt(k,376)*y(k,200) &
                      + rxt(k,338)*y(k,201) + rxt(k,416)*y(k,202) + rxt(k,347) &
                      *y(k,203) + rxt(k,419)*y(k,204) + rxt(k,350)*y(k,205) &
                      + 1.600_r8*rxt(k,449)*y(k,206) + rxt(k,422)*y(k,209) &
                      + rxt(k,321)*y(k,210) + rxt(k,325)*y(k,211) + .900_r8*rxt(k,454) &
                      *y(k,212) + .800_r8*rxt(k,459)*y(k,213) + rxt(k,429)*y(k,214) &
                      + rxt(k,395)*y(k,215) + rxt(k,435)*y(k,216) + rxt(k,438) &
                      *y(k,217)
         mat(k,1697) = mat(k,1697) + rxt(k,329)*y(k,15) + rxt(k,385)*y(k,100) &
                      + 2.000_r8*rxt(k,161)*y(k,121) + rxt(k,162)*y(k,130) &
                      + rxt(k,160)*y(k,197) + rxt(k,371)*y(k,199) + rxt(k,377) &
                      *y(k,200) + rxt(k,337)*y(k,201) + rxt(k,348)*y(k,203) &
                      + 2.000_r8*rxt(k,450)*y(k,206) + rxt(k,163)*y(k,208) &
                      + rxt(k,396)*y(k,215)
         mat(k,740) = rxt(k,319)*y(k,208)
         mat(k,1361) = mat(k,1361) + rxt(k,167)*y(k,121) + rxt(k,162)*y(k,123)
         mat(k,1798) = mat(k,1798) + rxt(k,166)*y(k,121)
         mat(k,501) = rxt(k,456)*y(k,208)
         mat(k,413) = mat(k,413) + rxt(k,399)*y(k,121)
         mat(k,819) = rxt(k,360)*y(k,121)
         mat(k,381) = rxt(k,402)*y(k,121)
         mat(k,338) = rxt(k,407)*y(k,121)
         mat(k,695) = rxt(k,285)*y(k,121)
         mat(k,671) = rxt(k,313)*y(k,121)
         mat(k,507) = rxt(k,409)*y(k,121)
         mat(k,1244) = mat(k,1244) + rxt(k,296)*y(k,121)
         mat(k,1606) = rxt(k,263)*y(k,121) + .500_r8*rxt(k,447)*y(k,206)
         mat(k,629) = mat(k,629) + rxt(k,413)*y(k,121)
         mat(k,457) = rxt(k,331)*y(k,121)
         mat(k,583) = rxt(k,300)*y(k,121)
         mat(k,1891) = mat(k,1891) + rxt(k,165)*y(k,121) + rxt(k,160)*y(k,123)
         mat(k,344) = rxt(k,272)*y(k,121)
         mat(k,1122) = .920_r8*rxt(k,370)*y(k,121) + rxt(k,371)*y(k,123)
         mat(k,1196) = .920_r8*rxt(k,376)*y(k,121) + rxt(k,377)*y(k,123)
         mat(k,1164) = rxt(k,338)*y(k,121) + rxt(k,337)*y(k,123)
         mat(k,589) = mat(k,589) + rxt(k,416)*y(k,121)
         mat(k,1214) = mat(k,1214) + rxt(k,347)*y(k,121) + rxt(k,348)*y(k,123)
         mat(k,752) = mat(k,752) + rxt(k,419)*y(k,121)
         mat(k,551) = rxt(k,350)*y(k,121)
         mat(k,992) = 1.600_r8*rxt(k,449)*y(k,121) + 2.000_r8*rxt(k,450)*y(k,123) &
                      + .500_r8*rxt(k,447)*y(k,192)
         mat(k,1532) = mat(k,1532) + rxt(k,358)*y(k,1) + rxt(k,151)*y(k,89) &
                      + .700_r8*rxt(k,378)*y(k,98) + rxt(k,163)*y(k,123) + rxt(k,319) &
                      *y(k,124) + rxt(k,456)*y(k,171)
         mat(k,351) = rxt(k,422)*y(k,121)
         mat(k,638) = rxt(k,321)*y(k,121)
         mat(k,1005) = rxt(k,325)*y(k,121)
         mat(k,972) = .900_r8*rxt(k,454)*y(k,121)
         mat(k,953) = .800_r8*rxt(k,459)*y(k,121)
         mat(k,604) = rxt(k,429)*y(k,121)
         mat(k,1036) = rxt(k,395)*y(k,121) + rxt(k,396)*y(k,123)
         mat(k,621) = rxt(k,435)*y(k,121)
         mat(k,389) = rxt(k,438)*y(k,121)
      end do
      end subroutine nlnmat04
      subroutine nlnmat05( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1696) = -(rxt(k,160)*y(k,197) + rxt(k,161)*y(k,121) + rxt(k,162) &
                      *y(k,130) + rxt(k,163)*y(k,208) + rxt(k,171)*y(k,122) + rxt(k,257) &
                      *y(k,41) + rxt(k,290)*y(k,44) + rxt(k,309)*y(k,28) + rxt(k,316) &
                      *y(k,48) + rxt(k,329)*y(k,15) + rxt(k,337)*y(k,201) + rxt(k,348) &
                      *y(k,203) + rxt(k,371)*y(k,199) + rxt(k,377)*y(k,200) + rxt(k,380) &
                      *y(k,97) + rxt(k,385)*y(k,100) + rxt(k,396)*y(k,215) + rxt(k,441) &
                      *y(k,5) + rxt(k,444)*y(k,109) + rxt(k,450)*y(k,206) + rxt(k,461) &
                      *y(k,173) + rxt(k,478)*y(k,66))
         mat(k,1890) = -rxt(k,160)*y(k,123)
         mat(k,1991) = -rxt(k,161)*y(k,123)
         mat(k,1360) = -rxt(k,162)*y(k,123)
         mat(k,1531) = -rxt(k,163)*y(k,123)
         mat(k,1737) = -rxt(k,171)*y(k,123)
         mat(k,1304) = -rxt(k,257)*y(k,123)
         mat(k,894) = -rxt(k,290)*y(k,123)
         mat(k,861) = -rxt(k,309)*y(k,123)
         mat(k,1081) = -rxt(k,316)*y(k,123)
         mat(k,265) = -rxt(k,329)*y(k,123)
         mat(k,1163) = -rxt(k,337)*y(k,123)
         mat(k,1213) = -rxt(k,348)*y(k,123)
         mat(k,1121) = -rxt(k,371)*y(k,123)
         mat(k,1195) = -rxt(k,377)*y(k,123)
         mat(k,731) = -rxt(k,380)*y(k,123)
         mat(k,1058) = -rxt(k,385)*y(k,123)
         mat(k,1035) = -rxt(k,396)*y(k,123)
         mat(k,777) = -rxt(k,441)*y(k,123)
         mat(k,803) = -rxt(k,444)*y(k,123)
         mat(k,991) = -rxt(k,450)*y(k,123)
         mat(k,846) = -rxt(k,461)*y(k,123)
         mat(k,198) = -rxt(k,478)*y(k,123)
         mat(k,438) = rxt(k,222)*y(k,130)
         mat(k,1639) = rxt(k,189)*y(k,59)
         mat(k,836) = rxt(k,189)*y(k,55) + rxt(k,191)*y(k,130) + rxt(k,192)*y(k,208)
         mat(k,658) = rxt(k,236)*y(k,88)
         mat(k,1271) = rxt(k,236)*y(k,72) + rxt(k,173)*y(k,208)
         mat(k,465) = .500_r8*rxt(k,353)*y(k,208)
         mat(k,1737) = mat(k,1737) + rxt(k,159)*y(k,130) + rxt(k,158)*y(k,131)
         mat(k,1360) = mat(k,1360) + rxt(k,222)*y(k,19) + rxt(k,191)*y(k,59) &
                      + rxt(k,159)*y(k,122)
         mat(k,1797) = rxt(k,158)*y(k,122)
         mat(k,362) = rxt(k,305)*y(k,208)
         mat(k,1531) = mat(k,1531) + rxt(k,192)*y(k,59) + rxt(k,173)*y(k,88) &
                      + .500_r8*rxt(k,353)*y(k,108) + rxt(k,305)*y(k,136)
         mat(k,735) = -(rxt(k,319)*y(k,208))
         mat(k,1487) = -rxt(k,319)*y(k,124)
         mat(k,851) = rxt(k,309)*y(k,123)
         mat(k,417) = .500_r8*rxt(k,379)*y(k,208)
         mat(k,311) = rxt(k,386)*y(k,208)
         mat(k,280) = rxt(k,390)*y(k,208)
         mat(k,874) = rxt(k,391)*y(k,208)
         mat(k,1655) = rxt(k,309)*y(k,28)
         mat(k,1487) = mat(k,1487) + .500_r8*rxt(k,379)*y(k,99) + rxt(k,386)*y(k,101) &
                      + rxt(k,390)*y(k,114) + rxt(k,391)*y(k,115)
         mat(k,297) = -(rxt(k,451)*y(k,208))
         mat(k,1436) = -rxt(k,451)*y(k,125)
         mat(k,1815) = rxt(k,448)*y(k,206)
         mat(k,976) = rxt(k,448)*y(k,197)
         mat(k,1354) = -(rxt(k,131)*y(k,131) + 4._r8*rxt(k,132)*y(k,130) + rxt(k,134) &
                      *y(k,76) + rxt(k,135)*y(k,78) + rxt(k,140)*y(k,197) + rxt(k,146) &
                      *y(k,208) + (rxt(k,157) + rxt(k,159)) * y(k,122) + rxt(k,162) &
                      *y(k,123) + rxt(k,167)*y(k,121) + rxt(k,191)*y(k,59) + rxt(k,193) &
                      *y(k,58) + rxt(k,196)*y(k,84) + rxt(k,199)*y(k,91) + rxt(k,222) &
                      *y(k,19) + rxt(k,223)*y(k,18) + rxt(k,225)*y(k,80) + rxt(k,227) &
                      *y(k,90) + rxt(k,258)*y(k,41) + rxt(k,464)*y(k,134))
         mat(k,1791) = -rxt(k,131)*y(k,130)
         mat(k,1014) = -rxt(k,134)*y(k,130)
         mat(k,469) = -rxt(k,135)*y(k,130)
         mat(k,1884) = -rxt(k,140)*y(k,130)
         mat(k,1525) = -rxt(k,146)*y(k,130)
         mat(k,1731) = -(rxt(k,157) + rxt(k,159)) * y(k,130)
         mat(k,1690) = -rxt(k,162)*y(k,130)
         mat(k,1985) = -rxt(k,167)*y(k,130)
         mat(k,833) = -rxt(k,191)*y(k,130)
         mat(k,1324) = -rxt(k,193)*y(k,130)
         mat(k,1907) = -rxt(k,196)*y(k,130)
         mat(k,682) = -rxt(k,199)*y(k,130)
         mat(k,436) = -rxt(k,222)*y(k,130)
         mat(k,1549) = -rxt(k,223)*y(k,130)
         mat(k,701) = -rxt(k,225)*y(k,130)
         mat(k,645) = -rxt(k,227)*y(k,130)
         mat(k,1298) = -rxt(k,258)*y(k,130)
         mat(k,257) = -rxt(k,464)*y(k,130)
         mat(k,1278) = rxt(k,138)*y(k,197)
         mat(k,269) = rxt(k,152)*y(k,121) + rxt(k,153)*y(k,122)
         mat(k,1985) = mat(k,1985) + rxt(k,152)*y(k,111)
         mat(k,1731) = mat(k,1731) + rxt(k,153)*y(k,111)
         mat(k,1884) = mat(k,1884) + rxt(k,138)*y(k,75)
         mat(k,1525) = mat(k,1525) + 2.000_r8*rxt(k,148)*y(k,208)
         mat(k,1799) = -(rxt(k,130)*y(k,207) + rxt(k,131)*y(k,130) + rxt(k,141) &
                      *y(k,197) + rxt(k,142)*y(k,75) + rxt(k,147)*y(k,208) + rxt(k,158) &
                      *y(k,122) + rxt(k,166)*y(k,121) + rxt(k,182)*y(k,55) + rxt(k,214) &
                      *y(k,16) + rxt(k,281)*y(k,24) + rxt(k,310)*y(k,28) + rxt(k,340) &
                      *y(k,104) + rxt(k,354)*y(k,110) + rxt(k,387)*y(k,97) + rxt(k,425) &
                      *y(k,138) + rxt(k,442)*y(k,5) + rxt(k,445)*y(k,109) + rxt(k,467) &
                      *y(k,145) + rxt(k,473)*y(k,147))
         mat(k,1386) = -rxt(k,130)*y(k,131)
         mat(k,1362) = -rxt(k,131)*y(k,131)
         mat(k,1892) = -rxt(k,141)*y(k,131)
         mat(k,1285) = -rxt(k,142)*y(k,131)
         mat(k,1533) = -rxt(k,147)*y(k,131)
         mat(k,1739) = -rxt(k,158)*y(k,131)
         mat(k,1993) = -rxt(k,166)*y(k,131)
         mat(k,1641) = -rxt(k,182)*y(k,131)
         mat(k,1258) = -rxt(k,214)*y(k,131)
         mat(k,446) = -rxt(k,281)*y(k,131)
         mat(k,863) = -rxt(k,310)*y(k,131)
         mat(k,1072) = -rxt(k,340)*y(k,131)
         mat(k,1143) = -rxt(k,354)*y(k,131)
         mat(k,732) = -rxt(k,387)*y(k,131)
         mat(k,367) = -rxt(k,425)*y(k,131)
         mat(k,778) = -rxt(k,442)*y(k,131)
         mat(k,804) = -rxt(k,445)*y(k,131)
         mat(k,397) = -rxt(k,467)*y(k,131)
         mat(k,1096) = -rxt(k,473)*y(k,131)
         mat(k,1245) = .150_r8*rxt(k,295)*y(k,197)
         mat(k,1892) = mat(k,1892) + .150_r8*rxt(k,295)*y(k,191) + .150_r8*rxt(k,345) &
                      *y(k,203)
         mat(k,1215) = .150_r8*rxt(k,345)*y(k,197)
         mat(k,230) = -(rxt(k,474)*y(k,147))
         mat(k,1084) = -rxt(k,474)*y(k,133)
         mat(k,1539) = rxt(k,216)*y(k,58)
         mat(k,1314) = rxt(k,216)*y(k,18) + 2.000_r8*rxt(k,186)*y(k,58)
         mat(k,251) = -(rxt(k,464)*y(k,130) + rxt(k,465)*y(k,208))
         mat(k,1337) = -rxt(k,464)*y(k,134)
         mat(k,1430) = -rxt(k,465)*y(k,134)
         mat(k,911) = rxt(k,333)*y(k,208)
         mat(k,1920) = .100_r8*rxt(k,454)*y(k,212)
         mat(k,1416) = rxt(k,333)*y(k,92)
         mat(k,957) = .100_r8*rxt(k,454)*y(k,121)
         mat(k,357) = -(rxt(k,305)*y(k,208))
         mat(k,1445) = -rxt(k,305)*y(k,136)
         mat(k,1707) = rxt(k,307)*y(k,191)
         mat(k,1220) = rxt(k,307)*y(k,122)
         mat(k,1703) = rxt(k,427)*y(k,184)
         mat(k,409) = rxt(k,427)*y(k,122)
         mat(k,364) = -(rxt(k,424)*y(k,122) + rxt(k,425)*y(k,131))
         mat(k,1708) = -rxt(k,424)*y(k,138)
         mat(k,1751) = -rxt(k,425)*y(k,138)
         mat(k,119) = .070_r8*rxt(k,411)*y(k,208)
         mat(k,1931) = rxt(k,409)*y(k,190)
         mat(k,94) = .060_r8*rxt(k,423)*y(k,208)
         mat(k,144) = .070_r8*rxt(k,439)*y(k,208)
         mat(k,504) = rxt(k,409)*y(k,121)
         mat(k,1446) = .070_r8*rxt(k,411)*y(k,65) + .060_r8*rxt(k,423)*y(k,139) &
                      + .070_r8*rxt(k,439)*y(k,180)
         mat(k,92) = -(rxt(k,423)*y(k,208))
         mat(k,1405) = -rxt(k,423)*y(k,139)
         mat(k,84) = .530_r8*rxt(k,400)*y(k,208)
         mat(k,1405) = mat(k,1405) + .530_r8*rxt(k,400)*y(k,6)
         mat(k,235) = -(rxt(k,426)*y(k,208))
         mat(k,1427) = -rxt(k,426)*y(k,140)
         mat(k,1810) = rxt(k,421)*y(k,209)
         mat(k,347) = rxt(k,421)*y(k,197)
         mat(k,424) = -(rxt(k,322)*y(k,208))
         mat(k,1455) = -rxt(k,322)*y(k,143)
         mat(k,1831) = rxt(k,320)*y(k,210)
         mat(k,632) = rxt(k,320)*y(k,197)
         mat(k,303) = -(rxt(k,326)*y(k,208))
         mat(k,1437) = -rxt(k,326)*y(k,144)
         mat(k,1816) = .850_r8*rxt(k,324)*y(k,211)
         mat(k,996) = .850_r8*rxt(k,324)*y(k,197)
         mat(k,392) = -(rxt(k,467)*y(k,131) + rxt(k,470)*y(k,208))
         mat(k,1752) = -rxt(k,467)*y(k,145)
         mat(k,1450) = -rxt(k,470)*y(k,145)
         mat(k,1087) = -(rxt(k,468)*y(k,18) + rxt(k,469)*y(k,58) + rxt(k,471)*y(k,122) &
                      + rxt(k,473)*y(k,131) + rxt(k,474)*y(k,133) + rxt(k,475) &
                      *y(k,208))
         mat(k,1543) = -rxt(k,468)*y(k,147)
         mat(k,1318) = -rxt(k,469)*y(k,147)
         mat(k,1723) = -rxt(k,471)*y(k,147)
         mat(k,1779) = -rxt(k,473)*y(k,147)
         mat(k,232) = -rxt(k,474)*y(k,147)
         mat(k,1513) = -rxt(k,475)*y(k,147)
         mat(k,1348) = rxt(k,464)*y(k,134)
         mat(k,1779) = mat(k,1779) + rxt(k,467)*y(k,145)
         mat(k,255) = rxt(k,464)*y(k,130)
         mat(k,393) = rxt(k,467)*y(k,131) + rxt(k,470)*y(k,208)
         mat(k,1513) = mat(k,1513) + rxt(k,470)*y(k,145)
         mat(k,707) = -(rxt(k,476)*y(k,208))
         mat(k,1485) = -rxt(k,476)*y(k,148)
         mat(k,1542) = rxt(k,468)*y(k,147)
         mat(k,1316) = rxt(k,469)*y(k,147)
         mat(k,195) = rxt(k,478)*y(k,123) + (rxt(k,479)+.500_r8*rxt(k,481))*y(k,208)
         mat(k,1716) = rxt(k,471)*y(k,147)
         mat(k,1653) = rxt(k,478)*y(k,66)
         mat(k,1758) = rxt(k,473)*y(k,147)
         mat(k,231) = rxt(k,474)*y(k,147)
         mat(k,253) = rxt(k,465)*y(k,208)
         mat(k,1086) = rxt(k,468)*y(k,18) + rxt(k,469)*y(k,58) + rxt(k,471)*y(k,122) &
                      + rxt(k,473)*y(k,131) + rxt(k,474)*y(k,133) + rxt(k,475) &
                      *y(k,208)
         mat(k,1485) = mat(k,1485) + (rxt(k,479)+.500_r8*rxt(k,481))*y(k,66) &
                      + rxt(k,465)*y(k,134) + rxt(k,475)*y(k,147)
         mat(k,169) = -(rxt(k,477)*y(k,218))
         mat(k,2000) = -rxt(k,477)*y(k,149)
         mat(k,706) = rxt(k,476)*y(k,208)
         mat(k,1418) = rxt(k,476)*y(k,148)
         mat(k,755) = .2202005_r8*rxt(k,497)*y(k,131) + .2202005_r8*rxt(k,498) &
                      *y(k,208)
         mat(k,77) = .0023005_r8*rxt(k,499)*y(k,208)
         mat(k,713) = .0031005_r8*rxt(k,502)*y(k,208)
         mat(k,35) = .2381005_r8*rxt(k,503)*y(k,208)
         mat(k,781) = .0508005_r8*rxt(k,505)*y(k,131) + .0508005_r8*rxt(k,506) &
                      *y(k,208)
         mat(k,1744) = .2202005_r8*rxt(k,497)*y(k,5) + .0508005_r8*rxt(k,505)*y(k,109)
         mat(k,41) = .5931005_r8*rxt(k,507)*y(k,208)
         mat(k,105) = .1364005_r8*rxt(k,508)*y(k,208)
         mat(k,129) = .1677005_r8*rxt(k,509)*y(k,208)
         mat(k,1391) = .2202005_r8*rxt(k,498)*y(k,5) + .0023005_r8*rxt(k,499)*y(k,6) &
                      + .0031005_r8*rxt(k,502)*y(k,97) + .2381005_r8*rxt(k,503) &
                      *y(k,103) + .0508005_r8*rxt(k,506)*y(k,109) &
                      + .5931005_r8*rxt(k,507)*y(k,168) + .1364005_r8*rxt(k,508) &
                      *y(k,176) + .1677005_r8*rxt(k,509)*y(k,178)
      end do
      end subroutine nlnmat05
      subroutine nlnmat06( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,756) = .2067005_r8*rxt(k,497)*y(k,131) + .2067005_r8*rxt(k,498) &
                      *y(k,208)
         mat(k,78) = .0008005_r8*rxt(k,499)*y(k,208)
         mat(k,714) = .0035005_r8*rxt(k,502)*y(k,208)
         mat(k,36) = .1308005_r8*rxt(k,503)*y(k,208)
         mat(k,782) = .1149005_r8*rxt(k,505)*y(k,131) + .1149005_r8*rxt(k,506) &
                      *y(k,208)
         mat(k,1745) = .2067005_r8*rxt(k,497)*y(k,5) + .1149005_r8*rxt(k,505)*y(k,109)
         mat(k,42) = .1534005_r8*rxt(k,507)*y(k,208)
         mat(k,106) = .0101005_r8*rxt(k,508)*y(k,208)
         mat(k,130) = .0174005_r8*rxt(k,509)*y(k,208)
         mat(k,1392) = .2067005_r8*rxt(k,498)*y(k,5) + .0008005_r8*rxt(k,499)*y(k,6) &
                      + .0035005_r8*rxt(k,502)*y(k,97) + .1308005_r8*rxt(k,503) &
                      *y(k,103) + .1149005_r8*rxt(k,506)*y(k,109) &
                      + .1534005_r8*rxt(k,507)*y(k,168) + .0101005_r8*rxt(k,508) &
                      *y(k,176) + .0174005_r8*rxt(k,509)*y(k,178)
         mat(k,757) = .0653005_r8*rxt(k,497)*y(k,131) + .0653005_r8*rxt(k,498) &
                      *y(k,208)
         mat(k,79) = .0843005_r8*rxt(k,499)*y(k,208)
         mat(k,715) = .0003005_r8*rxt(k,502)*y(k,208)
         mat(k,37) = .0348005_r8*rxt(k,503)*y(k,208)
         mat(k,783) = .0348005_r8*rxt(k,505)*y(k,131) + .0348005_r8*rxt(k,506) &
                      *y(k,208)
         mat(k,1746) = .0653005_r8*rxt(k,497)*y(k,5) + .0348005_r8*rxt(k,505)*y(k,109)
         mat(k,43) = .0459005_r8*rxt(k,507)*y(k,208)
         mat(k,107) = .0763005_r8*rxt(k,508)*y(k,208)
         mat(k,131) = .086_r8*rxt(k,509)*y(k,208)
         mat(k,1393) = .0653005_r8*rxt(k,498)*y(k,5) + .0843005_r8*rxt(k,499)*y(k,6) &
                      + .0003005_r8*rxt(k,502)*y(k,97) + .0348005_r8*rxt(k,503) &
                      *y(k,103) + .0348005_r8*rxt(k,506)*y(k,109) &
                      + .0459005_r8*rxt(k,507)*y(k,168) + .0763005_r8*rxt(k,508) &
                      *y(k,176) + .086_r8*rxt(k,509)*y(k,178)
         mat(k,758) = .1749305_r8*rxt(k,496)*y(k,123) + .1284005_r8*rxt(k,497) &
                      *y(k,131) + .1284005_r8*rxt(k,498)*y(k,208)
         mat(k,80) = .0443005_r8*rxt(k,499)*y(k,208)
         mat(k,716) = .0590245_r8*rxt(k,500)*y(k,123) + .0033005_r8*rxt(k,501) &
                      *y(k,131) + .0271005_r8*rxt(k,502)*y(k,208)
         mat(k,38) = .0076005_r8*rxt(k,503)*y(k,208)
         mat(k,784) = .1749305_r8*rxt(k,504)*y(k,123) + .0554005_r8*rxt(k,505) &
                      *y(k,131) + .0554005_r8*rxt(k,506)*y(k,208)
         mat(k,1646) = .1749305_r8*rxt(k,496)*y(k,5) + .0590245_r8*rxt(k,500)*y(k,97) &
                      + .1749305_r8*rxt(k,504)*y(k,109)
         mat(k,1747) = .1284005_r8*rxt(k,497)*y(k,5) + .0033005_r8*rxt(k,501)*y(k,97) &
                      + .0554005_r8*rxt(k,505)*y(k,109)
         mat(k,44) = .0085005_r8*rxt(k,507)*y(k,208)
         mat(k,108) = .2157005_r8*rxt(k,508)*y(k,208)
         mat(k,132) = .0512005_r8*rxt(k,509)*y(k,208)
         mat(k,1394) = .1284005_r8*rxt(k,498)*y(k,5) + .0443005_r8*rxt(k,499)*y(k,6) &
                      + .0271005_r8*rxt(k,502)*y(k,97) + .0076005_r8*rxt(k,503) &
                      *y(k,103) + .0554005_r8*rxt(k,506)*y(k,109) &
                      + .0085005_r8*rxt(k,507)*y(k,168) + .2157005_r8*rxt(k,508) &
                      *y(k,176) + .0512005_r8*rxt(k,509)*y(k,178)
         mat(k,759) = .5901905_r8*rxt(k,496)*y(k,123) + .114_r8*rxt(k,497)*y(k,131) &
                      + .114_r8*rxt(k,498)*y(k,208)
         mat(k,81) = .1621005_r8*rxt(k,499)*y(k,208)
         mat(k,717) = .0250245_r8*rxt(k,500)*y(k,123) + .0474005_r8*rxt(k,502) &
                      *y(k,208)
         mat(k,39) = .0113005_r8*rxt(k,503)*y(k,208)
         mat(k,785) = .5901905_r8*rxt(k,504)*y(k,123) + .1278005_r8*rxt(k,505) &
                      *y(k,131) + .1278005_r8*rxt(k,506)*y(k,208)
         mat(k,1647) = .5901905_r8*rxt(k,496)*y(k,5) + .0250245_r8*rxt(k,500)*y(k,97) &
                      + .5901905_r8*rxt(k,504)*y(k,109)
         mat(k,1748) = .114_r8*rxt(k,497)*y(k,5) + .1278005_r8*rxt(k,505)*y(k,109)
         mat(k,45) = .0128005_r8*rxt(k,507)*y(k,208)
         mat(k,109) = .0232005_r8*rxt(k,508)*y(k,208)
         mat(k,133) = .1598005_r8*rxt(k,509)*y(k,208)
         mat(k,1395) = .114_r8*rxt(k,498)*y(k,5) + .1621005_r8*rxt(k,499)*y(k,6) &
                      + .0474005_r8*rxt(k,502)*y(k,97) + .0113005_r8*rxt(k,503) &
                      *y(k,103) + .1278005_r8*rxt(k,506)*y(k,109) &
                      + .0128005_r8*rxt(k,507)*y(k,168) + .0232005_r8*rxt(k,508) &
                      *y(k,176) + .1598005_r8*rxt(k,509)*y(k,178)
         mat(k,46) = -(rxt(k,507)*y(k,208))
         mat(k,1397) = -rxt(k,507)*y(k,168)
         mat(k,112) = .100_r8*rxt(k,431)*y(k,208)
         mat(k,134) = .230_r8*rxt(k,433)*y(k,208)
         mat(k,1410) = .100_r8*rxt(k,431)*y(k,176) + .230_r8*rxt(k,433)*y(k,178)
         mat(k,488) = -(rxt(k,455)*y(k,208))
         mat(k,1463) = -rxt(k,455)*y(k,170)
         mat(k,1834) = rxt(k,453)*y(k,212)
         mat(k,958) = rxt(k,453)*y(k,197)
         mat(k,497) = -(rxt(k,456)*y(k,208))
         mat(k,1464) = -rxt(k,456)*y(k,171)
         mat(k,1940) = .200_r8*rxt(k,449)*y(k,206) + .200_r8*rxt(k,459)*y(k,213)
         mat(k,1567) = .500_r8*rxt(k,447)*y(k,206)
         mat(k,977) = .200_r8*rxt(k,449)*y(k,121) + .500_r8*rxt(k,447)*y(k,192)
         mat(k,936) = .200_r8*rxt(k,459)*y(k,121)
         mat(k,368) = -(rxt(k,460)*y(k,208))
         mat(k,1447) = -rxt(k,460)*y(k,172)
         mat(k,1826) = rxt(k,458)*y(k,213)
         mat(k,935) = rxt(k,458)*y(k,197)
         mat(k,840) = -(rxt(k,461)*y(k,123) + rxt(k,462)*y(k,208))
         mat(k,1661) = -rxt(k,461)*y(k,173)
         mat(k,1495) = -rxt(k,462)*y(k,173)
         mat(k,767) = .330_r8*rxt(k,442)*y(k,131)
         mat(k,793) = .330_r8*rxt(k,445)*y(k,131)
         mat(k,1958) = .800_r8*rxt(k,449)*y(k,206) + .800_r8*rxt(k,459)*y(k,213)
         mat(k,1661) = mat(k,1661) + rxt(k,450)*y(k,206)
         mat(k,1765) = .330_r8*rxt(k,442)*y(k,5) + .330_r8*rxt(k,445)*y(k,109)
         mat(k,498) = rxt(k,456)*y(k,208)
         mat(k,1574) = .500_r8*rxt(k,447)*y(k,206) + rxt(k,457)*y(k,213)
         mat(k,979) = .800_r8*rxt(k,449)*y(k,121) + rxt(k,450)*y(k,123) &
                      + .500_r8*rxt(k,447)*y(k,192)
         mat(k,1495) = mat(k,1495) + rxt(k,456)*y(k,171)
         mat(k,939) = .800_r8*rxt(k,459)*y(k,121) + rxt(k,457)*y(k,192)
         mat(k,898) = -(rxt(k,463)*y(k,208))
         mat(k,1500) = -rxt(k,463)*y(k,174)
         mat(k,768) = .300_r8*rxt(k,442)*y(k,131)
         mat(k,794) = .300_r8*rxt(k,445)*y(k,131)
         mat(k,1962) = .900_r8*rxt(k,454)*y(k,212)
         mat(k,1769) = .300_r8*rxt(k,442)*y(k,5) + .300_r8*rxt(k,445)*y(k,109)
         mat(k,1578) = rxt(k,452)*y(k,212)
         mat(k,962) = .900_r8*rxt(k,454)*y(k,121) + rxt(k,452)*y(k,192)
         mat(k,525) = -(rxt(k,430)*y(k,208))
         mat(k,1467) = -rxt(k,430)*y(k,175)
         mat(k,1837) = rxt(k,428)*y(k,214)
         mat(k,595) = rxt(k,428)*y(k,197)
         mat(k,110) = -(rxt(k,431)*y(k,208))
         mat(k,1408) = -rxt(k,431)*y(k,176)
         mat(k,126) = -(rxt(k,397)*y(k,208))
         mat(k,1411) = -rxt(k,397)*y(k,177)
         mat(k,1805) = rxt(k,394)*y(k,215)
         mat(k,1022) = rxt(k,394)*y(k,197)
         mat(k,135) = -(rxt(k,433)*y(k,208))
         mat(k,1412) = -rxt(k,433)*y(k,178)
         mat(k,566) = -(rxt(k,436)*y(k,208))
         mat(k,1471) = -rxt(k,436)*y(k,179)
         mat(k,1840) = rxt(k,434)*y(k,216)
         mat(k,611) = rxt(k,434)*y(k,197)
         mat(k,143) = -(rxt(k,439)*y(k,208))
         mat(k,1413) = -rxt(k,439)*y(k,180)
         mat(k,136) = .150_r8*rxt(k,433)*y(k,208)
         mat(k,1413) = mat(k,1413) + .150_r8*rxt(k,433)*y(k,178)
         mat(k,327) = -(rxt(k,440)*y(k,208))
         mat(k,1441) = -rxt(k,440)*y(k,181)
         mat(k,1820) = rxt(k,437)*y(k,217)
         mat(k,384) = rxt(k,437)*y(k,197)
         mat(k,410) = -(rxt(k,398)*y(k,197) + rxt(k,399)*y(k,121) + rxt(k,427) &
                      *y(k,122))
         mat(k,1830) = -rxt(k,398)*y(k,184)
         mat(k,1935) = -rxt(k,399)*y(k,184)
         mat(k,1709) = -rxt(k,427)*y(k,184)
         mat(k,166) = rxt(k,404)*y(k,208)
         mat(k,1453) = rxt(k,404)*y(k,21)
         mat(k,812) = -(rxt(k,359)*y(k,197) + (rxt(k,360) + rxt(k,361)) * y(k,121))
         mat(k,1856) = -rxt(k,359)*y(k,185)
         mat(k,1956) = -(rxt(k,360) + rxt(k,361)) * y(k,185)
         mat(k,515) = rxt(k,362)*y(k,208)
         mat(k,157) = rxt(k,363)*y(k,208)
         mat(k,1492) = rxt(k,362)*y(k,2) + rxt(k,363)*y(k,14)
         mat(k,377) = -(rxt(k,401)*y(k,197) + rxt(k,402)*y(k,121))
         mat(k,1827) = -rxt(k,401)*y(k,186)
         mat(k,1932) = -rxt(k,402)*y(k,186)
         mat(k,85) = .350_r8*rxt(k,400)*y(k,208)
         mat(k,287) = rxt(k,403)*y(k,208)
         mat(k,1448) = .350_r8*rxt(k,400)*y(k,6) + rxt(k,403)*y(k,7)
         mat(k,335) = -(rxt(k,405)*y(k,197) + rxt(k,407)*y(k,121))
         mat(k,1821) = -rxt(k,405)*y(k,187)
         mat(k,1927) = -rxt(k,407)*y(k,187)
         mat(k,242) = rxt(k,406)*y(k,208)
         mat(k,113) = .070_r8*rxt(k,431)*y(k,208)
         mat(k,137) = .060_r8*rxt(k,433)*y(k,208)
         mat(k,1442) = rxt(k,406)*y(k,22) + .070_r8*rxt(k,431)*y(k,176) &
                      + .060_r8*rxt(k,433)*y(k,178)
         mat(k,690) = -(4._r8*rxt(k,282)*y(k,188) + rxt(k,283)*y(k,192) + rxt(k,284) &
                      *y(k,197) + rxt(k,285)*y(k,121))
         mat(k,1570) = -rxt(k,283)*y(k,188)
         mat(k,1851) = -rxt(k,284)*y(k,188)
         mat(k,1952) = -rxt(k,285)*y(k,188)
         mat(k,247) = .500_r8*rxt(k,287)*y(k,208)
         mat(k,207) = rxt(k,288)*y(k,55) + rxt(k,289)*y(k,208)
         mat(k,1620) = rxt(k,288)*y(k,27)
         mat(k,1483) = .500_r8*rxt(k,287)*y(k,26) + rxt(k,289)*y(k,27)
         mat(k,665) = -(rxt(k,311)*y(k,192) + rxt(k,312)*y(k,197) + rxt(k,313) &
                      *y(k,121))
         mat(k,1568) = -rxt(k,311)*y(k,189)
         mat(k,1849) = -rxt(k,312)*y(k,189)
         mat(k,1951) = -rxt(k,313)*y(k,189)
         mat(k,316) = rxt(k,314)*y(k,208)
         mat(k,57) = rxt(k,315)*y(k,208)
         mat(k,1480) = rxt(k,314)*y(k,29) + rxt(k,315)*y(k,30)
         mat(k,505) = -(rxt(k,408)*y(k,197) + rxt(k,409)*y(k,121))
         mat(k,1835) = -rxt(k,408)*y(k,190)
         mat(k,1941) = -rxt(k,409)*y(k,190)
         mat(k,179) = rxt(k,410)*y(k,208)
         mat(k,1941) = mat(k,1941) + rxt(k,399)*y(k,184)
         mat(k,1755) = rxt(k,425)*y(k,138)
         mat(k,365) = rxt(k,425)*y(k,131)
         mat(k,411) = rxt(k,399)*y(k,121) + .400_r8*rxt(k,398)*y(k,197)
         mat(k,1835) = mat(k,1835) + .400_r8*rxt(k,398)*y(k,184)
         mat(k,1465) = rxt(k,410)*y(k,31)
         mat(k,1237) = -(4._r8*rxt(k,293)*y(k,191) + rxt(k,294)*y(k,192) + rxt(k,295) &
                      *y(k,197) + rxt(k,296)*y(k,121) + rxt(k,307)*y(k,122) + rxt(k,334) &
                      *y(k,201) + rxt(k,367)*y(k,199) + rxt(k,372)*y(k,200) + rxt(k,381) &
                      *y(k,100) + rxt(k,392)*y(k,215))
         mat(k,1594) = -rxt(k,294)*y(k,191)
         mat(k,1878) = -rxt(k,295)*y(k,191)
         mat(k,1979) = -rxt(k,296)*y(k,191)
         mat(k,1725) = -rxt(k,307)*y(k,191)
         mat(k,1157) = -rxt(k,334)*y(k,191)
         mat(k,1115) = -rxt(k,367)*y(k,191)
         mat(k,1189) = -rxt(k,372)*y(k,191)
         mat(k,1052) = -rxt(k,381)*y(k,191)
         mat(k,1030) = -rxt(k,392)*y(k,191)
         mat(k,774) = .060_r8*rxt(k,442)*y(k,131)
         mat(k,890) = rxt(k,290)*y(k,123) + rxt(k,291)*y(k,208)
         mat(k,1077) = rxt(k,316)*y(k,123) + rxt(k,317)*y(k,208)
         mat(k,404) = .500_r8*rxt(k,298)*y(k,208)
         mat(k,727) = .080_r8*rxt(k,387)*y(k,131)
         mat(k,1068) = .100_r8*rxt(k,340)*y(k,131)
         mat(k,800) = .060_r8*rxt(k,445)*y(k,131)
         mat(k,1135) = .280_r8*rxt(k,354)*y(k,131)
         mat(k,1979) = mat(k,1979) + .530_r8*rxt(k,338)*y(k,201) + rxt(k,347)*y(k,203) &
                      + rxt(k,350)*y(k,205) + rxt(k,325)*y(k,211)
         mat(k,1684) = rxt(k,290)*y(k,44) + rxt(k,316)*y(k,48) + .530_r8*rxt(k,337) &
                      *y(k,201) + rxt(k,348)*y(k,203)
         mat(k,1785) = .060_r8*rxt(k,442)*y(k,5) + .080_r8*rxt(k,387)*y(k,97) &
                      + .100_r8*rxt(k,340)*y(k,104) + .060_r8*rxt(k,445)*y(k,109) &
                      + .280_r8*rxt(k,354)*y(k,110)
         mat(k,901) = .650_r8*rxt(k,463)*y(k,208)
         mat(k,1237) = mat(k,1237) + .530_r8*rxt(k,334)*y(k,201)
         mat(k,1594) = mat(k,1594) + .260_r8*rxt(k,335)*y(k,201) + rxt(k,344)*y(k,203) &
                      + .300_r8*rxt(k,323)*y(k,211)
         mat(k,1878) = mat(k,1878) + .450_r8*rxt(k,345)*y(k,203) + .200_r8*rxt(k,349) &
                      *y(k,205) + .150_r8*rxt(k,324)*y(k,211)
         mat(k,1157) = mat(k,1157) + .530_r8*rxt(k,338)*y(k,121) + .530_r8*rxt(k,337) &
                      *y(k,123) + .530_r8*rxt(k,334)*y(k,191) + .260_r8*rxt(k,335) &
                      *y(k,192)
         mat(k,1207) = rxt(k,347)*y(k,121) + rxt(k,348)*y(k,123) + rxt(k,344)*y(k,192) &
                      + .450_r8*rxt(k,345)*y(k,197) + 4.000_r8*rxt(k,346)*y(k,203)
         mat(k,549) = rxt(k,350)*y(k,121) + .200_r8*rxt(k,349)*y(k,197)
         mat(k,1519) = rxt(k,291)*y(k,44) + rxt(k,317)*y(k,48) + .500_r8*rxt(k,298) &
                      *y(k,50) + .650_r8*rxt(k,463)*y(k,174)
         mat(k,1001) = rxt(k,325)*y(k,121) + .300_r8*rxt(k,323)*y(k,192) &
                      + .150_r8*rxt(k,324)*y(k,197)
      end do
      end subroutine nlnmat06
      subroutine nlnmat07( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1603) = -(rxt(k,183)*y(k,58) + (4._r8*rxt(k,260) + 4._r8*rxt(k,261) &
                      ) * y(k,192) + rxt(k,262)*y(k,197) + rxt(k,263)*y(k,121) &
                      + rxt(k,283)*y(k,188) + rxt(k,294)*y(k,191) + rxt(k,311) &
                      *y(k,189) + rxt(k,323)*y(k,211) + rxt(k,335)*y(k,201) + rxt(k,344) &
                      *y(k,203) + rxt(k,368)*y(k,199) + rxt(k,373)*y(k,200) + rxt(k,382) &
                      *y(k,100) + rxt(k,393)*y(k,215) + rxt(k,447)*y(k,206) + rxt(k,452) &
                      *y(k,212) + rxt(k,457)*y(k,213))
         mat(k,1328) = -rxt(k,183)*y(k,192)
         mat(k,1888) = -rxt(k,262)*y(k,192)
         mat(k,1989) = -rxt(k,263)*y(k,192)
         mat(k,694) = -rxt(k,283)*y(k,192)
         mat(k,1242) = -rxt(k,294)*y(k,192)
         mat(k,670) = -rxt(k,311)*y(k,192)
         mat(k,1004) = -rxt(k,323)*y(k,192)
         mat(k,1162) = -rxt(k,335)*y(k,192)
         mat(k,1212) = -rxt(k,344)*y(k,192)
         mat(k,1120) = -rxt(k,368)*y(k,192)
         mat(k,1194) = -rxt(k,373)*y(k,192)
         mat(k,1057) = -rxt(k,382)*y(k,192)
         mat(k,1034) = -rxt(k,393)*y(k,192)
         mat(k,990) = -rxt(k,447)*y(k,192)
         mat(k,971) = -rxt(k,452)*y(k,192)
         mat(k,951) = -rxt(k,457)*y(k,192)
         mat(k,860) = .280_r8*rxt(k,310)*y(k,131)
         mat(k,450) = rxt(k,297)*y(k,208)
         mat(k,325) = .700_r8*rxt(k,265)*y(k,208)
         mat(k,730) = .050_r8*rxt(k,387)*y(k,131)
         mat(k,1057) = mat(k,1057) + rxt(k,381)*y(k,191)
         mat(k,1989) = mat(k,1989) + rxt(k,296)*y(k,191) + .830_r8*rxt(k,413)*y(k,193) &
                      + .170_r8*rxt(k,419)*y(k,204)
         mat(k,1795) = .280_r8*rxt(k,310)*y(k,28) + .050_r8*rxt(k,387)*y(k,97)
         mat(k,1242) = mat(k,1242) + rxt(k,381)*y(k,100) + rxt(k,296)*y(k,121) &
                      + 4.000_r8*rxt(k,293)*y(k,191) + .900_r8*rxt(k,294)*y(k,192) &
                      + .450_r8*rxt(k,295)*y(k,197) + rxt(k,367)*y(k,199) + rxt(k,372) &
                      *y(k,200) + rxt(k,334)*y(k,201) + rxt(k,343)*y(k,203) &
                      + rxt(k,392)*y(k,215)
         mat(k,1603) = mat(k,1603) + .900_r8*rxt(k,294)*y(k,191)
         mat(k,628) = .830_r8*rxt(k,413)*y(k,121) + .330_r8*rxt(k,412)*y(k,197)
         mat(k,1888) = mat(k,1888) + .450_r8*rxt(k,295)*y(k,191) + .330_r8*rxt(k,412) &
                      *y(k,193) + .070_r8*rxt(k,418)*y(k,204)
         mat(k,1120) = mat(k,1120) + rxt(k,367)*y(k,191)
         mat(k,1194) = mat(k,1194) + rxt(k,372)*y(k,191)
         mat(k,1162) = mat(k,1162) + rxt(k,334)*y(k,191)
         mat(k,1212) = mat(k,1212) + rxt(k,343)*y(k,191)
         mat(k,751) = .170_r8*rxt(k,419)*y(k,121) + .070_r8*rxt(k,418)*y(k,197)
         mat(k,1529) = rxt(k,297)*y(k,49) + .700_r8*rxt(k,265)*y(k,52)
         mat(k,1034) = mat(k,1034) + rxt(k,392)*y(k,191)
         mat(k,624) = -(rxt(k,412)*y(k,197) + rxt(k,413)*y(k,121) + rxt(k,414) &
                      *y(k,122))
         mat(k,1845) = -rxt(k,412)*y(k,193)
         mat(k,1948) = -rxt(k,413)*y(k,193)
         mat(k,1714) = -rxt(k,414)*y(k,193)
         mat(k,452) = -((rxt(k,331) + rxt(k,332)) * y(k,121))
         mat(k,1937) = -(rxt(k,331) + rxt(k,332)) * y(k,194)
         mat(k,260) = rxt(k,330)*y(k,208)
         mat(k,1458) = rxt(k,330)*y(k,15)
         mat(k,1922) = .750_r8*rxt(k,300)*y(k,196)
         mat(k,578) = .750_r8*rxt(k,300)*y(k,121)
         mat(k,579) = -(rxt(k,299)*y(k,197) + rxt(k,300)*y(k,121))
         mat(k,1841) = -rxt(k,299)*y(k,196)
         mat(k,1944) = -rxt(k,300)*y(k,196)
         mat(k,441) = rxt(k,306)*y(k,208)
         mat(k,1472) = rxt(k,306)*y(k,24)
         mat(k,1893) = -((rxt(k,136) + rxt(k,137) + rxt(k,138)) * y(k,75) + rxt(k,140) &
                      *y(k,130) + rxt(k,141)*y(k,131) + rxt(k,145)*y(k,208) &
                      + 4._r8*rxt(k,150)*y(k,197) + rxt(k,160)*y(k,123) + rxt(k,165) &
                      *y(k,121) + rxt(k,170)*y(k,122) + (rxt(k,180) + rxt(k,181) &
                      ) * y(k,55) + rxt(k,187)*y(k,58) + rxt(k,213)*y(k,16) + rxt(k,219) &
                      *y(k,18) + rxt(k,256)*y(k,41) + rxt(k,262)*y(k,192) + rxt(k,270) &
                      *y(k,198) + rxt(k,284)*y(k,188) + rxt(k,295)*y(k,191) + rxt(k,299) &
                      *y(k,196) + rxt(k,312)*y(k,189) + rxt(k,320)*y(k,210) + rxt(k,324) &
                      *y(k,211) + rxt(k,336)*y(k,201) + rxt(k,345)*y(k,203) + rxt(k,349) &
                      *y(k,205) + rxt(k,359)*y(k,185) + rxt(k,369)*y(k,199) + rxt(k,374) &
                      *y(k,200) + rxt(k,383)*y(k,100) + rxt(k,394)*y(k,215) + rxt(k,398) &
                      *y(k,184) + rxt(k,401)*y(k,186) + rxt(k,405)*y(k,187) + rxt(k,408) &
                      *y(k,190) + rxt(k,412)*y(k,193) + rxt(k,415)*y(k,202) + rxt(k,418) &
                      *y(k,204) + rxt(k,421)*y(k,209) + rxt(k,428)*y(k,214) + rxt(k,434) &
                      *y(k,216) + rxt(k,437)*y(k,217) + rxt(k,448)*y(k,206) + rxt(k,453) &
                      *y(k,212) + rxt(k,458)*y(k,213))
         mat(k,1286) = -(rxt(k,136) + rxt(k,137) + rxt(k,138)) * y(k,197)
         mat(k,1363) = -rxt(k,140)*y(k,197)
         mat(k,1800) = -rxt(k,141)*y(k,197)
         mat(k,1534) = -rxt(k,145)*y(k,197)
         mat(k,1699) = -rxt(k,160)*y(k,197)
         mat(k,1994) = -rxt(k,165)*y(k,197)
         mat(k,1740) = -rxt(k,170)*y(k,197)
         mat(k,1642) = -(rxt(k,180) + rxt(k,181)) * y(k,197)
         mat(k,1333) = -rxt(k,187)*y(k,197)
         mat(k,1259) = -rxt(k,213)*y(k,197)
         mat(k,1558) = -rxt(k,219)*y(k,197)
         mat(k,1307) = -rxt(k,256)*y(k,197)
         mat(k,1608) = -rxt(k,262)*y(k,197)
         mat(k,345) = -rxt(k,270)*y(k,197)
         mat(k,696) = -rxt(k,284)*y(k,197)
         mat(k,1246) = -rxt(k,295)*y(k,197)
         mat(k,584) = -rxt(k,299)*y(k,197)
         mat(k,672) = -rxt(k,312)*y(k,197)
         mat(k,639) = -rxt(k,320)*y(k,197)
         mat(k,1006) = -rxt(k,324)*y(k,197)
         mat(k,1165) = -rxt(k,336)*y(k,197)
         mat(k,1216) = -rxt(k,345)*y(k,197)
         mat(k,552) = -rxt(k,349)*y(k,197)
         mat(k,820) = -rxt(k,359)*y(k,197)
         mat(k,1124) = -rxt(k,369)*y(k,197)
         mat(k,1198) = -rxt(k,374)*y(k,197)
         mat(k,1060) = -rxt(k,383)*y(k,197)
         mat(k,1037) = -rxt(k,394)*y(k,197)
         mat(k,414) = -rxt(k,398)*y(k,197)
         mat(k,382) = -rxt(k,401)*y(k,197)
         mat(k,339) = -rxt(k,405)*y(k,197)
         mat(k,509) = -rxt(k,408)*y(k,197)
         mat(k,630) = -rxt(k,412)*y(k,197)
         mat(k,590) = -rxt(k,415)*y(k,197)
         mat(k,753) = -rxt(k,418)*y(k,197)
         mat(k,352) = -rxt(k,421)*y(k,197)
         mat(k,605) = -rxt(k,428)*y(k,197)
         mat(k,622) = -rxt(k,434)*y(k,197)
         mat(k,390) = -rxt(k,437)*y(k,197)
         mat(k,993) = -rxt(k,448)*y(k,197)
         mat(k,973) = -rxt(k,453)*y(k,197)
         mat(k,954) = -rxt(k,458)*y(k,197)
         mat(k,779) = .570_r8*rxt(k,442)*y(k,131)
         mat(k,87) = .650_r8*rxt(k,400)*y(k,208)
         mat(k,1259) = mat(k,1259) + rxt(k,212)*y(k,41)
         mat(k,1558) = mat(k,1558) + rxt(k,224)*y(k,208)
         mat(k,205) = .350_r8*rxt(k,279)*y(k,208)
         mat(k,447) = .130_r8*rxt(k,281)*y(k,131)
         mat(k,176) = rxt(k,286)*y(k,208)
         mat(k,864) = .280_r8*rxt(k,310)*y(k,131)
         mat(k,1307) = mat(k,1307) + rxt(k,212)*y(k,16) + rxt(k,176)*y(k,55) &
                      + rxt(k,257)*y(k,123) + rxt(k,258)*y(k,130)
         mat(k,55) = rxt(k,292)*y(k,208)
         mat(k,678) = rxt(k,264)*y(k,208)
         mat(k,1642) = mat(k,1642) + rxt(k,176)*y(k,41) + rxt(k,179)*y(k,78)
         mat(k,1333) = mat(k,1333) + rxt(k,183)*y(k,192) + rxt(k,194)*y(k,208)
         mat(k,910) = rxt(k,267)*y(k,208)
         mat(k,121) = .730_r8*rxt(k,411)*y(k,208)
         mat(k,199) = .500_r8*rxt(k,481)*y(k,208)
         mat(k,872) = rxt(k,303)*y(k,208)
         mat(k,745) = rxt(k,304)*y(k,208)
         mat(k,472) = rxt(k,179)*y(k,55) + rxt(k,135)*y(k,130) + rxt(k,144)*y(k,208)
         mat(k,104) = rxt(k,268)*y(k,208)
         mat(k,662) = rxt(k,269)*y(k,208)
         mat(k,927) = rxt(k,333)*y(k,208)
         mat(k,934) = rxt(k,318)*y(k,208)
         mat(k,733) = .370_r8*rxt(k,387)*y(k,131)
         mat(k,483) = .300_r8*rxt(k,378)*y(k,208)
         mat(k,423) = rxt(k,379)*y(k,208)
         mat(k,1060) = mat(k,1060) + rxt(k,384)*y(k,121) + rxt(k,385)*y(k,123) &
                      + rxt(k,381)*y(k,191) + 1.200_r8*rxt(k,382)*y(k,192)
         mat(k,314) = rxt(k,386)*y(k,208)
         mat(k,1073) = .140_r8*rxt(k,340)*y(k,131)
         mat(k,219) = .200_r8*rxt(k,342)*y(k,208)
         mat(k,467) = .500_r8*rxt(k,353)*y(k,208)
         mat(k,805) = .570_r8*rxt(k,445)*y(k,131)
         mat(k,1144) = .280_r8*rxt(k,354)*y(k,131)
         mat(k,284) = rxt(k,390)*y(k,208)
         mat(k,886) = rxt(k,391)*y(k,208)
         mat(k,1994) = mat(k,1994) + rxt(k,384)*y(k,100) + rxt(k,360)*y(k,185) &
                      + rxt(k,402)*y(k,186) + rxt(k,407)*y(k,187) + rxt(k,285) &
                      *y(k,188) + rxt(k,313)*y(k,189) + rxt(k,263)*y(k,192) &
                      + .170_r8*rxt(k,413)*y(k,193) + rxt(k,331)*y(k,194) &
                      + .250_r8*rxt(k,300)*y(k,196) + rxt(k,272)*y(k,198) &
                      + .920_r8*rxt(k,370)*y(k,199) + .920_r8*rxt(k,376)*y(k,200) &
                      + .470_r8*rxt(k,338)*y(k,201) + .400_r8*rxt(k,416)*y(k,202) &
                      + .830_r8*rxt(k,419)*y(k,204) + rxt(k,422)*y(k,209) + rxt(k,321) &
                      *y(k,210) + .900_r8*rxt(k,454)*y(k,212) + .800_r8*rxt(k,459) &
                      *y(k,213) + rxt(k,429)*y(k,214) + rxt(k,395)*y(k,215) &
                      + rxt(k,435)*y(k,216) + rxt(k,438)*y(k,217)
         mat(k,1699) = mat(k,1699) + rxt(k,257)*y(k,41) + rxt(k,385)*y(k,100) &
                      + rxt(k,371)*y(k,199) + rxt(k,377)*y(k,200) + .470_r8*rxt(k,337) &
                      *y(k,201) + rxt(k,163)*y(k,208) + rxt(k,396)*y(k,215)
         mat(k,1363) = mat(k,1363) + rxt(k,258)*y(k,41) + rxt(k,135)*y(k,78)
         mat(k,1800) = mat(k,1800) + .570_r8*rxt(k,442)*y(k,5) + .130_r8*rxt(k,281) &
                      *y(k,24) + .280_r8*rxt(k,310)*y(k,28) + .370_r8*rxt(k,387) &
                      *y(k,97) + .140_r8*rxt(k,340)*y(k,104) + .570_r8*rxt(k,445) &
                      *y(k,109) + .280_r8*rxt(k,354)*y(k,110) + rxt(k,147)*y(k,208)
         mat(k,96) = .800_r8*rxt(k,423)*y(k,208)
         mat(k,711) = rxt(k,476)*y(k,208)
         mat(k,905) = .200_r8*rxt(k,463)*y(k,208)
         mat(k,116) = .280_r8*rxt(k,431)*y(k,208)
         mat(k,142) = .380_r8*rxt(k,433)*y(k,208)
         mat(k,147) = .630_r8*rxt(k,439)*y(k,208)
         mat(k,820) = mat(k,820) + rxt(k,360)*y(k,121)
         mat(k,382) = mat(k,382) + rxt(k,402)*y(k,121)
         mat(k,339) = mat(k,339) + rxt(k,407)*y(k,121)
         mat(k,696) = mat(k,696) + rxt(k,285)*y(k,121) + 2.400_r8*rxt(k,282)*y(k,188) &
                      + rxt(k,283)*y(k,192)
         mat(k,672) = mat(k,672) + rxt(k,313)*y(k,121) + rxt(k,311)*y(k,192)
         mat(k,1246) = mat(k,1246) + rxt(k,381)*y(k,100) + .900_r8*rxt(k,294)*y(k,192) &
                      + rxt(k,367)*y(k,199) + rxt(k,372)*y(k,200) + .470_r8*rxt(k,334) &
                      *y(k,201) + rxt(k,392)*y(k,215)
         mat(k,1608) = mat(k,1608) + rxt(k,183)*y(k,58) + 1.200_r8*rxt(k,382)*y(k,100) &
                      + rxt(k,263)*y(k,121) + rxt(k,283)*y(k,188) + rxt(k,311) &
                      *y(k,189) + .900_r8*rxt(k,294)*y(k,191) + 4.000_r8*rxt(k,260) &
                      *y(k,192) + rxt(k,368)*y(k,199) + rxt(k,373)*y(k,200) &
                      + .730_r8*rxt(k,335)*y(k,201) + rxt(k,344)*y(k,203) &
                      + .500_r8*rxt(k,447)*y(k,206) + .300_r8*rxt(k,323)*y(k,211) &
                      + rxt(k,452)*y(k,212) + rxt(k,457)*y(k,213) + .800_r8*rxt(k,393) &
                      *y(k,215)
         mat(k,630) = mat(k,630) + .170_r8*rxt(k,413)*y(k,121) + .070_r8*rxt(k,412) &
                      *y(k,197)
         mat(k,458) = rxt(k,331)*y(k,121)
         mat(k,584) = mat(k,584) + .250_r8*rxt(k,300)*y(k,121)
         mat(k,1893) = mat(k,1893) + .070_r8*rxt(k,412)*y(k,193) + .160_r8*rxt(k,415) &
                      *y(k,202) + .330_r8*rxt(k,418)*y(k,204)
         mat(k,345) = mat(k,345) + rxt(k,272)*y(k,121)
         mat(k,1124) = mat(k,1124) + .920_r8*rxt(k,370)*y(k,121) + rxt(k,371)*y(k,123) &
                      + rxt(k,367)*y(k,191) + rxt(k,368)*y(k,192)
         mat(k,1198) = mat(k,1198) + .920_r8*rxt(k,376)*y(k,121) + rxt(k,377)*y(k,123) &
                      + rxt(k,372)*y(k,191) + rxt(k,373)*y(k,192)
         mat(k,1165) = mat(k,1165) + .470_r8*rxt(k,338)*y(k,121) + .470_r8*rxt(k,337) &
                      *y(k,123) + .470_r8*rxt(k,334)*y(k,191) + .730_r8*rxt(k,335) &
                      *y(k,192)
         mat(k,590) = mat(k,590) + .400_r8*rxt(k,416)*y(k,121) + .160_r8*rxt(k,415) &
                      *y(k,197)
         mat(k,1216) = mat(k,1216) + rxt(k,344)*y(k,192)
         mat(k,753) = mat(k,753) + .830_r8*rxt(k,419)*y(k,121) + .330_r8*rxt(k,418) &
                      *y(k,197)
         mat(k,993) = mat(k,993) + .500_r8*rxt(k,447)*y(k,192)
         mat(k,1534) = mat(k,1534) + .650_r8*rxt(k,400)*y(k,6) + rxt(k,224)*y(k,18) &
                      + .350_r8*rxt(k,279)*y(k,23) + rxt(k,286)*y(k,25) + rxt(k,292) &
                      *y(k,46) + rxt(k,264)*y(k,51) + rxt(k,194)*y(k,58) + rxt(k,267) &
                      *y(k,61) + .730_r8*rxt(k,411)*y(k,65) + .500_r8*rxt(k,481) &
                      *y(k,66) + rxt(k,303)*y(k,73) + rxt(k,304)*y(k,74) + rxt(k,144) &
                      *y(k,78) + rxt(k,268)*y(k,85) + rxt(k,269)*y(k,86) + rxt(k,333) &
                      *y(k,92) + rxt(k,318)*y(k,94) + .300_r8*rxt(k,378)*y(k,98) &
                      + rxt(k,379)*y(k,99) + rxt(k,386)*y(k,101) + .200_r8*rxt(k,342) &
                      *y(k,105) + .500_r8*rxt(k,353)*y(k,108) + rxt(k,390)*y(k,114) &
                      + rxt(k,391)*y(k,115) + rxt(k,163)*y(k,123) + rxt(k,147) &
                      *y(k,131) + .800_r8*rxt(k,423)*y(k,139) + rxt(k,476)*y(k,148) &
                      + .200_r8*rxt(k,463)*y(k,174) + .280_r8*rxt(k,431)*y(k,176) &
                      + .380_r8*rxt(k,433)*y(k,178) + .630_r8*rxt(k,439)*y(k,180)
         mat(k,352) = mat(k,352) + rxt(k,422)*y(k,121)
         mat(k,639) = mat(k,639) + rxt(k,321)*y(k,121)
         mat(k,1006) = mat(k,1006) + .300_r8*rxt(k,323)*y(k,192)
         mat(k,973) = mat(k,973) + .900_r8*rxt(k,454)*y(k,121) + rxt(k,452)*y(k,192)
         mat(k,954) = mat(k,954) + .800_r8*rxt(k,459)*y(k,121) + rxt(k,457)*y(k,192)
         mat(k,605) = mat(k,605) + rxt(k,429)*y(k,121)
         mat(k,1037) = mat(k,1037) + rxt(k,395)*y(k,121) + rxt(k,396)*y(k,123) &
                      + rxt(k,392)*y(k,191) + .800_r8*rxt(k,393)*y(k,192)
         mat(k,622) = mat(k,622) + rxt(k,435)*y(k,121)
         mat(k,390) = mat(k,390) + rxt(k,438)*y(k,121)
      end do
      end subroutine nlnmat07
      subroutine nlnmat08( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,341) = -(rxt(k,270)*y(k,197) + rxt(k,272)*y(k,121))
         mat(k,1822) = -rxt(k,270)*y(k,198)
         mat(k,1928) = -rxt(k,272)*y(k,198)
         mat(k,1289) = rxt(k,256)*y(k,197)
         mat(k,1822) = mat(k,1822) + rxt(k,256)*y(k,41)
         mat(k,1111) = -(rxt(k,367)*y(k,191) + rxt(k,368)*y(k,192) + rxt(k,369) &
                      *y(k,197) + rxt(k,370)*y(k,121) + rxt(k,371)*y(k,123))
         mat(k,1232) = -rxt(k,367)*y(k,199)
         mat(k,1589) = -rxt(k,368)*y(k,199)
         mat(k,1873) = -rxt(k,369)*y(k,199)
         mat(k,1974) = -rxt(k,370)*y(k,199)
         mat(k,1679) = -rxt(k,371)*y(k,199)
         mat(k,724) = .600_r8*rxt(k,388)*y(k,208)
         mat(k,1514) = .600_r8*rxt(k,388)*y(k,97)
         mat(k,1187) = -(rxt(k,372)*y(k,191) + rxt(k,373)*y(k,192) + rxt(k,374) &
                      *y(k,197) + rxt(k,376)*y(k,121) + rxt(k,377)*y(k,123))
         mat(k,1235) = -rxt(k,372)*y(k,200)
         mat(k,1592) = -rxt(k,373)*y(k,200)
         mat(k,1876) = -rxt(k,374)*y(k,200)
         mat(k,1977) = -rxt(k,376)*y(k,200)
         mat(k,1682) = -rxt(k,377)*y(k,200)
         mat(k,726) = .400_r8*rxt(k,388)*y(k,208)
         mat(k,1517) = .400_r8*rxt(k,388)*y(k,97)
         mat(k,1155) = -(rxt(k,334)*y(k,191) + rxt(k,335)*y(k,192) + rxt(k,336) &
                      *y(k,197) + rxt(k,337)*y(k,123) + (rxt(k,338) + rxt(k,339) &
                      ) * y(k,121))
         mat(k,1234) = -rxt(k,334)*y(k,201)
         mat(k,1591) = -rxt(k,335)*y(k,201)
         mat(k,1875) = -rxt(k,336)*y(k,201)
         mat(k,1681) = -rxt(k,337)*y(k,201)
         mat(k,1976) = -(rxt(k,338) + rxt(k,339)) * y(k,201)
         mat(k,1066) = .500_r8*rxt(k,341)*y(k,208)
         mat(k,216) = .200_r8*rxt(k,342)*y(k,208)
         mat(k,1134) = rxt(k,355)*y(k,208)
         mat(k,1516) = .500_r8*rxt(k,341)*y(k,104) + .200_r8*rxt(k,342)*y(k,105) &
                      + rxt(k,355)*y(k,110)
         mat(k,586) = -(rxt(k,415)*y(k,197) + rxt(k,416)*y(k,121) + rxt(k,417) &
                      *y(k,122))
         mat(k,1842) = -rxt(k,415)*y(k,202)
         mat(k,1945) = -rxt(k,416)*y(k,202)
         mat(k,1713) = -rxt(k,417)*y(k,202)
         mat(k,1206) = -(rxt(k,343)*y(k,191) + rxt(k,344)*y(k,192) + rxt(k,345) &
                      *y(k,197) + 4._r8*rxt(k,346)*y(k,203) + rxt(k,347)*y(k,121) &
                      + rxt(k,348)*y(k,123) + rxt(k,356)*y(k,122))
         mat(k,1236) = -rxt(k,343)*y(k,203)
         mat(k,1593) = -rxt(k,344)*y(k,203)
         mat(k,1877) = -rxt(k,345)*y(k,203)
         mat(k,1978) = -rxt(k,347)*y(k,203)
         mat(k,1683) = -rxt(k,348)*y(k,203)
         mat(k,1724) = -rxt(k,356)*y(k,203)
         mat(k,1067) = .500_r8*rxt(k,341)*y(k,208)
         mat(k,217) = .500_r8*rxt(k,342)*y(k,208)
         mat(k,1518) = .500_r8*rxt(k,341)*y(k,104) + .500_r8*rxt(k,342)*y(k,105)
         mat(k,747) = -(rxt(k,418)*y(k,197) + rxt(k,419)*y(k,121) + rxt(k,420) &
                      *y(k,122))
         mat(k,1855) = -rxt(k,418)*y(k,204)
         mat(k,1955) = -rxt(k,419)*y(k,204)
         mat(k,1718) = -rxt(k,420)*y(k,204)
         mat(k,547) = -(rxt(k,349)*y(k,197) + rxt(k,350)*y(k,121))
         mat(k,1838) = -rxt(k,349)*y(k,205)
         mat(k,1943) = -rxt(k,350)*y(k,205)
         mat(k,399) = rxt(k,351)*y(k,208)
         mat(k,221) = rxt(k,352)*y(k,208)
         mat(k,1469) = rxt(k,351)*y(k,106) + rxt(k,352)*y(k,107)
         mat(k,983) = -(rxt(k,447)*y(k,192) + rxt(k,448)*y(k,197) + rxt(k,449) &
                      *y(k,121) + rxt(k,450)*y(k,123))
         mat(k,1583) = -rxt(k,447)*y(k,206)
         mat(k,1866) = -rxt(k,448)*y(k,206)
         mat(k,1968) = -rxt(k,449)*y(k,206)
         mat(k,1672) = -rxt(k,450)*y(k,206)
         mat(k,771) = rxt(k,441)*y(k,123)
         mat(k,797) = rxt(k,444)*y(k,123)
         mat(k,1672) = mat(k,1672) + rxt(k,441)*y(k,5) + rxt(k,444)*y(k,109) &
                      + .500_r8*rxt(k,461)*y(k,173)
         mat(k,299) = rxt(k,451)*y(k,208)
         mat(k,844) = .500_r8*rxt(k,461)*y(k,123)
         mat(k,1506) = rxt(k,451)*y(k,125)
         mat(k,1379) = -(rxt(k,126)*y(k,76) + rxt(k,127)*y(k,218) + rxt(k,130) &
                      *y(k,131) + (rxt(k,208) + rxt(k,209)) * y(k,84) + (rxt(k,231) &
                      + rxt(k,232)) * y(k,80) + rxt(k,237)*y(k,63) + rxt(k,238) &
                      *y(k,64) + rxt(k,276)*y(k,85))
         mat(k,1015) = -rxt(k,126)*y(k,207)
         mat(k,2011) = -rxt(k,127)*y(k,207)
         mat(k,1792) = -rxt(k,130)*y(k,207)
         mat(k,1908) = -(rxt(k,208) + rxt(k,209)) * y(k,207)
         mat(k,702) = -(rxt(k,231) + rxt(k,232)) * y(k,207)
         mat(k,62) = -rxt(k,237)*y(k,207)
         mat(k,99) = -rxt(k,238)*y(k,207)
         mat(k,102) = -rxt(k,276)*y(k,207)
         mat(k,1527) = -(rxt(k,143)*y(k,76) + rxt(k,144)*y(k,78) + rxt(k,145)*y(k,197) &
                      + rxt(k,146)*y(k,130) + rxt(k,147)*y(k,131) + (4._r8*rxt(k,148) &
                      + 4._r8*rxt(k,149)) * y(k,208) + rxt(k,151)*y(k,89) + rxt(k,163) &
                      *y(k,123) + rxt(k,164)*y(k,111) + rxt(k,172)*y(k,122) + rxt(k,173) &
                      *y(k,88) + rxt(k,192)*y(k,59) + (rxt(k,194) + rxt(k,195) &
                      ) * y(k,58) + rxt(k,197)*y(k,84) + rxt(k,200)*y(k,91) + rxt(k,224) &
                      *y(k,18) + rxt(k,226)*y(k,80) + rxt(k,259)*y(k,41) + rxt(k,264) &
                      *y(k,51) + rxt(k,265)*y(k,52) + (rxt(k,267) + rxt(k,277) &
                      ) * y(k,61) + rxt(k,268)*y(k,85) + rxt(k,269)*y(k,86) + rxt(k,279) &
                      *y(k,23) + rxt(k,286)*y(k,25) + rxt(k,287)*y(k,26) + rxt(k,289) &
                      *y(k,27) + rxt(k,291)*y(k,44) + rxt(k,292)*y(k,46) + rxt(k,297) &
                      *y(k,49) + rxt(k,298)*y(k,50) + rxt(k,303)*y(k,73) + rxt(k,304) &
                      *y(k,74) + rxt(k,305)*y(k,136) + rxt(k,306)*y(k,24) + rxt(k,314) &
                      *y(k,29) + rxt(k,315)*y(k,30) + rxt(k,317)*y(k,48) + rxt(k,318) &
                      *y(k,94) + rxt(k,319)*y(k,124) + rxt(k,322)*y(k,143) + rxt(k,326) &
                      *y(k,144) + rxt(k,327)*y(k,28) + rxt(k,328)*y(k,47) + rxt(k,330) &
                      *y(k,15) + rxt(k,333)*y(k,92) + rxt(k,341)*y(k,104) + rxt(k,342) &
                      *y(k,105) + rxt(k,351)*y(k,106) + rxt(k,352)*y(k,107) + rxt(k,353) &
                      *y(k,108) + rxt(k,355)*y(k,110) + rxt(k,358)*y(k,1) + rxt(k,362) &
                      *y(k,2) + rxt(k,363)*y(k,14) + rxt(k,364)*y(k,93) + rxt(k,365) &
                      *y(k,95) + rxt(k,366)*y(k,96) + rxt(k,378)*y(k,98) + rxt(k,379) &
                      *y(k,99) + rxt(k,386)*y(k,101) + rxt(k,388)*y(k,97) + rxt(k,389) &
                      *y(k,102) + rxt(k,390)*y(k,114) + rxt(k,391)*y(k,115) + rxt(k,397) &
                      *y(k,177) + rxt(k,400)*y(k,6) + rxt(k,403)*y(k,7) + rxt(k,404) &
                      *y(k,21) + rxt(k,406)*y(k,22) + rxt(k,410)*y(k,31) + rxt(k,411) &
                      *y(k,65) + rxt(k,423)*y(k,139) + rxt(k,426)*y(k,140) + rxt(k,430) &
                      *y(k,175) + rxt(k,431)*y(k,176) + rxt(k,433)*y(k,178) + rxt(k,436) &
                      *y(k,179) + rxt(k,439)*y(k,180) + rxt(k,440)*y(k,181) + rxt(k,443) &
                      *y(k,5) + rxt(k,446)*y(k,109) + rxt(k,451)*y(k,125) + rxt(k,455) &
                      *y(k,170) + rxt(k,456)*y(k,171) + rxt(k,460)*y(k,172) + rxt(k,462) &
                      *y(k,173) + rxt(k,463)*y(k,174) + rxt(k,465)*y(k,134) + rxt(k,470) &
                      *y(k,145) + rxt(k,475)*y(k,147) + rxt(k,476)*y(k,148) + (rxt(k,479) &
                      + rxt(k,481)) * y(k,66) + rxt(k,480)*y(k,119))
         mat(k,1016) = -rxt(k,143)*y(k,208)
         mat(k,470) = -rxt(k,144)*y(k,208)
         mat(k,1886) = -rxt(k,145)*y(k,208)
         mat(k,1356) = -rxt(k,146)*y(k,208)
         mat(k,1793) = -rxt(k,147)*y(k,208)
         mat(k,274) = -rxt(k,151)*y(k,208)
         mat(k,1692) = -rxt(k,163)*y(k,208)
         mat(k,270) = -rxt(k,164)*y(k,208)
         mat(k,1733) = -rxt(k,172)*y(k,208)
         mat(k,1268) = -rxt(k,173)*y(k,208)
         mat(k,834) = -rxt(k,192)*y(k,208)
         mat(k,1326) = -(rxt(k,194) + rxt(k,195)) * y(k,208)
         mat(k,1909) = -rxt(k,197)*y(k,208)
         mat(k,683) = -rxt(k,200)*y(k,208)
         mat(k,1551) = -rxt(k,224)*y(k,208)
         mat(k,703) = -rxt(k,226)*y(k,208)
         mat(k,1300) = -rxt(k,259)*y(k,208)
         mat(k,677) = -rxt(k,264)*y(k,208)
         mat(k,324) = -rxt(k,265)*y(k,208)
         mat(k,909) = -(rxt(k,267) + rxt(k,277)) * y(k,208)
         mat(k,103) = -rxt(k,268)*y(k,208)
         mat(k,661) = -rxt(k,269)*y(k,208)
         mat(k,204) = -rxt(k,279)*y(k,208)
         mat(k,175) = -rxt(k,286)*y(k,208)
         mat(k,249) = -rxt(k,287)*y(k,208)
         mat(k,208) = -rxt(k,289)*y(k,208)
         mat(k,892) = -rxt(k,291)*y(k,208)
         mat(k,54) = -rxt(k,292)*y(k,208)
         mat(k,449) = -rxt(k,297)*y(k,208)
         mat(k,406) = -rxt(k,298)*y(k,208)
         mat(k,871) = -rxt(k,303)*y(k,208)
         mat(k,744) = -rxt(k,304)*y(k,208)
         mat(k,360) = -rxt(k,305)*y(k,208)
         mat(k,445) = -rxt(k,306)*y(k,208)
         mat(k,318) = -rxt(k,314)*y(k,208)
         mat(k,58) = -rxt(k,315)*y(k,208)
         mat(k,1080) = -rxt(k,317)*y(k,208)
         mat(k,933) = -rxt(k,318)*y(k,208)
         mat(k,739) = -rxt(k,319)*y(k,208)
         mat(k,429) = -rxt(k,322)*y(k,208)
         mat(k,307) = -rxt(k,326)*y(k,208)
         mat(k,859) = -rxt(k,327)*y(k,208)
         mat(k,825) = -rxt(k,328)*y(k,208)
         mat(k,264) = -rxt(k,330)*y(k,208)
         mat(k,923) = -rxt(k,333)*y(k,208)
         mat(k,1071) = -rxt(k,341)*y(k,208)
         mat(k,218) = -rxt(k,342)*y(k,208)
         mat(k,402) = -rxt(k,351)*y(k,208)
         mat(k,224) = -rxt(k,352)*y(k,208)
         mat(k,464) = -rxt(k,353)*y(k,208)
         mat(k,1139) = -rxt(k,355)*y(k,208)
         mat(k,543) = -rxt(k,358)*y(k,208)
         mat(k,520) = -rxt(k,362)*y(k,208)
         mat(k,158) = -rxt(k,363)*y(k,208)
         mat(k,154) = -rxt(k,364)*y(k,208)
         mat(k,214) = -rxt(k,365)*y(k,208)
         mat(k,71) = -rxt(k,366)*y(k,208)
         mat(k,481) = -rxt(k,378)*y(k,208)
         mat(k,422) = -rxt(k,379)*y(k,208)
         mat(k,312) = -rxt(k,386)*y(k,208)
         mat(k,729) = -rxt(k,388)*y(k,208)
         mat(k,560) = -rxt(k,389)*y(k,208)
         mat(k,283) = -rxt(k,390)*y(k,208)
         mat(k,883) = -rxt(k,391)*y(k,208)
         mat(k,128) = -rxt(k,397)*y(k,208)
         mat(k,86) = -rxt(k,400)*y(k,208)
         mat(k,289) = -rxt(k,403)*y(k,208)
         mat(k,167) = -rxt(k,404)*y(k,208)
         mat(k,244) = -rxt(k,406)*y(k,208)
         mat(k,180) = -rxt(k,410)*y(k,208)
         mat(k,120) = -rxt(k,411)*y(k,208)
         mat(k,95) = -rxt(k,423)*y(k,208)
         mat(k,238) = -rxt(k,426)*y(k,208)
         mat(k,533) = -rxt(k,430)*y(k,208)
         mat(k,115) = -rxt(k,431)*y(k,208)
         mat(k,141) = -rxt(k,433)*y(k,208)
         mat(k,575) = -rxt(k,436)*y(k,208)
         mat(k,146) = -rxt(k,439)*y(k,208)
         mat(k,331) = -rxt(k,440)*y(k,208)
         mat(k,776) = -rxt(k,443)*y(k,208)
         mat(k,802) = -rxt(k,446)*y(k,208)
         mat(k,301) = -rxt(k,451)*y(k,208)
         mat(k,495) = -rxt(k,455)*y(k,208)
         mat(k,500) = -rxt(k,456)*y(k,208)
         mat(k,373) = -rxt(k,460)*y(k,208)
         mat(k,845) = -rxt(k,462)*y(k,208)
         mat(k,903) = -rxt(k,463)*y(k,208)
         mat(k,258) = -rxt(k,465)*y(k,208)
         mat(k,396) = -rxt(k,470)*y(k,208)
         mat(k,1092) = -rxt(k,475)*y(k,208)
         mat(k,710) = -rxt(k,476)*y(k,208)
         mat(k,197) = -(rxt(k,479) + rxt(k,481)) * y(k,208)
         mat(k,51) = -rxt(k,480)*y(k,208)
         mat(k,776) = mat(k,776) + .630_r8*rxt(k,442)*y(k,131)
         mat(k,204) = mat(k,204) + .650_r8*rxt(k,279)*y(k,208)
         mat(k,445) = mat(k,445) + .130_r8*rxt(k,281)*y(k,131)
         mat(k,249) = mat(k,249) + .500_r8*rxt(k,287)*y(k,208)
         mat(k,859) = mat(k,859) + .360_r8*rxt(k,310)*y(k,131)
         mat(k,1300) = mat(k,1300) + rxt(k,258)*y(k,130)
         mat(k,324) = mat(k,324) + .300_r8*rxt(k,265)*y(k,208)
         mat(k,1635) = rxt(k,181)*y(k,197)
         mat(k,656) = rxt(k,235)*y(k,218)
         mat(k,1280) = rxt(k,142)*y(k,131) + 2.000_r8*rxt(k,137)*y(k,197)
         mat(k,1016) = mat(k,1016) + rxt(k,134)*y(k,130) + rxt(k,126)*y(k,207)
         mat(k,470) = mat(k,470) + rxt(k,135)*y(k,130)
         mat(k,703) = mat(k,703) + rxt(k,225)*y(k,130) + rxt(k,231)*y(k,207)
         mat(k,1909) = mat(k,1909) + rxt(k,196)*y(k,130) + rxt(k,208)*y(k,207)
         mat(k,103) = mat(k,103) + rxt(k,276)*y(k,207)
         mat(k,646) = rxt(k,227)*y(k,130)
         mat(k,683) = mat(k,683) + rxt(k,199)*y(k,130)
         mat(k,729) = mat(k,729) + .320_r8*rxt(k,387)*y(k,131)
         mat(k,560) = mat(k,560) + .600_r8*rxt(k,389)*y(k,208)
         mat(k,1071) = mat(k,1071) + .240_r8*rxt(k,340)*y(k,131)
         mat(k,218) = mat(k,218) + .100_r8*rxt(k,342)*y(k,208)
         mat(k,802) = mat(k,802) + .630_r8*rxt(k,445)*y(k,131)
         mat(k,1139) = mat(k,1139) + .360_r8*rxt(k,354)*y(k,131)
         mat(k,1987) = rxt(k,165)*y(k,197)
         mat(k,1692) = mat(k,1692) + rxt(k,160)*y(k,197)
         mat(k,1356) = mat(k,1356) + rxt(k,258)*y(k,41) + rxt(k,134)*y(k,76) &
                      + rxt(k,135)*y(k,78) + rxt(k,225)*y(k,80) + rxt(k,196)*y(k,84) &
                      + rxt(k,227)*y(k,90) + rxt(k,199)*y(k,91) + rxt(k,140)*y(k,197)
         mat(k,1793) = mat(k,1793) + .630_r8*rxt(k,442)*y(k,5) + .130_r8*rxt(k,281) &
                      *y(k,24) + .360_r8*rxt(k,310)*y(k,28) + rxt(k,142)*y(k,75) &
                      + .320_r8*rxt(k,387)*y(k,97) + .240_r8*rxt(k,340)*y(k,104) &
                      + .630_r8*rxt(k,445)*y(k,109) + .360_r8*rxt(k,354)*y(k,110) &
                      + rxt(k,141)*y(k,197)
         mat(k,429) = mat(k,429) + .500_r8*rxt(k,322)*y(k,208)
         mat(k,128) = mat(k,128) + .500_r8*rxt(k,397)*y(k,208)
         mat(k,412) = .400_r8*rxt(k,398)*y(k,197)
         mat(k,1241) = .450_r8*rxt(k,295)*y(k,197)
         mat(k,627) = .400_r8*rxt(k,412)*y(k,197)
         mat(k,1886) = mat(k,1886) + rxt(k,181)*y(k,55) + 2.000_r8*rxt(k,137)*y(k,75) &
                      + rxt(k,165)*y(k,121) + rxt(k,160)*y(k,123) + rxt(k,140) &
                      *y(k,130) + rxt(k,141)*y(k,131) + .400_r8*rxt(k,398)*y(k,184) &
                      + .450_r8*rxt(k,295)*y(k,191) + .400_r8*rxt(k,412)*y(k,193) &
                      + .450_r8*rxt(k,345)*y(k,203) + .400_r8*rxt(k,418)*y(k,204) &
                      + .200_r8*rxt(k,349)*y(k,205) + .150_r8*rxt(k,324)*y(k,211)
         mat(k,1211) = .450_r8*rxt(k,345)*y(k,197)
         mat(k,750) = .400_r8*rxt(k,418)*y(k,197)
         mat(k,550) = .200_r8*rxt(k,349)*y(k,197)
         mat(k,1380) = rxt(k,126)*y(k,76) + rxt(k,231)*y(k,80) + rxt(k,208)*y(k,84) &
                      + rxt(k,276)*y(k,85) + 2.000_r8*rxt(k,127)*y(k,218)
         mat(k,1527) = mat(k,1527) + .650_r8*rxt(k,279)*y(k,23) + .500_r8*rxt(k,287) &
                      *y(k,26) + .300_r8*rxt(k,265)*y(k,52) + .600_r8*rxt(k,389) &
                      *y(k,102) + .100_r8*rxt(k,342)*y(k,105) + .500_r8*rxt(k,322) &
                      *y(k,143) + .500_r8*rxt(k,397)*y(k,177)
         mat(k,1003) = .150_r8*rxt(k,324)*y(k,197)
         mat(k,2012) = rxt(k,235)*y(k,72) + 2.000_r8*rxt(k,127)*y(k,207)
      end do
      end subroutine nlnmat08
      subroutine nlnmat09( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,348) = -(rxt(k,421)*y(k,197) + rxt(k,422)*y(k,121))
         mat(k,1823) = -rxt(k,421)*y(k,209)
         mat(k,1929) = -rxt(k,422)*y(k,209)
         mat(k,118) = .200_r8*rxt(k,411)*y(k,208)
         mat(k,93) = .140_r8*rxt(k,423)*y(k,208)
         mat(k,236) = rxt(k,426)*y(k,208)
         mat(k,1443) = .200_r8*rxt(k,411)*y(k,65) + .140_r8*rxt(k,423)*y(k,139) &
                      + rxt(k,426)*y(k,140)
         mat(k,633) = -(rxt(k,320)*y(k,197) + rxt(k,321)*y(k,121))
         mat(k,1846) = -rxt(k,320)*y(k,210)
         mat(k,1949) = -rxt(k,321)*y(k,210)
         mat(k,849) = rxt(k,327)*y(k,208)
         mat(k,425) = .500_r8*rxt(k,322)*y(k,208)
         mat(k,1477) = rxt(k,327)*y(k,28) + .500_r8*rxt(k,322)*y(k,143)
         mat(k,999) = -(rxt(k,323)*y(k,192) + rxt(k,324)*y(k,197) + rxt(k,325) &
                      *y(k,121))
         mat(k,1584) = -rxt(k,323)*y(k,211)
         mat(k,1867) = -rxt(k,324)*y(k,211)
         mat(k,1969) = -rxt(k,325)*y(k,211)
         mat(k,772) = .060_r8*rxt(k,442)*y(k,131)
         mat(k,823) = rxt(k,328)*y(k,208)
         mat(k,798) = .060_r8*rxt(k,445)*y(k,131)
         mat(k,1775) = .060_r8*rxt(k,442)*y(k,5) + .060_r8*rxt(k,445)*y(k,109)
         mat(k,304) = rxt(k,326)*y(k,208)
         mat(k,900) = .150_r8*rxt(k,463)*y(k,208)
         mat(k,1507) = rxt(k,328)*y(k,47) + rxt(k,326)*y(k,144) + .150_r8*rxt(k,463) &
                      *y(k,174)
         mat(k,964) = -(rxt(k,452)*y(k,192) + rxt(k,453)*y(k,197) + rxt(k,454) &
                      *y(k,121))
         mat(k,1582) = -rxt(k,452)*y(k,212)
         mat(k,1865) = -rxt(k,453)*y(k,212)
         mat(k,1967) = -rxt(k,454)*y(k,212)
         mat(k,1671) = .500_r8*rxt(k,461)*y(k,173)
         mat(k,493) = rxt(k,455)*y(k,208)
         mat(k,843) = .500_r8*rxt(k,461)*y(k,123) + rxt(k,462)*y(k,208)
         mat(k,1505) = rxt(k,455)*y(k,170) + rxt(k,462)*y(k,173)
         mat(k,942) = -(rxt(k,457)*y(k,192) + rxt(k,458)*y(k,197) + rxt(k,459) &
                      *y(k,121))
         mat(k,1581) = -rxt(k,457)*y(k,213)
         mat(k,1864) = -rxt(k,458)*y(k,213)
         mat(k,1966) = -rxt(k,459)*y(k,213)
         mat(k,770) = rxt(k,443)*y(k,208)
         mat(k,796) = rxt(k,446)*y(k,208)
         mat(k,371) = rxt(k,460)*y(k,208)
         mat(k,1504) = rxt(k,443)*y(k,5) + rxt(k,446)*y(k,109) + rxt(k,460)*y(k,172)
         mat(k,597) = -(rxt(k,428)*y(k,197) + rxt(k,429)*y(k,121))
         mat(k,1843) = -rxt(k,428)*y(k,214)
         mat(k,1946) = -rxt(k,429)*y(k,214)
         mat(k,527) = rxt(k,430)*y(k,208)
         mat(k,114) = .650_r8*rxt(k,431)*y(k,208)
         mat(k,1474) = rxt(k,430)*y(k,175) + .650_r8*rxt(k,431)*y(k,176)
         mat(k,1028) = -(rxt(k,392)*y(k,191) + rxt(k,393)*y(k,192) + rxt(k,394) &
                      *y(k,197) + rxt(k,395)*y(k,121) + rxt(k,396)*y(k,123))
         mat(k,1228) = -rxt(k,392)*y(k,215)
         mat(k,1585) = -rxt(k,393)*y(k,215)
         mat(k,1869) = -rxt(k,394)*y(k,215)
         mat(k,1970) = -rxt(k,395)*y(k,215)
         mat(k,1674) = -rxt(k,396)*y(k,215)
         mat(k,153) = rxt(k,364)*y(k,208)
         mat(k,213) = rxt(k,365)*y(k,208)
         mat(k,70) = rxt(k,366)*y(k,208)
         mat(k,556) = .400_r8*rxt(k,389)*y(k,208)
         mat(k,127) = .500_r8*rxt(k,397)*y(k,208)
         mat(k,1509) = rxt(k,364)*y(k,93) + rxt(k,365)*y(k,95) + rxt(k,366)*y(k,96) &
                      + .400_r8*rxt(k,389)*y(k,102) + .500_r8*rxt(k,397)*y(k,177)
         mat(k,613) = -(rxt(k,434)*y(k,197) + rxt(k,435)*y(k,121))
         mat(k,1844) = -rxt(k,434)*y(k,216)
         mat(k,1947) = -rxt(k,435)*y(k,216)
         mat(k,138) = .560_r8*rxt(k,433)*y(k,208)
         mat(k,568) = rxt(k,436)*y(k,208)
         mat(k,1475) = .560_r8*rxt(k,433)*y(k,178) + rxt(k,436)*y(k,179)
         mat(k,385) = -(rxt(k,437)*y(k,197) + rxt(k,438)*y(k,121))
         mat(k,1828) = -rxt(k,437)*y(k,217)
         mat(k,1933) = -rxt(k,438)*y(k,217)
         mat(k,145) = .300_r8*rxt(k,439)*y(k,208)
         mat(k,328) = rxt(k,440)*y(k,208)
         mat(k,1449) = .300_r8*rxt(k,439)*y(k,180) + rxt(k,440)*y(k,181)
         mat(k,2022) = -(rxt(k,127)*y(k,207) + rxt(k,235)*y(k,72) + rxt(k,477) &
                      *y(k,149))
         mat(k,1390) = -rxt(k,127)*y(k,218)
         mat(k,659) = -rxt(k,235)*y(k,218)
         mat(k,172) = -rxt(k,477)*y(k,218)
         mat(k,211) = rxt(k,289)*y(k,208)
         mat(k,320) = rxt(k,314)*y(k,208)
         mat(k,59) = rxt(k,315)*y(k,208)
         mat(k,1310) = rxt(k,259)*y(k,208)
         mat(k,896) = rxt(k,291)*y(k,208)
         mat(k,827) = rxt(k,328)*y(k,208)
         mat(k,1083) = rxt(k,317)*y(k,208)
         mat(k,451) = rxt(k,297)*y(k,208)
         mat(k,408) = rxt(k,298)*y(k,208)
         mat(k,326) = rxt(k,265)*y(k,208)
         mat(k,1288) = rxt(k,138)*y(k,197)
         mat(k,1021) = rxt(k,143)*y(k,208)
         mat(k,474) = rxt(k,144)*y(k,208)
         mat(k,705) = rxt(k,226)*y(k,208)
         mat(k,1919) = (rxt(k,518)+rxt(k,523))*y(k,90) + (rxt(k,511)+rxt(k,517) &
                       +rxt(k,522))*y(k,91) + rxt(k,197)*y(k,208)
         mat(k,663) = rxt(k,269)*y(k,208)
         mat(k,1274) = rxt(k,173)*y(k,208)
         mat(k,278) = rxt(k,151)*y(k,208)
         mat(k,650) = (rxt(k,518)+rxt(k,523))*y(k,84)
         mat(k,686) = (rxt(k,511)+rxt(k,517)+rxt(k,522))*y(k,84) + rxt(k,200)*y(k,208)
         mat(k,1074) = .500_r8*rxt(k,341)*y(k,208)
         mat(k,52) = rxt(k,480)*y(k,208)
         mat(k,431) = rxt(k,322)*y(k,208)
         mat(k,308) = rxt(k,326)*y(k,208)
         mat(k,1896) = rxt(k,138)*y(k,75) + rxt(k,145)*y(k,208)
         mat(k,1537) = rxt(k,289)*y(k,27) + rxt(k,314)*y(k,29) + rxt(k,315)*y(k,30) &
                      + rxt(k,259)*y(k,41) + rxt(k,291)*y(k,44) + rxt(k,328)*y(k,47) &
                      + rxt(k,317)*y(k,48) + rxt(k,297)*y(k,49) + rxt(k,298)*y(k,50) &
                      + rxt(k,265)*y(k,52) + rxt(k,143)*y(k,76) + rxt(k,144)*y(k,78) &
                      + rxt(k,226)*y(k,80) + rxt(k,197)*y(k,84) + rxt(k,269)*y(k,86) &
                      + rxt(k,173)*y(k,88) + rxt(k,151)*y(k,89) + rxt(k,200)*y(k,91) &
                      + .500_r8*rxt(k,341)*y(k,104) + rxt(k,480)*y(k,119) + rxt(k,322) &
                      *y(k,143) + rxt(k,326)*y(k,144) + rxt(k,145)*y(k,197) &
                      + 2.000_r8*rxt(k,148)*y(k,208)
      end do
      end subroutine nlnmat09
      subroutine nlnmat_finit( avec_len, mat, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k, 1) = lmat(k, 1)
         mat(k, 2) = lmat(k, 2)
         mat(k, 3) = lmat(k, 3)
         mat(k, 4) = lmat(k, 4)
         mat(k, 5) = lmat(k, 5)
         mat(k, 6) = lmat(k, 6)
         mat(k, 7) = lmat(k, 7)
         mat(k, 8) = lmat(k, 8)
         mat(k, 9) = lmat(k, 9)
         mat(k, 10) = lmat(k, 10)
         mat(k, 11) = lmat(k, 11)
         mat(k, 12) = lmat(k, 12)
         mat(k, 13) = lmat(k, 13)
         mat(k, 14) = lmat(k, 14)
         mat(k, 15) = lmat(k, 15)
         mat(k, 16) = lmat(k, 16)
         mat(k, 17) = lmat(k, 17)
         mat(k, 18) = lmat(k, 18)
         mat(k, 19) = lmat(k, 19)
         mat(k, 20) = lmat(k, 20)
         mat(k, 21) = lmat(k, 21)
         mat(k, 22) = lmat(k, 22)
         mat(k, 23) = lmat(k, 23)
         mat(k, 24) = lmat(k, 24)
         mat(k, 25) = lmat(k, 25)
         mat(k, 26) = lmat(k, 26)
         mat(k, 27) = lmat(k, 27)
         mat(k, 28) = lmat(k, 28)
         mat(k, 29) = lmat(k, 29)
         mat(k, 30) = lmat(k, 30)
         mat(k, 31) = lmat(k, 31)
         mat(k, 32) = lmat(k, 32)
         mat(k, 33) = lmat(k, 33)
         mat(k, 34) = lmat(k, 34)
         mat(k, 40) = mat(k, 40) + lmat(k, 40)
         mat(k, 46) = mat(k, 46) + lmat(k, 46)
         mat(k, 47) = lmat(k, 47)
         mat(k, 48) = lmat(k, 48)
         mat(k, 49) = lmat(k, 49)
         mat(k, 50) = mat(k, 50) + lmat(k, 50)
         mat(k, 53) = mat(k, 53) + lmat(k, 53)
         mat(k, 56) = mat(k, 56) + lmat(k, 56)
         mat(k, 60) = mat(k, 60) + lmat(k, 60)
         mat(k, 61) = mat(k, 61) + lmat(k, 61)
         mat(k, 63) = lmat(k, 63)
         mat(k, 64) = lmat(k, 64)
         mat(k, 65) = lmat(k, 65)
         mat(k, 66) = lmat(k, 66)
         mat(k, 67) = lmat(k, 67)
         mat(k, 68) = lmat(k, 68)
         mat(k, 69) = mat(k, 69) + lmat(k, 69)
         mat(k, 72) = lmat(k, 72)
         mat(k, 73) = lmat(k, 73)
         mat(k, 74) = lmat(k, 74)
         mat(k, 75) = lmat(k, 75)
         mat(k, 76) = lmat(k, 76)
         mat(k, 82) = mat(k, 82) + lmat(k, 82)
         mat(k, 88) = lmat(k, 88)
         mat(k, 89) = lmat(k, 89)
         mat(k, 90) = lmat(k, 90)
         mat(k, 91) = lmat(k, 91)
         mat(k, 92) = mat(k, 92) + lmat(k, 92)
         mat(k, 97) = mat(k, 97) + lmat(k, 97)
         mat(k, 98) = mat(k, 98) + lmat(k, 98)
         mat(k, 100) = mat(k, 100) + lmat(k, 100)
         mat(k, 101) = mat(k, 101) + lmat(k, 101)
         mat(k, 110) = mat(k, 110) + lmat(k, 110)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 122) = lmat(k, 122)
         mat(k, 123) = lmat(k, 123)
         mat(k, 124) = lmat(k, 124)
         mat(k, 125) = lmat(k, 125)
         mat(k, 126) = mat(k, 126) + lmat(k, 126)
         mat(k, 128) = mat(k, 128) + lmat(k, 128)
         mat(k, 135) = mat(k, 135) + lmat(k, 135)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 148) = lmat(k, 148)
         mat(k, 149) = lmat(k, 149)
         mat(k, 150) = lmat(k, 150)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = lmat(k, 152)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 155) = lmat(k, 155)
         mat(k, 156) = mat(k, 156) + lmat(k, 156)
         mat(k, 159) = lmat(k, 159)
         mat(k, 160) = lmat(k, 160)
         mat(k, 161) = lmat(k, 161)
         mat(k, 162) = lmat(k, 162)
         mat(k, 163) = lmat(k, 163)
         mat(k, 164) = lmat(k, 164)
         mat(k, 165) = mat(k, 165) + lmat(k, 165)
         mat(k, 169) = mat(k, 169) + lmat(k, 169)
         mat(k, 170) = lmat(k, 170)
         mat(k, 171) = lmat(k, 171)
         mat(k, 173) = mat(k, 173) + lmat(k, 173)
         mat(k, 177) = mat(k, 177) + lmat(k, 177)
         mat(k, 178) = lmat(k, 178)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = lmat(k, 181)
         mat(k, 182) = lmat(k, 182)
         mat(k, 183) = lmat(k, 183)
         mat(k, 184) = lmat(k, 184)
         mat(k, 185) = lmat(k, 185)
         mat(k, 186) = lmat(k, 186)
         mat(k, 187) = lmat(k, 187)
         mat(k, 188) = lmat(k, 188)
         mat(k, 189) = lmat(k, 189)
         mat(k, 190) = lmat(k, 190)
         mat(k, 191) = lmat(k, 191)
         mat(k, 192) = lmat(k, 192)
         mat(k, 193) = lmat(k, 193)
         mat(k, 194) = mat(k, 194) + lmat(k, 194)
         mat(k, 200) = mat(k, 200) + lmat(k, 200)
         mat(k, 206) = mat(k, 206) + lmat(k, 206)
         mat(k, 212) = mat(k, 212) + lmat(k, 212)
         mat(k, 215) = mat(k, 215) + lmat(k, 215)
         mat(k, 220) = mat(k, 220) + lmat(k, 220)
         mat(k, 222) = lmat(k, 222)
         mat(k, 223) = lmat(k, 223)
         mat(k, 224) = mat(k, 224) + lmat(k, 224)
         mat(k, 225) = lmat(k, 225)
         mat(k, 226) = lmat(k, 226)
         mat(k, 227) = lmat(k, 227)
         mat(k, 228) = lmat(k, 228)
         mat(k, 229) = lmat(k, 229)
         mat(k, 230) = mat(k, 230) + lmat(k, 230)
         mat(k, 233) = mat(k, 233) + lmat(k, 233)
         mat(k, 234) = lmat(k, 234)
         mat(k, 235) = mat(k, 235) + lmat(k, 235)
         mat(k, 237) = lmat(k, 237)
         mat(k, 238) = mat(k, 238) + lmat(k, 238)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 241) = mat(k, 241) + lmat(k, 241)
         mat(k, 244) = mat(k, 244) + lmat(k, 244)
         mat(k, 245) = lmat(k, 245)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 248) = mat(k, 248) + lmat(k, 248)
         mat(k, 249) = mat(k, 249) + lmat(k, 249)
         mat(k, 250) = lmat(k, 250)
         mat(k, 251) = mat(k, 251) + lmat(k, 251)
         mat(k, 252) = lmat(k, 252)
         mat(k, 254) = mat(k, 254) + lmat(k, 254)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 269) = mat(k, 269) + lmat(k, 269)
         mat(k, 272) = mat(k, 272) + lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 275) = lmat(k, 275)
         mat(k, 276) = mat(k, 276) + lmat(k, 276)
         mat(k, 277) = lmat(k, 277)
         mat(k, 279) = mat(k, 279) + lmat(k, 279)
         mat(k, 282) = lmat(k, 282)
         mat(k, 285) = mat(k, 285) + lmat(k, 285)
         mat(k, 286) = lmat(k, 286)
         mat(k, 288) = lmat(k, 288)
         mat(k, 289) = mat(k, 289) + lmat(k, 289)
         mat(k, 290) = lmat(k, 290)
         mat(k, 291) = lmat(k, 291)
         mat(k, 292) = lmat(k, 292)
         mat(k, 293) = lmat(k, 293)
         mat(k, 294) = lmat(k, 294)
         mat(k, 295) = lmat(k, 295)
         mat(k, 296) = lmat(k, 296)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 298) = lmat(k, 298)
         mat(k, 300) = lmat(k, 300)
         mat(k, 301) = mat(k, 301) + lmat(k, 301)
         mat(k, 302) = lmat(k, 302)
         mat(k, 303) = mat(k, 303) + lmat(k, 303)
         mat(k, 305) = lmat(k, 305)
         mat(k, 306) = lmat(k, 306)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 309) = mat(k, 309) + lmat(k, 309)
         mat(k, 310) = lmat(k, 310)
         mat(k, 313) = lmat(k, 313)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 315) = mat(k, 315) + lmat(k, 315)
         mat(k, 317) = lmat(k, 317)
         mat(k, 318) = mat(k, 318) + lmat(k, 318)
         mat(k, 319) = lmat(k, 319)
         mat(k, 321) = mat(k, 321) + lmat(k, 321)
         mat(k, 322) = lmat(k, 322)
         mat(k, 323) = mat(k, 323) + lmat(k, 323)
         mat(k, 324) = mat(k, 324) + lmat(k, 324)
         mat(k, 327) = mat(k, 327) + lmat(k, 327)
         mat(k, 329) = lmat(k, 329)
         mat(k, 330) = lmat(k, 330)
         mat(k, 331) = mat(k, 331) + lmat(k, 331)
         mat(k, 332) = lmat(k, 332)
         mat(k, 335) = mat(k, 335) + lmat(k, 335)
         mat(k, 341) = mat(k, 341) + lmat(k, 341)
         mat(k, 343) = lmat(k, 343)
         mat(k, 345) = mat(k, 345) + lmat(k, 345)
         mat(k, 348) = mat(k, 348) + lmat(k, 348)
         mat(k, 354) = lmat(k, 354)
         mat(k, 355) = lmat(k, 355)
         mat(k, 356) = lmat(k, 356)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 358) = lmat(k, 358)
         mat(k, 361) = lmat(k, 361)
         mat(k, 362) = mat(k, 362) + lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = mat(k, 364) + lmat(k, 364)
         mat(k, 368) = mat(k, 368) + lmat(k, 368)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 372) = lmat(k, 372)
         mat(k, 373) = mat(k, 373) + lmat(k, 373)
         mat(k, 374) = lmat(k, 374)
         mat(k, 377) = mat(k, 377) + lmat(k, 377)
         mat(k, 385) = mat(k, 385) + lmat(k, 385)
         mat(k, 392) = mat(k, 392) + lmat(k, 392)
         mat(k, 393) = mat(k, 393) + lmat(k, 393)
         mat(k, 395) = lmat(k, 395)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 400) = lmat(k, 400)
         mat(k, 401) = lmat(k, 401)
         mat(k, 403) = mat(k, 403) + lmat(k, 403)
         mat(k, 406) = mat(k, 406) + lmat(k, 406)
         mat(k, 407) = lmat(k, 407)
         mat(k, 410) = mat(k, 410) + lmat(k, 410)
         mat(k, 416) = mat(k, 416) + lmat(k, 416)
         mat(k, 421) = lmat(k, 421)
         mat(k, 424) = mat(k, 424) + lmat(k, 424)
         mat(k, 426) = lmat(k, 426)
         mat(k, 428) = lmat(k, 428)
         mat(k, 429) = mat(k, 429) + lmat(k, 429)
         mat(k, 430) = lmat(k, 430)
         mat(k, 432) = mat(k, 432) + lmat(k, 432)
         mat(k, 433) = lmat(k, 433)
         mat(k, 434) = lmat(k, 434)
         mat(k, 435) = lmat(k, 435)
         mat(k, 437) = mat(k, 437) + lmat(k, 437)
         mat(k, 438) = mat(k, 438) + lmat(k, 438)
         mat(k, 439) = lmat(k, 439)
         mat(k, 440) = mat(k, 440) + lmat(k, 440)
         mat(k, 448) = mat(k, 448) + lmat(k, 448)
         mat(k, 452) = mat(k, 452) + lmat(k, 452)
         mat(k, 460) = mat(k, 460) + lmat(k, 460)
         mat(k, 462) = lmat(k, 462)
         mat(k, 466) = lmat(k, 466)
         mat(k, 468) = mat(k, 468) + lmat(k, 468)
         mat(k, 470) = mat(k, 470) + lmat(k, 470)
         mat(k, 475) = mat(k, 475) + lmat(k, 475)
         mat(k, 479) = lmat(k, 479)
         mat(k, 484) = lmat(k, 484)
         mat(k, 485) = lmat(k, 485)
         mat(k, 486) = lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 488) = mat(k, 488) + lmat(k, 488)
         mat(k, 489) = lmat(k, 489)
         mat(k, 490) = lmat(k, 490)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = lmat(k, 492)
         mat(k, 494) = lmat(k, 494)
         mat(k, 495) = mat(k, 495) + lmat(k, 495)
         mat(k, 496) = lmat(k, 496)
         mat(k, 497) = mat(k, 497) + lmat(k, 497)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 499) = lmat(k, 499)
         mat(k, 501) = mat(k, 501) + lmat(k, 501)
         mat(k, 502) = lmat(k, 502)
         mat(k, 505) = mat(k, 505) + lmat(k, 505)
         mat(k, 511) = lmat(k, 511)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 516) = lmat(k, 516)
         mat(k, 517) = lmat(k, 517)
         mat(k, 519) = lmat(k, 519)
         mat(k, 520) = mat(k, 520) + lmat(k, 520)
         mat(k, 521) = lmat(k, 521)
         mat(k, 522) = lmat(k, 522)
         mat(k, 523) = lmat(k, 523)
         mat(k, 524) = lmat(k, 524)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 529) = lmat(k, 529)
         mat(k, 532) = lmat(k, 532)
         mat(k, 533) = mat(k, 533) + lmat(k, 533)
         mat(k, 534) = lmat(k, 534)
         mat(k, 535) = lmat(k, 535)
         mat(k, 536) = mat(k, 536) + lmat(k, 536)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
         mat(k, 540) = mat(k, 540) + lmat(k, 540)
         mat(k, 542) = mat(k, 542) + lmat(k, 542)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 545) = lmat(k, 545)
         mat(k, 547) = mat(k, 547) + lmat(k, 547)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 557) = lmat(k, 557)
         mat(k, 558) = lmat(k, 558)
         mat(k, 559) = lmat(k, 559)
         mat(k, 561) = lmat(k, 561)
         mat(k, 562) = lmat(k, 562)
         mat(k, 563) = lmat(k, 563)
         mat(k, 564) = lmat(k, 564)
         mat(k, 565) = lmat(k, 565)
         mat(k, 566) = mat(k, 566) + lmat(k, 566)
         mat(k, 570) = lmat(k, 570)
         mat(k, 573) = lmat(k, 573)
         mat(k, 575) = mat(k, 575) + lmat(k, 575)
         mat(k, 576) = lmat(k, 576)
         mat(k, 579) = mat(k, 579) + lmat(k, 579)
         mat(k, 586) = mat(k, 586) + lmat(k, 586)
         mat(k, 597) = mat(k, 597) + lmat(k, 597)
         mat(k, 613) = mat(k, 613) + lmat(k, 613)
         mat(k, 624) = mat(k, 624) + lmat(k, 624)
         mat(k, 633) = mat(k, 633) + lmat(k, 633)
         mat(k, 643) = mat(k, 643) + lmat(k, 643)
         mat(k, 644) = lmat(k, 644)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 651) = mat(k, 651) + lmat(k, 651)
         mat(k, 652) = mat(k, 652) + lmat(k, 652)
         mat(k, 657) = lmat(k, 657)
         mat(k, 660) = mat(k, 660) + lmat(k, 660)
         mat(k, 665) = mat(k, 665) + lmat(k, 665)
         mat(k, 675) = mat(k, 675) + lmat(k, 675)
         mat(k, 680) = mat(k, 680) + lmat(k, 680)
         mat(k, 683) = mat(k, 683) + lmat(k, 683)
         mat(k, 684) = mat(k, 684) + lmat(k, 684)
         mat(k, 690) = mat(k, 690) + lmat(k, 690)
         mat(k, 698) = mat(k, 698) + lmat(k, 698)
         mat(k, 699) = mat(k, 699) + lmat(k, 699)
         mat(k, 700) = mat(k, 700) + lmat(k, 700)
         mat(k, 707) = mat(k, 707) + lmat(k, 707)
         mat(k, 708) = lmat(k, 708)
         mat(k, 709) = lmat(k, 709)
         mat(k, 719) = mat(k, 719) + lmat(k, 719)
         mat(k, 735) = mat(k, 735) + lmat(k, 735)
         mat(k, 737) = lmat(k, 737)
         mat(k, 738) = lmat(k, 738)
         mat(k, 740) = mat(k, 740) + lmat(k, 740)
         mat(k, 741) = lmat(k, 741)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 743) = mat(k, 743) + lmat(k, 743)
         mat(k, 745) = mat(k, 745) + lmat(k, 745)
         mat(k, 747) = mat(k, 747) + lmat(k, 747)
         mat(k, 764) = mat(k, 764) + lmat(k, 764)
         mat(k, 790) = mat(k, 790) + lmat(k, 790)
         mat(k, 812) = mat(k, 812) + lmat(k, 812)
         mat(k, 822) = mat(k, 822) + lmat(k, 822)
         mat(k, 824) = lmat(k, 824)
         mat(k, 826) = lmat(k, 826)
         mat(k, 829) = mat(k, 829) + lmat(k, 829)
         mat(k, 830) = mat(k, 830) + lmat(k, 830)
         mat(k, 831) = mat(k, 831) + lmat(k, 831)
         mat(k, 832) = mat(k, 832) + lmat(k, 832)
         mat(k, 835) = mat(k, 835) + lmat(k, 835)
         mat(k, 836) = mat(k, 836) + lmat(k, 836)
         mat(k, 837) = lmat(k, 837)
         mat(k, 840) = mat(k, 840) + lmat(k, 840)
         mat(k, 841) = lmat(k, 841)
         mat(k, 842) = lmat(k, 842)
         mat(k, 847) = lmat(k, 847)
         mat(k, 852) = mat(k, 852) + lmat(k, 852)
         mat(k, 868) = mat(k, 868) + lmat(k, 868)
         mat(k, 869) = lmat(k, 869)
         mat(k, 870) = mat(k, 870) + lmat(k, 870)
         mat(k, 872) = mat(k, 872) + lmat(k, 872)
         mat(k, 873) = lmat(k, 873)
         mat(k, 877) = mat(k, 877) + lmat(k, 877)
         mat(k, 881) = lmat(k, 881)
         mat(k, 885) = lmat(k, 885)
         mat(k, 886) = mat(k, 886) + lmat(k, 886)
         mat(k, 888) = mat(k, 888) + lmat(k, 888)
         mat(k, 889) = lmat(k, 889)
         mat(k, 893) = lmat(k, 893)
         mat(k, 895) = lmat(k, 895)
         mat(k, 897) = mat(k, 897) + lmat(k, 897)
         mat(k, 898) = mat(k, 898) + lmat(k, 898)
         mat(k, 899) = mat(k, 899) + lmat(k, 899)
         mat(k, 900) = mat(k, 900) + lmat(k, 900)
         mat(k, 901) = mat(k, 901) + lmat(k, 901)
         mat(k, 902) = mat(k, 902) + lmat(k, 902)
         mat(k, 905) = mat(k, 905) + lmat(k, 905)
         mat(k, 907) = mat(k, 907) + lmat(k, 907)
         mat(k, 912) = lmat(k, 912)
         mat(k, 913) = lmat(k, 913)
         mat(k, 914) = lmat(k, 914)
         mat(k, 915) = lmat(k, 915)
         mat(k, 916) = mat(k, 916) + lmat(k, 916)
         mat(k, 917) = lmat(k, 917)
         mat(k, 919) = lmat(k, 919)
         mat(k, 920) = lmat(k, 920)
         mat(k, 922) = lmat(k, 922)
         mat(k, 926) = lmat(k, 926)
         mat(k, 927) = mat(k, 927) + lmat(k, 927)
         mat(k, 929) = mat(k, 929) + lmat(k, 929)
         mat(k, 931) = lmat(k, 931)
         mat(k, 932) = lmat(k, 932)
         mat(k, 934) = mat(k, 934) + lmat(k, 934)
         mat(k, 942) = mat(k, 942) + lmat(k, 942)
         mat(k, 964) = mat(k, 964) + lmat(k, 964)
         mat(k, 983) = mat(k, 983) + lmat(k, 983)
         mat(k, 999) = mat(k, 999) + lmat(k, 999)
         mat(k,1011) = mat(k,1011) + lmat(k,1011)
         mat(k,1028) = mat(k,1028) + lmat(k,1028)
         mat(k,1048) = mat(k,1048) + lmat(k,1048)
         mat(k,1063) = mat(k,1063) + lmat(k,1063)
         mat(k,1064) = mat(k,1064) + lmat(k,1064)
         mat(k,1067) = mat(k,1067) + lmat(k,1067)
         mat(k,1068) = mat(k,1068) + lmat(k,1068)
         mat(k,1070) = mat(k,1070) + lmat(k,1070)
         mat(k,1073) = mat(k,1073) + lmat(k,1073)
         mat(k,1075) = mat(k,1075) + lmat(k,1075)
         mat(k,1076) = mat(k,1076) + lmat(k,1076)
         mat(k,1077) = mat(k,1077) + lmat(k,1077)
         mat(k,1082) = lmat(k,1082)
         mat(k,1085) = lmat(k,1085)
         mat(k,1086) = mat(k,1086) + lmat(k,1086)
         mat(k,1087) = mat(k,1087) + lmat(k,1087)
         mat(k,1091) = lmat(k,1091)
         mat(k,1111) = mat(k,1111) + lmat(k,1111)
         mat(k,1128) = lmat(k,1128)
         mat(k,1130) = mat(k,1130) + lmat(k,1130)
         mat(k,1133) = mat(k,1133) + lmat(k,1133)
         mat(k,1135) = mat(k,1135) + lmat(k,1135)
         mat(k,1140) = lmat(k,1140)
         mat(k,1155) = mat(k,1155) + lmat(k,1155)
         mat(k,1168) = lmat(k,1168)
         mat(k,1187) = mat(k,1187) + lmat(k,1187)
         mat(k,1198) = mat(k,1198) + lmat(k,1198)
         mat(k,1206) = mat(k,1206) + lmat(k,1206)
         mat(k,1237) = mat(k,1237) + lmat(k,1237)
         mat(k,1251) = mat(k,1251) + lmat(k,1251)
         mat(k,1264) = mat(k,1264) + lmat(k,1264)
         mat(k,1268) = mat(k,1268) + lmat(k,1268)
         mat(k,1272) = lmat(k,1272)
         mat(k,1277) = mat(k,1277) + lmat(k,1277)
         mat(k,1286) = mat(k,1286) + lmat(k,1286)
         mat(k,1292) = mat(k,1292) + lmat(k,1292)
         mat(k,1293) = lmat(k,1293)
         mat(k,1296) = mat(k,1296) + lmat(k,1296)
         mat(k,1297) = mat(k,1297) + lmat(k,1297)
         mat(k,1323) = mat(k,1323) + lmat(k,1323)
         mat(k,1324) = mat(k,1324) + lmat(k,1324)
         mat(k,1329) = mat(k,1329) + lmat(k,1329)
         mat(k,1354) = mat(k,1354) + lmat(k,1354)
         mat(k,1362) = mat(k,1362) + lmat(k,1362)
         mat(k,1367) = mat(k,1367) + lmat(k,1367)
         mat(k,1368) = mat(k,1368) + lmat(k,1368)
         mat(k,1370) = mat(k,1370) + lmat(k,1370)
         mat(k,1372) = mat(k,1372) + lmat(k,1372)
         mat(k,1373) = mat(k,1373) + lmat(k,1373)
         mat(k,1375) = mat(k,1375) + lmat(k,1375)
         mat(k,1376) = lmat(k,1376)
         mat(k,1378) = lmat(k,1378)
         mat(k,1379) = mat(k,1379) + lmat(k,1379)
         mat(k,1380) = mat(k,1380) + lmat(k,1380)
         mat(k,1382) = lmat(k,1382)
         mat(k,1383) = mat(k,1383) + lmat(k,1383)
         mat(k,1387) = lmat(k,1387)
         mat(k,1389) = lmat(k,1389)
         mat(k,1401) = lmat(k,1401)
         mat(k,1406) = lmat(k,1406)
         mat(k,1520) = mat(k,1520) + lmat(k,1520)
         mat(k,1527) = mat(k,1527) + lmat(k,1527)
         mat(k,1529) = mat(k,1529) + lmat(k,1529)
         mat(k,1530) = mat(k,1530) + lmat(k,1530)
         mat(k,1534) = mat(k,1534) + lmat(k,1534)
         mat(k,1537) = mat(k,1537) + lmat(k,1537)
         mat(k,1544) = mat(k,1544) + lmat(k,1544)
         mat(k,1549) = mat(k,1549) + lmat(k,1549)
         mat(k,1552) = mat(k,1552) + lmat(k,1552)
         mat(k,1603) = mat(k,1603) + lmat(k,1603)
         mat(k,1625) = mat(k,1625) + lmat(k,1625)
         mat(k,1628) = lmat(k,1628)
         mat(k,1637) = lmat(k,1637)
         mat(k,1638) = mat(k,1638) + lmat(k,1638)
         mat(k,1642) = mat(k,1642) + lmat(k,1642)
         mat(k,1643) = mat(k,1643) + lmat(k,1643)
         mat(k,1686) = mat(k,1686) + lmat(k,1686)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1696) = mat(k,1696) + lmat(k,1696)
         mat(k,1697) = mat(k,1697) + lmat(k,1697)
         mat(k,1701) = mat(k,1701) + lmat(k,1701)
         mat(k,1727) = mat(k,1727) + lmat(k,1727)
         mat(k,1731) = mat(k,1731) + lmat(k,1731)
         mat(k,1733) = mat(k,1733) + lmat(k,1733)
         mat(k,1738) = mat(k,1738) + lmat(k,1738)
         mat(k,1742) = mat(k,1742) + lmat(k,1742)
         mat(k,1791) = mat(k,1791) + lmat(k,1791)
         mat(k,1792) = mat(k,1792) + lmat(k,1792)
         mat(k,1799) = mat(k,1799) + lmat(k,1799)
         mat(k,1833) = mat(k,1833) + lmat(k,1833)
         mat(k,1893) = mat(k,1893) + lmat(k,1893)
         mat(k,1904) = mat(k,1904) + lmat(k,1904)
         mat(k,1912) = mat(k,1912) + lmat(k,1912)
         mat(k,1917) = mat(k,1917) + lmat(k,1917)
         mat(k,1926) = mat(k,1926) + lmat(k,1926)
         mat(k,1985) = mat(k,1985) + lmat(k,1985)
         mat(k,1996) = mat(k,1996) + lmat(k,1996)
         mat(k,2003) = lmat(k,2003)
         mat(k,2007) = lmat(k,2007)
         mat(k,2010) = lmat(k,2010)
         mat(k,2011) = mat(k,2011) + lmat(k,2011)
         mat(k,2012) = mat(k,2012) + lmat(k,2012)
         mat(k,2022) = mat(k,2022) + lmat(k,2022)
         mat(k, 139) = 0._r8
         mat(k, 140) = 0._r8
         mat(k, 243) = 0._r8
         mat(k, 336) = 0._r8
         mat(k, 337) = 0._r8
         mat(k, 350) = 0._r8
         mat(k, 378) = 0._r8
         mat(k, 380) = 0._r8
         mat(k, 388) = 0._r8
         mat(k, 506) = 0._r8
         mat(k, 508) = 0._r8
         mat(k, 513) = 0._r8
         mat(k, 514) = 0._r8
         mat(k, 518) = 0._r8
         mat(k, 526) = 0._r8
         mat(k, 528) = 0._r8
         mat(k, 530) = 0._r8
         mat(k, 531) = 0._r8
         mat(k, 537) = 0._r8
         mat(k, 538) = 0._r8
         mat(k, 541) = 0._r8
         mat(k, 567) = 0._r8
         mat(k, 569) = 0._r8
         mat(k, 571) = 0._r8
         mat(k, 572) = 0._r8
         mat(k, 574) = 0._r8
         mat(k, 580) = 0._r8
         mat(k, 582) = 0._r8
         mat(k, 596) = 0._r8
         mat(k, 598) = 0._r8
         mat(k, 600) = 0._r8
         mat(k, 601) = 0._r8
         mat(k, 603) = 0._r8
         mat(k, 612) = 0._r8
         mat(k, 614) = 0._r8
         mat(k, 616) = 0._r8
         mat(k, 617) = 0._r8
         mat(k, 619) = 0._r8
         mat(k, 620) = 0._r8
         mat(k, 635) = 0._r8
         mat(k, 637) = 0._r8
         mat(k, 641) = 0._r8
         mat(k, 648) = 0._r8
         mat(k, 669) = 0._r8
         mat(k, 674) = 0._r8
         mat(k, 693) = 0._r8
         mat(k, 712) = 0._r8
         mat(k, 734) = 0._r8
         mat(k, 763) = 0._r8
         mat(k, 765) = 0._r8
         mat(k, 773) = 0._r8
         mat(k, 780) = 0._r8
         mat(k, 789) = 0._r8
         mat(k, 791) = 0._r8
         mat(k, 799) = 0._r8
         mat(k, 806) = 0._r8
         mat(k, 810) = 0._r8
         mat(k, 811) = 0._r8
         mat(k, 815) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 818) = 0._r8
         mat(k, 839) = 0._r8
         mat(k, 855) = 0._r8
         mat(k, 856) = 0._r8
         mat(k, 857) = 0._r8
         mat(k, 862) = 0._r8
         mat(k, 865) = 0._r8
         mat(k, 866) = 0._r8
         mat(k, 876) = 0._r8
         mat(k, 878) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 880) = 0._r8
         mat(k, 882) = 0._r8
         mat(k, 884) = 0._r8
         mat(k, 887) = 0._r8
         mat(k, 904) = 0._r8
         mat(k, 906) = 0._r8
         mat(k, 918) = 0._r8
         mat(k, 921) = 0._r8
         mat(k, 924) = 0._r8
         mat(k, 925) = 0._r8
         mat(k, 928) = 0._r8
         mat(k, 940) = 0._r8
         mat(k, 941) = 0._r8
         mat(k, 943) = 0._r8
         mat(k, 944) = 0._r8
         mat(k, 945) = 0._r8
         mat(k, 946) = 0._r8
         mat(k, 947) = 0._r8
         mat(k, 948) = 0._r8
         mat(k, 950) = 0._r8
         mat(k, 952) = 0._r8
         mat(k, 956) = 0._r8
         mat(k, 965) = 0._r8
         mat(k, 966) = 0._r8
         mat(k, 967) = 0._r8
         mat(k, 968) = 0._r8
         mat(k, 970) = 0._r8
         mat(k, 975) = 0._r8
         mat(k, 980) = 0._r8
         mat(k, 981) = 0._r8
         mat(k, 982) = 0._r8
         mat(k, 984) = 0._r8
         mat(k, 985) = 0._r8
         mat(k, 986) = 0._r8
         mat(k, 987) = 0._r8
         mat(k, 989) = 0._r8
         mat(k, 995) = 0._r8
         mat(k,1008) = 0._r8
         mat(k,1012) = 0._r8
         mat(k,1017) = 0._r8
         mat(k,1019) = 0._r8
         mat(k,1031) = 0._r8
         mat(k,1033) = 0._r8
         mat(k,1041) = 0._r8
         mat(k,1043) = 0._r8
         mat(k,1044) = 0._r8
         mat(k,1046) = 0._r8
         mat(k,1047) = 0._r8
         mat(k,1049) = 0._r8
         mat(k,1050) = 0._r8
         mat(k,1051) = 0._r8
         mat(k,1053) = 0._r8
         mat(k,1054) = 0._r8
         mat(k,1056) = 0._r8
         mat(k,1069) = 0._r8
         mat(k,1079) = 0._r8
         mat(k,1097) = 0._r8
         mat(k,1099) = 0._r8
         mat(k,1103) = 0._r8
         mat(k,1104) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1106) = 0._r8
         mat(k,1107) = 0._r8
         mat(k,1108) = 0._r8
         mat(k,1110) = 0._r8
         mat(k,1113) = 0._r8
         mat(k,1114) = 0._r8
         mat(k,1116) = 0._r8
         mat(k,1117) = 0._r8
         mat(k,1119) = 0._r8
         mat(k,1123) = 0._r8
         mat(k,1126) = 0._r8
         mat(k,1131) = 0._r8
         mat(k,1136) = 0._r8
         mat(k,1137) = 0._r8
         mat(k,1141) = 0._r8
         mat(k,1142) = 0._r8
         mat(k,1145) = 0._r8
         mat(k,1146) = 0._r8
         mat(k,1153) = 0._r8
         mat(k,1156) = 0._r8
         mat(k,1158) = 0._r8
         mat(k,1159) = 0._r8
         mat(k,1161) = 0._r8
         mat(k,1167) = 0._r8
         mat(k,1171) = 0._r8
         mat(k,1174) = 0._r8
         mat(k,1176) = 0._r8
         mat(k,1178) = 0._r8
         mat(k,1179) = 0._r8
         mat(k,1181) = 0._r8
         mat(k,1182) = 0._r8
         mat(k,1183) = 0._r8
         mat(k,1185) = 0._r8
         mat(k,1186) = 0._r8
         mat(k,1188) = 0._r8
         mat(k,1190) = 0._r8
         mat(k,1191) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1197) = 0._r8
         mat(k,1200) = 0._r8
         mat(k,1204) = 0._r8
         mat(k,1205) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1209) = 0._r8
         mat(k,1218) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1239) = 0._r8
         mat(k,1243) = 0._r8
         mat(k,1248) = 0._r8
         mat(k,1252) = 0._r8
         mat(k,1254) = 0._r8
         mat(k,1255) = 0._r8
         mat(k,1256) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1263) = 0._r8
         mat(k,1265) = 0._r8
         mat(k,1266) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1269) = 0._r8
         mat(k,1270) = 0._r8
         mat(k,1273) = 0._r8
         mat(k,1276) = 0._r8
         mat(k,1279) = 0._r8
         mat(k,1281) = 0._r8
         mat(k,1282) = 0._r8
         mat(k,1283) = 0._r8
         mat(k,1284) = 0._r8
         mat(k,1287) = 0._r8
         mat(k,1290) = 0._r8
         mat(k,1299) = 0._r8
         mat(k,1301) = 0._r8
         mat(k,1302) = 0._r8
         mat(k,1305) = 0._r8
         mat(k,1306) = 0._r8
         mat(k,1309) = 0._r8
         mat(k,1320) = 0._r8
         mat(k,1321) = 0._r8
         mat(k,1325) = 0._r8
         mat(k,1330) = 0._r8
         mat(k,1332) = 0._r8
         mat(k,1336) = 0._r8
         mat(k,1338) = 0._r8
         mat(k,1344) = 0._r8
         mat(k,1350) = 0._r8
         mat(k,1355) = 0._r8
         mat(k,1358) = 0._r8
         mat(k,1366) = 0._r8
         mat(k,1374) = 0._r8
         mat(k,1384) = 0._r8
         mat(k,1385) = 0._r8
         mat(k,1444) = 0._r8
         mat(k,1462) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1476) = 0._r8
         mat(k,1478) = 0._r8
         mat(k,1489) = 0._r8
         mat(k,1510) = 0._r8
         mat(k,1526) = 0._r8
         mat(k,1545) = 0._r8
         mat(k,1546) = 0._r8
         mat(k,1547) = 0._r8
         mat(k,1550) = 0._r8
         mat(k,1553) = 0._r8
         mat(k,1555) = 0._r8
         mat(k,1557) = 0._r8
         mat(k,1559) = 0._r8
         mat(k,1561) = 0._r8
         mat(k,1571) = 0._r8
         mat(k,1595) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1599) = 0._r8
         mat(k,1600) = 0._r8
         mat(k,1601) = 0._r8
         mat(k,1602) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1607) = 0._r8
         mat(k,1609) = 0._r8
         mat(k,1611) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1618) = 0._r8
         mat(k,1621) = 0._r8
         mat(k,1623) = 0._r8
         mat(k,1624) = 0._r8
         mat(k,1627) = 0._r8
         mat(k,1629) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1634) = 0._r8
         mat(k,1636) = 0._r8
         mat(k,1640) = 0._r8
         mat(k,1644) = 0._r8
         mat(k,1645) = 0._r8
         mat(k,1652) = 0._r8
         mat(k,1659) = 0._r8
         mat(k,1666) = 0._r8
         mat(k,1668) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1673) = 0._r8
         mat(k,1678) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1691) = 0._r8
         mat(k,1693) = 0._r8
         mat(k,1694) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1698) = 0._r8
         mat(k,1700) = 0._r8
         mat(k,1702) = 0._r8
         mat(k,1712) = 0._r8
         mat(k,1715) = 0._r8
         mat(k,1717) = 0._r8
         mat(k,1720) = 0._r8
         mat(k,1721) = 0._r8
         mat(k,1722) = 0._r8
         mat(k,1726) = 0._r8
         mat(k,1728) = 0._r8
         mat(k,1729) = 0._r8
         mat(k,1732) = 0._r8
         mat(k,1735) = 0._r8
         mat(k,1736) = 0._r8
         mat(k,1741) = 0._r8
         mat(k,1743) = 0._r8
         mat(k,1756) = 0._r8
         mat(k,1760) = 0._r8
         mat(k,1763) = 0._r8
         mat(k,1767) = 0._r8
         mat(k,1771) = 0._r8
         mat(k,1772) = 0._r8
         mat(k,1773) = 0._r8
         mat(k,1774) = 0._r8
         mat(k,1776) = 0._r8
         mat(k,1780) = 0._r8
         mat(k,1782) = 0._r8
         mat(k,1783) = 0._r8
         mat(k,1784) = 0._r8
         mat(k,1787) = 0._r8
         mat(k,1801) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1807) = 0._r8
         mat(k,1824) = 0._r8
         mat(k,1825) = 0._r8
         mat(k,1853) = 0._r8
         mat(k,1857) = 0._r8
         mat(k,1858) = 0._r8
         mat(k,1859) = 0._r8
         mat(k,1861) = 0._r8
         mat(k,1863) = 0._r8
         mat(k,1871) = 0._r8
         mat(k,1874) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1885) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1905) = 0._r8
         mat(k,1910) = 0._r8
         mat(k,1911) = 0._r8
         mat(k,1913) = 0._r8
         mat(k,1914) = 0._r8
         mat(k,1915) = 0._r8
         mat(k,1916) = 0._r8
         mat(k,1918) = 0._r8
         mat(k,1953) = 0._r8
         mat(k,1981) = 0._r8
         mat(k,1982) = 0._r8
         mat(k,1986) = 0._r8
         mat(k,1995) = 0._r8
         mat(k,1997) = 0._r8
         mat(k,2002) = 0._r8
         mat(k,2004) = 0._r8
         mat(k,2005) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2008) = 0._r8
         mat(k,2009) = 0._r8
         mat(k,2013) = 0._r8
         mat(k,2014) = 0._r8
         mat(k,2015) = 0._r8
         mat(k,2016) = 0._r8
         mat(k,2017) = 0._r8
         mat(k,2018) = 0._r8
         mat(k,2019) = 0._r8
         mat(k,2020) = 0._r8
         mat(k,2021) = 0._r8
         mat(k, 1) = mat(k, 1) - dti(k)
         mat(k, 2) = mat(k, 2) - dti(k)
         mat(k, 3) = mat(k, 3) - dti(k)
         mat(k, 4) = mat(k, 4) - dti(k)
         mat(k, 5) = mat(k, 5) - dti(k)
         mat(k, 6) = mat(k, 6) - dti(k)
         mat(k, 7) = mat(k, 7) - dti(k)
         mat(k, 8) = mat(k, 8) - dti(k)
         mat(k, 9) = mat(k, 9) - dti(k)
         mat(k, 10) = mat(k, 10) - dti(k)
         mat(k, 11) = mat(k, 11) - dti(k)
         mat(k, 12) = mat(k, 12) - dti(k)
         mat(k, 13) = mat(k, 13) - dti(k)
         mat(k, 14) = mat(k, 14) - dti(k)
         mat(k, 15) = mat(k, 15) - dti(k)
         mat(k, 16) = mat(k, 16) - dti(k)
         mat(k, 17) = mat(k, 17) - dti(k)
         mat(k, 18) = mat(k, 18) - dti(k)
         mat(k, 19) = mat(k, 19) - dti(k)
         mat(k, 20) = mat(k, 20) - dti(k)
         mat(k, 21) = mat(k, 21) - dti(k)
         mat(k, 22) = mat(k, 22) - dti(k)
         mat(k, 23) = mat(k, 23) - dti(k)
         mat(k, 24) = mat(k, 24) - dti(k)
         mat(k, 25) = mat(k, 25) - dti(k)
         mat(k, 26) = mat(k, 26) - dti(k)
         mat(k, 27) = mat(k, 27) - dti(k)
         mat(k, 28) = mat(k, 28) - dti(k)
         mat(k, 29) = mat(k, 29) - dti(k)
         mat(k, 30) = mat(k, 30) - dti(k)
         mat(k, 31) = mat(k, 31) - dti(k)
         mat(k, 32) = mat(k, 32) - dti(k)
         mat(k, 33) = mat(k, 33) - dti(k)
         mat(k, 34) = mat(k, 34) - dti(k)
         mat(k, 40) = mat(k, 40) - dti(k)
         mat(k, 46) = mat(k, 46) - dti(k)
         mat(k, 47) = mat(k, 47) - dti(k)
         mat(k, 50) = mat(k, 50) - dti(k)
         mat(k, 53) = mat(k, 53) - dti(k)
         mat(k, 56) = mat(k, 56) - dti(k)
         mat(k, 60) = mat(k, 60) - dti(k)
         mat(k, 63) = mat(k, 63) - dti(k)
         mat(k, 66) = mat(k, 66) - dti(k)
         mat(k, 69) = mat(k, 69) - dti(k)
         mat(k, 72) = mat(k, 72) - dti(k)
         mat(k, 75) = mat(k, 75) - dti(k)
         mat(k, 82) = mat(k, 82) - dti(k)
         mat(k, 88) = mat(k, 88) - dti(k)
         mat(k, 92) = mat(k, 92) - dti(k)
         mat(k, 97) = mat(k, 97) - dti(k)
         mat(k, 101) = mat(k, 101) - dti(k)
         mat(k, 110) = mat(k, 110) - dti(k)
         mat(k, 117) = mat(k, 117) - dti(k)
         mat(k, 122) = mat(k, 122) - dti(k)
         mat(k, 126) = mat(k, 126) - dti(k)
         mat(k, 135) = mat(k, 135) - dti(k)
         mat(k, 143) = mat(k, 143) - dti(k)
         mat(k, 148) = mat(k, 148) - dti(k)
         mat(k, 151) = mat(k, 151) - dti(k)
         mat(k, 156) = mat(k, 156) - dti(k)
         mat(k, 159) = mat(k, 159) - dti(k)
         mat(k, 162) = mat(k, 162) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 169) = mat(k, 169) - dti(k)
         mat(k, 173) = mat(k, 173) - dti(k)
         mat(k, 177) = mat(k, 177) - dti(k)
         mat(k, 181) = mat(k, 181) - dti(k)
         mat(k, 185) = mat(k, 185) - dti(k)
         mat(k, 191) = mat(k, 191) - dti(k)
         mat(k, 194) = mat(k, 194) - dti(k)
         mat(k, 200) = mat(k, 200) - dti(k)
         mat(k, 206) = mat(k, 206) - dti(k)
         mat(k, 212) = mat(k, 212) - dti(k)
         mat(k, 215) = mat(k, 215) - dti(k)
         mat(k, 220) = mat(k, 220) - dti(k)
         mat(k, 225) = mat(k, 225) - dti(k)
         mat(k, 230) = mat(k, 230) - dti(k)
         mat(k, 235) = mat(k, 235) - dti(k)
         mat(k, 241) = mat(k, 241) - dti(k)
         mat(k, 246) = mat(k, 246) - dti(k)
         mat(k, 251) = mat(k, 251) - dti(k)
         mat(k, 259) = mat(k, 259) - dti(k)
         mat(k, 267) = mat(k, 267) - dti(k)
         mat(k, 273) = mat(k, 273) - dti(k)
         mat(k, 279) = mat(k, 279) - dti(k)
         mat(k, 285) = mat(k, 285) - dti(k)
         mat(k, 291) = mat(k, 291) - dti(k)
         mat(k, 297) = mat(k, 297) - dti(k)
         mat(k, 303) = mat(k, 303) - dti(k)
         mat(k, 309) = mat(k, 309) - dti(k)
         mat(k, 315) = mat(k, 315) - dti(k)
         mat(k, 321) = mat(k, 321) - dti(k)
         mat(k, 327) = mat(k, 327) - dti(k)
         mat(k, 335) = mat(k, 335) - dti(k)
         mat(k, 341) = mat(k, 341) - dti(k)
         mat(k, 348) = mat(k, 348) - dti(k)
         mat(k, 354) = mat(k, 354) - dti(k)
         mat(k, 357) = mat(k, 357) - dti(k)
         mat(k, 364) = mat(k, 364) - dti(k)
         mat(k, 368) = mat(k, 368) - dti(k)
         mat(k, 377) = mat(k, 377) - dti(k)
         mat(k, 385) = mat(k, 385) - dti(k)
         mat(k, 392) = mat(k, 392) - dti(k)
         mat(k, 398) = mat(k, 398) - dti(k)
         mat(k, 403) = mat(k, 403) - dti(k)
         mat(k, 410) = mat(k, 410) - dti(k)
         mat(k, 416) = mat(k, 416) - dti(k)
         mat(k, 424) = mat(k, 424) - dti(k)
         mat(k, 432) = mat(k, 432) - dti(k)
         mat(k, 440) = mat(k, 440) - dti(k)
         mat(k, 448) = mat(k, 448) - dti(k)
         mat(k, 452) = mat(k, 452) - dti(k)
         mat(k, 460) = mat(k, 460) - dti(k)
         mat(k, 468) = mat(k, 468) - dti(k)
         mat(k, 475) = mat(k, 475) - dti(k)
         mat(k, 484) = mat(k, 484) - dti(k)
         mat(k, 488) = mat(k, 488) - dti(k)
         mat(k, 497) = mat(k, 497) - dti(k)
         mat(k, 505) = mat(k, 505) - dti(k)
         mat(k, 512) = mat(k, 512) - dti(k)
         mat(k, 525) = mat(k, 525) - dti(k)
         mat(k, 536) = mat(k, 536) - dti(k)
         mat(k, 547) = mat(k, 547) - dti(k)
         mat(k, 555) = mat(k, 555) - dti(k)
         mat(k, 566) = mat(k, 566) - dti(k)
         mat(k, 579) = mat(k, 579) - dti(k)
         mat(k, 586) = mat(k, 586) - dti(k)
         mat(k, 597) = mat(k, 597) - dti(k)
         mat(k, 613) = mat(k, 613) - dti(k)
         mat(k, 624) = mat(k, 624) - dti(k)
         mat(k, 633) = mat(k, 633) - dti(k)
         mat(k, 643) = mat(k, 643) - dti(k)
         mat(k, 652) = mat(k, 652) - dti(k)
         mat(k, 660) = mat(k, 660) - dti(k)
         mat(k, 665) = mat(k, 665) - dti(k)
         mat(k, 675) = mat(k, 675) - dti(k)
         mat(k, 680) = mat(k, 680) - dti(k)
         mat(k, 690) = mat(k, 690) - dti(k)
         mat(k, 698) = mat(k, 698) - dti(k)
         mat(k, 707) = mat(k, 707) - dti(k)
         mat(k, 719) = mat(k, 719) - dti(k)
         mat(k, 735) = mat(k, 735) - dti(k)
         mat(k, 742) = mat(k, 742) - dti(k)
         mat(k, 747) = mat(k, 747) - dti(k)
         mat(k, 764) = mat(k, 764) - dti(k)
         mat(k, 790) = mat(k, 790) - dti(k)
         mat(k, 812) = mat(k, 812) - dti(k)
         mat(k, 822) = mat(k, 822) - dti(k)
         mat(k, 830) = mat(k, 830) - dti(k)
         mat(k, 840) = mat(k, 840) - dti(k)
         mat(k, 852) = mat(k, 852) - dti(k)
         mat(k, 868) = mat(k, 868) - dti(k)
         mat(k, 877) = mat(k, 877) - dti(k)
         mat(k, 888) = mat(k, 888) - dti(k)
         mat(k, 898) = mat(k, 898) - dti(k)
         mat(k, 907) = mat(k, 907) - dti(k)
         mat(k, 916) = mat(k, 916) - dti(k)
         mat(k, 929) = mat(k, 929) - dti(k)
         mat(k, 942) = mat(k, 942) - dti(k)
         mat(k, 964) = mat(k, 964) - dti(k)
         mat(k, 983) = mat(k, 983) - dti(k)
         mat(k, 999) = mat(k, 999) - dti(k)
         mat(k,1011) = mat(k,1011) - dti(k)
         mat(k,1028) = mat(k,1028) - dti(k)
         mat(k,1048) = mat(k,1048) - dti(k)
         mat(k,1064) = mat(k,1064) - dti(k)
         mat(k,1076) = mat(k,1076) - dti(k)
         mat(k,1087) = mat(k,1087) - dti(k)
         mat(k,1111) = mat(k,1111) - dti(k)
         mat(k,1133) = mat(k,1133) - dti(k)
         mat(k,1155) = mat(k,1155) - dti(k)
         mat(k,1187) = mat(k,1187) - dti(k)
         mat(k,1206) = mat(k,1206) - dti(k)
         mat(k,1237) = mat(k,1237) - dti(k)
         mat(k,1251) = mat(k,1251) - dti(k)
         mat(k,1264) = mat(k,1264) - dti(k)
         mat(k,1277) = mat(k,1277) - dti(k)
         mat(k,1297) = mat(k,1297) - dti(k)
         mat(k,1323) = mat(k,1323) - dti(k)
         mat(k,1354) = mat(k,1354) - dti(k)
         mat(k,1379) = mat(k,1379) - dti(k)
         mat(k,1527) = mat(k,1527) - dti(k)
         mat(k,1552) = mat(k,1552) - dti(k)
         mat(k,1603) = mat(k,1603) - dti(k)
         mat(k,1638) = mat(k,1638) - dti(k)
         mat(k,1696) = mat(k,1696) - dti(k)
         mat(k,1738) = mat(k,1738) - dti(k)
         mat(k,1799) = mat(k,1799) - dti(k)
         mat(k,1893) = mat(k,1893) - dti(k)
         mat(k,1917) = mat(k,1917) - dti(k)
         mat(k,1996) = mat(k,1996) - dti(k)
         mat(k,2022) = mat(k,2022) - dti(k)
      end do
      end subroutine nlnmat_finit
      subroutine nlnmat( avec_len, mat, y, rxt, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
      call nlnmat01( avec_len, mat, y, rxt )
      call nlnmat02( avec_len, mat, y, rxt )
      call nlnmat03( avec_len, mat, y, rxt )
      call nlnmat04( avec_len, mat, y, rxt )
      call nlnmat05( avec_len, mat, y, rxt )
      call nlnmat06( avec_len, mat, y, rxt )
      call nlnmat07( avec_len, mat, y, rxt )
      call nlnmat08( avec_len, mat, y, rxt )
      call nlnmat09( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
