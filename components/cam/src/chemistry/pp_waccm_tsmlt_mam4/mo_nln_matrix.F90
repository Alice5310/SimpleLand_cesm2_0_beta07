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
         mat(k,537) = -(rxt(k,398)*y(k,217))
         mat(k,1686) = -rxt(k,398)*y(k,1)
         mat(k,1460) = rxt(k,401)*y(k,186)
         mat(k,877) = rxt(k,401)*y(k,121)
         mat(k,560) = -(rxt(k,402)*y(k,217))
         mat(k,1687) = -rxt(k,402)*y(k,2)
         mat(k,878) = rxt(k,399)*y(k,199)
         mat(k,2032) = rxt(k,399)*y(k,186)
         mat(k,859) = -(rxt(k,481)*y(k,123) + rxt(k,482)*y(k,132) + rxt(k,483) &
                      *y(k,217))
         mat(k,1531) = -rxt(k,481)*y(k,5)
         mat(k,1779) = -rxt(k,482)*y(k,5)
         mat(k,1711) = -rxt(k,483)*y(k,5)
         mat(k,85) = -(rxt(k,440)*y(k,217))
         mat(k,1624) = -rxt(k,440)*y(k,6)
         mat(k,274) = -(rxt(k,443)*y(k,217))
         mat(k,1654) = -rxt(k,443)*y(k,7)
         mat(k,376) = rxt(k,441)*y(k,199)
         mat(k,2006) = rxt(k,441)*y(k,187)
         mat(k,86) = .120_r8*rxt(k,440)*y(k,217)
         mat(k,1625) = .120_r8*rxt(k,440)*y(k,6)
         mat(k,856) = .100_r8*rxt(k,482)*y(k,132)
         mat(k,830) = .100_r8*rxt(k,485)*y(k,132)
         mat(k,1768) = .100_r8*rxt(k,482)*y(k,5) + .100_r8*rxt(k,485)*y(k,109)
         mat(k,1447) = .500_r8*rxt(k,442)*y(k,187) + .200_r8*rxt(k,469)*y(k,224) &
                      + .060_r8*rxt(k,475)*y(k,226)
         mat(k,377) = .500_r8*rxt(k,442)*y(k,121)
         mat(k,615) = .200_r8*rxt(k,469)*y(k,121)
         mat(k,631) = .060_r8*rxt(k,475)*y(k,121)
         mat(k,1441) = .200_r8*rxt(k,469)*y(k,224) + .200_r8*rxt(k,475)*y(k,226)
         mat(k,614) = .200_r8*rxt(k,469)*y(k,121)
         mat(k,629) = .200_r8*rxt(k,475)*y(k,121)
         mat(k,1456) = .200_r8*rxt(k,469)*y(k,224) + .150_r8*rxt(k,475)*y(k,226)
         mat(k,617) = .200_r8*rxt(k,469)*y(k,121)
         mat(k,632) = .150_r8*rxt(k,475)*y(k,121)
         mat(k,1443) = .210_r8*rxt(k,475)*y(k,226)
         mat(k,630) = .210_r8*rxt(k,475)*y(k,121)
         mat(k,161) = -(rxt(k,403)*y(k,217))
         mat(k,1636) = -rxt(k,403)*y(k,14)
         mat(k,855) = .050_r8*rxt(k,482)*y(k,132)
         mat(k,829) = .050_r8*rxt(k,485)*y(k,132)
         mat(k,1767) = .050_r8*rxt(k,482)*y(k,5) + .050_r8*rxt(k,485)*y(k,109)
         mat(k,260) = -(rxt(k,369)*y(k,123) + rxt(k,370)*y(k,217))
         mat(k,1525) = -rxt(k,369)*y(k,15)
         mat(k,1652) = -rxt(k,370)*y(k,15)
         mat(k,1350) = -(rxt(k,252)*y(k,41) + rxt(k,253)*y(k,199) + rxt(k,254) &
                      *y(k,132))
         mat(k,2125) = -rxt(k,252)*y(k,16)
         mat(k,2075) = -rxt(k,253)*y(k,16)
         mat(k,1804) = -rxt(k,254)*y(k,16)
         mat(k,1849) = 4.000_r8*rxt(k,255)*y(k,18) + (rxt(k,256)+rxt(k,257))*y(k,58) &
                      + rxt(k,260)*y(k,121) + rxt(k,263)*y(k,130) + rxt(k,508) &
                      *y(k,148) + rxt(k,264)*y(k,217)
         mat(k,1980) = (rxt(k,256)+rxt(k,257))*y(k,18)
         mat(k,755) = rxt(k,265)*y(k,130) + rxt(k,271)*y(k,213) + rxt(k,266)*y(k,217)
         mat(k,1502) = rxt(k,260)*y(k,18)
         mat(k,1889) = rxt(k,263)*y(k,18) + rxt(k,265)*y(k,80)
         mat(k,1317) = rxt(k,508)*y(k,18)
         mat(k,2101) = rxt(k,271)*y(k,80)
         mat(k,1742) = rxt(k,264)*y(k,18) + rxt(k,266)*y(k,80)
         mat(k,1842) = rxt(k,258)*y(k,58)
         mat(k,1973) = rxt(k,258)*y(k,18)
         mat(k,1331) = (rxt(k,558)+rxt(k,563))*y(k,90)
         mat(k,654) = (rxt(k,558)+rxt(k,563))*y(k,84)
         mat(k,1857) = -(4._r8*rxt(k,255)*y(k,18) + (rxt(k,256) + rxt(k,257) + rxt(k,258) &
                      ) * y(k,58) + rxt(k,259)*y(k,199) + rxt(k,260)*y(k,121) &
                      + rxt(k,261)*y(k,122) + rxt(k,263)*y(k,130) + rxt(k,264) &
                      *y(k,217) + rxt(k,508)*y(k,148))
         mat(k,1989) = -(rxt(k,256) + rxt(k,257) + rxt(k,258)) * y(k,18)
         mat(k,2084) = -rxt(k,259)*y(k,18)
         mat(k,1511) = -rxt(k,260)*y(k,18)
         mat(k,1940) = -rxt(k,261)*y(k,18)
         mat(k,1898) = -rxt(k,263)*y(k,18)
         mat(k,1751) = -rxt(k,264)*y(k,18)
         mat(k,1324) = -rxt(k,508)*y(k,18)
         mat(k,1355) = rxt(k,254)*y(k,132)
         mat(k,443) = rxt(k,262)*y(k,130)
         mat(k,758) = rxt(k,272)*y(k,213)
         mat(k,660) = rxt(k,267)*y(k,130)
         mat(k,1898) = mat(k,1898) + rxt(k,262)*y(k,19) + rxt(k,267)*y(k,90)
         mat(k,1813) = rxt(k,254)*y(k,16)
         mat(k,2110) = rxt(k,272)*y(k,80)
         mat(k,439) = -(rxt(k,262)*y(k,130))
         mat(k,1871) = -rxt(k,262)*y(k,19)
         mat(k,1844) = rxt(k,261)*y(k,122)
         mat(k,1913) = rxt(k,261)*y(k,18)
         mat(k,170) = -(rxt(k,444)*y(k,217))
         mat(k,1638) = -rxt(k,444)*y(k,21)
         mat(k,1440) = rxt(k,447)*y(k,188)
         mat(k,322) = rxt(k,447)*y(k,121)
         mat(k,242) = -(rxt(k,446)*y(k,217))
         mat(k,1649) = -rxt(k,446)*y(k,22)
         mat(k,323) = rxt(k,445)*y(k,199)
         mat(k,2004) = rxt(k,445)*y(k,188)
         mat(k,201) = -(rxt(k,318)*y(k,55) + rxt(k,319)*y(k,217))
         mat(k,1578) = -rxt(k,318)*y(k,23)
         mat(k,1643) = -rxt(k,319)*y(k,23)
         mat(k,447) = -(rxt(k,320)*y(k,55) + rxt(k,321)*y(k,132) + rxt(k,346)*y(k,217))
         mat(k,1580) = -rxt(k,320)*y(k,24)
         mat(k,1770) = -rxt(k,321)*y(k,24)
         mat(k,1675) = -rxt(k,346)*y(k,24)
         mat(k,178) = -(rxt(k,326)*y(k,217))
         mat(k,1640) = -rxt(k,326)*y(k,25)
         mat(k,798) = .800_r8*rxt(k,322)*y(k,189) + .200_r8*rxt(k,323)*y(k,193)
         mat(k,1361) = .200_r8*rxt(k,323)*y(k,189)
         mat(k,247) = -(rxt(k,327)*y(k,217))
         mat(k,1650) = -rxt(k,327)*y(k,26)
         mat(k,799) = rxt(k,324)*y(k,199)
         mat(k,2005) = rxt(k,324)*y(k,189)
         mat(k,207) = -(rxt(k,328)*y(k,55) + rxt(k,329)*y(k,217))
         mat(k,1579) = -rxt(k,328)*y(k,27)
         mat(k,1644) = -rxt(k,329)*y(k,27)
         mat(k,944) = -(rxt(k,349)*y(k,123) + rxt(k,350)*y(k,132) + rxt(k,367) &
                      *y(k,217))
         mat(k,1537) = -rxt(k,349)*y(k,28)
         mat(k,1784) = -rxt(k,350)*y(k,28)
         mat(k,1718) = -rxt(k,367)*y(k,28)
         mat(k,783) = .130_r8*rxt(k,427)*y(k,132)
         mat(k,1784) = mat(k,1784) + .130_r8*rxt(k,427)*y(k,97)
         mat(k,304) = -(rxt(k,354)*y(k,217))
         mat(k,1658) = -rxt(k,354)*y(k,29)
         mat(k,728) = rxt(k,352)*y(k,199)
         mat(k,2010) = rxt(k,352)*y(k,190)
         mat(k,56) = -(rxt(k,355)*y(k,217))
         mat(k,1621) = -rxt(k,355)*y(k,30)
         mat(k,182) = -(rxt(k,450)*y(k,217))
         mat(k,1641) = -rxt(k,450)*y(k,31)
         mat(k,528) = rxt(k,448)*y(k,199)
         mat(k,2000) = rxt(k,448)*y(k,191)
         mat(k,2141) = -(rxt(k,216)*y(k,55) + rxt(k,252)*y(k,16) + rxt(k,296)*y(k,199) &
                      + rxt(k,297)*y(k,123) + rxt(k,298)*y(k,130) + rxt(k,299) &
                      *y(k,217))
         mat(k,1610) = -rxt(k,216)*y(k,41)
         mat(k,1359) = -rxt(k,252)*y(k,41)
         mat(k,2091) = -rxt(k,296)*y(k,41)
         mat(k,1575) = -rxt(k,297)*y(k,41)
         mat(k,1905) = -rxt(k,298)*y(k,41)
         mat(k,1758) = -rxt(k,299)*y(k,41)
         mat(k,546) = .400_r8*rxt(k,398)*y(k,217)
         mat(k,874) = .340_r8*rxt(k,482)*y(k,132)
         mat(k,267) = .500_r8*rxt(k,369)*y(k,123)
         mat(k,454) = rxt(k,321)*y(k,132)
         mat(k,958) = .500_r8*rxt(k,350)*y(k,132)
         mat(k,404) = .500_r8*rxt(k,338)*y(k,217)
         mat(k,716) = rxt(k,304)*y(k,217)
         mat(k,314) = .300_r8*rxt(k,305)*y(k,217)
         mat(k,1996) = rxt(k,223)*y(k,193)
         mat(k,965) = .800_r8*rxt(k,343)*y(k,217)
         mat(k,796) = .910_r8*rxt(k,427)*y(k,132)
         mat(k,514) = .300_r8*rxt(k,418)*y(k,217)
         mat(k,1138) = .800_r8*rxt(k,422)*y(k,193)
         mat(k,1150) = .120_r8*rxt(k,380)*y(k,132)
         mat(k,474) = .500_r8*rxt(k,393)*y(k,217)
         mat(k,848) = .340_r8*rxt(k,485)*y(k,132)
         mat(k,1262) = .600_r8*rxt(k,394)*y(k,132)
         mat(k,1518) = .100_r8*rxt(k,400)*y(k,186) + rxt(k,303)*y(k,193) &
                      + .500_r8*rxt(k,371)*y(k,196) + .500_r8*rxt(k,340)*y(k,198) &
                      + .920_r8*rxt(k,410)*y(k,201) + .250_r8*rxt(k,378)*y(k,203) &
                      + rxt(k,387)*y(k,205) + rxt(k,361)*y(k,220) + rxt(k,365) &
                      *y(k,221) + .340_r8*rxt(k,494)*y(k,222) + .320_r8*rxt(k,499) &
                      *y(k,223) + .250_r8*rxt(k,435)*y(k,225)
         mat(k,1575) = mat(k,1575) + .500_r8*rxt(k,369)*y(k,15) + rxt(k,411)*y(k,201) &
                      + .250_r8*rxt(k,377)*y(k,203) + rxt(k,388)*y(k,205)
         mat(k,1820) = .340_r8*rxt(k,482)*y(k,5) + rxt(k,321)*y(k,24) &
                      + .500_r8*rxt(k,350)*y(k,28) + .910_r8*rxt(k,427)*y(k,97) &
                      + .120_r8*rxt(k,380)*y(k,104) + .340_r8*rxt(k,485)*y(k,109) &
                      + .600_r8*rxt(k,394)*y(k,110)
         mat(k,357) = rxt(k,345)*y(k,217)
         mat(k,983) = .680_r8*rxt(k,503)*y(k,217)
         mat(k,891) = .100_r8*rxt(k,400)*y(k,121)
         mat(k,809) = .700_r8*rxt(k,323)*y(k,193)
         mat(k,738) = rxt(k,351)*y(k,193)
         mat(k,1311) = rxt(k,334)*y(k,193) + rxt(k,407)*y(k,201) + .250_r8*rxt(k,374) &
                      *y(k,203) + rxt(k,383)*y(k,205) + .250_r8*rxt(k,432)*y(k,225)
         mat(k,1406) = rxt(k,223)*y(k,58) + .800_r8*rxt(k,422)*y(k,100) + rxt(k,303) &
                      *y(k,121) + .700_r8*rxt(k,323)*y(k,189) + rxt(k,351)*y(k,190) &
                      + rxt(k,334)*y(k,192) + (4.000_r8*rxt(k,300)+2.000_r8*rxt(k,301)) &
                      *y(k,193) + 1.500_r8*rxt(k,408)*y(k,201) + .750_r8*rxt(k,413) &
                      *y(k,202) + .880_r8*rxt(k,375)*y(k,203) + 2.000_r8*rxt(k,384) &
                      *y(k,205) + .750_r8*rxt(k,487)*y(k,212) + .800_r8*rxt(k,363) &
                      *y(k,221) + .930_r8*rxt(k,492)*y(k,222) + .950_r8*rxt(k,497) &
                      *y(k,223) + .800_r8*rxt(k,433)*y(k,225)
         mat(k,466) = .500_r8*rxt(k,371)*y(k,121)
         mat(k,681) = .500_r8*rxt(k,340)*y(k,121)
         mat(k,2091) = mat(k,2091) + .450_r8*rxt(k,385)*y(k,205) + .150_r8*rxt(k,364) &
                      *y(k,221)
         mat(k,1186) = .920_r8*rxt(k,410)*y(k,121) + rxt(k,411)*y(k,123) + rxt(k,407) &
                      *y(k,192) + 1.500_r8*rxt(k,408)*y(k,193)
         mat(k,1219) = .750_r8*rxt(k,413)*y(k,193)
         mat(k,1240) = .250_r8*rxt(k,378)*y(k,121) + .250_r8*rxt(k,377)*y(k,123) &
                      + .250_r8*rxt(k,374)*y(k,192) + .880_r8*rxt(k,375)*y(k,193)
         mat(k,1280) = rxt(k,387)*y(k,121) + rxt(k,388)*y(k,123) + rxt(k,383)*y(k,192) &
                      + 2.000_r8*rxt(k,384)*y(k,193) + .450_r8*rxt(k,385)*y(k,199) &
                      + 4.000_r8*rxt(k,386)*y(k,205)
         mat(k,1071) = .750_r8*rxt(k,487)*y(k,193)
         mat(k,1758) = mat(k,1758) + .400_r8*rxt(k,398)*y(k,1) + .500_r8*rxt(k,338) &
                      *y(k,50) + rxt(k,304)*y(k,51) + .300_r8*rxt(k,305)*y(k,52) &
                      + .800_r8*rxt(k,343)*y(k,73) + .300_r8*rxt(k,418)*y(k,98) &
                      + .500_r8*rxt(k,393)*y(k,108) + rxt(k,345)*y(k,137) &
                      + .680_r8*rxt(k,503)*y(k,175)
         mat(k,726) = rxt(k,361)*y(k,121)
         mat(k,1084) = rxt(k,365)*y(k,121) + .800_r8*rxt(k,363)*y(k,193) &
                      + .150_r8*rxt(k,364)*y(k,199)
         mat(k,1051) = .340_r8*rxt(k,494)*y(k,121) + .930_r8*rxt(k,492)*y(k,193)
         mat(k,1032) = .320_r8*rxt(k,499)*y(k,121) + .950_r8*rxt(k,497)*y(k,193)
         mat(k,1115) = .250_r8*rxt(k,435)*y(k,121) + .250_r8*rxt(k,432)*y(k,192) &
                      + .800_r8*rxt(k,433)*y(k,193)
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
         mat(k,966) = -(rxt(k,330)*y(k,123) + rxt(k,331)*y(k,217))
         mat(k,1539) = -rxt(k,330)*y(k,44)
         mat(k,1720) = -rxt(k,331)*y(k,44)
         mat(k,541) = .800_r8*rxt(k,398)*y(k,217)
         mat(k,263) = rxt(k,369)*y(k,123)
         mat(k,179) = rxt(k,326)*y(k,217)
         mat(k,249) = .500_r8*rxt(k,327)*y(k,217)
         mat(k,945) = .500_r8*rxt(k,350)*y(k,132)
         mat(k,1244) = .100_r8*rxt(k,394)*y(k,132)
         mat(k,1483) = .400_r8*rxt(k,400)*y(k,186) + rxt(k,325)*y(k,189) &
                      + .270_r8*rxt(k,353)*y(k,190) + rxt(k,371)*y(k,196) + rxt(k,390) &
                      *y(k,207) + rxt(k,361)*y(k,220)
         mat(k,1539) = mat(k,1539) + rxt(k,369)*y(k,15)
         mat(k,1786) = .500_r8*rxt(k,350)*y(k,28) + .100_r8*rxt(k,394)*y(k,110)
         mat(k,883) = .400_r8*rxt(k,400)*y(k,121)
         mat(k,802) = rxt(k,325)*y(k,121) + 3.200_r8*rxt(k,322)*y(k,189) &
                      + .800_r8*rxt(k,323)*y(k,193)
         mat(k,731) = .270_r8*rxt(k,353)*y(k,121)
         mat(k,1376) = .800_r8*rxt(k,323)*y(k,189)
         mat(k,461) = rxt(k,371)*y(k,121)
         mat(k,2055) = .200_r8*rxt(k,389)*y(k,207)
         mat(k,572) = rxt(k,390)*y(k,121) + .200_r8*rxt(k,389)*y(k,199)
         mat(k,1720) = mat(k,1720) + .800_r8*rxt(k,398)*y(k,1) + rxt(k,326)*y(k,25) &
                      + .500_r8*rxt(k,327)*y(k,26)
         mat(k,719) = rxt(k,361)*y(k,121)
         mat(k,53) = -(rxt(k,332)*y(k,217))
         mat(k,1620) = -rxt(k,332)*y(k,46)
         mat(k,892) = -(rxt(k,368)*y(k,217))
         mat(k,1713) = -rxt(k,368)*y(k,47)
         mat(k,540) = .800_r8*rxt(k,398)*y(k,217)
         mat(k,861) = .520_r8*rxt(k,482)*y(k,132)
         mat(k,262) = .500_r8*rxt(k,369)*y(k,123)
         mat(k,835) = .520_r8*rxt(k,485)*y(k,132)
         mat(k,1479) = .250_r8*rxt(k,400)*y(k,186) + .820_r8*rxt(k,353)*y(k,190) &
                      + .500_r8*rxt(k,371)*y(k,196) + .270_r8*rxt(k,494)*y(k,222) &
                      + .040_r8*rxt(k,499)*y(k,223)
         mat(k,1533) = .500_r8*rxt(k,369)*y(k,15)
         mat(k,1781) = .520_r8*rxt(k,482)*y(k,5) + .520_r8*rxt(k,485)*y(k,109)
         mat(k,975) = .500_r8*rxt(k,503)*y(k,217)
         mat(k,882) = .250_r8*rxt(k,400)*y(k,121)
         mat(k,730) = .820_r8*rxt(k,353)*y(k,121) + .820_r8*rxt(k,351)*y(k,193)
         mat(k,1372) = .820_r8*rxt(k,351)*y(k,190) + .150_r8*rxt(k,492)*y(k,222) &
                      + .025_r8*rxt(k,497)*y(k,223)
         mat(k,460) = .500_r8*rxt(k,371)*y(k,121)
         mat(k,1713) = mat(k,1713) + .800_r8*rxt(k,398)*y(k,1) + .500_r8*rxt(k,503) &
                      *y(k,175)
         mat(k,1037) = .270_r8*rxt(k,494)*y(k,121) + .150_r8*rxt(k,492)*y(k,193)
         mat(k,1015) = .040_r8*rxt(k,499)*y(k,121) + .025_r8*rxt(k,497)*y(k,193)
         mat(k,1153) = -(rxt(k,356)*y(k,123) + rxt(k,357)*y(k,217))
         mat(k,1551) = -rxt(k,356)*y(k,48)
         mat(k,1733) = -rxt(k,357)*y(k,48)
         mat(k,1007) = rxt(k,358)*y(k,217)
         mat(k,1142) = .880_r8*rxt(k,380)*y(k,132)
         mat(k,1247) = .500_r8*rxt(k,394)*y(k,132)
         mat(k,1495) = .170_r8*rxt(k,453)*y(k,194) + .050_r8*rxt(k,416)*y(k,202) &
                      + .250_r8*rxt(k,378)*y(k,203) + .170_r8*rxt(k,459)*y(k,206) &
                      + .400_r8*rxt(k,469)*y(k,224) + .250_r8*rxt(k,435)*y(k,225) &
                      + .540_r8*rxt(k,475)*y(k,226) + .510_r8*rxt(k,478)*y(k,227)
         mat(k,1551) = mat(k,1551) + .050_r8*rxt(k,417)*y(k,202) + .250_r8*rxt(k,377) &
                      *y(k,203) + .250_r8*rxt(k,436)*y(k,225)
         mat(k,771) = rxt(k,359)*y(k,217)
         mat(k,1796) = .880_r8*rxt(k,380)*y(k,104) + .500_r8*rxt(k,394)*y(k,110)
         mat(k,1294) = .250_r8*rxt(k,374)*y(k,203) + .250_r8*rxt(k,432)*y(k,225)
         mat(k,1387) = .240_r8*rxt(k,375)*y(k,203) + .500_r8*rxt(k,363)*y(k,221) &
                      + .100_r8*rxt(k,433)*y(k,225)
         mat(k,648) = .170_r8*rxt(k,453)*y(k,121) + .070_r8*rxt(k,452)*y(k,199)
         mat(k,2067) = .070_r8*rxt(k,452)*y(k,194) + .070_r8*rxt(k,458)*y(k,206)
         mat(k,1204) = .050_r8*rxt(k,416)*y(k,121) + .050_r8*rxt(k,417)*y(k,123)
         mat(k,1228) = .250_r8*rxt(k,378)*y(k,121) + .250_r8*rxt(k,377)*y(k,123) &
                      + .250_r8*rxt(k,374)*y(k,192) + .240_r8*rxt(k,375)*y(k,193)
         mat(k,813) = .170_r8*rxt(k,459)*y(k,121) + .070_r8*rxt(k,458)*y(k,199)
         mat(k,1733) = mat(k,1733) + rxt(k,358)*y(k,94) + rxt(k,359)*y(k,124)
         mat(k,1077) = .500_r8*rxt(k,363)*y(k,193)
         mat(k,624) = .400_r8*rxt(k,469)*y(k,121)
         mat(k,1106) = .250_r8*rxt(k,435)*y(k,121) + .250_r8*rxt(k,436)*y(k,123) &
                      + .250_r8*rxt(k,432)*y(k,192) + .100_r8*rxt(k,433)*y(k,193)
         mat(k,640) = .540_r8*rxt(k,475)*y(k,121)
         mat(k,395) = .510_r8*rxt(k,478)*y(k,121)
         mat(k,455) = -(rxt(k,337)*y(k,217))
         mat(k,1676) = -rxt(k,337)*y(k,49)
         mat(k,940) = .120_r8*rxt(k,350)*y(k,132)
         mat(k,1771) = .120_r8*rxt(k,350)*y(k,28)
         mat(k,1285) = .100_r8*rxt(k,334)*y(k,193) + .150_r8*rxt(k,335)*y(k,199)
         mat(k,1365) = .100_r8*rxt(k,334)*y(k,192)
         mat(k,2026) = .150_r8*rxt(k,335)*y(k,192) + .150_r8*rxt(k,385)*y(k,205)
         mat(k,1265) = .150_r8*rxt(k,385)*y(k,199)
         mat(k,400) = -(rxt(k,338)*y(k,217))
         mat(k,1670) = -rxt(k,338)*y(k,50)
         mat(k,1284) = .400_r8*rxt(k,335)*y(k,199)
         mat(k,2023) = .400_r8*rxt(k,335)*y(k,192) + .400_r8*rxt(k,385)*y(k,205)
         mat(k,1264) = .400_r8*rxt(k,385)*y(k,199)
         mat(k,713) = -(rxt(k,304)*y(k,217))
         mat(k,1699) = -rxt(k,304)*y(k,51)
         mat(k,1119) = .200_r8*rxt(k,422)*y(k,193)
         mat(k,800) = .300_r8*rxt(k,323)*y(k,193)
         mat(k,1367) = .200_r8*rxt(k,422)*y(k,100) + .300_r8*rxt(k,323)*y(k,189) &
                      + 2.000_r8*rxt(k,301)*y(k,193) + .250_r8*rxt(k,408)*y(k,201) &
                      + .250_r8*rxt(k,413)*y(k,202) + .250_r8*rxt(k,375)*y(k,203) &
                      + .250_r8*rxt(k,487)*y(k,212) + .500_r8*rxt(k,363)*y(k,221) &
                      + .250_r8*rxt(k,492)*y(k,222) + .250_r8*rxt(k,497)*y(k,223) &
                      + .300_r8*rxt(k,433)*y(k,225)
         mat(k,1163) = .250_r8*rxt(k,408)*y(k,193)
         mat(k,1193) = .250_r8*rxt(k,413)*y(k,193)
         mat(k,1222) = .250_r8*rxt(k,375)*y(k,193)
         mat(k,1055) = .250_r8*rxt(k,487)*y(k,193)
         mat(k,1074) = .500_r8*rxt(k,363)*y(k,193)
         mat(k,1036) = .250_r8*rxt(k,492)*y(k,193)
         mat(k,1014) = .250_r8*rxt(k,497)*y(k,193)
         mat(k,1100) = .300_r8*rxt(k,433)*y(k,193)
         mat(k,310) = -(rxt(k,305)*y(k,217))
         mat(k,1659) = -rxt(k,305)*y(k,52)
         mat(k,1364) = rxt(k,302)*y(k,199)
         mat(k,2011) = rxt(k,302)*y(k,193)
         mat(k,1599) = -(rxt(k,216)*y(k,41) + rxt(k,218)*y(k,76) + rxt(k,219)*y(k,78) &
                      + (rxt(k,220) + rxt(k,221)) * y(k,199) + rxt(k,222)*y(k,132) &
                      + rxt(k,229)*y(k,59) + rxt(k,238)*y(k,91) + rxt(k,328)*y(k,27))
         mat(k,2130) = -rxt(k,216)*y(k,55)
         mat(k,1092) = -rxt(k,218)*y(k,55)
         mat(k,477) = -rxt(k,219)*y(k,55)
         mat(k,2080) = -(rxt(k,220) + rxt(k,221)) * y(k,55)
         mat(k,1809) = -rxt(k,222)*y(k,55)
         mat(k,910) = -rxt(k,229)*y(k,55)
         mat(k,765) = -rxt(k,238)*y(k,55)
         mat(k,210) = -rxt(k,328)*y(k,55)
         mat(k,1853) = rxt(k,257)*y(k,58)
         mat(k,1985) = rxt(k,257)*y(k,18) + (4.000_r8*rxt(k,224)+2.000_r8*rxt(k,226)) &
                      *y(k,58) + rxt(k,228)*y(k,121) + rxt(k,233)*y(k,130) &
                      + rxt(k,509)*y(k,148) + rxt(k,223)*y(k,193) + rxt(k,234) &
                      *y(k,217)
         mat(k,132) = rxt(k,278)*y(k,213)
         mat(k,1338) = rxt(k,236)*y(k,130) + rxt(k,248)*y(k,213) + rxt(k,237)*y(k,217)
         mat(k,1507) = rxt(k,228)*y(k,58)
         mat(k,1894) = rxt(k,233)*y(k,58) + rxt(k,236)*y(k,84)
         mat(k,1320) = rxt(k,509)*y(k,58)
         mat(k,1398) = rxt(k,223)*y(k,58)
         mat(k,2106) = rxt(k,278)*y(k,64) + rxt(k,248)*y(k,84)
         mat(k,1747) = rxt(k,234)*y(k,58) + rxt(k,237)*y(k,84)
         mat(k,1577) = rxt(k,229)*y(k,59)
         mat(k,1972) = 2.000_r8*rxt(k,225)*y(k,58)
         mat(k,905) = rxt(k,229)*y(k,55) + (rxt(k,556)+rxt(k,561)+rxt(k,566))*y(k,84)
         mat(k,1330) = (rxt(k,556)+rxt(k,561)+rxt(k,566))*y(k,59) + (rxt(k,551) &
                       +rxt(k,557)+rxt(k,562))*y(k,91)
         mat(k,762) = (rxt(k,551)+rxt(k,557)+rxt(k,562))*y(k,84)
         mat(k,1971) = 2.000_r8*rxt(k,250)*y(k,58)
         mat(k,1993) = -(rxt(k,223)*y(k,193) + (4._r8*rxt(k,224) + 4._r8*rxt(k,225) &
                      + 4._r8*rxt(k,226) + 4._r8*rxt(k,250)) * y(k,58) + rxt(k,227) &
                      *y(k,199) + rxt(k,228)*y(k,121) + rxt(k,230)*y(k,122) + rxt(k,233) &
                      *y(k,130) + (rxt(k,234) + rxt(k,235)) * y(k,217) + (rxt(k,256) &
                      + rxt(k,257) + rxt(k,258)) * y(k,18) + rxt(k,509)*y(k,148))
         mat(k,1404) = -rxt(k,223)*y(k,58)
         mat(k,2088) = -rxt(k,227)*y(k,58)
         mat(k,1515) = -rxt(k,228)*y(k,58)
         mat(k,1944) = -rxt(k,230)*y(k,58)
         mat(k,1902) = -rxt(k,233)*y(k,58)
         mat(k,1755) = -(rxt(k,234) + rxt(k,235)) * y(k,58)
         mat(k,1861) = -(rxt(k,256) + rxt(k,257) + rxt(k,258)) * y(k,58)
         mat(k,1327) = -rxt(k,509)*y(k,58)
         mat(k,1607) = rxt(k,238)*y(k,91) + rxt(k,222)*y(k,132) + rxt(k,221)*y(k,199)
         mat(k,915) = rxt(k,231)*y(k,130)
         mat(k,1345) = rxt(k,249)*y(k,213)
         mat(k,768) = rxt(k,238)*y(k,55) + rxt(k,239)*y(k,130) + rxt(k,240)*y(k,217)
         mat(k,1902) = mat(k,1902) + rxt(k,231)*y(k,59) + rxt(k,239)*y(k,91)
         mat(k,1817) = rxt(k,222)*y(k,55)
         mat(k,235) = rxt(k,514)*y(k,148)
         mat(k,1327) = mat(k,1327) + rxt(k,514)*y(k,134)
         mat(k,2088) = mat(k,2088) + rxt(k,221)*y(k,55)
         mat(k,2114) = rxt(k,249)*y(k,84)
         mat(k,1755) = mat(k,1755) + rxt(k,240)*y(k,91)
         mat(k,907) = -(rxt(k,229)*y(k,55) + rxt(k,231)*y(k,130) + rxt(k,232)*y(k,217) &
                      + (rxt(k,556) + rxt(k,561) + rxt(k,566)) * y(k,84))
         mat(k,1587) = -rxt(k,229)*y(k,59)
         mat(k,1884) = -rxt(k,231)*y(k,59)
         mat(k,1715) = -rxt(k,232)*y(k,59)
         mat(k,1334) = -(rxt(k,556) + rxt(k,561) + rxt(k,566)) * y(k,59)
         mat(k,1977) = rxt(k,230)*y(k,122)
         mat(k,1923) = rxt(k,230)*y(k,58)
         mat(k,1002) = -((rxt(k,307) + rxt(k,317)) * y(k,217))
         mat(k,1723) = -(rxt(k,307) + rxt(k,317)) * y(k,61)
         mat(k,864) = .230_r8*rxt(k,482)*y(k,132)
         mat(k,1349) = rxt(k,252)*y(k,41)
         mat(k,204) = .350_r8*rxt(k,319)*y(k,217)
         mat(k,450) = .630_r8*rxt(k,321)*y(k,132)
         mat(k,946) = .560_r8*rxt(k,350)*y(k,132)
         mat(k,2122) = rxt(k,252)*y(k,16) + rxt(k,216)*y(k,55) + rxt(k,297)*y(k,123) &
                      + rxt(k,298)*y(k,130) + rxt(k,299)*y(k,217)
         mat(k,1152) = rxt(k,356)*y(k,123) + rxt(k,357)*y(k,217)
         mat(k,1590) = rxt(k,216)*y(k,41)
         mat(k,821) = rxt(k,344)*y(k,217)
         mat(k,784) = .620_r8*rxt(k,427)*y(k,132)
         mat(k,1140) = .650_r8*rxt(k,380)*y(k,132)
         mat(k,838) = .230_r8*rxt(k,485)*y(k,132)
         mat(k,1245) = .560_r8*rxt(k,394)*y(k,132)
         mat(k,1486) = .170_r8*rxt(k,453)*y(k,194) + .220_r8*rxt(k,378)*y(k,203) &
                      + .400_r8*rxt(k,456)*y(k,204) + .350_r8*rxt(k,459)*y(k,206) &
                      + .225_r8*rxt(k,494)*y(k,222) + .250_r8*rxt(k,435)*y(k,225)
         mat(k,1542) = rxt(k,297)*y(k,41) + rxt(k,356)*y(k,48) + .220_r8*rxt(k,377) &
                      *y(k,203) + .500_r8*rxt(k,436)*y(k,225)
         mat(k,1885) = rxt(k,298)*y(k,41) + rxt(k,504)*y(k,135)
         mat(k,1788) = .230_r8*rxt(k,482)*y(k,5) + .630_r8*rxt(k,321)*y(k,24) &
                      + .560_r8*rxt(k,350)*y(k,28) + .620_r8*rxt(k,427)*y(k,97) &
                      + .650_r8*rxt(k,380)*y(k,104) + .230_r8*rxt(k,485)*y(k,109) &
                      + .560_r8*rxt(k,394)*y(k,110)
         mat(k,255) = rxt(k,504)*y(k,130) + rxt(k,505)*y(k,217)
         mat(k,977) = .700_r8*rxt(k,503)*y(k,217)
         mat(k,1289) = .220_r8*rxt(k,374)*y(k,203) + .250_r8*rxt(k,432)*y(k,225)
         mat(k,1378) = .110_r8*rxt(k,375)*y(k,203) + .125_r8*rxt(k,492)*y(k,222) &
                      + .200_r8*rxt(k,433)*y(k,225)
         mat(k,647) = .170_r8*rxt(k,453)*y(k,121) + .070_r8*rxt(k,452)*y(k,199)
         mat(k,2057) = .070_r8*rxt(k,452)*y(k,194) + .160_r8*rxt(k,455)*y(k,204) &
                      + .140_r8*rxt(k,458)*y(k,206)
         mat(k,1225) = .220_r8*rxt(k,378)*y(k,121) + .220_r8*rxt(k,377)*y(k,123) &
                      + .220_r8*rxt(k,374)*y(k,192) + .110_r8*rxt(k,375)*y(k,193)
         mat(k,610) = .400_r8*rxt(k,456)*y(k,121) + .160_r8*rxt(k,455)*y(k,199)
         mat(k,812) = .350_r8*rxt(k,459)*y(k,121) + .140_r8*rxt(k,458)*y(k,199)
         mat(k,1723) = mat(k,1723) + .350_r8*rxt(k,319)*y(k,23) + rxt(k,299)*y(k,41) &
                      + rxt(k,357)*y(k,48) + rxt(k,344)*y(k,74) + rxt(k,505)*y(k,135) &
                      + .700_r8*rxt(k,503)*y(k,175)
         mat(k,1040) = .225_r8*rxt(k,494)*y(k,121) + .125_r8*rxt(k,492)*y(k,193)
         mat(k,1103) = .250_r8*rxt(k,435)*y(k,121) + .500_r8*rxt(k,436)*y(k,123) &
                      + .250_r8*rxt(k,432)*y(k,192) + .200_r8*rxt(k,433)*y(k,193)
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
         mat(k,60) = -(rxt(k,277)*y(k,213))
         mat(k,2093) = -rxt(k,277)*y(k,63)
         mat(k,130) = -(rxt(k,278)*y(k,213))
         mat(k,2096) = -rxt(k,278)*y(k,64)
         mat(k,118) = -(rxt(k,451)*y(k,217))
         mat(k,1629) = -rxt(k,451)*y(k,65)
         mat(k,112) = .180_r8*rxt(k,471)*y(k,217)
         mat(k,1629) = mat(k,1629) + .180_r8*rxt(k,471)*y(k,177)
         mat(k,192) = -(rxt(k,518)*y(k,123) + (rxt(k,519) + rxt(k,521)) * y(k,217))
         mat(k,1523) = -rxt(k,518)*y(k,66)
         mat(k,1642) = -(rxt(k,519) + rxt(k,521)) * y(k,66)
         mat(k,672) = rxt(k,339)*y(k,199)
         mat(k,1998) = rxt(k,339)*y(k,198)
         mat(k,664) = -(rxt(k,274)*y(k,76) + rxt(k,275)*y(k,228) + rxt(k,276)*y(k,88))
         mat(k,1087) = -rxt(k,274)*y(k,72)
         mat(k,2146) = -rxt(k,275)*y(k,72)
         mat(k,1950) = -rxt(k,276)*y(k,72)
         mat(k,61) = 2.000_r8*rxt(k,277)*y(k,213)
         mat(k,131) = rxt(k,278)*y(k,213)
         mat(k,2097) = 2.000_r8*rxt(k,277)*y(k,63) + rxt(k,278)*y(k,64)
         mat(k,961) = -(rxt(k,343)*y(k,217))
         mat(k,1719) = -rxt(k,343)*y(k,73)
         mat(k,507) = .700_r8*rxt(k,418)*y(k,217)
         mat(k,425) = .500_r8*rxt(k,419)*y(k,217)
         mat(k,270) = rxt(k,430)*y(k,217)
         mat(k,1482) = .050_r8*rxt(k,416)*y(k,202) + .530_r8*rxt(k,378)*y(k,203) &
                      + .225_r8*rxt(k,494)*y(k,222) + .250_r8*rxt(k,435)*y(k,225)
         mat(k,1538) = .050_r8*rxt(k,417)*y(k,202) + .530_r8*rxt(k,377)*y(k,203) &
                      + .250_r8*rxt(k,436)*y(k,225)
         mat(k,1421) = rxt(k,342)*y(k,197)
         mat(k,1288) = .530_r8*rxt(k,374)*y(k,203) + .250_r8*rxt(k,432)*y(k,225)
         mat(k,1375) = .260_r8*rxt(k,375)*y(k,203) + .125_r8*rxt(k,492)*y(k,222) &
                      + .100_r8*rxt(k,433)*y(k,225)
         mat(k,347) = rxt(k,342)*y(k,131)
         mat(k,1197) = .050_r8*rxt(k,416)*y(k,121) + .050_r8*rxt(k,417)*y(k,123)
         mat(k,1223) = .530_r8*rxt(k,378)*y(k,121) + .530_r8*rxt(k,377)*y(k,123) &
                      + .530_r8*rxt(k,374)*y(k,192) + .260_r8*rxt(k,375)*y(k,193)
         mat(k,1719) = mat(k,1719) + .700_r8*rxt(k,418)*y(k,98) + .500_r8*rxt(k,419) &
                      *y(k,99) + rxt(k,430)*y(k,114)
         mat(k,1038) = .225_r8*rxt(k,494)*y(k,121) + .125_r8*rxt(k,492)*y(k,193)
         mat(k,1102) = .250_r8*rxt(k,435)*y(k,121) + .250_r8*rxt(k,436)*y(k,123) &
                      + .250_r8*rxt(k,432)*y(k,192) + .100_r8*rxt(k,433)*y(k,193)
         mat(k,820) = -(rxt(k,344)*y(k,217))
         mat(k,1709) = -rxt(k,344)*y(k,74)
         mat(k,203) = .650_r8*rxt(k,319)*y(k,217)
         mat(k,960) = .200_r8*rxt(k,343)*y(k,217)
         mat(k,928) = rxt(k,431)*y(k,217)
         mat(k,1477) = rxt(k,442)*y(k,187) + .050_r8*rxt(k,416)*y(k,202) &
                      + .400_r8*rxt(k,456)*y(k,204) + .170_r8*rxt(k,459)*y(k,206) &
                      + .700_r8*rxt(k,462)*y(k,219) + .600_r8*rxt(k,469)*y(k,224) &
                      + .250_r8*rxt(k,435)*y(k,225) + .340_r8*rxt(k,475)*y(k,226) &
                      + .170_r8*rxt(k,478)*y(k,227)
         mat(k,1529) = .050_r8*rxt(k,417)*y(k,202) + .250_r8*rxt(k,436)*y(k,225)
         mat(k,380) = rxt(k,442)*y(k,121)
         mat(k,1286) = .250_r8*rxt(k,432)*y(k,225)
         mat(k,1371) = .100_r8*rxt(k,433)*y(k,225)
         mat(k,2050) = .160_r8*rxt(k,455)*y(k,204) + .070_r8*rxt(k,458)*y(k,206)
         mat(k,1196) = .050_r8*rxt(k,416)*y(k,121) + .050_r8*rxt(k,417)*y(k,123)
         mat(k,609) = .400_r8*rxt(k,456)*y(k,121) + .160_r8*rxt(k,455)*y(k,199)
         mat(k,811) = .170_r8*rxt(k,459)*y(k,121) + .070_r8*rxt(k,458)*y(k,199)
         mat(k,1709) = mat(k,1709) + .650_r8*rxt(k,319)*y(k,23) + .200_r8*rxt(k,343) &
                      *y(k,73) + rxt(k,431)*y(k,115)
         mat(k,338) = .700_r8*rxt(k,462)*y(k,121)
         mat(k,622) = .600_r8*rxt(k,469)*y(k,121)
         mat(k,1101) = .250_r8*rxt(k,435)*y(k,121) + .250_r8*rxt(k,436)*y(k,123) &
                      + .250_r8*rxt(k,432)*y(k,192) + .100_r8*rxt(k,433)*y(k,193)
         mat(k,638) = .340_r8*rxt(k,475)*y(k,121)
         mat(k,394) = .170_r8*rxt(k,478)*y(k,121)
         mat(k,1832) = -((rxt(k,174) + rxt(k,175) + rxt(k,176)) * y(k,199) + rxt(k,177) &
                      *y(k,131) + rxt(k,180)*y(k,132))
         mat(k,2083) = -(rxt(k,174) + rxt(k,175) + rxt(k,176)) * y(k,75)
         mat(k,1430) = -rxt(k,177)*y(k,75)
         mat(k,1812) = -rxt(k,180)*y(k,75)
         mat(k,2133) = rxt(k,299)*y(k,217)
         mat(k,1602) = rxt(k,218)*y(k,76)
         mat(k,1004) = rxt(k,317)*y(k,217)
         mat(k,669) = rxt(k,274)*y(k,76)
         mat(k,1094) = rxt(k,218)*y(k,55) + rxt(k,274)*y(k,72) + rxt(k,172)*y(k,130) &
                      + rxt(k,155)*y(k,213) + rxt(k,181)*y(k,217)
         mat(k,757) = rxt(k,272)*y(k,213)
         mat(k,1340) = rxt(k,249)*y(k,213)
         mat(k,750) = rxt(k,204)*y(k,217)
         mat(k,1897) = rxt(k,172)*y(k,76) + rxt(k,184)*y(k,217)
         mat(k,258) = rxt(k,505)*y(k,217)
         mat(k,606) = rxt(k,510)*y(k,217)
         mat(k,1323) = rxt(k,515)*y(k,217)
         mat(k,2109) = rxt(k,155)*y(k,76) + rxt(k,272)*y(k,80) + rxt(k,249)*y(k,84)
         mat(k,1750) = rxt(k,299)*y(k,41) + rxt(k,317)*y(k,61) + rxt(k,181)*y(k,76) &
                      + rxt(k,204)*y(k,111) + rxt(k,184)*y(k,130) + rxt(k,505) &
                      *y(k,135) + rxt(k,510)*y(k,146) + rxt(k,515)*y(k,148)
         mat(k,1088) = -(rxt(k,155)*y(k,213) + rxt(k,172)*y(k,130) + rxt(k,181) &
                      *y(k,217) + rxt(k,218)*y(k,55) + rxt(k,274)*y(k,72))
         mat(k,2099) = -rxt(k,155)*y(k,76)
         mat(k,1886) = -rxt(k,172)*y(k,76)
         mat(k,1729) = -rxt(k,181)*y(k,76)
         mat(k,1591) = -rxt(k,218)*y(k,76)
         mat(k,665) = -rxt(k,274)*y(k,76)
         mat(k,1822) = rxt(k,174)*y(k,199)
         mat(k,2063) = rxt(k,174)*y(k,75)
         mat(k,475) = -(rxt(k,173)*y(k,130) + rxt(k,182)*y(k,217) + rxt(k,219)*y(k,55))
         mat(k,1872) = -rxt(k,173)*y(k,78)
         mat(k,1679) = -rxt(k,182)*y(k,78)
         mat(k,1581) = -rxt(k,219)*y(k,78)
         mat(k,2027) = 2.000_r8*rxt(k,188)*y(k,199)
         mat(k,1679) = mat(k,1679) + 2.000_r8*rxt(k,187)*y(k,217)
         mat(k,173) = rxt(k,517)*y(k,228)
         mat(k,2143) = rxt(k,517)*y(k,150)
         mat(k,754) = -(rxt(k,265)*y(k,130) + rxt(k,266)*y(k,217) + (rxt(k,271) &
                      + rxt(k,272)) * y(k,213))
         mat(k,1881) = -rxt(k,265)*y(k,80)
         mat(k,1703) = -rxt(k,266)*y(k,80)
         mat(k,2098) = -(rxt(k,271) + rxt(k,272)) * y(k,80)
         mat(k,1348) = rxt(k,252)*y(k,41) + rxt(k,253)*y(k,199)
         mat(k,2121) = rxt(k,252)*y(k,16)
         mat(k,2045) = rxt(k,253)*y(k,16)
         mat(k,1335) = -(rxt(k,236)*y(k,130) + rxt(k,237)*y(k,217) + (rxt(k,248) &
                      + rxt(k,249)) * y(k,213) + (rxt(k,551) + rxt(k,557) + rxt(k,562) &
                      ) * y(k,91) + (rxt(k,556) + rxt(k,561) + rxt(k,566)) * y(k,59) &
                      + (rxt(k,558) + rxt(k,563)) * y(k,90))
         mat(k,1888) = -rxt(k,236)*y(k,84)
         mat(k,1741) = -rxt(k,237)*y(k,84)
         mat(k,2100) = -(rxt(k,248) + rxt(k,249)) * y(k,84)
         mat(k,764) = -(rxt(k,551) + rxt(k,557) + rxt(k,562)) * y(k,84)
         mat(k,908) = -(rxt(k,556) + rxt(k,561) + rxt(k,566)) * y(k,84)
         mat(k,656) = -(rxt(k,558) + rxt(k,563)) * y(k,84)
         mat(k,209) = rxt(k,328)*y(k,55)
         mat(k,2124) = rxt(k,216)*y(k,55)
         mat(k,1593) = rxt(k,328)*y(k,27) + rxt(k,216)*y(k,41) + rxt(k,218)*y(k,76) &
                      + rxt(k,219)*y(k,78) + rxt(k,238)*y(k,91) + rxt(k,220)*y(k,199)
         mat(k,1979) = rxt(k,235)*y(k,217)
         mat(k,1089) = rxt(k,218)*y(k,55)
         mat(k,476) = rxt(k,219)*y(k,55)
         mat(k,764) = mat(k,764) + rxt(k,238)*y(k,55)
         mat(k,2074) = rxt(k,220)*y(k,55)
         mat(k,1741) = mat(k,1741) + rxt(k,235)*y(k,58)
         mat(k,102) = -(rxt(k,308)*y(k,217) + rxt(k,316)*y(k,213))
         mat(k,1627) = -rxt(k,308)*y(k,85)
         mat(k,2095) = -rxt(k,316)*y(k,85)
         mat(k,709) = -(rxt(k,309)*y(k,217))
         mat(k,1698) = -rxt(k,309)*y(k,86)
         mat(k,857) = .050_r8*rxt(k,482)*y(k,132)
         mat(k,202) = .350_r8*rxt(k,319)*y(k,217)
         mat(k,449) = .370_r8*rxt(k,321)*y(k,132)
         mat(k,941) = .120_r8*rxt(k,350)*y(k,132)
         mat(k,781) = .110_r8*rxt(k,427)*y(k,132)
         mat(k,1139) = .330_r8*rxt(k,380)*y(k,132)
         mat(k,831) = .050_r8*rxt(k,485)*y(k,132)
         mat(k,1242) = .120_r8*rxt(k,394)*y(k,132)
         mat(k,1470) = rxt(k,312)*y(k,200)
         mat(k,1775) = .050_r8*rxt(k,482)*y(k,5) + .370_r8*rxt(k,321)*y(k,24) &
                      + .120_r8*rxt(k,350)*y(k,28) + .110_r8*rxt(k,427)*y(k,97) &
                      + .330_r8*rxt(k,380)*y(k,104) + .050_r8*rxt(k,485)*y(k,109) &
                      + .120_r8*rxt(k,394)*y(k,110)
         mat(k,2042) = rxt(k,310)*y(k,200)
         mat(k,331) = rxt(k,312)*y(k,121) + rxt(k,310)*y(k,199)
         mat(k,1698) = mat(k,1698) + .350_r8*rxt(k,319)*y(k,23)
         mat(k,663) = rxt(k,274)*y(k,76) + rxt(k,276)*y(k,88) + rxt(k,275)*y(k,228)
         mat(k,1086) = rxt(k,274)*y(k,72)
         mat(k,1949) = rxt(k,276)*y(k,72)
         mat(k,2144) = rxt(k,275)*y(k,72)
         mat(k,1965) = -(rxt(k,213)*y(k,217) + rxt(k,276)*y(k,72))
         mat(k,1754) = -rxt(k,213)*y(k,88)
         mat(k,670) = -rxt(k,276)*y(k,88)
         mat(k,2137) = rxt(k,297)*y(k,123)
         mat(k,972) = rxt(k,330)*y(k,123)
         mat(k,1158) = rxt(k,356)*y(k,123)
         mat(k,914) = (rxt(k,556)+rxt(k,561)+rxt(k,566))*y(k,84)
         mat(k,196) = rxt(k,518)*y(k,123)
         mat(k,1344) = (rxt(k,556)+rxt(k,561)+rxt(k,566))*y(k,59)
         mat(k,1943) = rxt(k,212)*y(k,217)
         mat(k,1571) = rxt(k,297)*y(k,41) + rxt(k,330)*y(k,44) + rxt(k,356)*y(k,48) &
                      + rxt(k,518)*y(k,66)
         mat(k,1754) = mat(k,1754) + rxt(k,212)*y(k,122)
         mat(k,362) = -(rxt(k,189)*y(k,217))
         mat(k,1666) = -rxt(k,189)*y(k,89)
         mat(k,1911) = rxt(k,210)*y(k,199)
         mat(k,2019) = rxt(k,210)*y(k,122)
         mat(k,655) = -(rxt(k,267)*y(k,130) + (rxt(k,558) + rxt(k,563)) * y(k,84))
         mat(k,1876) = -rxt(k,267)*y(k,90)
         mat(k,1332) = -(rxt(k,558) + rxt(k,563)) * y(k,90)
         mat(k,1845) = rxt(k,259)*y(k,199)
         mat(k,2040) = rxt(k,259)*y(k,18)
         mat(k,763) = -(rxt(k,238)*y(k,55) + rxt(k,239)*y(k,130) + rxt(k,240)*y(k,217) &
                      + (rxt(k,551) + rxt(k,557) + rxt(k,562)) * y(k,84))
         mat(k,1584) = -rxt(k,238)*y(k,91)
         mat(k,1882) = -rxt(k,239)*y(k,91)
         mat(k,1704) = -rxt(k,240)*y(k,91)
         mat(k,1333) = -(rxt(k,551) + rxt(k,557) + rxt(k,562)) * y(k,91)
         mat(k,1975) = rxt(k,227)*y(k,199)
         mat(k,906) = rxt(k,232)*y(k,217)
         mat(k,2046) = rxt(k,227)*y(k,58)
         mat(k,1704) = mat(k,1704) + rxt(k,232)*y(k,59)
         mat(k,989) = -(rxt(k,373)*y(k,217))
         mat(k,1722) = -rxt(k,373)*y(k,92)
         mat(k,508) = .300_r8*rxt(k,418)*y(k,217)
         mat(k,426) = .500_r8*rxt(k,419)*y(k,217)
         mat(k,1485) = rxt(k,372)*y(k,196) + rxt(k,379)*y(k,203)
         mat(k,462) = rxt(k,372)*y(k,121)
         mat(k,1224) = rxt(k,379)*y(k,121)
         mat(k,1722) = mat(k,1722) + .300_r8*rxt(k,418)*y(k,98) + .500_r8*rxt(k,419) &
                      *y(k,99)
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
         mat(k,156) = -(rxt(k,404)*y(k,217))
         mat(k,1635) = -rxt(k,404)*y(k,93)
         mat(k,1006) = -(rxt(k,358)*y(k,217))
         mat(k,1724) = -rxt(k,358)*y(k,94)
         mat(k,509) = .700_r8*rxt(k,418)*y(k,217)
         mat(k,427) = .500_r8*rxt(k,419)*y(k,217)
         mat(k,468) = .500_r8*rxt(k,393)*y(k,217)
         mat(k,1487) = .050_r8*rxt(k,416)*y(k,202) + .220_r8*rxt(k,378)*y(k,203) &
                      + .250_r8*rxt(k,435)*y(k,225)
         mat(k,1543) = .050_r8*rxt(k,417)*y(k,202) + .220_r8*rxt(k,377)*y(k,203) &
                      + .250_r8*rxt(k,436)*y(k,225)
         mat(k,434) = .500_r8*rxt(k,362)*y(k,217)
         mat(k,1290) = .220_r8*rxt(k,374)*y(k,203) + .250_r8*rxt(k,432)*y(k,225)
         mat(k,1379) = .230_r8*rxt(k,375)*y(k,203) + .200_r8*rxt(k,363)*y(k,221) &
                      + .100_r8*rxt(k,433)*y(k,225)
         mat(k,1200) = .050_r8*rxt(k,416)*y(k,121) + .050_r8*rxt(k,417)*y(k,123)
         mat(k,1226) = .220_r8*rxt(k,378)*y(k,121) + .220_r8*rxt(k,377)*y(k,123) &
                      + .220_r8*rxt(k,374)*y(k,192) + .230_r8*rxt(k,375)*y(k,193)
         mat(k,1724) = mat(k,1724) + .700_r8*rxt(k,418)*y(k,98) + .500_r8*rxt(k,419) &
                      *y(k,99) + .500_r8*rxt(k,393)*y(k,108) + .500_r8*rxt(k,362) &
                      *y(k,144)
         mat(k,1075) = .200_r8*rxt(k,363)*y(k,193)
         mat(k,1104) = .250_r8*rxt(k,435)*y(k,121) + .250_r8*rxt(k,436)*y(k,123) &
                      + .250_r8*rxt(k,432)*y(k,192) + .100_r8*rxt(k,433)*y(k,193)
         mat(k,213) = -(rxt(k,405)*y(k,217))
         mat(k,1645) = -rxt(k,405)*y(k,95)
         mat(k,1442) = .870_r8*rxt(k,416)*y(k,202)
         mat(k,1524) = .950_r8*rxt(k,417)*y(k,202)
         mat(k,1282) = rxt(k,412)*y(k,202)
         mat(k,1362) = .750_r8*rxt(k,413)*y(k,202)
         mat(k,1189) = .870_r8*rxt(k,416)*y(k,121) + .950_r8*rxt(k,417)*y(k,123) &
                      + rxt(k,412)*y(k,192) + .750_r8*rxt(k,413)*y(k,193)
         mat(k,69) = -(rxt(k,406)*y(k,217))
         mat(k,1623) = -rxt(k,406)*y(k,96)
         mat(k,578) = .600_r8*rxt(k,429)*y(k,217)
         mat(k,1623) = mat(k,1623) + .600_r8*rxt(k,429)*y(k,102)
         mat(k,782) = -(rxt(k,420)*y(k,123) + rxt(k,427)*y(k,132) + rxt(k,428) &
                      *y(k,217))
         mat(k,1528) = -rxt(k,420)*y(k,97)
         mat(k,1776) = -rxt(k,427)*y(k,97)
         mat(k,1706) = -rxt(k,428)*y(k,97)
         mat(k,506) = -(rxt(k,418)*y(k,217))
         mat(k,1683) = -rxt(k,418)*y(k,98)
         mat(k,1457) = .080_r8*rxt(k,410)*y(k,201)
         mat(k,1161) = .080_r8*rxt(k,410)*y(k,121)
         mat(k,423) = -(rxt(k,419)*y(k,217))
         mat(k,1673) = -rxt(k,419)*y(k,99)
         mat(k,1454) = .080_r8*rxt(k,416)*y(k,202)
         mat(k,1190) = .080_r8*rxt(k,416)*y(k,121)
         mat(k,1125) = -(rxt(k,421)*y(k,192) + rxt(k,422)*y(k,193) + rxt(k,423) &
                      *y(k,199) + rxt(k,424)*y(k,121) + rxt(k,425)*y(k,123))
         mat(k,1292) = -rxt(k,421)*y(k,100)
         mat(k,1385) = -rxt(k,422)*y(k,100)
         mat(k,2065) = -rxt(k,423)*y(k,100)
         mat(k,1493) = -rxt(k,424)*y(k,100)
         mat(k,1549) = -rxt(k,425)*y(k,100)
         mat(k,785) = rxt(k,420)*y(k,123)
         mat(k,1549) = mat(k,1549) + rxt(k,420)*y(k,97)
         mat(k,298) = -(rxt(k,426)*y(k,217))
         mat(k,1657) = -rxt(k,426)*y(k,101)
         mat(k,1117) = rxt(k,423)*y(k,199)
         mat(k,2009) = rxt(k,423)*y(k,100)
         mat(k,579) = -(rxt(k,429)*y(k,217))
         mat(k,1689) = -rxt(k,429)*y(k,102)
         mat(k,2034) = rxt(k,409)*y(k,201) + rxt(k,414)*y(k,202)
         mat(k,1162) = rxt(k,409)*y(k,199)
         mat(k,1192) = rxt(k,414)*y(k,199)
         mat(k,40) = -(rxt(k,543)*y(k,217))
         mat(k,1617) = -rxt(k,543)*y(k,103)
         mat(k,1141) = -(rxt(k,380)*y(k,132) + rxt(k,381)*y(k,217))
         mat(k,1795) = -rxt(k,380)*y(k,104)
         mat(k,1732) = -rxt(k,381)*y(k,104)
         mat(k,786) = .300_r8*rxt(k,427)*y(k,132)
         mat(k,1494) = .360_r8*rxt(k,410)*y(k,201)
         mat(k,1550) = .400_r8*rxt(k,411)*y(k,201)
         mat(k,1795) = mat(k,1795) + .300_r8*rxt(k,427)*y(k,97)
         mat(k,1293) = .390_r8*rxt(k,407)*y(k,201)
         mat(k,1386) = .310_r8*rxt(k,408)*y(k,201)
         mat(k,1170) = .360_r8*rxt(k,410)*y(k,121) + .400_r8*rxt(k,411)*y(k,123) &
                      + .390_r8*rxt(k,407)*y(k,192) + .310_r8*rxt(k,408)*y(k,193)
         mat(k,216) = -(rxt(k,382)*y(k,217))
         mat(k,1646) = -rxt(k,382)*y(k,105)
         mat(k,2001) = rxt(k,376)*y(k,203)
         mat(k,1221) = rxt(k,376)*y(k,199)
         mat(k,413) = -(rxt(k,391)*y(k,217))
         mat(k,1672) = -rxt(k,391)*y(k,106)
         mat(k,1453) = .800_r8*rxt(k,400)*y(k,186)
         mat(k,876) = .800_r8*rxt(k,400)*y(k,121)
         mat(k,221) = -(rxt(k,392)*y(k,217))
         mat(k,1647) = -rxt(k,392)*y(k,107)
         mat(k,2002) = .800_r8*rxt(k,389)*y(k,207)
         mat(k,570) = .800_r8*rxt(k,389)*y(k,199)
         mat(k,467) = -(rxt(k,393)*y(k,217))
         mat(k,1678) = -rxt(k,393)*y(k,108)
         mat(k,1914) = rxt(k,396)*y(k,205)
         mat(k,1266) = rxt(k,396)*y(k,122)
         mat(k,833) = -(rxt(k,484)*y(k,123) + rxt(k,485)*y(k,132) + rxt(k,486) &
                      *y(k,217))
         mat(k,1530) = -rxt(k,484)*y(k,109)
         mat(k,1778) = -rxt(k,485)*y(k,109)
         mat(k,1710) = -rxt(k,486)*y(k,109)
         mat(k,1249) = -(rxt(k,394)*y(k,132) + rxt(k,395)*y(k,217))
         mat(k,1800) = -rxt(k,394)*y(k,110)
         mat(k,1737) = -rxt(k,395)*y(k,110)
         mat(k,789) = .200_r8*rxt(k,427)*y(k,132)
         mat(k,1499) = .560_r8*rxt(k,410)*y(k,201)
         mat(k,1555) = .600_r8*rxt(k,411)*y(k,201)
         mat(k,1800) = mat(k,1800) + .200_r8*rxt(k,427)*y(k,97)
         mat(k,1298) = .610_r8*rxt(k,407)*y(k,201)
         mat(k,1391) = .440_r8*rxt(k,408)*y(k,201)
         mat(k,1174) = .560_r8*rxt(k,410)*y(k,121) + .600_r8*rxt(k,411)*y(k,123) &
                      + .610_r8*rxt(k,407)*y(k,192) + .440_r8*rxt(k,408)*y(k,193)
         mat(k,745) = -(rxt(k,192)*y(k,121) + (rxt(k,193) + rxt(k,194) + rxt(k,195) &
                      ) * y(k,122) + rxt(k,196)*y(k,131) + rxt(k,204)*y(k,217) &
                      + rxt(k,576)*y(k,216))
         mat(k,1473) = -rxt(k,192)*y(k,111)
         mat(k,1919) = -(rxt(k,193) + rxt(k,194) + rxt(k,195)) * y(k,111)
         mat(k,1419) = -rxt(k,196)*y(k,111)
         mat(k,1702) = -rxt(k,204)*y(k,111)
         mat(k,685) = -rxt(k,576)*y(k,111)
         mat(k,1880) = rxt(k,190)*y(k,208) + rxt(k,573)*y(k,211)
         mat(k,1419) = mat(k,1419) + rxt(k,574)*y(k,211)
         mat(k,703) = 1.100_r8*rxt(k,569)*y(k,209) + .200_r8*rxt(k,567)*y(k,210)
         mat(k,419) = rxt(k,190)*y(k,130)
         mat(k,553) = 1.100_r8*rxt(k,569)*y(k,195)
         mat(k,693) = .200_r8*rxt(k,567)*y(k,195)
         mat(k,389) = rxt(k,573)*y(k,130) + rxt(k,574)*y(k,131)
         mat(k,1908) = rxt(k,211)*y(k,123)
         mat(k,1522) = rxt(k,211)*y(k,122)
         mat(k,268) = -(rxt(k,430)*y(k,217))
         mat(k,1653) = -rxt(k,430)*y(k,114)
         mat(k,1116) = .200_r8*rxt(k,422)*y(k,193)
         mat(k,1363) = .200_r8*rxt(k,422)*y(k,100)
         mat(k,929) = -(rxt(k,431)*y(k,217))
         mat(k,1717) = -rxt(k,431)*y(k,115)
         mat(k,1121) = rxt(k,424)*y(k,121) + rxt(k,425)*y(k,123) + rxt(k,421)*y(k,192) &
                      + .800_r8*rxt(k,422)*y(k,193)
         mat(k,1481) = rxt(k,424)*y(k,100)
         mat(k,1536) = rxt(k,425)*y(k,100)
         mat(k,1287) = rxt(k,421)*y(k,100)
         mat(k,1374) = .800_r8*rxt(k,422)*y(k,100)
         mat(k,50) = -(rxt(k,520)*y(k,217))
         mat(k,1619) = -rxt(k,520)*y(k,119)
         mat(k,1505) = -(rxt(k,192)*y(k,111) + rxt(k,201)*y(k,123) + rxt(k,205) &
                      *y(k,199) + rxt(k,206)*y(k,132) + rxt(k,207)*y(k,130) + rxt(k,228) &
                      *y(k,58) + rxt(k,260)*y(k,18) + rxt(k,303)*y(k,193) + rxt(k,312) &
                      *y(k,200) + rxt(k,325)*y(k,189) + rxt(k,336)*y(k,192) + rxt(k,340) &
                      *y(k,198) + rxt(k,353)*y(k,190) + rxt(k,361)*y(k,220) + rxt(k,365) &
                      *y(k,221) + (rxt(k,371) + rxt(k,372)) * y(k,196) + (rxt(k,378) &
                      + rxt(k,379)) * y(k,203) + rxt(k,387)*y(k,205) + rxt(k,390) &
                      *y(k,207) + (rxt(k,400) + rxt(k,401)) * y(k,186) + rxt(k,410) &
                      *y(k,201) + rxt(k,416)*y(k,202) + rxt(k,424)*y(k,100) + rxt(k,435) &
                      *y(k,225) + rxt(k,439)*y(k,185) + rxt(k,442)*y(k,187) + rxt(k,447) &
                      *y(k,188) + rxt(k,449)*y(k,191) + rxt(k,453)*y(k,194) + rxt(k,456) &
                      *y(k,204) + rxt(k,459)*y(k,206) + rxt(k,462)*y(k,219) + rxt(k,469) &
                      *y(k,224) + rxt(k,475)*y(k,226) + rxt(k,478)*y(k,227) + rxt(k,489) &
                      *y(k,212) + rxt(k,494)*y(k,222) + rxt(k,499)*y(k,223) + rxt(k,578) &
                      *y(k,216))
         mat(k,748) = -rxt(k,192)*y(k,121)
         mat(k,1562) = -rxt(k,201)*y(k,121)
         mat(k,2078) = -rxt(k,205)*y(k,121)
         mat(k,1807) = -rxt(k,206)*y(k,121)
         mat(k,1892) = -rxt(k,207)*y(k,121)
         mat(k,1983) = -rxt(k,228)*y(k,121)
         mat(k,1851) = -rxt(k,260)*y(k,121)
         mat(k,1396) = -rxt(k,303)*y(k,121)
         mat(k,332) = -rxt(k,312)*y(k,121)
         mat(k,805) = -rxt(k,325)*y(k,121)
         mat(k,1303) = -rxt(k,336)*y(k,121)
         mat(k,677) = -rxt(k,340)*y(k,121)
         mat(k,734) = -rxt(k,353)*y(k,121)
         mat(k,722) = -rxt(k,361)*y(k,121)
         mat(k,1080) = -rxt(k,365)*y(k,121)
         mat(k,463) = -(rxt(k,371) + rxt(k,372)) * y(k,121)
         mat(k,1233) = -(rxt(k,378) + rxt(k,379)) * y(k,121)
         mat(k,1272) = -rxt(k,387)*y(k,121)
         mat(k,574) = -rxt(k,390)*y(k,121)
         mat(k,887) = -(rxt(k,400) + rxt(k,401)) * y(k,121)
         mat(k,1178) = -rxt(k,410)*y(k,121)
         mat(k,1211) = -rxt(k,416)*y(k,121)
         mat(k,1131) = -rxt(k,424)*y(k,121)
         mat(k,1109) = -rxt(k,435)*y(k,121)
         mat(k,409) = -rxt(k,439)*y(k,121)
         mat(k,381) = -rxt(k,442)*y(k,121)
         mat(k,326) = -rxt(k,447)*y(k,121)
         mat(k,531) = -rxt(k,449)*y(k,121)
         mat(k,650) = -rxt(k,453)*y(k,121)
         mat(k,611) = -rxt(k,456)*y(k,121)
         mat(k,815) = -rxt(k,459)*y(k,121)
         mat(k,339) = -rxt(k,462)*y(k,121)
         mat(k,625) = -rxt(k,469)*y(k,121)
         mat(k,642) = -rxt(k,475)*y(k,121)
         mat(k,396) = -rxt(k,478)*y(k,121)
         mat(k,1064) = -rxt(k,489)*y(k,121)
         mat(k,1045) = -rxt(k,494)*y(k,121)
         mat(k,1025) = -rxt(k,499)*y(k,121)
         mat(k,687) = -rxt(k,578)*y(k,121)
         mat(k,748) = mat(k,748) + 2.000_r8*rxt(k,194)*y(k,122) + rxt(k,196)*y(k,131) &
                      + rxt(k,204)*y(k,217)
         mat(k,1934) = 2.000_r8*rxt(k,194)*y(k,111) + rxt(k,197)*y(k,130) + rxt(k,511) &
                      *y(k,148)
         mat(k,1892) = mat(k,1892) + rxt(k,197)*y(k,122)
         mat(k,1426) = rxt(k,196)*y(k,111) + rxt(k,191)*y(k,208)
         mat(k,1319) = rxt(k,511)*y(k,122)
         mat(k,421) = rxt(k,191)*y(k,131)
         mat(k,1745) = rxt(k,204)*y(k,111)
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
         mat(k,1942) = -((rxt(k,193) + rxt(k,194) + rxt(k,195)) * y(k,111) + (rxt(k,197) &
                      + rxt(k,199)) * y(k,130) + rxt(k,198)*y(k,132) + rxt(k,210) &
                      *y(k,199) + rxt(k,211)*y(k,123) + rxt(k,212)*y(k,217) + rxt(k,230) &
                      *y(k,58) + rxt(k,261)*y(k,18) + rxt(k,347)*y(k,192) + rxt(k,396) &
                      *y(k,205) + rxt(k,454)*y(k,194) + rxt(k,457)*y(k,204) + rxt(k,460) &
                      *y(k,206) + rxt(k,464)*y(k,139) + rxt(k,467)*y(k,185) + rxt(k,511) &
                      *y(k,148))
         mat(k,752) = -(rxt(k,193) + rxt(k,194) + rxt(k,195)) * y(k,122)
         mat(k,1900) = -(rxt(k,197) + rxt(k,199)) * y(k,122)
         mat(k,1815) = -rxt(k,198)*y(k,122)
         mat(k,2086) = -rxt(k,210)*y(k,122)
         mat(k,1570) = -rxt(k,211)*y(k,122)
         mat(k,1753) = -rxt(k,212)*y(k,122)
         mat(k,1991) = -rxt(k,230)*y(k,122)
         mat(k,1859) = -rxt(k,261)*y(k,122)
         mat(k,1308) = -rxt(k,347)*y(k,122)
         mat(k,1277) = -rxt(k,396)*y(k,122)
         mat(k,652) = -rxt(k,454)*y(k,122)
         mat(k,612) = -rxt(k,457)*y(k,122)
         mat(k,817) = -rxt(k,460)*y(k,122)
         mat(k,361) = -rxt(k,464)*y(k,122)
         mat(k,411) = -rxt(k,467)*y(k,122)
         mat(k,1326) = -rxt(k,511)*y(k,122)
         mat(k,544) = rxt(k,398)*y(k,217)
         mat(k,266) = rxt(k,369)*y(k,123)
         mat(k,1859) = mat(k,1859) + rxt(k,260)*y(k,121)
         mat(k,1991) = mat(k,1991) + rxt(k,228)*y(k,121)
         mat(k,366) = rxt(k,189)*y(k,217)
         mat(k,511) = .700_r8*rxt(k,418)*y(k,217)
         mat(k,1135) = rxt(k,424)*y(k,121) + rxt(k,425)*y(k,123)
         mat(k,1513) = rxt(k,260)*y(k,18) + rxt(k,228)*y(k,58) + rxt(k,424)*y(k,100) &
                      + 2.000_r8*rxt(k,201)*y(k,123) + rxt(k,207)*y(k,130) &
                      + rxt(k,206)*y(k,132) + rxt(k,439)*y(k,185) + rxt(k,400) &
                      *y(k,186) + rxt(k,442)*y(k,187) + rxt(k,447)*y(k,188) &
                      + rxt(k,325)*y(k,189) + rxt(k,353)*y(k,190) + rxt(k,449) &
                      *y(k,191) + rxt(k,336)*y(k,192) + rxt(k,303)*y(k,193) &
                      + rxt(k,453)*y(k,194) + rxt(k,371)*y(k,196) + rxt(k,340) &
                      *y(k,198) + rxt(k,205)*y(k,199) + rxt(k,312)*y(k,200) &
                      + .920_r8*rxt(k,410)*y(k,201) + .920_r8*rxt(k,416)*y(k,202) &
                      + rxt(k,378)*y(k,203) + rxt(k,456)*y(k,204) + rxt(k,387) &
                      *y(k,205) + rxt(k,459)*y(k,206) + rxt(k,390)*y(k,207) &
                      + 1.600_r8*rxt(k,489)*y(k,212) + rxt(k,462)*y(k,219) &
                      + rxt(k,361)*y(k,220) + rxt(k,365)*y(k,221) + .900_r8*rxt(k,494) &
                      *y(k,222) + .800_r8*rxt(k,499)*y(k,223) + rxt(k,469)*y(k,224) &
                      + rxt(k,435)*y(k,225) + rxt(k,475)*y(k,226) + rxt(k,478) &
                      *y(k,227)
         mat(k,1570) = mat(k,1570) + rxt(k,369)*y(k,15) + rxt(k,425)*y(k,100) &
                      + 2.000_r8*rxt(k,201)*y(k,121) + rxt(k,202)*y(k,130) &
                      + rxt(k,200)*y(k,199) + rxt(k,411)*y(k,201) + rxt(k,417) &
                      *y(k,202) + rxt(k,377)*y(k,203) + rxt(k,388)*y(k,205) &
                      + 2.000_r8*rxt(k,490)*y(k,212) + rxt(k,203)*y(k,217) &
                      + rxt(k,436)*y(k,225)
         mat(k,774) = rxt(k,359)*y(k,217)
         mat(k,1900) = mat(k,1900) + rxt(k,207)*y(k,121) + rxt(k,202)*y(k,123)
         mat(k,1815) = mat(k,1815) + rxt(k,206)*y(k,121)
         mat(k,525) = rxt(k,496)*y(k,217)
         mat(k,411) = mat(k,411) + rxt(k,439)*y(k,121)
         mat(k,889) = rxt(k,400)*y(k,121)
         mat(k,383) = rxt(k,442)*y(k,121)
         mat(k,328) = rxt(k,447)*y(k,121)
         mat(k,807) = rxt(k,325)*y(k,121)
         mat(k,736) = rxt(k,353)*y(k,121)
         mat(k,534) = rxt(k,449)*y(k,121)
         mat(k,1308) = mat(k,1308) + rxt(k,336)*y(k,121)
         mat(k,1402) = rxt(k,303)*y(k,121) + .500_r8*rxt(k,487)*y(k,212)
         mat(k,652) = mat(k,652) + rxt(k,453)*y(k,121)
         mat(k,464) = rxt(k,371)*y(k,121)
         mat(k,679) = rxt(k,340)*y(k,121)
         mat(k,2086) = mat(k,2086) + rxt(k,205)*y(k,121) + rxt(k,200)*y(k,123)
         mat(k,333) = rxt(k,312)*y(k,121)
         mat(k,1183) = .920_r8*rxt(k,410)*y(k,121) + rxt(k,411)*y(k,123)
         mat(k,1216) = .920_r8*rxt(k,416)*y(k,121) + rxt(k,417)*y(k,123)
         mat(k,1237) = rxt(k,378)*y(k,121) + rxt(k,377)*y(k,123)
         mat(k,612) = mat(k,612) + rxt(k,456)*y(k,121)
         mat(k,1277) = mat(k,1277) + rxt(k,387)*y(k,121) + rxt(k,388)*y(k,123)
         mat(k,817) = mat(k,817) + rxt(k,459)*y(k,121)
         mat(k,576) = rxt(k,390)*y(k,121)
         mat(k,1068) = 1.600_r8*rxt(k,489)*y(k,121) + 2.000_r8*rxt(k,490)*y(k,123) &
                      + .500_r8*rxt(k,487)*y(k,193)
         mat(k,1753) = mat(k,1753) + rxt(k,398)*y(k,1) + rxt(k,189)*y(k,89) &
                      + .700_r8*rxt(k,418)*y(k,98) + rxt(k,203)*y(k,123) + rxt(k,359) &
                      *y(k,124) + rxt(k,496)*y(k,172)
         mat(k,341) = rxt(k,462)*y(k,121)
         mat(k,724) = rxt(k,361)*y(k,121)
         mat(k,1082) = rxt(k,365)*y(k,121)
         mat(k,1048) = .900_r8*rxt(k,494)*y(k,121)
         mat(k,1029) = .800_r8*rxt(k,499)*y(k,121)
         mat(k,627) = rxt(k,469)*y(k,121)
         mat(k,1113) = rxt(k,435)*y(k,121) + rxt(k,436)*y(k,123)
         mat(k,644) = rxt(k,475)*y(k,121)
         mat(k,398) = rxt(k,478)*y(k,121)
         mat(k,1563) = -(rxt(k,200)*y(k,199) + rxt(k,201)*y(k,121) + rxt(k,202) &
                      *y(k,130) + rxt(k,203)*y(k,217) + rxt(k,211)*y(k,122) + rxt(k,297) &
                      *y(k,41) + rxt(k,330)*y(k,44) + rxt(k,349)*y(k,28) + rxt(k,356) &
                      *y(k,48) + rxt(k,369)*y(k,15) + rxt(k,377)*y(k,203) + rxt(k,388) &
                      *y(k,205) + rxt(k,411)*y(k,201) + rxt(k,417)*y(k,202) + rxt(k,420) &
                      *y(k,97) + rxt(k,425)*y(k,100) + rxt(k,436)*y(k,225) + rxt(k,481) &
                      *y(k,5) + rxt(k,484)*y(k,109) + rxt(k,490)*y(k,212) + rxt(k,501) &
                      *y(k,174) + rxt(k,518)*y(k,66))
         mat(k,2079) = -rxt(k,200)*y(k,123)
         mat(k,1506) = -rxt(k,201)*y(k,123)
         mat(k,1893) = -rxt(k,202)*y(k,123)
         mat(k,1746) = -rxt(k,203)*y(k,123)
         mat(k,1935) = -rxt(k,211)*y(k,123)
         mat(k,2129) = -rxt(k,297)*y(k,123)
         mat(k,970) = -rxt(k,330)*y(k,123)
         mat(k,953) = -rxt(k,349)*y(k,123)
         mat(k,1155) = -rxt(k,356)*y(k,123)
         mat(k,264) = -rxt(k,369)*y(k,123)
         mat(k,1234) = -rxt(k,377)*y(k,123)
         mat(k,1273) = -rxt(k,388)*y(k,123)
         mat(k,1179) = -rxt(k,411)*y(k,123)
         mat(k,1212) = -rxt(k,417)*y(k,123)
         mat(k,792) = -rxt(k,420)*y(k,123)
         mat(k,1132) = -rxt(k,425)*y(k,123)
         mat(k,1110) = -rxt(k,436)*y(k,123)
         mat(k,870) = -rxt(k,481)*y(k,123)
         mat(k,844) = -rxt(k,484)*y(k,123)
         mat(k,1065) = -rxt(k,490)*y(k,123)
         mat(k,922) = -rxt(k,501)*y(k,123)
         mat(k,194) = -rxt(k,518)*y(k,123)
         mat(k,442) = rxt(k,262)*y(k,130)
         mat(k,1598) = rxt(k,229)*y(k,59)
         mat(k,909) = rxt(k,229)*y(k,55) + rxt(k,231)*y(k,130) + rxt(k,232)*y(k,217)
         mat(k,667) = rxt(k,276)*y(k,88)
         mat(k,1957) = rxt(k,276)*y(k,72) + rxt(k,213)*y(k,217)
         mat(k,470) = .500_r8*rxt(k,393)*y(k,217)
         mat(k,1935) = mat(k,1935) + rxt(k,199)*y(k,130) + rxt(k,198)*y(k,132)
         mat(k,1893) = mat(k,1893) + rxt(k,262)*y(k,19) + rxt(k,231)*y(k,59) &
                      + rxt(k,199)*y(k,122)
         mat(k,1808) = rxt(k,198)*y(k,122)
         mat(k,354) = rxt(k,345)*y(k,217)
         mat(k,1746) = mat(k,1746) + rxt(k,232)*y(k,59) + rxt(k,213)*y(k,88) &
                      + .500_r8*rxt(k,393)*y(k,108) + rxt(k,345)*y(k,137)
         mat(k,770) = -(rxt(k,359)*y(k,217))
         mat(k,1705) = -rxt(k,359)*y(k,124)
         mat(k,943) = rxt(k,349)*y(k,123)
         mat(k,424) = .500_r8*rxt(k,419)*y(k,217)
         mat(k,300) = rxt(k,426)*y(k,217)
         mat(k,269) = rxt(k,430)*y(k,217)
         mat(k,926) = rxt(k,431)*y(k,217)
         mat(k,1527) = rxt(k,349)*y(k,28)
         mat(k,1705) = mat(k,1705) + .500_r8*rxt(k,419)*y(k,99) + rxt(k,426)*y(k,101) &
                      + rxt(k,430)*y(k,114) + rxt(k,431)*y(k,115)
         mat(k,286) = -(rxt(k,491)*y(k,217))
         mat(k,1655) = -rxt(k,491)*y(k,125)
         mat(k,2007) = rxt(k,488)*y(k,212)
         mat(k,1053) = rxt(k,488)*y(k,199)
         mat(k,1899) = -(rxt(k,169)*y(k,132) + 4._r8*rxt(k,170)*y(k,130) + rxt(k,171) &
                      *y(k,131) + rxt(k,172)*y(k,76) + rxt(k,173)*y(k,78) + rxt(k,178) &
                      *y(k,199) + rxt(k,184)*y(k,217) + (rxt(k,197) + rxt(k,199) &
                      ) * y(k,122) + rxt(k,202)*y(k,123) + rxt(k,207)*y(k,121) &
                      + rxt(k,231)*y(k,59) + rxt(k,233)*y(k,58) + rxt(k,236)*y(k,84) &
                      + rxt(k,239)*y(k,91) + rxt(k,262)*y(k,19) + rxt(k,263)*y(k,18) &
                      + rxt(k,265)*y(k,80) + rxt(k,267)*y(k,90) + rxt(k,298)*y(k,41) &
                      + rxt(k,504)*y(k,135) + (rxt(k,571) + rxt(k,572)) * y(k,209) &
                      + rxt(k,573)*y(k,211))
         mat(k,1814) = -rxt(k,169)*y(k,130)
         mat(k,1432) = -rxt(k,171)*y(k,130)
         mat(k,1095) = -rxt(k,172)*y(k,130)
         mat(k,479) = -rxt(k,173)*y(k,130)
         mat(k,2085) = -rxt(k,178)*y(k,130)
         mat(k,1752) = -rxt(k,184)*y(k,130)
         mat(k,1941) = -(rxt(k,197) + rxt(k,199)) * y(k,130)
         mat(k,1569) = -rxt(k,202)*y(k,130)
         mat(k,1512) = -rxt(k,207)*y(k,130)
         mat(k,912) = -rxt(k,231)*y(k,130)
         mat(k,1990) = -rxt(k,233)*y(k,130)
         mat(k,1342) = -rxt(k,236)*y(k,130)
         mat(k,767) = -rxt(k,239)*y(k,130)
         mat(k,444) = -rxt(k,262)*y(k,130)
         mat(k,1858) = -rxt(k,263)*y(k,130)
         mat(k,759) = -rxt(k,265)*y(k,130)
         mat(k,661) = -rxt(k,267)*y(k,130)
         mat(k,2135) = -rxt(k,298)*y(k,130)
         mat(k,259) = -rxt(k,504)*y(k,130)
         mat(k,557) = -(rxt(k,571) + rxt(k,572)) * y(k,130)
         mat(k,391) = -rxt(k,573)*y(k,130)
         mat(k,1834) = rxt(k,176)*y(k,199)
         mat(k,751) = rxt(k,192)*y(k,121) + rxt(k,193)*y(k,122) + rxt(k,196)*y(k,131) &
                      + rxt(k,576)*y(k,216)
         mat(k,1512) = mat(k,1512) + rxt(k,192)*y(k,111)
         mat(k,1941) = mat(k,1941) + rxt(k,193)*y(k,111)
         mat(k,1432) = mat(k,1432) + rxt(k,196)*y(k,111) + rxt(k,506)*y(k,146) &
                      + rxt(k,512)*y(k,148) + rxt(k,575)*y(k,211) + (rxt(k,158) &
                       +rxt(k,159))*y(k,213) + rxt(k,581)*y(k,218)
         mat(k,607) = rxt(k,506)*y(k,131)
         mat(k,1325) = rxt(k,512)*y(k,131)
         mat(k,707) = rxt(k,567)*y(k,210) + 1.150_r8*rxt(k,568)*y(k,216)
         mat(k,2085) = mat(k,2085) + rxt(k,176)*y(k,75)
         mat(k,696) = rxt(k,567)*y(k,195)
         mat(k,391) = mat(k,391) + rxt(k,575)*y(k,131)
         mat(k,2111) = (rxt(k,158)+rxt(k,159))*y(k,131)
         mat(k,688) = rxt(k,576)*y(k,111) + 1.150_r8*rxt(k,568)*y(k,195)
         mat(k,1752) = mat(k,1752) + 2.000_r8*rxt(k,186)*y(k,217)
         mat(k,521) = rxt(k,581)*y(k,131)
         mat(k,1425) = -(rxt(k,158)*y(k,213) + rxt(k,163)*y(k,214) + rxt(k,171) &
                      *y(k,130) + rxt(k,177)*y(k,75) + rxt(k,191)*y(k,208) + rxt(k,196) &
                      *y(k,111) + rxt(k,342)*y(k,197) + rxt(k,506)*y(k,146) + rxt(k,512) &
                      *y(k,148) + rxt(k,570)*y(k,209) + (rxt(k,574) + rxt(k,575) &
                      ) * y(k,211) + rxt(k,581)*y(k,218))
         mat(k,2103) = -rxt(k,158)*y(k,131)
         mat(k,76) = -rxt(k,163)*y(k,131)
         mat(k,1891) = -rxt(k,171)*y(k,131)
         mat(k,1826) = -rxt(k,177)*y(k,131)
         mat(k,420) = -rxt(k,191)*y(k,131)
         mat(k,747) = -rxt(k,196)*y(k,131)
         mat(k,348) = -rxt(k,342)*y(k,131)
         mat(k,603) = -rxt(k,506)*y(k,131)
         mat(k,1318) = -rxt(k,512)*y(k,131)
         mat(k,555) = -rxt(k,570)*y(k,131)
         mat(k,390) = -(rxt(k,574) + rxt(k,575)) * y(k,131)
         mat(k,520) = -rxt(k,581)*y(k,131)
         mat(k,1351) = rxt(k,254)*y(k,132) + rxt(k,253)*y(k,199)
         mat(k,1850) = 2.000_r8*rxt(k,255)*y(k,18) + (rxt(k,257)+rxt(k,258))*y(k,58) &
                      + rxt(k,263)*y(k,130) + rxt(k,259)*y(k,199)
         mat(k,1596) = rxt(k,222)*y(k,132) + rxt(k,220)*y(k,199)
         mat(k,1982) = (rxt(k,257)+rxt(k,258))*y(k,18) + (2.000_r8*rxt(k,224) &
                       +2.000_r8*rxt(k,225))*y(k,58) + rxt(k,233)*y(k,130) &
                      + rxt(k,227)*y(k,199) + rxt(k,235)*y(k,217)
         mat(k,1826) = mat(k,1826) + rxt(k,180)*y(k,132) + rxt(k,174)*y(k,199)
         mat(k,363) = rxt(k,189)*y(k,217)
         mat(k,747) = mat(k,747) + rxt(k,195)*y(k,122)
         mat(k,1504) = rxt(k,206)*y(k,132) + rxt(k,578)*y(k,216)
         mat(k,1933) = rxt(k,195)*y(k,111) + rxt(k,197)*y(k,130) + rxt(k,198)*y(k,132)
         mat(k,1561) = rxt(k,202)*y(k,130) + rxt(k,200)*y(k,199)
         mat(k,1891) = mat(k,1891) + rxt(k,263)*y(k,18) + rxt(k,233)*y(k,58) &
                      + rxt(k,197)*y(k,122) + rxt(k,202)*y(k,123) &
                      + 2.000_r8*rxt(k,170)*y(k,130) + 2.000_r8*rxt(k,169)*y(k,132) &
                      + rxt(k,178)*y(k,199) + rxt(k,162)*y(k,214) + rxt(k,184) &
                      *y(k,217)
         mat(k,1425) = mat(k,1425) + 2.000_r8*rxt(k,163)*y(k,214)
         mat(k,1806) = rxt(k,254)*y(k,16) + rxt(k,222)*y(k,55) + rxt(k,180)*y(k,75) &
                      + rxt(k,206)*y(k,121) + rxt(k,198)*y(k,122) &
                      + 2.000_r8*rxt(k,169)*y(k,130) + rxt(k,507)*y(k,146) &
                      + rxt(k,513)*y(k,148) + 2.000_r8*rxt(k,179)*y(k,199) &
                      + 2.000_r8*rxt(k,160)*y(k,213) + rxt(k,185)*y(k,217)
         mat(k,603) = mat(k,603) + rxt(k,507)*y(k,132)
         mat(k,1318) = mat(k,1318) + rxt(k,513)*y(k,132)
         mat(k,804) = rxt(k,324)*y(k,199)
         mat(k,733) = rxt(k,352)*y(k,199)
         mat(k,1395) = rxt(k,302)*y(k,199)
         mat(k,2077) = rxt(k,253)*y(k,16) + rxt(k,259)*y(k,18) + rxt(k,220)*y(k,55) &
                      + rxt(k,227)*y(k,58) + rxt(k,174)*y(k,75) + rxt(k,200)*y(k,123) &
                      + rxt(k,178)*y(k,130) + 2.000_r8*rxt(k,179)*y(k,132) &
                      + rxt(k,324)*y(k,189) + rxt(k,352)*y(k,190) + rxt(k,302) &
                      *y(k,193) + 2.000_r8*rxt(k,188)*y(k,199) + rxt(k,183)*y(k,217) &
                      + rxt(k,360)*y(k,220)
         mat(k,2103) = mat(k,2103) + 2.000_r8*rxt(k,160)*y(k,132)
         mat(k,76) = mat(k,76) + rxt(k,162)*y(k,130) + 2.000_r8*rxt(k,163)*y(k,131)
         mat(k,686) = rxt(k,578)*y(k,121)
         mat(k,1744) = rxt(k,235)*y(k,58) + rxt(k,189)*y(k,89) + rxt(k,184)*y(k,130) &
                      + rxt(k,185)*y(k,132) + rxt(k,183)*y(k,199)
         mat(k,721) = rxt(k,360)*y(k,199)
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
         mat(k,1811) = -(rxt(k,160)*y(k,213) + rxt(k,169)*y(k,130) + rxt(k,179) &
                      *y(k,199) + rxt(k,180)*y(k,75) + rxt(k,185)*y(k,217) + rxt(k,198) &
                      *y(k,122) + rxt(k,206)*y(k,121) + rxt(k,222)*y(k,55) + rxt(k,254) &
                      *y(k,16) + rxt(k,321)*y(k,24) + rxt(k,350)*y(k,28) + rxt(k,380) &
                      *y(k,104) + rxt(k,394)*y(k,110) + rxt(k,427)*y(k,97) + rxt(k,465) &
                      *y(k,139) + rxt(k,482)*y(k,5) + rxt(k,485)*y(k,109) + rxt(k,507) &
                      *y(k,146) + rxt(k,513)*y(k,148))
         mat(k,2108) = -rxt(k,160)*y(k,132)
         mat(k,1896) = -rxt(k,169)*y(k,132)
         mat(k,2082) = -rxt(k,179)*y(k,132)
         mat(k,1831) = -rxt(k,180)*y(k,132)
         mat(k,1749) = -rxt(k,185)*y(k,132)
         mat(k,1938) = -rxt(k,198)*y(k,132)
         mat(k,1509) = -rxt(k,206)*y(k,132)
         mat(k,1601) = -rxt(k,222)*y(k,132)
         mat(k,1353) = -rxt(k,254)*y(k,132)
         mat(k,452) = -rxt(k,321)*y(k,132)
         mat(k,955) = -rxt(k,350)*y(k,132)
         mat(k,1147) = -rxt(k,380)*y(k,132)
         mat(k,1257) = -rxt(k,394)*y(k,132)
         mat(k,794) = -rxt(k,427)*y(k,132)
         mat(k,360) = -rxt(k,465)*y(k,132)
         mat(k,872) = -rxt(k,482)*y(k,132)
         mat(k,846) = -rxt(k,485)*y(k,132)
         mat(k,605) = -rxt(k,507)*y(k,132)
         mat(k,1322) = -rxt(k,513)*y(k,132)
         mat(k,1896) = mat(k,1896) + rxt(k,171)*y(k,131)
         mat(k,1429) = rxt(k,171)*y(k,130)
         mat(k,1306) = .150_r8*rxt(k,335)*y(k,199)
         mat(k,2082) = mat(k,2082) + .150_r8*rxt(k,335)*y(k,192) + .150_r8*rxt(k,385) &
                      *y(k,205)
         mat(k,1275) = .150_r8*rxt(k,385)*y(k,199)
         mat(k,231) = -(rxt(k,514)*y(k,148))
         mat(k,1313) = -rxt(k,514)*y(k,134)
         mat(k,1843) = rxt(k,256)*y(k,58)
         mat(k,1974) = rxt(k,256)*y(k,18) + 2.000_r8*rxt(k,226)*y(k,58)
         mat(k,252) = -(rxt(k,504)*y(k,130) + rxt(k,505)*y(k,217))
         mat(k,1868) = -rxt(k,504)*y(k,135)
         mat(k,1651) = -rxt(k,505)*y(k,135)
         mat(k,985) = rxt(k,373)*y(k,217)
         mat(k,1439) = .100_r8*rxt(k,494)*y(k,222)
         mat(k,1637) = rxt(k,373)*y(k,92)
         mat(k,1034) = .100_r8*rxt(k,494)*y(k,121)
         mat(k,351) = -(rxt(k,345)*y(k,217))
         mat(k,1664) = -rxt(k,345)*y(k,137)
         mat(k,1909) = rxt(k,347)*y(k,192)
         mat(k,1283) = rxt(k,347)*y(k,122)
         mat(k,1907) = rxt(k,467)*y(k,185)
         mat(k,406) = rxt(k,467)*y(k,122)
         mat(k,358) = -(rxt(k,464)*y(k,122) + rxt(k,465)*y(k,132))
         mat(k,1910) = -rxt(k,464)*y(k,139)
         mat(k,1769) = -rxt(k,465)*y(k,139)
         mat(k,120) = .070_r8*rxt(k,451)*y(k,217)
         mat(k,1449) = rxt(k,449)*y(k,191)
         mat(k,97) = .060_r8*rxt(k,463)*y(k,217)
         mat(k,149) = .070_r8*rxt(k,479)*y(k,217)
         mat(k,529) = rxt(k,449)*y(k,121)
         mat(k,1665) = .070_r8*rxt(k,451)*y(k,65) + .060_r8*rxt(k,463)*y(k,140) &
                      + .070_r8*rxt(k,479)*y(k,181)
         mat(k,95) = -(rxt(k,463)*y(k,217))
         mat(k,1626) = -rxt(k,463)*y(k,140)
         mat(k,87) = .530_r8*rxt(k,440)*y(k,217)
         mat(k,1626) = mat(k,1626) + .530_r8*rxt(k,440)*y(k,6)
         mat(k,236) = -(rxt(k,466)*y(k,217))
         mat(k,1648) = -rxt(k,466)*y(k,141)
         mat(k,2003) = rxt(k,461)*y(k,219)
         mat(k,336) = rxt(k,461)*y(k,199)
         mat(k,431) = -(rxt(k,362)*y(k,217))
         mat(k,1674) = -rxt(k,362)*y(k,144)
         mat(k,2025) = rxt(k,360)*y(k,220)
         mat(k,717) = rxt(k,360)*y(k,199)
         mat(k,292) = -(rxt(k,366)*y(k,217))
         mat(k,1656) = -rxt(k,366)*y(k,145)
         mat(k,2008) = .850_r8*rxt(k,364)*y(k,221)
         mat(k,1073) = .850_r8*rxt(k,364)*y(k,199)
         mat(k,601) = -(rxt(k,506)*y(k,131) + rxt(k,507)*y(k,132) + rxt(k,510) &
                      *y(k,217))
         mat(k,1415) = -rxt(k,506)*y(k,146)
         mat(k,1773) = -rxt(k,507)*y(k,146)
         mat(k,1691) = -rxt(k,510)*y(k,146)
         mat(k,1316) = -(rxt(k,508)*y(k,18) + rxt(k,509)*y(k,58) + rxt(k,511)*y(k,122) &
                      + rxt(k,512)*y(k,131) + rxt(k,513)*y(k,132) + rxt(k,514) &
                      *y(k,134) + rxt(k,515)*y(k,217))
         mat(k,1847) = -rxt(k,508)*y(k,148)
         mat(k,1978) = -rxt(k,509)*y(k,148)
         mat(k,1929) = -rxt(k,511)*y(k,148)
         mat(k,1423) = -rxt(k,512)*y(k,148)
         mat(k,1803) = -rxt(k,513)*y(k,148)
         mat(k,233) = -rxt(k,514)*y(k,148)
         mat(k,1740) = -rxt(k,515)*y(k,148)
         mat(k,1887) = rxt(k,504)*y(k,135)
         mat(k,1423) = mat(k,1423) + rxt(k,506)*y(k,146)
         mat(k,1803) = mat(k,1803) + rxt(k,507)*y(k,146)
         mat(k,256) = rxt(k,504)*y(k,130)
         mat(k,602) = rxt(k,506)*y(k,131) + rxt(k,507)*y(k,132) + rxt(k,510)*y(k,217)
         mat(k,1740) = mat(k,1740) + rxt(k,510)*y(k,146)
         mat(k,899) = -(rxt(k,516)*y(k,217))
         mat(k,1714) = -rxt(k,516)*y(k,149)
         mat(k,1846) = rxt(k,508)*y(k,148)
         mat(k,1976) = rxt(k,509)*y(k,148)
         mat(k,193) = rxt(k,518)*y(k,123) + (rxt(k,519)+.500_r8*rxt(k,521))*y(k,217)
         mat(k,1922) = rxt(k,511)*y(k,148)
         mat(k,1534) = rxt(k,518)*y(k,66)
         mat(k,1420) = rxt(k,512)*y(k,148)
         mat(k,1782) = rxt(k,513)*y(k,148)
         mat(k,232) = rxt(k,514)*y(k,148)
         mat(k,254) = rxt(k,505)*y(k,217)
         mat(k,1315) = rxt(k,508)*y(k,18) + rxt(k,509)*y(k,58) + rxt(k,511)*y(k,122) &
                      + rxt(k,512)*y(k,131) + rxt(k,513)*y(k,132) + rxt(k,514) &
                      *y(k,134) + rxt(k,515)*y(k,217)
         mat(k,1714) = mat(k,1714) + (rxt(k,519)+.500_r8*rxt(k,521))*y(k,66) &
                      + rxt(k,505)*y(k,135) + rxt(k,515)*y(k,148)
         mat(k,174) = -(rxt(k,517)*y(k,228))
         mat(k,2145) = -rxt(k,517)*y(k,150)
         mat(k,898) = rxt(k,516)*y(k,217)
         mat(k,1639) = rxt(k,516)*y(k,149)
         mat(k,850) = .2202005_r8*rxt(k,537)*y(k,132) + .2202005_r8*rxt(k,538) &
                      *y(k,217)
         mat(k,80) = .0023005_r8*rxt(k,539)*y(k,217)
         mat(k,776) = .0031005_r8*rxt(k,542)*y(k,217)
         mat(k,35) = .2381005_r8*rxt(k,543)*y(k,217)
         mat(k,824) = .0508005_r8*rxt(k,545)*y(k,132) + .0508005_r8*rxt(k,546) &
                      *y(k,217)
         mat(k,1760) = .2202005_r8*rxt(k,537)*y(k,5) + .0508005_r8*rxt(k,545)*y(k,109)
         mat(k,41) = .5931005_r8*rxt(k,547)*y(k,217)
         mat(k,106) = .1364005_r8*rxt(k,548)*y(k,217)
         mat(k,134) = .1677005_r8*rxt(k,549)*y(k,217)
         mat(k,1612) = .2202005_r8*rxt(k,538)*y(k,5) + .0023005_r8*rxt(k,539)*y(k,6) &
                      + .0031005_r8*rxt(k,542)*y(k,97) + .2381005_r8*rxt(k,543) &
                      *y(k,103) + .0508005_r8*rxt(k,546)*y(k,109) &
                      + .5931005_r8*rxt(k,547)*y(k,169) + .1364005_r8*rxt(k,548) &
                      *y(k,177) + .1677005_r8*rxt(k,549)*y(k,179)
         mat(k,851) = .2067005_r8*rxt(k,537)*y(k,132) + .2067005_r8*rxt(k,538) &
                      *y(k,217)
         mat(k,81) = .0008005_r8*rxt(k,539)*y(k,217)
         mat(k,777) = .0035005_r8*rxt(k,542)*y(k,217)
         mat(k,36) = .1308005_r8*rxt(k,543)*y(k,217)
         mat(k,825) = .1149005_r8*rxt(k,545)*y(k,132) + .1149005_r8*rxt(k,546) &
                      *y(k,217)
         mat(k,1761) = .2067005_r8*rxt(k,537)*y(k,5) + .1149005_r8*rxt(k,545)*y(k,109)
         mat(k,42) = .1534005_r8*rxt(k,547)*y(k,217)
         mat(k,107) = .0101005_r8*rxt(k,548)*y(k,217)
         mat(k,135) = .0174005_r8*rxt(k,549)*y(k,217)
         mat(k,1613) = .2067005_r8*rxt(k,538)*y(k,5) + .0008005_r8*rxt(k,539)*y(k,6) &
                      + .0035005_r8*rxt(k,542)*y(k,97) + .1308005_r8*rxt(k,543) &
                      *y(k,103) + .1149005_r8*rxt(k,546)*y(k,109) &
                      + .1534005_r8*rxt(k,547)*y(k,169) + .0101005_r8*rxt(k,548) &
                      *y(k,177) + .0174005_r8*rxt(k,549)*y(k,179)
         mat(k,852) = .0653005_r8*rxt(k,537)*y(k,132) + .0653005_r8*rxt(k,538) &
                      *y(k,217)
         mat(k,82) = .0843005_r8*rxt(k,539)*y(k,217)
         mat(k,778) = .0003005_r8*rxt(k,542)*y(k,217)
         mat(k,37) = .0348005_r8*rxt(k,543)*y(k,217)
         mat(k,826) = .0348005_r8*rxt(k,545)*y(k,132) + .0348005_r8*rxt(k,546) &
                      *y(k,217)
         mat(k,1762) = .0653005_r8*rxt(k,537)*y(k,5) + .0348005_r8*rxt(k,545)*y(k,109)
         mat(k,43) = .0459005_r8*rxt(k,547)*y(k,217)
         mat(k,108) = .0763005_r8*rxt(k,548)*y(k,217)
         mat(k,136) = .086_r8*rxt(k,549)*y(k,217)
         mat(k,1614) = .0653005_r8*rxt(k,538)*y(k,5) + .0843005_r8*rxt(k,539)*y(k,6) &
                      + .0003005_r8*rxt(k,542)*y(k,97) + .0348005_r8*rxt(k,543) &
                      *y(k,103) + .0348005_r8*rxt(k,546)*y(k,109) &
                      + .0459005_r8*rxt(k,547)*y(k,169) + .0763005_r8*rxt(k,548) &
                      *y(k,177) + .086_r8*rxt(k,549)*y(k,179)
         mat(k,853) = .1749305_r8*rxt(k,536)*y(k,123) + .1284005_r8*rxt(k,537) &
                      *y(k,132) + .1284005_r8*rxt(k,538)*y(k,217)
         mat(k,83) = .0443005_r8*rxt(k,539)*y(k,217)
         mat(k,779) = .0590245_r8*rxt(k,540)*y(k,123) + .0033005_r8*rxt(k,541) &
                      *y(k,132) + .0271005_r8*rxt(k,542)*y(k,217)
         mat(k,38) = .0076005_r8*rxt(k,543)*y(k,217)
         mat(k,827) = .1749305_r8*rxt(k,544)*y(k,123) + .0554005_r8*rxt(k,545) &
                      *y(k,132) + .0554005_r8*rxt(k,546)*y(k,217)
         mat(k,1520) = .1749305_r8*rxt(k,536)*y(k,5) + .0590245_r8*rxt(k,540)*y(k,97) &
                      + .1749305_r8*rxt(k,544)*y(k,109)
         mat(k,1763) = .1284005_r8*rxt(k,537)*y(k,5) + .0033005_r8*rxt(k,541)*y(k,97) &
                      + .0554005_r8*rxt(k,545)*y(k,109)
         mat(k,44) = .0085005_r8*rxt(k,547)*y(k,217)
         mat(k,109) = .2157005_r8*rxt(k,548)*y(k,217)
         mat(k,137) = .0512005_r8*rxt(k,549)*y(k,217)
         mat(k,1615) = .1284005_r8*rxt(k,538)*y(k,5) + .0443005_r8*rxt(k,539)*y(k,6) &
                      + .0271005_r8*rxt(k,542)*y(k,97) + .0076005_r8*rxt(k,543) &
                      *y(k,103) + .0554005_r8*rxt(k,546)*y(k,109) &
                      + .0085005_r8*rxt(k,547)*y(k,169) + .2157005_r8*rxt(k,548) &
                      *y(k,177) + .0512005_r8*rxt(k,549)*y(k,179)
         mat(k,854) = .5901905_r8*rxt(k,536)*y(k,123) + .114_r8*rxt(k,537)*y(k,132) &
                      + .114_r8*rxt(k,538)*y(k,217)
         mat(k,84) = .1621005_r8*rxt(k,539)*y(k,217)
         mat(k,780) = .0250245_r8*rxt(k,540)*y(k,123) + .0474005_r8*rxt(k,542) &
                      *y(k,217)
         mat(k,39) = .0113005_r8*rxt(k,543)*y(k,217)
         mat(k,828) = .5901905_r8*rxt(k,544)*y(k,123) + .1278005_r8*rxt(k,545) &
                      *y(k,132) + .1278005_r8*rxt(k,546)*y(k,217)
         mat(k,1521) = .5901905_r8*rxt(k,536)*y(k,5) + .0250245_r8*rxt(k,540)*y(k,97) &
                      + .5901905_r8*rxt(k,544)*y(k,109)
         mat(k,1764) = .114_r8*rxt(k,537)*y(k,5) + .1278005_r8*rxt(k,545)*y(k,109)
         mat(k,45) = .0128005_r8*rxt(k,547)*y(k,217)
         mat(k,110) = .0232005_r8*rxt(k,548)*y(k,217)
         mat(k,138) = .1598005_r8*rxt(k,549)*y(k,217)
         mat(k,1616) = .114_r8*rxt(k,538)*y(k,5) + .1621005_r8*rxt(k,539)*y(k,6) &
                      + .0474005_r8*rxt(k,542)*y(k,97) + .0113005_r8*rxt(k,543) &
                      *y(k,103) + .1278005_r8*rxt(k,546)*y(k,109) &
                      + .0128005_r8*rxt(k,547)*y(k,169) + .0232005_r8*rxt(k,548) &
                      *y(k,177) + .1598005_r8*rxt(k,549)*y(k,179)
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
         mat(k,46) = -(rxt(k,547)*y(k,217))
         mat(k,1618) = -rxt(k,547)*y(k,169)
         mat(k,113) = .100_r8*rxt(k,471)*y(k,217)
         mat(k,139) = .230_r8*rxt(k,473)*y(k,217)
         mat(k,1630) = .100_r8*rxt(k,471)*y(k,177) + .230_r8*rxt(k,473)*y(k,179)
         mat(k,482) = -(rxt(k,495)*y(k,217))
         mat(k,1680) = -rxt(k,495)*y(k,171)
         mat(k,2028) = rxt(k,493)*y(k,222)
         mat(k,1035) = rxt(k,493)*y(k,199)
         mat(k,522) = -(rxt(k,496)*y(k,217))
         mat(k,1684) = -rxt(k,496)*y(k,172)
         mat(k,1458) = .200_r8*rxt(k,489)*y(k,212) + .200_r8*rxt(k,499)*y(k,223)
         mat(k,1366) = .500_r8*rxt(k,487)*y(k,212)
         mat(k,1054) = .200_r8*rxt(k,489)*y(k,121) + .500_r8*rxt(k,487)*y(k,193)
         mat(k,1013) = .200_r8*rxt(k,499)*y(k,121)
         mat(k,369) = -(rxt(k,500)*y(k,217))
         mat(k,1667) = -rxt(k,500)*y(k,173)
         mat(k,2020) = rxt(k,498)*y(k,223)
         mat(k,1012) = rxt(k,498)*y(k,199)
         mat(k,917) = -(rxt(k,501)*y(k,123) + rxt(k,502)*y(k,217))
         mat(k,1535) = -rxt(k,501)*y(k,174)
         mat(k,1716) = -rxt(k,502)*y(k,174)
         mat(k,862) = .330_r8*rxt(k,482)*y(k,132)
         mat(k,836) = .330_r8*rxt(k,485)*y(k,132)
         mat(k,1480) = .800_r8*rxt(k,489)*y(k,212) + .800_r8*rxt(k,499)*y(k,223)
         mat(k,1535) = mat(k,1535) + rxt(k,490)*y(k,212)
         mat(k,1783) = .330_r8*rxt(k,482)*y(k,5) + .330_r8*rxt(k,485)*y(k,109)
         mat(k,523) = rxt(k,496)*y(k,217)
         mat(k,1373) = .500_r8*rxt(k,487)*y(k,212) + rxt(k,497)*y(k,223)
         mat(k,1056) = .800_r8*rxt(k,489)*y(k,121) + rxt(k,490)*y(k,123) &
                      + .500_r8*rxt(k,487)*y(k,193)
         mat(k,1716) = mat(k,1716) + rxt(k,496)*y(k,172)
         mat(k,1016) = .800_r8*rxt(k,499)*y(k,121) + rxt(k,497)*y(k,193)
         mat(k,976) = -(rxt(k,503)*y(k,217))
         mat(k,1721) = -rxt(k,503)*y(k,175)
         mat(k,863) = .300_r8*rxt(k,482)*y(k,132)
         mat(k,837) = .300_r8*rxt(k,485)*y(k,132)
         mat(k,1484) = .900_r8*rxt(k,494)*y(k,222)
         mat(k,1787) = .300_r8*rxt(k,482)*y(k,5) + .300_r8*rxt(k,485)*y(k,109)
         mat(k,1377) = rxt(k,492)*y(k,222)
         mat(k,1039) = .900_r8*rxt(k,494)*y(k,121) + rxt(k,492)*y(k,193)
         mat(k,493) = -(rxt(k,470)*y(k,217))
         mat(k,1681) = -rxt(k,470)*y(k,176)
         mat(k,2029) = rxt(k,468)*y(k,224)
         mat(k,616) = rxt(k,468)*y(k,199)
         mat(k,111) = -(rxt(k,471)*y(k,217))
         mat(k,1628) = -rxt(k,471)*y(k,177)
         mat(k,127) = -(rxt(k,437)*y(k,217))
         mat(k,1631) = -rxt(k,437)*y(k,178)
         mat(k,1999) = rxt(k,434)*y(k,225)
         mat(k,1099) = rxt(k,434)*y(k,199)
         mat(k,140) = -(rxt(k,473)*y(k,217))
         mat(k,1633) = -rxt(k,473)*y(k,179)
         mat(k,590) = -(rxt(k,476)*y(k,217))
         mat(k,1690) = -rxt(k,476)*y(k,180)
         mat(k,2035) = rxt(k,474)*y(k,226)
         mat(k,633) = rxt(k,474)*y(k,199)
         mat(k,148) = -(rxt(k,479)*y(k,217))
         mat(k,1634) = -rxt(k,479)*y(k,181)
         mat(k,141) = .150_r8*rxt(k,473)*y(k,217)
         mat(k,1634) = mat(k,1634) + .150_r8*rxt(k,473)*y(k,179)
         mat(k,316) = -(rxt(k,480)*y(k,217))
         mat(k,1660) = -rxt(k,480)*y(k,182)
         mat(k,2012) = rxt(k,477)*y(k,227)
         mat(k,392) = rxt(k,477)*y(k,199)
         mat(k,407) = -(rxt(k,438)*y(k,199) + rxt(k,439)*y(k,121) + rxt(k,467) &
                      *y(k,122))
         mat(k,2024) = -rxt(k,438)*y(k,185)
         mat(k,1452) = -rxt(k,439)*y(k,185)
         mat(k,1912) = -rxt(k,467)*y(k,185)
         mat(k,171) = rxt(k,444)*y(k,217)
         mat(k,1671) = rxt(k,444)*y(k,21)
         mat(k,881) = -(rxt(k,399)*y(k,199) + (rxt(k,400) + rxt(k,401)) * y(k,121))
         mat(k,2051) = -rxt(k,399)*y(k,186)
         mat(k,1478) = -(rxt(k,400) + rxt(k,401)) * y(k,186)
         mat(k,563) = rxt(k,402)*y(k,217)
         mat(k,162) = rxt(k,403)*y(k,217)
         mat(k,1712) = rxt(k,402)*y(k,2) + rxt(k,403)*y(k,14)
         mat(k,378) = -(rxt(k,441)*y(k,199) + rxt(k,442)*y(k,121))
         mat(k,2021) = -rxt(k,441)*y(k,187)
         mat(k,1450) = -rxt(k,442)*y(k,187)
         mat(k,88) = .350_r8*rxt(k,440)*y(k,217)
         mat(k,276) = rxt(k,443)*y(k,217)
         mat(k,1668) = .350_r8*rxt(k,440)*y(k,6) + rxt(k,443)*y(k,7)
         mat(k,324) = -(rxt(k,445)*y(k,199) + rxt(k,447)*y(k,121))
         mat(k,2013) = -rxt(k,445)*y(k,188)
         mat(k,1444) = -rxt(k,447)*y(k,188)
         mat(k,243) = rxt(k,446)*y(k,217)
         mat(k,114) = .070_r8*rxt(k,471)*y(k,217)
         mat(k,142) = .060_r8*rxt(k,473)*y(k,217)
         mat(k,1661) = rxt(k,446)*y(k,22) + .070_r8*rxt(k,471)*y(k,177) &
                      + .060_r8*rxt(k,473)*y(k,179)
         mat(k,801) = -(4._r8*rxt(k,322)*y(k,189) + rxt(k,323)*y(k,193) + rxt(k,324) &
                      *y(k,199) + rxt(k,325)*y(k,121))
         mat(k,1370) = -rxt(k,323)*y(k,189)
         mat(k,2048) = -rxt(k,324)*y(k,189)
         mat(k,1475) = -rxt(k,325)*y(k,189)
         mat(k,248) = .500_r8*rxt(k,327)*y(k,217)
         mat(k,208) = rxt(k,328)*y(k,55) + rxt(k,329)*y(k,217)
         mat(k,1585) = rxt(k,328)*y(k,27)
         mat(k,1707) = .500_r8*rxt(k,327)*y(k,26) + rxt(k,329)*y(k,27)
         mat(k,729) = -(rxt(k,351)*y(k,193) + rxt(k,352)*y(k,199) + rxt(k,353) &
                      *y(k,121))
         mat(k,1368) = -rxt(k,351)*y(k,190)
         mat(k,2044) = -rxt(k,352)*y(k,190)
         mat(k,1472) = -rxt(k,353)*y(k,190)
         mat(k,305) = rxt(k,354)*y(k,217)
         mat(k,57) = rxt(k,355)*y(k,217)
         mat(k,1701) = rxt(k,354)*y(k,29) + rxt(k,355)*y(k,30)
         mat(k,530) = -(rxt(k,448)*y(k,199) + rxt(k,449)*y(k,121))
         mat(k,2031) = -rxt(k,448)*y(k,191)
         mat(k,1459) = -rxt(k,449)*y(k,191)
         mat(k,184) = rxt(k,450)*y(k,217)
         mat(k,1459) = mat(k,1459) + rxt(k,439)*y(k,185)
         mat(k,1772) = rxt(k,465)*y(k,139)
         mat(k,359) = rxt(k,465)*y(k,132)
         mat(k,408) = rxt(k,439)*y(k,121) + .400_r8*rxt(k,438)*y(k,199)
         mat(k,2031) = mat(k,2031) + .400_r8*rxt(k,438)*y(k,185)
         mat(k,1685) = rxt(k,450)*y(k,31)
         mat(k,1300) = -(4._r8*rxt(k,333)*y(k,192) + rxt(k,334)*y(k,193) + rxt(k,335) &
                      *y(k,199) + rxt(k,336)*y(k,121) + rxt(k,347)*y(k,122) + rxt(k,374) &
                      *y(k,203) + rxt(k,407)*y(k,201) + rxt(k,412)*y(k,202) + rxt(k,421) &
                      *y(k,100) + rxt(k,432)*y(k,225))
         mat(k,1393) = -rxt(k,334)*y(k,192)
         mat(k,2073) = -rxt(k,335)*y(k,192)
         mat(k,1501) = -rxt(k,336)*y(k,192)
         mat(k,1928) = -rxt(k,347)*y(k,192)
         mat(k,1231) = -rxt(k,374)*y(k,192)
         mat(k,1176) = -rxt(k,407)*y(k,192)
         mat(k,1209) = -rxt(k,412)*y(k,192)
         mat(k,1129) = -rxt(k,421)*y(k,192)
         mat(k,1107) = -rxt(k,432)*y(k,192)
         mat(k,869) = .060_r8*rxt(k,482)*y(k,132)
         mat(k,968) = rxt(k,330)*y(k,123) + rxt(k,331)*y(k,217)
         mat(k,1154) = rxt(k,356)*y(k,123) + rxt(k,357)*y(k,217)
         mat(k,401) = .500_r8*rxt(k,338)*y(k,217)
         mat(k,790) = .080_r8*rxt(k,427)*y(k,132)
         mat(k,1145) = .100_r8*rxt(k,380)*y(k,132)
         mat(k,843) = .060_r8*rxt(k,485)*y(k,132)
         mat(k,1251) = .280_r8*rxt(k,394)*y(k,132)
         mat(k,1501) = mat(k,1501) + .530_r8*rxt(k,378)*y(k,203) + rxt(k,387)*y(k,205) &
                      + rxt(k,390)*y(k,207) + rxt(k,365)*y(k,221)
         mat(k,1557) = rxt(k,330)*y(k,44) + rxt(k,356)*y(k,48) + .530_r8*rxt(k,377) &
                      *y(k,203) + rxt(k,388)*y(k,205)
         mat(k,1802) = .060_r8*rxt(k,482)*y(k,5) + .080_r8*rxt(k,427)*y(k,97) &
                      + .100_r8*rxt(k,380)*y(k,104) + .060_r8*rxt(k,485)*y(k,109) &
                      + .280_r8*rxt(k,394)*y(k,110)
         mat(k,979) = .650_r8*rxt(k,503)*y(k,217)
         mat(k,1300) = mat(k,1300) + .530_r8*rxt(k,374)*y(k,203)
         mat(k,1393) = mat(k,1393) + .260_r8*rxt(k,375)*y(k,203) + rxt(k,384)*y(k,205) &
                      + .300_r8*rxt(k,363)*y(k,221)
         mat(k,2073) = mat(k,2073) + .450_r8*rxt(k,385)*y(k,205) + .200_r8*rxt(k,389) &
                      *y(k,207) + .150_r8*rxt(k,364)*y(k,221)
         mat(k,1231) = mat(k,1231) + .530_r8*rxt(k,378)*y(k,121) + .530_r8*rxt(k,377) &
                      *y(k,123) + .530_r8*rxt(k,374)*y(k,192) + .260_r8*rxt(k,375) &
                      *y(k,193)
         mat(k,1270) = rxt(k,387)*y(k,121) + rxt(k,388)*y(k,123) + rxt(k,384)*y(k,193) &
                      + .450_r8*rxt(k,385)*y(k,199) + 4.000_r8*rxt(k,386)*y(k,205)
         mat(k,573) = rxt(k,390)*y(k,121) + .200_r8*rxt(k,389)*y(k,199)
         mat(k,1739) = rxt(k,331)*y(k,44) + rxt(k,357)*y(k,48) + .500_r8*rxt(k,338) &
                      *y(k,50) + .650_r8*rxt(k,503)*y(k,175)
         mat(k,1078) = rxt(k,365)*y(k,121) + .300_r8*rxt(k,363)*y(k,193) &
                      + .150_r8*rxt(k,364)*y(k,199)
         mat(k,1394) = -(rxt(k,223)*y(k,58) + (4._r8*rxt(k,300) + 4._r8*rxt(k,301) &
                      ) * y(k,193) + rxt(k,302)*y(k,199) + rxt(k,303)*y(k,121) &
                      + rxt(k,323)*y(k,189) + rxt(k,334)*y(k,192) + rxt(k,351) &
                      *y(k,190) + rxt(k,363)*y(k,221) + rxt(k,375)*y(k,203) + rxt(k,384) &
                      *y(k,205) + rxt(k,408)*y(k,201) + rxt(k,413)*y(k,202) + rxt(k,422) &
                      *y(k,100) + rxt(k,433)*y(k,225) + rxt(k,487)*y(k,212) + rxt(k,492) &
                      *y(k,222) + rxt(k,497)*y(k,223))
         mat(k,1981) = -rxt(k,223)*y(k,193)
         mat(k,2076) = -rxt(k,302)*y(k,193)
         mat(k,1503) = -rxt(k,303)*y(k,193)
         mat(k,803) = -rxt(k,323)*y(k,193)
         mat(k,1301) = -rxt(k,334)*y(k,193)
         mat(k,732) = -rxt(k,351)*y(k,193)
         mat(k,1079) = -rxt(k,363)*y(k,193)
         mat(k,1232) = -rxt(k,375)*y(k,193)
         mat(k,1271) = -rxt(k,384)*y(k,193)
         mat(k,1177) = -rxt(k,408)*y(k,193)
         mat(k,1210) = -rxt(k,413)*y(k,193)
         mat(k,1130) = -rxt(k,422)*y(k,193)
         mat(k,1108) = -rxt(k,433)*y(k,193)
         mat(k,1063) = -rxt(k,487)*y(k,193)
         mat(k,1044) = -rxt(k,492)*y(k,193)
         mat(k,1024) = -rxt(k,497)*y(k,193)
         mat(k,950) = .280_r8*rxt(k,350)*y(k,132)
         mat(k,456) = rxt(k,337)*y(k,217)
         mat(k,311) = .700_r8*rxt(k,305)*y(k,217)
         mat(k,791) = .050_r8*rxt(k,427)*y(k,132)
         mat(k,1130) = mat(k,1130) + rxt(k,421)*y(k,192)
         mat(k,1503) = mat(k,1503) + rxt(k,336)*y(k,192) + .830_r8*rxt(k,453)*y(k,194) &
                      + .170_r8*rxt(k,459)*y(k,206)
         mat(k,1805) = .280_r8*rxt(k,350)*y(k,28) + .050_r8*rxt(k,427)*y(k,97)
         mat(k,1301) = mat(k,1301) + rxt(k,421)*y(k,100) + rxt(k,336)*y(k,121) &
                      + 4.000_r8*rxt(k,333)*y(k,192) + .900_r8*rxt(k,334)*y(k,193) &
                      + .450_r8*rxt(k,335)*y(k,199) + rxt(k,407)*y(k,201) + rxt(k,412) &
                      *y(k,202) + rxt(k,374)*y(k,203) + rxt(k,383)*y(k,205) &
                      + rxt(k,432)*y(k,225)
         mat(k,1394) = mat(k,1394) + .900_r8*rxt(k,334)*y(k,192)
         mat(k,649) = .830_r8*rxt(k,453)*y(k,121) + .330_r8*rxt(k,452)*y(k,199)
         mat(k,2076) = mat(k,2076) + .450_r8*rxt(k,335)*y(k,192) + .330_r8*rxt(k,452) &
                      *y(k,194) + .070_r8*rxt(k,458)*y(k,206)
         mat(k,1177) = mat(k,1177) + rxt(k,407)*y(k,192)
         mat(k,1210) = mat(k,1210) + rxt(k,412)*y(k,192)
         mat(k,1232) = mat(k,1232) + rxt(k,374)*y(k,192)
         mat(k,1271) = mat(k,1271) + rxt(k,383)*y(k,192)
         mat(k,814) = .170_r8*rxt(k,459)*y(k,121) + .070_r8*rxt(k,458)*y(k,199)
         mat(k,1743) = rxt(k,337)*y(k,49) + .700_r8*rxt(k,305)*y(k,52)
         mat(k,1108) = mat(k,1108) + rxt(k,432)*y(k,192)
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
         mat(k,646) = -(rxt(k,452)*y(k,199) + rxt(k,453)*y(k,121) + rxt(k,454) &
                      *y(k,122))
         mat(k,2039) = -rxt(k,452)*y(k,194)
         mat(k,1465) = -rxt(k,453)*y(k,194)
         mat(k,1917) = -rxt(k,454)*y(k,194)
         mat(k,702) = -(rxt(k,567)*y(k,210) + rxt(k,568)*y(k,216) + rxt(k,569) &
                      *y(k,209))
         mat(k,692) = -rxt(k,567)*y(k,195)
         mat(k,684) = -rxt(k,568)*y(k,195)
         mat(k,552) = -rxt(k,569)*y(k,195)
         mat(k,459) = -((rxt(k,371) + rxt(k,372)) * y(k,121))
         mat(k,1455) = -(rxt(k,371) + rxt(k,372)) * y(k,196)
         mat(k,261) = rxt(k,370)*y(k,217)
         mat(k,1677) = rxt(k,370)*y(k,15)
         mat(k,346) = -(rxt(k,342)*y(k,131))
         mat(k,1410) = -rxt(k,342)*y(k,197)
         mat(k,1448) = .750_r8*rxt(k,340)*y(k,198)
         mat(k,673) = .750_r8*rxt(k,340)*y(k,121)
         mat(k,674) = -(rxt(k,339)*y(k,199) + rxt(k,340)*y(k,121))
         mat(k,2041) = -rxt(k,339)*y(k,198)
         mat(k,1466) = -rxt(k,340)*y(k,198)
         mat(k,448) = rxt(k,346)*y(k,217)
         mat(k,1697) = rxt(k,346)*y(k,24)
         mat(k,2089) = -((rxt(k,174) + rxt(k,175) + rxt(k,176)) * y(k,75) + rxt(k,178) &
                      *y(k,130) + rxt(k,179)*y(k,132) + rxt(k,183)*y(k,217) &
                      + 4._r8*rxt(k,188)*y(k,199) + rxt(k,200)*y(k,123) + rxt(k,205) &
                      *y(k,121) + rxt(k,210)*y(k,122) + (rxt(k,220) + rxt(k,221) &
                      ) * y(k,55) + rxt(k,227)*y(k,58) + rxt(k,253)*y(k,16) + rxt(k,259) &
                      *y(k,18) + rxt(k,296)*y(k,41) + rxt(k,302)*y(k,193) + rxt(k,310) &
                      *y(k,200) + rxt(k,324)*y(k,189) + rxt(k,335)*y(k,192) + rxt(k,339) &
                      *y(k,198) + rxt(k,352)*y(k,190) + rxt(k,360)*y(k,220) + rxt(k,364) &
                      *y(k,221) + rxt(k,376)*y(k,203) + rxt(k,385)*y(k,205) + rxt(k,389) &
                      *y(k,207) + rxt(k,399)*y(k,186) + rxt(k,409)*y(k,201) + rxt(k,414) &
                      *y(k,202) + rxt(k,423)*y(k,100) + rxt(k,434)*y(k,225) + rxt(k,438) &
                      *y(k,185) + rxt(k,441)*y(k,187) + rxt(k,445)*y(k,188) + rxt(k,448) &
                      *y(k,191) + rxt(k,452)*y(k,194) + rxt(k,455)*y(k,204) + rxt(k,458) &
                      *y(k,206) + rxt(k,461)*y(k,219) + rxt(k,468)*y(k,224) + rxt(k,474) &
                      *y(k,226) + rxt(k,477)*y(k,227) + rxt(k,488)*y(k,212) + rxt(k,493) &
                      *y(k,222) + rxt(k,498)*y(k,223))
         mat(k,1838) = -(rxt(k,174) + rxt(k,175) + rxt(k,176)) * y(k,199)
         mat(k,1903) = -rxt(k,178)*y(k,199)
         mat(k,1818) = -rxt(k,179)*y(k,199)
         mat(k,1756) = -rxt(k,183)*y(k,199)
         mat(k,1573) = -rxt(k,200)*y(k,199)
         mat(k,1516) = -rxt(k,205)*y(k,199)
         mat(k,1945) = -rxt(k,210)*y(k,199)
         mat(k,1608) = -(rxt(k,220) + rxt(k,221)) * y(k,199)
         mat(k,1994) = -rxt(k,227)*y(k,199)
         mat(k,1357) = -rxt(k,253)*y(k,199)
         mat(k,1862) = -rxt(k,259)*y(k,199)
         mat(k,2139) = -rxt(k,296)*y(k,199)
         mat(k,1405) = -rxt(k,302)*y(k,199)
         mat(k,334) = -rxt(k,310)*y(k,199)
         mat(k,808) = -rxt(k,324)*y(k,199)
         mat(k,1310) = -rxt(k,335)*y(k,199)
         mat(k,680) = -rxt(k,339)*y(k,199)
         mat(k,737) = -rxt(k,352)*y(k,199)
         mat(k,725) = -rxt(k,360)*y(k,199)
         mat(k,1083) = -rxt(k,364)*y(k,199)
         mat(k,1239) = -rxt(k,376)*y(k,199)
         mat(k,1279) = -rxt(k,385)*y(k,199)
         mat(k,577) = -rxt(k,389)*y(k,199)
         mat(k,890) = -rxt(k,399)*y(k,199)
         mat(k,1185) = -rxt(k,409)*y(k,199)
         mat(k,1218) = -rxt(k,414)*y(k,199)
         mat(k,1137) = -rxt(k,423)*y(k,199)
         mat(k,1114) = -rxt(k,434)*y(k,199)
         mat(k,412) = -rxt(k,438)*y(k,199)
         mat(k,384) = -rxt(k,441)*y(k,199)
         mat(k,329) = -rxt(k,445)*y(k,199)
         mat(k,535) = -rxt(k,448)*y(k,199)
         mat(k,653) = -rxt(k,452)*y(k,199)
         mat(k,613) = -rxt(k,455)*y(k,199)
         mat(k,818) = -rxt(k,458)*y(k,199)
         mat(k,342) = -rxt(k,461)*y(k,199)
         mat(k,628) = -rxt(k,468)*y(k,199)
         mat(k,645) = -rxt(k,474)*y(k,199)
         mat(k,399) = -rxt(k,477)*y(k,199)
         mat(k,1070) = -rxt(k,488)*y(k,199)
         mat(k,1050) = -rxt(k,493)*y(k,199)
         mat(k,1031) = -rxt(k,498)*y(k,199)
         mat(k,873) = .570_r8*rxt(k,482)*y(k,132)
         mat(k,90) = .650_r8*rxt(k,440)*y(k,217)
         mat(k,1357) = mat(k,1357) + rxt(k,252)*y(k,41)
         mat(k,1862) = mat(k,1862) + rxt(k,264)*y(k,217)
         mat(k,206) = .350_r8*rxt(k,319)*y(k,217)
         mat(k,453) = .130_r8*rxt(k,321)*y(k,132)
         mat(k,181) = rxt(k,326)*y(k,217)
         mat(k,957) = .280_r8*rxt(k,350)*y(k,132)
         mat(k,2139) = mat(k,2139) + rxt(k,252)*y(k,16) + rxt(k,216)*y(k,55) &
                      + rxt(k,297)*y(k,123) + rxt(k,298)*y(k,130)
         mat(k,55) = rxt(k,332)*y(k,217)
         mat(k,715) = rxt(k,304)*y(k,217)
         mat(k,1608) = mat(k,1608) + rxt(k,216)*y(k,41) + rxt(k,219)*y(k,78)
         mat(k,1994) = mat(k,1994) + rxt(k,223)*y(k,193) + rxt(k,234)*y(k,217)
         mat(k,1005) = rxt(k,307)*y(k,217)
         mat(k,122) = .730_r8*rxt(k,451)*y(k,217)
         mat(k,197) = .500_r8*rxt(k,521)*y(k,217)
         mat(k,964) = rxt(k,343)*y(k,217)
         mat(k,823) = rxt(k,344)*y(k,217)
         mat(k,1838) = mat(k,1838) + rxt(k,177)*y(k,131)
         mat(k,480) = rxt(k,219)*y(k,55) + rxt(k,173)*y(k,130) + rxt(k,182)*y(k,217)
         mat(k,104) = rxt(k,308)*y(k,217)
         mat(k,711) = rxt(k,309)*y(k,217)
         mat(k,999) = rxt(k,373)*y(k,217)
         mat(k,1010) = rxt(k,358)*y(k,217)
         mat(k,795) = .370_r8*rxt(k,427)*y(k,132)
         mat(k,513) = .300_r8*rxt(k,418)*y(k,217)
         mat(k,430) = rxt(k,419)*y(k,217)
         mat(k,1137) = mat(k,1137) + rxt(k,424)*y(k,121) + rxt(k,425)*y(k,123) &
                      + rxt(k,421)*y(k,192) + 1.200_r8*rxt(k,422)*y(k,193)
         mat(k,303) = rxt(k,426)*y(k,217)
         mat(k,1149) = .140_r8*rxt(k,380)*y(k,132)
         mat(k,220) = .200_r8*rxt(k,382)*y(k,217)
         mat(k,473) = .500_r8*rxt(k,393)*y(k,217)
         mat(k,847) = .570_r8*rxt(k,485)*y(k,132)
         mat(k,1261) = .280_r8*rxt(k,394)*y(k,132)
         mat(k,273) = rxt(k,430)*y(k,217)
         mat(k,938) = rxt(k,431)*y(k,217)
         mat(k,1516) = mat(k,1516) + rxt(k,424)*y(k,100) + rxt(k,400)*y(k,186) &
                      + rxt(k,442)*y(k,187) + rxt(k,447)*y(k,188) + rxt(k,325) &
                      *y(k,189) + rxt(k,353)*y(k,190) + rxt(k,303)*y(k,193) &
                      + .170_r8*rxt(k,453)*y(k,194) + rxt(k,371)*y(k,196) &
                      + .250_r8*rxt(k,340)*y(k,198) + rxt(k,312)*y(k,200) &
                      + .920_r8*rxt(k,410)*y(k,201) + .920_r8*rxt(k,416)*y(k,202) &
                      + .470_r8*rxt(k,378)*y(k,203) + .400_r8*rxt(k,456)*y(k,204) &
                      + .830_r8*rxt(k,459)*y(k,206) + rxt(k,462)*y(k,219) + rxt(k,361) &
                      *y(k,220) + .900_r8*rxt(k,494)*y(k,222) + .800_r8*rxt(k,499) &
                      *y(k,223) + rxt(k,469)*y(k,224) + rxt(k,435)*y(k,225) &
                      + rxt(k,475)*y(k,226) + rxt(k,478)*y(k,227)
         mat(k,1573) = mat(k,1573) + rxt(k,297)*y(k,41) + rxt(k,425)*y(k,100) &
                      + rxt(k,411)*y(k,201) + rxt(k,417)*y(k,202) + .470_r8*rxt(k,377) &
                      *y(k,203) + rxt(k,203)*y(k,217) + rxt(k,436)*y(k,225)
         mat(k,1903) = mat(k,1903) + rxt(k,298)*y(k,41) + rxt(k,173)*y(k,78)
         mat(k,1435) = rxt(k,177)*y(k,75) + rxt(k,342)*y(k,197)
         mat(k,1818) = mat(k,1818) + .570_r8*rxt(k,482)*y(k,5) + .130_r8*rxt(k,321) &
                      *y(k,24) + .280_r8*rxt(k,350)*y(k,28) + .370_r8*rxt(k,427) &
                      *y(k,97) + .140_r8*rxt(k,380)*y(k,104) + .570_r8*rxt(k,485) &
                      *y(k,109) + .280_r8*rxt(k,394)*y(k,110) + rxt(k,185)*y(k,217)
         mat(k,99) = .800_r8*rxt(k,463)*y(k,217)
         mat(k,903) = rxt(k,516)*y(k,217)
         mat(k,982) = .200_r8*rxt(k,503)*y(k,217)
         mat(k,117) = .280_r8*rxt(k,471)*y(k,217)
         mat(k,147) = .380_r8*rxt(k,473)*y(k,217)
         mat(k,152) = .630_r8*rxt(k,479)*y(k,217)
         mat(k,890) = mat(k,890) + rxt(k,400)*y(k,121)
         mat(k,384) = mat(k,384) + rxt(k,442)*y(k,121)
         mat(k,329) = mat(k,329) + rxt(k,447)*y(k,121)
         mat(k,808) = mat(k,808) + rxt(k,325)*y(k,121) + 2.400_r8*rxt(k,322)*y(k,189) &
                      + rxt(k,323)*y(k,193)
         mat(k,737) = mat(k,737) + rxt(k,353)*y(k,121) + rxt(k,351)*y(k,193)
         mat(k,1310) = mat(k,1310) + rxt(k,421)*y(k,100) + .900_r8*rxt(k,334)*y(k,193) &
                      + rxt(k,407)*y(k,201) + rxt(k,412)*y(k,202) + .470_r8*rxt(k,374) &
                      *y(k,203) + rxt(k,432)*y(k,225)
         mat(k,1405) = mat(k,1405) + rxt(k,223)*y(k,58) + 1.200_r8*rxt(k,422)*y(k,100) &
                      + rxt(k,303)*y(k,121) + rxt(k,323)*y(k,189) + rxt(k,351) &
                      *y(k,190) + .900_r8*rxt(k,334)*y(k,192) + 4.000_r8*rxt(k,300) &
                      *y(k,193) + rxt(k,408)*y(k,201) + rxt(k,413)*y(k,202) &
                      + .730_r8*rxt(k,375)*y(k,203) + rxt(k,384)*y(k,205) &
                      + .500_r8*rxt(k,487)*y(k,212) + .300_r8*rxt(k,363)*y(k,221) &
                      + rxt(k,492)*y(k,222) + rxt(k,497)*y(k,223) + .800_r8*rxt(k,433) &
                      *y(k,225)
         mat(k,653) = mat(k,653) + .170_r8*rxt(k,453)*y(k,121) + .070_r8*rxt(k,452) &
                      *y(k,199)
         mat(k,465) = rxt(k,371)*y(k,121)
         mat(k,349) = rxt(k,342)*y(k,131)
         mat(k,680) = mat(k,680) + .250_r8*rxt(k,340)*y(k,121)
         mat(k,2089) = mat(k,2089) + .070_r8*rxt(k,452)*y(k,194) + .160_r8*rxt(k,455) &
                      *y(k,204) + .330_r8*rxt(k,458)*y(k,206)
         mat(k,334) = mat(k,334) + rxt(k,312)*y(k,121)
         mat(k,1185) = mat(k,1185) + .920_r8*rxt(k,410)*y(k,121) + rxt(k,411)*y(k,123) &
                      + rxt(k,407)*y(k,192) + rxt(k,408)*y(k,193)
         mat(k,1218) = mat(k,1218) + .920_r8*rxt(k,416)*y(k,121) + rxt(k,417)*y(k,123) &
                      + rxt(k,412)*y(k,192) + rxt(k,413)*y(k,193)
         mat(k,1239) = mat(k,1239) + .470_r8*rxt(k,378)*y(k,121) + .470_r8*rxt(k,377) &
                      *y(k,123) + .470_r8*rxt(k,374)*y(k,192) + .730_r8*rxt(k,375) &
                      *y(k,193)
         mat(k,613) = mat(k,613) + .400_r8*rxt(k,456)*y(k,121) + .160_r8*rxt(k,455) &
                      *y(k,199)
         mat(k,1279) = mat(k,1279) + rxt(k,384)*y(k,193)
         mat(k,818) = mat(k,818) + .830_r8*rxt(k,459)*y(k,121) + .330_r8*rxt(k,458) &
                      *y(k,199)
         mat(k,1070) = mat(k,1070) + .500_r8*rxt(k,487)*y(k,193)
         mat(k,1756) = mat(k,1756) + .650_r8*rxt(k,440)*y(k,6) + rxt(k,264)*y(k,18) &
                      + .350_r8*rxt(k,319)*y(k,23) + rxt(k,326)*y(k,25) + rxt(k,332) &
                      *y(k,46) + rxt(k,304)*y(k,51) + rxt(k,234)*y(k,58) + rxt(k,307) &
                      *y(k,61) + .730_r8*rxt(k,451)*y(k,65) + .500_r8*rxt(k,521) &
                      *y(k,66) + rxt(k,343)*y(k,73) + rxt(k,344)*y(k,74) + rxt(k,182) &
                      *y(k,78) + rxt(k,308)*y(k,85) + rxt(k,309)*y(k,86) + rxt(k,373) &
                      *y(k,92) + rxt(k,358)*y(k,94) + .300_r8*rxt(k,418)*y(k,98) &
                      + rxt(k,419)*y(k,99) + rxt(k,426)*y(k,101) + .200_r8*rxt(k,382) &
                      *y(k,105) + .500_r8*rxt(k,393)*y(k,108) + rxt(k,430)*y(k,114) &
                      + rxt(k,431)*y(k,115) + rxt(k,203)*y(k,123) + rxt(k,185) &
                      *y(k,132) + .800_r8*rxt(k,463)*y(k,140) + rxt(k,516)*y(k,149) &
                      + .200_r8*rxt(k,503)*y(k,175) + .280_r8*rxt(k,471)*y(k,177) &
                      + .380_r8*rxt(k,473)*y(k,179) + .630_r8*rxt(k,479)*y(k,181)
         mat(k,342) = mat(k,342) + rxt(k,462)*y(k,121)
         mat(k,725) = mat(k,725) + rxt(k,361)*y(k,121)
         mat(k,1083) = mat(k,1083) + .300_r8*rxt(k,363)*y(k,193)
         mat(k,1050) = mat(k,1050) + .900_r8*rxt(k,494)*y(k,121) + rxt(k,492)*y(k,193)
         mat(k,1031) = mat(k,1031) + .800_r8*rxt(k,499)*y(k,121) + rxt(k,497)*y(k,193)
         mat(k,628) = mat(k,628) + rxt(k,469)*y(k,121)
         mat(k,1114) = mat(k,1114) + rxt(k,435)*y(k,121) + rxt(k,436)*y(k,123) &
                      + rxt(k,432)*y(k,192) + .800_r8*rxt(k,433)*y(k,193)
         mat(k,645) = mat(k,645) + rxt(k,475)*y(k,121)
         mat(k,399) = mat(k,399) + rxt(k,478)*y(k,121)
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
         mat(k,330) = -(rxt(k,310)*y(k,199) + rxt(k,312)*y(k,121))
         mat(k,2014) = -rxt(k,310)*y(k,200)
         mat(k,1445) = -rxt(k,312)*y(k,200)
         mat(k,2119) = rxt(k,296)*y(k,199)
         mat(k,2014) = mat(k,2014) + rxt(k,296)*y(k,41)
         mat(k,1172) = -(rxt(k,407)*y(k,192) + rxt(k,408)*y(k,193) + rxt(k,409) &
                      *y(k,199) + rxt(k,410)*y(k,121) + rxt(k,411)*y(k,123))
         mat(k,1295) = -rxt(k,407)*y(k,201)
         mat(k,1388) = -rxt(k,408)*y(k,201)
         mat(k,2068) = -rxt(k,409)*y(k,201)
         mat(k,1496) = -rxt(k,410)*y(k,201)
         mat(k,1552) = -rxt(k,411)*y(k,201)
         mat(k,787) = .600_r8*rxt(k,428)*y(k,217)
         mat(k,1734) = .600_r8*rxt(k,428)*y(k,97)
         mat(k,1205) = -(rxt(k,412)*y(k,192) + rxt(k,413)*y(k,193) + rxt(k,414) &
                      *y(k,199) + rxt(k,416)*y(k,121) + rxt(k,417)*y(k,123))
         mat(k,1296) = -rxt(k,412)*y(k,202)
         mat(k,1389) = -rxt(k,413)*y(k,202)
         mat(k,2069) = -rxt(k,414)*y(k,202)
         mat(k,1497) = -rxt(k,416)*y(k,202)
         mat(k,1553) = -rxt(k,417)*y(k,202)
         mat(k,788) = .400_r8*rxt(k,428)*y(k,217)
         mat(k,1735) = .400_r8*rxt(k,428)*y(k,97)
         mat(k,1229) = -(rxt(k,374)*y(k,192) + rxt(k,375)*y(k,193) + rxt(k,376) &
                      *y(k,199) + rxt(k,377)*y(k,123) + (rxt(k,378) + rxt(k,379) &
                      ) * y(k,121))
         mat(k,1297) = -rxt(k,374)*y(k,203)
         mat(k,1390) = -rxt(k,375)*y(k,203)
         mat(k,2070) = -rxt(k,376)*y(k,203)
         mat(k,1554) = -rxt(k,377)*y(k,203)
         mat(k,1498) = -(rxt(k,378) + rxt(k,379)) * y(k,203)
         mat(k,1143) = .500_r8*rxt(k,381)*y(k,217)
         mat(k,217) = .200_r8*rxt(k,382)*y(k,217)
         mat(k,1248) = rxt(k,395)*y(k,217)
         mat(k,1736) = .500_r8*rxt(k,381)*y(k,104) + .200_r8*rxt(k,382)*y(k,105) &
                      + rxt(k,395)*y(k,110)
         mat(k,608) = -(rxt(k,455)*y(k,199) + rxt(k,456)*y(k,121) + rxt(k,457) &
                      *y(k,122))
         mat(k,2036) = -rxt(k,455)*y(k,204)
         mat(k,1462) = -rxt(k,456)*y(k,204)
         mat(k,1916) = -rxt(k,457)*y(k,204)
         mat(k,1269) = -(rxt(k,383)*y(k,192) + rxt(k,384)*y(k,193) + rxt(k,385) &
                      *y(k,199) + 4._r8*rxt(k,386)*y(k,205) + rxt(k,387)*y(k,121) &
                      + rxt(k,388)*y(k,123) + rxt(k,396)*y(k,122))
         mat(k,1299) = -rxt(k,383)*y(k,205)
         mat(k,1392) = -rxt(k,384)*y(k,205)
         mat(k,2072) = -rxt(k,385)*y(k,205)
         mat(k,1500) = -rxt(k,387)*y(k,205)
         mat(k,1556) = -rxt(k,388)*y(k,205)
         mat(k,1927) = -rxt(k,396)*y(k,205)
         mat(k,1144) = .500_r8*rxt(k,381)*y(k,217)
         mat(k,218) = .500_r8*rxt(k,382)*y(k,217)
         mat(k,1738) = .500_r8*rxt(k,381)*y(k,104) + .500_r8*rxt(k,382)*y(k,105)
         mat(k,810) = -(rxt(k,458)*y(k,199) + rxt(k,459)*y(k,121) + rxt(k,460) &
                      *y(k,122))
         mat(k,2049) = -rxt(k,458)*y(k,206)
         mat(k,1476) = -rxt(k,459)*y(k,206)
         mat(k,1920) = -rxt(k,460)*y(k,206)
         mat(k,571) = -(rxt(k,389)*y(k,199) + rxt(k,390)*y(k,121))
         mat(k,2033) = -rxt(k,389)*y(k,207)
         mat(k,1461) = -rxt(k,390)*y(k,207)
         mat(k,414) = rxt(k,391)*y(k,217)
         mat(k,222) = rxt(k,392)*y(k,217)
         mat(k,1688) = rxt(k,391)*y(k,106) + rxt(k,392)*y(k,107)
         mat(k,418) = -(rxt(k,190)*y(k,130) + rxt(k,191)*y(k,131))
         mat(k,1870) = -rxt(k,190)*y(k,208)
         mat(k,1412) = -rxt(k,191)*y(k,208)
         mat(k,1870) = mat(k,1870) + rxt(k,571)*y(k,209)
         mat(k,698) = .900_r8*rxt(k,569)*y(k,209) + .800_r8*rxt(k,567)*y(k,210)
         mat(k,547) = rxt(k,571)*y(k,130) + .900_r8*rxt(k,569)*y(k,195)
         mat(k,690) = .800_r8*rxt(k,567)*y(k,195)
         mat(k,549) = -(rxt(k,569)*y(k,195) + rxt(k,570)*y(k,131) + (rxt(k,571) &
                      + rxt(k,572)) * y(k,130))
         mat(k,699) = -rxt(k,569)*y(k,209)
         mat(k,1414) = -rxt(k,570)*y(k,209)
         mat(k,1874) = -(rxt(k,571) + rxt(k,572)) * y(k,209)
         mat(k,691) = -(rxt(k,567)*y(k,195))
         mat(k,701) = -rxt(k,567)*y(k,210)
         mat(k,743) = rxt(k,576)*y(k,216)
         mat(k,1468) = rxt(k,578)*y(k,216)
         mat(k,1878) = rxt(k,571)*y(k,209)
         mat(k,1417) = rxt(k,575)*y(k,211)
         mat(k,551) = rxt(k,571)*y(k,130)
         mat(k,388) = rxt(k,575)*y(k,131)
         mat(k,683) = rxt(k,576)*y(k,111) + rxt(k,578)*y(k,121)
         mat(k,385) = -(rxt(k,573)*y(k,130) + (rxt(k,574) + rxt(k,575)) * y(k,131))
         mat(k,1869) = -rxt(k,573)*y(k,211)
         mat(k,1411) = -(rxt(k,574) + rxt(k,575)) * y(k,211)
         mat(k,1060) = -(rxt(k,487)*y(k,193) + rxt(k,488)*y(k,199) + rxt(k,489) &
                      *y(k,121) + rxt(k,490)*y(k,123))
         mat(k,1382) = -rxt(k,487)*y(k,212)
         mat(k,2061) = -rxt(k,488)*y(k,212)
         mat(k,1490) = -rxt(k,489)*y(k,212)
         mat(k,1546) = -rxt(k,490)*y(k,212)
         mat(k,866) = rxt(k,481)*y(k,123)
         mat(k,840) = rxt(k,484)*y(k,123)
         mat(k,1546) = mat(k,1546) + rxt(k,481)*y(k,5) + rxt(k,484)*y(k,109) &
                      + .500_r8*rxt(k,501)*y(k,174)
         mat(k,288) = rxt(k,491)*y(k,217)
         mat(k,921) = .500_r8*rxt(k,501)*y(k,123)
         mat(k,1727) = rxt(k,491)*y(k,125)
         mat(k,2116) = -(rxt(k,155)*y(k,76) + rxt(k,156)*y(k,228) + (rxt(k,158) &
                      + rxt(k,159)) * y(k,131) + rxt(k,160)*y(k,132) + (rxt(k,248) &
                      + rxt(k,249)) * y(k,84) + (rxt(k,271) + rxt(k,272)) * y(k,80) &
                      + rxt(k,277)*y(k,63) + rxt(k,278)*y(k,64) + rxt(k,316)*y(k,85))
         mat(k,1097) = -rxt(k,155)*y(k,213)
         mat(k,2166) = -rxt(k,156)*y(k,213)
         mat(k,1436) = -(rxt(k,158) + rxt(k,159)) * y(k,213)
         mat(k,1819) = -rxt(k,160)*y(k,213)
         mat(k,1346) = -(rxt(k,248) + rxt(k,249)) * y(k,213)
         mat(k,760) = -(rxt(k,271) + rxt(k,272)) * y(k,213)
         mat(k,62) = -rxt(k,277)*y(k,213)
         mat(k,133) = -rxt(k,278)*y(k,213)
         mat(k,105) = -rxt(k,316)*y(k,213)
         mat(k,1436) = mat(k,1436) + rxt(k,191)*y(k,208)
         mat(k,708) = .850_r8*rxt(k,568)*y(k,216)
         mat(k,422) = rxt(k,191)*y(k,131)
         mat(k,689) = .850_r8*rxt(k,568)*y(k,195)
         mat(k,75) = -(rxt(k,162)*y(k,130) + rxt(k,163)*y(k,131))
         mat(k,1866) = -rxt(k,162)*y(k,214)
         mat(k,1408) = -rxt(k,163)*y(k,214)
         mat(k,1866) = mat(k,1866) + rxt(k,166)*y(k,215)
         mat(k,1408) = mat(k,1408) + rxt(k,167)*y(k,215)
         mat(k,1765) = rxt(k,168)*y(k,215)
         mat(k,77) = rxt(k,166)*y(k,130) + rxt(k,167)*y(k,131) + rxt(k,168)*y(k,132)
         mat(k,78) = -(rxt(k,166)*y(k,130) + rxt(k,167)*y(k,131) + rxt(k,168)*y(k,132))
         mat(k,1867) = -rxt(k,166)*y(k,215)
         mat(k,1409) = -rxt(k,167)*y(k,215)
         mat(k,1766) = -rxt(k,168)*y(k,215)
         mat(k,1409) = mat(k,1409) + rxt(k,158)*y(k,213)
         mat(k,2094) = rxt(k,158)*y(k,131)
         mat(k,682) = -(rxt(k,568)*y(k,195) + rxt(k,576)*y(k,111) + rxt(k,578) &
                      *y(k,121))
         mat(k,700) = -rxt(k,568)*y(k,216)
         mat(k,742) = -rxt(k,576)*y(k,216)
         mat(k,1467) = -rxt(k,578)*y(k,216)
         mat(k,1416) = rxt(k,570)*y(k,209) + rxt(k,574)*y(k,211) + rxt(k,581)*y(k,218)
         mat(k,550) = rxt(k,570)*y(k,131)
         mat(k,387) = rxt(k,574)*y(k,131)
         mat(k,516) = rxt(k,581)*y(k,131)
         mat(k,1748) = -(rxt(k,181)*y(k,76) + rxt(k,182)*y(k,78) + rxt(k,183)*y(k,199) &
                      + rxt(k,184)*y(k,130) + rxt(k,185)*y(k,132) + (4._r8*rxt(k,186) &
                      + 4._r8*rxt(k,187)) * y(k,217) + rxt(k,189)*y(k,89) + rxt(k,203) &
                      *y(k,123) + rxt(k,204)*y(k,111) + rxt(k,212)*y(k,122) + rxt(k,213) &
                      *y(k,88) + rxt(k,232)*y(k,59) + (rxt(k,234) + rxt(k,235) &
                      ) * y(k,58) + rxt(k,237)*y(k,84) + rxt(k,240)*y(k,91) + rxt(k,264) &
                      *y(k,18) + rxt(k,266)*y(k,80) + rxt(k,299)*y(k,41) + rxt(k,304) &
                      *y(k,51) + rxt(k,305)*y(k,52) + (rxt(k,307) + rxt(k,317) &
                      ) * y(k,61) + rxt(k,308)*y(k,85) + rxt(k,309)*y(k,86) + rxt(k,319) &
                      *y(k,23) + rxt(k,326)*y(k,25) + rxt(k,327)*y(k,26) + rxt(k,329) &
                      *y(k,27) + rxt(k,331)*y(k,44) + rxt(k,332)*y(k,46) + rxt(k,337) &
                      *y(k,49) + rxt(k,338)*y(k,50) + rxt(k,343)*y(k,73) + rxt(k,344) &
                      *y(k,74) + rxt(k,345)*y(k,137) + rxt(k,346)*y(k,24) + rxt(k,354) &
                      *y(k,29) + rxt(k,355)*y(k,30) + rxt(k,357)*y(k,48) + rxt(k,358) &
                      *y(k,94) + rxt(k,359)*y(k,124) + rxt(k,362)*y(k,144) + rxt(k,366) &
                      *y(k,145) + rxt(k,367)*y(k,28) + rxt(k,368)*y(k,47) + rxt(k,370) &
                      *y(k,15) + rxt(k,373)*y(k,92) + rxt(k,381)*y(k,104) + rxt(k,382) &
                      *y(k,105) + rxt(k,391)*y(k,106) + rxt(k,392)*y(k,107) + rxt(k,393) &
                      *y(k,108) + rxt(k,395)*y(k,110) + rxt(k,398)*y(k,1) + rxt(k,402) &
                      *y(k,2) + rxt(k,403)*y(k,14) + rxt(k,404)*y(k,93) + rxt(k,405) &
                      *y(k,95) + rxt(k,406)*y(k,96) + rxt(k,418)*y(k,98) + rxt(k,419) &
                      *y(k,99) + rxt(k,426)*y(k,101) + rxt(k,428)*y(k,97) + rxt(k,429) &
                      *y(k,102) + rxt(k,430)*y(k,114) + rxt(k,431)*y(k,115) + rxt(k,437) &
                      *y(k,178) + rxt(k,440)*y(k,6) + rxt(k,443)*y(k,7) + rxt(k,444) &
                      *y(k,21) + rxt(k,446)*y(k,22) + rxt(k,450)*y(k,31) + rxt(k,451) &
                      *y(k,65) + rxt(k,463)*y(k,140) + rxt(k,466)*y(k,141) + rxt(k,470) &
                      *y(k,176) + rxt(k,471)*y(k,177) + rxt(k,473)*y(k,179) + rxt(k,476) &
                      *y(k,180) + rxt(k,479)*y(k,181) + rxt(k,480)*y(k,182) + rxt(k,483) &
                      *y(k,5) + rxt(k,486)*y(k,109) + rxt(k,491)*y(k,125) + rxt(k,495) &
                      *y(k,171) + rxt(k,496)*y(k,172) + rxt(k,500)*y(k,173) + rxt(k,502) &
                      *y(k,174) + rxt(k,503)*y(k,175) + rxt(k,505)*y(k,135) + rxt(k,510) &
                      *y(k,146) + rxt(k,515)*y(k,148) + rxt(k,516)*y(k,149) + (rxt(k,519) &
                      + rxt(k,521)) * y(k,66) + rxt(k,520)*y(k,119))
         mat(k,1093) = -rxt(k,181)*y(k,217)
         mat(k,478) = -rxt(k,182)*y(k,217)
         mat(k,2081) = -rxt(k,183)*y(k,217)
         mat(k,1895) = -rxt(k,184)*y(k,217)
         mat(k,1810) = -rxt(k,185)*y(k,217)
         mat(k,365) = -rxt(k,189)*y(k,217)
         mat(k,1565) = -rxt(k,203)*y(k,217)
         mat(k,749) = -rxt(k,204)*y(k,217)
         mat(k,1937) = -rxt(k,212)*y(k,217)
         mat(k,1959) = -rxt(k,213)*y(k,217)
         mat(k,911) = -rxt(k,232)*y(k,217)
         mat(k,1986) = -(rxt(k,234) + rxt(k,235)) * y(k,217)
         mat(k,1339) = -rxt(k,237)*y(k,217)
         mat(k,766) = -rxt(k,240)*y(k,217)
         mat(k,1854) = -rxt(k,264)*y(k,217)
         mat(k,756) = -rxt(k,266)*y(k,217)
         mat(k,2131) = -rxt(k,299)*y(k,217)
         mat(k,714) = -rxt(k,304)*y(k,217)
         mat(k,312) = -rxt(k,305)*y(k,217)
         mat(k,1003) = -(rxt(k,307) + rxt(k,317)) * y(k,217)
         mat(k,103) = -rxt(k,308)*y(k,217)
         mat(k,710) = -rxt(k,309)*y(k,217)
         mat(k,205) = -rxt(k,319)*y(k,217)
         mat(k,180) = -rxt(k,326)*y(k,217)
         mat(k,250) = -rxt(k,327)*y(k,217)
         mat(k,211) = -rxt(k,329)*y(k,217)
         mat(k,971) = -rxt(k,331)*y(k,217)
         mat(k,54) = -rxt(k,332)*y(k,217)
         mat(k,457) = -rxt(k,337)*y(k,217)
         mat(k,403) = -rxt(k,338)*y(k,217)
         mat(k,963) = -rxt(k,343)*y(k,217)
         mat(k,822) = -rxt(k,344)*y(k,217)
         mat(k,355) = -rxt(k,345)*y(k,217)
         mat(k,451) = -rxt(k,346)*y(k,217)
         mat(k,307) = -rxt(k,354)*y(k,217)
         mat(k,58) = -rxt(k,355)*y(k,217)
         mat(k,1156) = -rxt(k,357)*y(k,217)
         mat(k,1009) = -rxt(k,358)*y(k,217)
         mat(k,773) = -rxt(k,359)*y(k,217)
         mat(k,435) = -rxt(k,362)*y(k,217)
         mat(k,295) = -rxt(k,366)*y(k,217)
         mat(k,954) = -rxt(k,367)*y(k,217)
         mat(k,896) = -rxt(k,368)*y(k,217)
         mat(k,265) = -rxt(k,370)*y(k,217)
         mat(k,996) = -rxt(k,373)*y(k,217)
         mat(k,1146) = -rxt(k,381)*y(k,217)
         mat(k,219) = -rxt(k,382)*y(k,217)
         mat(k,417) = -rxt(k,391)*y(k,217)
         mat(k,225) = -rxt(k,392)*y(k,217)
         mat(k,471) = -rxt(k,393)*y(k,217)
         mat(k,1256) = -rxt(k,395)*y(k,217)
         mat(k,543) = -rxt(k,398)*y(k,217)
         mat(k,567) = -rxt(k,402)*y(k,217)
         mat(k,163) = -rxt(k,403)*y(k,217)
         mat(k,159) = -rxt(k,404)*y(k,217)
         mat(k,215) = -rxt(k,405)*y(k,217)
         mat(k,71) = -rxt(k,406)*y(k,217)
         mat(k,510) = -rxt(k,418)*y(k,217)
         mat(k,428) = -rxt(k,419)*y(k,217)
         mat(k,301) = -rxt(k,426)*y(k,217)
         mat(k,793) = -rxt(k,428)*y(k,217)
         mat(k,583) = -rxt(k,429)*y(k,217)
         mat(k,271) = -rxt(k,430)*y(k,217)
         mat(k,935) = -rxt(k,431)*y(k,217)
         mat(k,129) = -rxt(k,437)*y(k,217)
         mat(k,89) = -rxt(k,440)*y(k,217)
         mat(k,278) = -rxt(k,443)*y(k,217)
         mat(k,172) = -rxt(k,444)*y(k,217)
         mat(k,245) = -rxt(k,446)*y(k,217)
         mat(k,185) = -rxt(k,450)*y(k,217)
         mat(k,121) = -rxt(k,451)*y(k,217)
         mat(k,98) = -rxt(k,463)*y(k,217)
         mat(k,239) = -rxt(k,466)*y(k,217)
         mat(k,500) = -rxt(k,470)*y(k,217)
         mat(k,116) = -rxt(k,471)*y(k,217)
         mat(k,146) = -rxt(k,473)*y(k,217)
         mat(k,599) = -rxt(k,476)*y(k,217)
         mat(k,151) = -rxt(k,479)*y(k,217)
         mat(k,320) = -rxt(k,480)*y(k,217)
         mat(k,871) = -rxt(k,483)*y(k,217)
         mat(k,845) = -rxt(k,486)*y(k,217)
         mat(k,289) = -rxt(k,491)*y(k,217)
         mat(k,488) = -rxt(k,495)*y(k,217)
         mat(k,524) = -rxt(k,496)*y(k,217)
         mat(k,373) = -rxt(k,500)*y(k,217)
         mat(k,923) = -rxt(k,502)*y(k,217)
         mat(k,981) = -rxt(k,503)*y(k,217)
         mat(k,257) = -rxt(k,505)*y(k,217)
         mat(k,604) = -rxt(k,510)*y(k,217)
         mat(k,1321) = -rxt(k,515)*y(k,217)
         mat(k,901) = -rxt(k,516)*y(k,217)
         mat(k,195) = -(rxt(k,519) + rxt(k,521)) * y(k,217)
         mat(k,51) = -rxt(k,520)*y(k,217)
         mat(k,871) = mat(k,871) + .630_r8*rxt(k,482)*y(k,132)
         mat(k,205) = mat(k,205) + .650_r8*rxt(k,319)*y(k,217)
         mat(k,451) = mat(k,451) + .130_r8*rxt(k,321)*y(k,132)
         mat(k,250) = mat(k,250) + .500_r8*rxt(k,327)*y(k,217)
         mat(k,954) = mat(k,954) + .360_r8*rxt(k,350)*y(k,132)
         mat(k,2131) = mat(k,2131) + rxt(k,298)*y(k,130)
         mat(k,312) = mat(k,312) + .300_r8*rxt(k,305)*y(k,217)
         mat(k,1600) = rxt(k,221)*y(k,199)
         mat(k,668) = rxt(k,275)*y(k,228)
         mat(k,1830) = rxt(k,180)*y(k,132) + 2.000_r8*rxt(k,175)*y(k,199)
         mat(k,1093) = mat(k,1093) + rxt(k,172)*y(k,130) + rxt(k,155)*y(k,213)
         mat(k,478) = mat(k,478) + rxt(k,173)*y(k,130)
         mat(k,756) = mat(k,756) + rxt(k,265)*y(k,130) + rxt(k,271)*y(k,213)
         mat(k,1339) = mat(k,1339) + rxt(k,236)*y(k,130) + rxt(k,248)*y(k,213)
         mat(k,103) = mat(k,103) + rxt(k,316)*y(k,213)
         mat(k,659) = rxt(k,267)*y(k,130)
         mat(k,766) = mat(k,766) + rxt(k,239)*y(k,130)
         mat(k,793) = mat(k,793) + .320_r8*rxt(k,427)*y(k,132)
         mat(k,583) = mat(k,583) + .600_r8*rxt(k,429)*y(k,217)
         mat(k,1146) = mat(k,1146) + .240_r8*rxt(k,380)*y(k,132)
         mat(k,219) = mat(k,219) + .100_r8*rxt(k,382)*y(k,217)
         mat(k,845) = mat(k,845) + .630_r8*rxt(k,485)*y(k,132)
         mat(k,1256) = mat(k,1256) + .360_r8*rxt(k,394)*y(k,132)
         mat(k,1508) = rxt(k,205)*y(k,199)
         mat(k,1565) = mat(k,1565) + rxt(k,200)*y(k,199)
         mat(k,1895) = mat(k,1895) + rxt(k,298)*y(k,41) + rxt(k,172)*y(k,76) &
                      + rxt(k,173)*y(k,78) + rxt(k,265)*y(k,80) + rxt(k,236)*y(k,84) &
                      + rxt(k,267)*y(k,90) + rxt(k,239)*y(k,91) + rxt(k,178)*y(k,199)
         mat(k,1810) = mat(k,1810) + .630_r8*rxt(k,482)*y(k,5) + .130_r8*rxt(k,321) &
                      *y(k,24) + .360_r8*rxt(k,350)*y(k,28) + rxt(k,180)*y(k,75) &
                      + .320_r8*rxt(k,427)*y(k,97) + .240_r8*rxt(k,380)*y(k,104) &
                      + .630_r8*rxt(k,485)*y(k,109) + .360_r8*rxt(k,394)*y(k,110) &
                      + rxt(k,179)*y(k,199)
         mat(k,435) = mat(k,435) + .500_r8*rxt(k,362)*y(k,217)
         mat(k,129) = mat(k,129) + .500_r8*rxt(k,437)*y(k,217)
         mat(k,410) = .400_r8*rxt(k,438)*y(k,199)
         mat(k,1305) = .450_r8*rxt(k,335)*y(k,199)
         mat(k,651) = .400_r8*rxt(k,452)*y(k,199)
         mat(k,2081) = mat(k,2081) + rxt(k,221)*y(k,55) + 2.000_r8*rxt(k,175)*y(k,75) &
                      + rxt(k,205)*y(k,121) + rxt(k,200)*y(k,123) + rxt(k,178) &
                      *y(k,130) + rxt(k,179)*y(k,132) + .400_r8*rxt(k,438)*y(k,185) &
                      + .450_r8*rxt(k,335)*y(k,192) + .400_r8*rxt(k,452)*y(k,194) &
                      + .450_r8*rxt(k,385)*y(k,205) + .400_r8*rxt(k,458)*y(k,206) &
                      + .200_r8*rxt(k,389)*y(k,207) + .150_r8*rxt(k,364)*y(k,221)
         mat(k,1274) = .450_r8*rxt(k,385)*y(k,199)
         mat(k,816) = .400_r8*rxt(k,458)*y(k,199)
         mat(k,575) = .200_r8*rxt(k,389)*y(k,199)
         mat(k,2107) = rxt(k,155)*y(k,76) + rxt(k,271)*y(k,80) + rxt(k,248)*y(k,84) &
                      + rxt(k,316)*y(k,85) + 2.000_r8*rxt(k,156)*y(k,228)
         mat(k,1748) = mat(k,1748) + .650_r8*rxt(k,319)*y(k,23) + .500_r8*rxt(k,327) &
                      *y(k,26) + .300_r8*rxt(k,305)*y(k,52) + .600_r8*rxt(k,429) &
                      *y(k,102) + .100_r8*rxt(k,382)*y(k,105) + .500_r8*rxt(k,362) &
                      *y(k,144) + .500_r8*rxt(k,437)*y(k,178)
         mat(k,1081) = .150_r8*rxt(k,364)*y(k,199)
         mat(k,2157) = rxt(k,275)*y(k,72) + 2.000_r8*rxt(k,156)*y(k,213)
      end do
      end subroutine nlnmat09
      subroutine nlnmat10( avec_len, mat, y, rxt )
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
         mat(k,515) = -(rxt(k,581)*y(k,131))
         mat(k,1413) = -rxt(k,581)*y(k,218)
         mat(k,1873) = rxt(k,572)*y(k,209) + rxt(k,573)*y(k,211)
         mat(k,548) = rxt(k,572)*y(k,130)
         mat(k,386) = rxt(k,573)*y(k,130)
         mat(k,337) = -(rxt(k,461)*y(k,199) + rxt(k,462)*y(k,121))
         mat(k,2015) = -rxt(k,461)*y(k,219)
         mat(k,1446) = -rxt(k,462)*y(k,219)
         mat(k,119) = .200_r8*rxt(k,451)*y(k,217)
         mat(k,96) = .140_r8*rxt(k,463)*y(k,217)
         mat(k,237) = rxt(k,466)*y(k,217)
         mat(k,1662) = .200_r8*rxt(k,451)*y(k,65) + .140_r8*rxt(k,463)*y(k,140) &
                      + rxt(k,466)*y(k,141)
         mat(k,718) = -(rxt(k,360)*y(k,199) + rxt(k,361)*y(k,121))
         mat(k,2043) = -rxt(k,360)*y(k,220)
         mat(k,1471) = -rxt(k,361)*y(k,220)
         mat(k,942) = rxt(k,367)*y(k,217)
         mat(k,432) = .500_r8*rxt(k,362)*y(k,217)
         mat(k,1700) = rxt(k,367)*y(k,28) + .500_r8*rxt(k,362)*y(k,144)
         mat(k,1076) = -(rxt(k,363)*y(k,193) + rxt(k,364)*y(k,199) + rxt(k,365) &
                      *y(k,121))
         mat(k,1383) = -rxt(k,363)*y(k,221)
         mat(k,2062) = -rxt(k,364)*y(k,221)
         mat(k,1491) = -rxt(k,365)*y(k,221)
         mat(k,867) = .060_r8*rxt(k,482)*y(k,132)
         mat(k,893) = rxt(k,368)*y(k,217)
         mat(k,841) = .060_r8*rxt(k,485)*y(k,132)
         mat(k,1793) = .060_r8*rxt(k,482)*y(k,5) + .060_r8*rxt(k,485)*y(k,109)
         mat(k,293) = rxt(k,366)*y(k,217)
         mat(k,978) = .150_r8*rxt(k,503)*y(k,217)
         mat(k,1728) = rxt(k,368)*y(k,47) + rxt(k,366)*y(k,145) + .150_r8*rxt(k,503) &
                      *y(k,175)
         mat(k,1041) = -(rxt(k,492)*y(k,193) + rxt(k,493)*y(k,199) + rxt(k,494) &
                      *y(k,121))
         mat(k,1381) = -rxt(k,492)*y(k,222)
         mat(k,2060) = -rxt(k,493)*y(k,222)
         mat(k,1489) = -rxt(k,494)*y(k,222)
         mat(k,1545) = .500_r8*rxt(k,501)*y(k,174)
         mat(k,487) = rxt(k,495)*y(k,217)
         mat(k,920) = .500_r8*rxt(k,501)*y(k,123) + rxt(k,502)*y(k,217)
         mat(k,1726) = rxt(k,495)*y(k,171) + rxt(k,502)*y(k,174)
         mat(k,1019) = -(rxt(k,497)*y(k,193) + rxt(k,498)*y(k,199) + rxt(k,499) &
                      *y(k,121))
         mat(k,1380) = -rxt(k,497)*y(k,223)
         mat(k,2059) = -rxt(k,498)*y(k,223)
         mat(k,1488) = -rxt(k,499)*y(k,223)
         mat(k,865) = rxt(k,483)*y(k,217)
         mat(k,839) = rxt(k,486)*y(k,217)
         mat(k,372) = rxt(k,500)*y(k,217)
         mat(k,1725) = rxt(k,483)*y(k,5) + rxt(k,486)*y(k,109) + rxt(k,500)*y(k,173)
         mat(k,619) = -(rxt(k,468)*y(k,199) + rxt(k,469)*y(k,121))
         mat(k,2037) = -rxt(k,468)*y(k,224)
         mat(k,1463) = -rxt(k,469)*y(k,224)
         mat(k,496) = rxt(k,470)*y(k,217)
         mat(k,115) = .650_r8*rxt(k,471)*y(k,217)
         mat(k,1693) = rxt(k,470)*y(k,176) + .650_r8*rxt(k,471)*y(k,177)
         mat(k,1105) = -(rxt(k,432)*y(k,192) + rxt(k,433)*y(k,193) + rxt(k,434) &
                      *y(k,199) + rxt(k,435)*y(k,121) + rxt(k,436)*y(k,123))
         mat(k,1291) = -rxt(k,432)*y(k,225)
         mat(k,1384) = -rxt(k,433)*y(k,225)
         mat(k,2064) = -rxt(k,434)*y(k,225)
         mat(k,1492) = -rxt(k,435)*y(k,225)
         mat(k,1548) = -rxt(k,436)*y(k,225)
         mat(k,158) = rxt(k,404)*y(k,217)
         mat(k,214) = rxt(k,405)*y(k,217)
         mat(k,70) = rxt(k,406)*y(k,217)
         mat(k,580) = .400_r8*rxt(k,429)*y(k,217)
         mat(k,128) = .500_r8*rxt(k,437)*y(k,217)
         mat(k,1730) = rxt(k,404)*y(k,93) + rxt(k,405)*y(k,95) + rxt(k,406)*y(k,96) &
                      + .400_r8*rxt(k,429)*y(k,102) + .500_r8*rxt(k,437)*y(k,178)
         mat(k,635) = -(rxt(k,474)*y(k,199) + rxt(k,475)*y(k,121))
         mat(k,2038) = -rxt(k,474)*y(k,226)
         mat(k,1464) = -rxt(k,475)*y(k,226)
         mat(k,143) = .560_r8*rxt(k,473)*y(k,217)
         mat(k,592) = rxt(k,476)*y(k,217)
         mat(k,1694) = .560_r8*rxt(k,473)*y(k,179) + rxt(k,476)*y(k,180)
         mat(k,393) = -(rxt(k,477)*y(k,199) + rxt(k,478)*y(k,121))
         mat(k,2022) = -rxt(k,477)*y(k,227)
         mat(k,1451) = -rxt(k,478)*y(k,227)
         mat(k,150) = .300_r8*rxt(k,479)*y(k,217)
         mat(k,317) = rxt(k,480)*y(k,217)
         mat(k,1669) = .300_r8*rxt(k,479)*y(k,181) + rxt(k,480)*y(k,182)
         mat(k,2168) = -(rxt(k,156)*y(k,213) + rxt(k,275)*y(k,72) + rxt(k,517) &
                      *y(k,150))
         mat(k,2118) = -rxt(k,156)*y(k,228)
         mat(k,671) = -rxt(k,275)*y(k,228)
         mat(k,177) = -rxt(k,517)*y(k,228)
         mat(k,212) = rxt(k,329)*y(k,217)
         mat(k,309) = rxt(k,354)*y(k,217)
         mat(k,59) = rxt(k,355)*y(k,217)
         mat(k,2142) = rxt(k,299)*y(k,217)
         mat(k,974) = rxt(k,331)*y(k,217)
         mat(k,897) = rxt(k,368)*y(k,217)
         mat(k,1160) = rxt(k,357)*y(k,217)
         mat(k,458) = rxt(k,337)*y(k,217)
         mat(k,405) = rxt(k,338)*y(k,217)
         mat(k,315) = rxt(k,305)*y(k,217)
         mat(k,1841) = rxt(k,176)*y(k,199)
         mat(k,1098) = rxt(k,181)*y(k,217)
         mat(k,481) = rxt(k,182)*y(k,217)
         mat(k,761) = rxt(k,266)*y(k,217)
         mat(k,1347) = (rxt(k,558)+rxt(k,563))*y(k,90) + (rxt(k,551)+rxt(k,557) &
                       +rxt(k,562))*y(k,91) + rxt(k,237)*y(k,217)
         mat(k,712) = rxt(k,309)*y(k,217)
         mat(k,1970) = rxt(k,213)*y(k,217)
         mat(k,368) = rxt(k,189)*y(k,217)
         mat(k,662) = (rxt(k,558)+rxt(k,563))*y(k,84)
         mat(k,769) = (rxt(k,551)+rxt(k,557)+rxt(k,562))*y(k,84) + rxt(k,240)*y(k,217)
         mat(k,1151) = .500_r8*rxt(k,381)*y(k,217)
         mat(k,52) = rxt(k,520)*y(k,217)
         mat(k,438) = rxt(k,362)*y(k,217)
         mat(k,297) = rxt(k,366)*y(k,217)
         mat(k,2092) = rxt(k,176)*y(k,75) + rxt(k,183)*y(k,217)
         mat(k,1759) = rxt(k,329)*y(k,27) + rxt(k,354)*y(k,29) + rxt(k,355)*y(k,30) &
                      + rxt(k,299)*y(k,41) + rxt(k,331)*y(k,44) + rxt(k,368)*y(k,47) &
                      + rxt(k,357)*y(k,48) + rxt(k,337)*y(k,49) + rxt(k,338)*y(k,50) &
                      + rxt(k,305)*y(k,52) + rxt(k,181)*y(k,76) + rxt(k,182)*y(k,78) &
                      + rxt(k,266)*y(k,80) + rxt(k,237)*y(k,84) + rxt(k,309)*y(k,86) &
                      + rxt(k,213)*y(k,88) + rxt(k,189)*y(k,89) + rxt(k,240)*y(k,91) &
                      + .500_r8*rxt(k,381)*y(k,104) + rxt(k,520)*y(k,119) + rxt(k,362) &
                      *y(k,144) + rxt(k,366)*y(k,145) + rxt(k,183)*y(k,199) &
                      + 2.000_r8*rxt(k,186)*y(k,217)
      end do
      end subroutine nlnmat10
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
         mat(k, 75) = mat(k, 75) + lmat(k, 75)
         mat(k, 76) = mat(k, 76) + lmat(k, 76)
         mat(k, 77) = mat(k, 77) + lmat(k, 77)
         mat(k, 78) = mat(k, 78) + lmat(k, 78)
         mat(k, 79) = lmat(k, 79)
         mat(k, 85) = mat(k, 85) + lmat(k, 85)
         mat(k, 91) = lmat(k, 91)
         mat(k, 92) = lmat(k, 92)
         mat(k, 93) = lmat(k, 93)
         mat(k, 94) = lmat(k, 94)
         mat(k, 95) = mat(k, 95) + lmat(k, 95)
         mat(k, 100) = lmat(k, 100)
         mat(k, 101) = lmat(k, 101)
         mat(k, 102) = mat(k, 102) + lmat(k, 102)
         mat(k, 111) = mat(k, 111) + lmat(k, 111)
         mat(k, 118) = mat(k, 118) + lmat(k, 118)
         mat(k, 123) = lmat(k, 123)
         mat(k, 124) = lmat(k, 124)
         mat(k, 125) = lmat(k, 125)
         mat(k, 126) = lmat(k, 126)
         mat(k, 127) = mat(k, 127) + lmat(k, 127)
         mat(k, 129) = mat(k, 129) + lmat(k, 129)
         mat(k, 130) = mat(k, 130) + lmat(k, 130)
         mat(k, 131) = mat(k, 131) + lmat(k, 131)
         mat(k, 132) = mat(k, 132) + lmat(k, 132)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 153) = lmat(k, 153)
         mat(k, 154) = lmat(k, 154)
         mat(k, 155) = lmat(k, 155)
         mat(k, 156) = mat(k, 156) + lmat(k, 156)
         mat(k, 157) = lmat(k, 157)
         mat(k, 159) = mat(k, 159) + lmat(k, 159)
         mat(k, 160) = lmat(k, 160)
         mat(k, 161) = mat(k, 161) + lmat(k, 161)
         mat(k, 164) = lmat(k, 164)
         mat(k, 165) = lmat(k, 165)
         mat(k, 166) = lmat(k, 166)
         mat(k, 167) = lmat(k, 167)
         mat(k, 168) = lmat(k, 168)
         mat(k, 169) = lmat(k, 169)
         mat(k, 170) = mat(k, 170) + lmat(k, 170)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = lmat(k, 175)
         mat(k, 176) = lmat(k, 176)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = lmat(k, 183)
         mat(k, 185) = mat(k, 185) + lmat(k, 185)
         mat(k, 186) = lmat(k, 186)
         mat(k, 187) = lmat(k, 187)
         mat(k, 188) = lmat(k, 188)
         mat(k, 189) = lmat(k, 189)
         mat(k, 190) = lmat(k, 190)
         mat(k, 191) = lmat(k, 191)
         mat(k, 192) = mat(k, 192) + lmat(k, 192)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = lmat(k, 199)
         mat(k, 200) = lmat(k, 200)
         mat(k, 201) = mat(k, 201) + lmat(k, 201)
         mat(k, 207) = mat(k, 207) + lmat(k, 207)
         mat(k, 213) = mat(k, 213) + lmat(k, 213)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 221) = mat(k, 221) + lmat(k, 221)
         mat(k, 223) = lmat(k, 223)
         mat(k, 224) = lmat(k, 224)
         mat(k, 225) = mat(k, 225) + lmat(k, 225)
         mat(k, 226) = lmat(k, 226)
         mat(k, 227) = lmat(k, 227)
         mat(k, 228) = lmat(k, 228)
         mat(k, 229) = lmat(k, 229)
         mat(k, 230) = lmat(k, 230)
         mat(k, 231) = mat(k, 231) + lmat(k, 231)
         mat(k, 234) = lmat(k, 234)
         mat(k, 235) = mat(k, 235) + lmat(k, 235)
         mat(k, 236) = mat(k, 236) + lmat(k, 236)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = mat(k, 239) + lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 241) = lmat(k, 241)
         mat(k, 242) = mat(k, 242) + lmat(k, 242)
         mat(k, 245) = mat(k, 245) + lmat(k, 245)
         mat(k, 246) = lmat(k, 246)
         mat(k, 247) = mat(k, 247) + lmat(k, 247)
         mat(k, 249) = mat(k, 249) + lmat(k, 249)
         mat(k, 250) = mat(k, 250) + lmat(k, 250)
         mat(k, 251) = lmat(k, 251)
         mat(k, 252) = mat(k, 252) + lmat(k, 252)
         mat(k, 253) = lmat(k, 253)
         mat(k, 255) = mat(k, 255) + lmat(k, 255)
         mat(k, 260) = mat(k, 260) + lmat(k, 260)
         mat(k, 268) = mat(k, 268) + lmat(k, 268)
         mat(k, 272) = lmat(k, 272)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 275) = lmat(k, 275)
         mat(k, 277) = lmat(k, 277)
         mat(k, 278) = mat(k, 278) + lmat(k, 278)
         mat(k, 279) = lmat(k, 279)
         mat(k, 280) = lmat(k, 280)
         mat(k, 281) = lmat(k, 281)
         mat(k, 282) = lmat(k, 282)
         mat(k, 283) = lmat(k, 283)
         mat(k, 284) = lmat(k, 284)
         mat(k, 285) = lmat(k, 285)
         mat(k, 286) = mat(k, 286) + lmat(k, 286)
         mat(k, 287) = lmat(k, 287)
         mat(k, 289) = mat(k, 289) + lmat(k, 289)
         mat(k, 290) = lmat(k, 290)
         mat(k, 291) = lmat(k, 291)
         mat(k, 292) = mat(k, 292) + lmat(k, 292)
         mat(k, 294) = lmat(k, 294)
         mat(k, 295) = mat(k, 295) + lmat(k, 295)
         mat(k, 296) = lmat(k, 296)
         mat(k, 298) = mat(k, 298) + lmat(k, 298)
         mat(k, 299) = lmat(k, 299)
         mat(k, 302) = lmat(k, 302)
         mat(k, 303) = mat(k, 303) + lmat(k, 303)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 306) = lmat(k, 306)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 308) = lmat(k, 308)
         mat(k, 310) = mat(k, 310) + lmat(k, 310)
         mat(k, 312) = mat(k, 312) + lmat(k, 312)
         mat(k, 313) = lmat(k, 313)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 316) = mat(k, 316) + lmat(k, 316)
         mat(k, 318) = lmat(k, 318)
         mat(k, 319) = lmat(k, 319)
         mat(k, 320) = mat(k, 320) + lmat(k, 320)
         mat(k, 321) = lmat(k, 321)
         mat(k, 324) = mat(k, 324) + lmat(k, 324)
         mat(k, 330) = mat(k, 330) + lmat(k, 330)
         mat(k, 334) = mat(k, 334) + lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 337) = mat(k, 337) + lmat(k, 337)
         mat(k, 343) = lmat(k, 343)
         mat(k, 344) = lmat(k, 344)
         mat(k, 345) = lmat(k, 345)
         mat(k, 346) = mat(k, 346) + lmat(k, 346)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 350) = lmat(k, 350)
         mat(k, 351) = mat(k, 351) + lmat(k, 351)
         mat(k, 352) = lmat(k, 352)
         mat(k, 353) = lmat(k, 353)
         mat(k, 354) = mat(k, 354) + lmat(k, 354)
         mat(k, 356) = lmat(k, 356)
         mat(k, 358) = mat(k, 358) + lmat(k, 358)
         mat(k, 362) = mat(k, 362) + lmat(k, 362)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = mat(k, 365) + lmat(k, 365)
         mat(k, 366) = mat(k, 366) + lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 369) = mat(k, 369) + lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 373) = mat(k, 373) + lmat(k, 373)
         mat(k, 374) = lmat(k, 374)
         mat(k, 375) = lmat(k, 375)
         mat(k, 378) = mat(k, 378) + lmat(k, 378)
         mat(k, 385) = mat(k, 385) + lmat(k, 385)
         mat(k, 393) = mat(k, 393) + lmat(k, 393)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 402) = lmat(k, 402)
         mat(k, 403) = mat(k, 403) + lmat(k, 403)
         mat(k, 407) = mat(k, 407) + lmat(k, 407)
         mat(k, 413) = mat(k, 413) + lmat(k, 413)
         mat(k, 415) = lmat(k, 415)
         mat(k, 416) = lmat(k, 416)
         mat(k, 418) = mat(k, 418) + lmat(k, 418)
         mat(k, 423) = mat(k, 423) + lmat(k, 423)
         mat(k, 429) = lmat(k, 429)
         mat(k, 431) = mat(k, 431) + lmat(k, 431)
         mat(k, 433) = lmat(k, 433)
         mat(k, 435) = mat(k, 435) + lmat(k, 435)
         mat(k, 436) = lmat(k, 436)
         mat(k, 437) = lmat(k, 437)
         mat(k, 439) = mat(k, 439) + lmat(k, 439)
         mat(k, 440) = lmat(k, 440)
         mat(k, 441) = lmat(k, 441)
         mat(k, 442) = mat(k, 442) + lmat(k, 442)
         mat(k, 443) = mat(k, 443) + lmat(k, 443)
         mat(k, 445) = lmat(k, 445)
         mat(k, 446) = lmat(k, 446)
         mat(k, 447) = mat(k, 447) + lmat(k, 447)
         mat(k, 455) = mat(k, 455) + lmat(k, 455)
         mat(k, 459) = mat(k, 459) + lmat(k, 459)
         mat(k, 467) = mat(k, 467) + lmat(k, 467)
         mat(k, 469) = lmat(k, 469)
         mat(k, 472) = lmat(k, 472)
         mat(k, 475) = mat(k, 475) + lmat(k, 475)
         mat(k, 478) = mat(k, 478) + lmat(k, 478)
         mat(k, 482) = mat(k, 482) + lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = lmat(k, 484)
         mat(k, 485) = lmat(k, 485)
         mat(k, 486) = lmat(k, 486)
         mat(k, 488) = mat(k, 488) + lmat(k, 488)
         mat(k, 489) = lmat(k, 489)
         mat(k, 490) = lmat(k, 490)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = lmat(k, 492)
         mat(k, 493) = mat(k, 493) + lmat(k, 493)
         mat(k, 494) = lmat(k, 494)
         mat(k, 498) = lmat(k, 498)
         mat(k, 499) = lmat(k, 499)
         mat(k, 500) = mat(k, 500) + lmat(k, 500)
         mat(k, 501) = lmat(k, 501)
         mat(k, 502) = lmat(k, 502)
         mat(k, 503) = lmat(k, 503)
         mat(k, 504) = lmat(k, 504)
         mat(k, 505) = lmat(k, 505)
         mat(k, 506) = mat(k, 506) + lmat(k, 506)
         mat(k, 512) = lmat(k, 512)
         mat(k, 515) = mat(k, 515) + lmat(k, 515)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 517) = lmat(k, 517)
         mat(k, 518) = lmat(k, 518)
         mat(k, 519) = lmat(k, 519)
         mat(k, 522) = mat(k, 522) + lmat(k, 522)
         mat(k, 523) = mat(k, 523) + lmat(k, 523)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 526) = lmat(k, 526)
         mat(k, 527) = lmat(k, 527)
         mat(k, 530) = mat(k, 530) + lmat(k, 530)
         mat(k, 536) = lmat(k, 536)
         mat(k, 537) = mat(k, 537) + lmat(k, 537)
         mat(k, 540) = mat(k, 540) + lmat(k, 540)
         mat(k, 541) = mat(k, 541) + lmat(k, 541)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 545) = lmat(k, 545)
         mat(k, 546) = mat(k, 546) + lmat(k, 546)
         mat(k, 549) = mat(k, 549) + lmat(k, 549)
         mat(k, 559) = lmat(k, 559)
         mat(k, 560) = mat(k, 560) + lmat(k, 560)
         mat(k, 564) = lmat(k, 564)
         mat(k, 565) = lmat(k, 565)
         mat(k, 567) = mat(k, 567) + lmat(k, 567)
         mat(k, 568) = lmat(k, 568)
         mat(k, 569) = lmat(k, 569)
         mat(k, 571) = mat(k, 571) + lmat(k, 571)
         mat(k, 579) = mat(k, 579) + lmat(k, 579)
         mat(k, 581) = lmat(k, 581)
         mat(k, 582) = lmat(k, 582)
         mat(k, 584) = lmat(k, 584)
         mat(k, 585) = lmat(k, 585)
         mat(k, 586) = lmat(k, 586)
         mat(k, 587) = lmat(k, 587)
         mat(k, 588) = lmat(k, 588)
         mat(k, 589) = lmat(k, 589)
         mat(k, 590) = mat(k, 590) + lmat(k, 590)
         mat(k, 595) = lmat(k, 595)
         mat(k, 597) = lmat(k, 597)
         mat(k, 599) = mat(k, 599) + lmat(k, 599)
         mat(k, 600) = lmat(k, 600)
         mat(k, 601) = mat(k, 601) + lmat(k, 601)
         mat(k, 608) = mat(k, 608) + lmat(k, 608)
         mat(k, 619) = mat(k, 619) + lmat(k, 619)
         mat(k, 635) = mat(k, 635) + lmat(k, 635)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 655) = mat(k, 655) + lmat(k, 655)
         mat(k, 657) = lmat(k, 657)
         mat(k, 659) = mat(k, 659) + lmat(k, 659)
         mat(k, 663) = mat(k, 663) + lmat(k, 663)
         mat(k, 664) = mat(k, 664) + lmat(k, 664)
         mat(k, 666) = lmat(k, 666)
         mat(k, 674) = mat(k, 674) + lmat(k, 674)
         mat(k, 682) = mat(k, 682) + lmat(k, 682)
         mat(k, 683) = mat(k, 683) + lmat(k, 683)
         mat(k, 687) = mat(k, 687) + lmat(k, 687)
         mat(k, 691) = mat(k, 691) + lmat(k, 691)
         mat(k, 702) = mat(k, 702) + lmat(k, 702)
         mat(k, 709) = mat(k, 709) + lmat(k, 709)
         mat(k, 713) = mat(k, 713) + lmat(k, 713)
         mat(k, 718) = mat(k, 718) + lmat(k, 718)
         mat(k, 729) = mat(k, 729) + lmat(k, 729)
         mat(k, 740) = lmat(k, 740)
         mat(k, 744) = lmat(k, 744)
         mat(k, 745) = mat(k, 745) + lmat(k, 745)
         mat(k, 754) = mat(k, 754) + lmat(k, 754)
         mat(k, 755) = mat(k, 755) + lmat(k, 755)
         mat(k, 757) = mat(k, 757) + lmat(k, 757)
         mat(k, 763) = mat(k, 763) + lmat(k, 763)
         mat(k, 765) = mat(k, 765) + lmat(k, 765)
         mat(k, 766) = mat(k, 766) + lmat(k, 766)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 772) = lmat(k, 772)
         mat(k, 774) = mat(k, 774) + lmat(k, 774)
         mat(k, 775) = lmat(k, 775)
         mat(k, 782) = mat(k, 782) + lmat(k, 782)
         mat(k, 801) = mat(k, 801) + lmat(k, 801)
         mat(k, 810) = mat(k, 810) + lmat(k, 810)
         mat(k, 819) = lmat(k, 819)
         mat(k, 820) = mat(k, 820) + lmat(k, 820)
         mat(k, 821) = mat(k, 821) + lmat(k, 821)
         mat(k, 823) = mat(k, 823) + lmat(k, 823)
         mat(k, 833) = mat(k, 833) + lmat(k, 833)
         mat(k, 859) = mat(k, 859) + lmat(k, 859)
         mat(k, 881) = mat(k, 881) + lmat(k, 881)
         mat(k, 892) = mat(k, 892) + lmat(k, 892)
         mat(k, 894) = lmat(k, 894)
         mat(k, 895) = lmat(k, 895)
         mat(k, 899) = mat(k, 899) + lmat(k, 899)
         mat(k, 900) = lmat(k, 900)
         mat(k, 902) = lmat(k, 902)
         mat(k, 906) = mat(k, 906) + lmat(k, 906)
         mat(k, 907) = mat(k, 907) + lmat(k, 907)
         mat(k, 909) = mat(k, 909) + lmat(k, 909)
         mat(k, 910) = mat(k, 910) + lmat(k, 910)
         mat(k, 913) = lmat(k, 913)
         mat(k, 914) = mat(k, 914) + lmat(k, 914)
         mat(k, 915) = mat(k, 915) + lmat(k, 915)
         mat(k, 917) = mat(k, 917) + lmat(k, 917)
         mat(k, 918) = lmat(k, 918)
         mat(k, 919) = lmat(k, 919)
         mat(k, 924) = lmat(k, 924)
         mat(k, 925) = lmat(k, 925)
         mat(k, 929) = mat(k, 929) + lmat(k, 929)
         mat(k, 936) = lmat(k, 936)
         mat(k, 937) = lmat(k, 937)
         mat(k, 938) = mat(k, 938) + lmat(k, 938)
         mat(k, 944) = mat(k, 944) + lmat(k, 944)
         mat(k, 961) = mat(k, 961) + lmat(k, 961)
         mat(k, 962) = lmat(k, 962)
         mat(k, 964) = mat(k, 964) + lmat(k, 964)
         mat(k, 965) = mat(k, 965) + lmat(k, 965)
         mat(k, 966) = mat(k, 966) + lmat(k, 966)
         mat(k, 967) = lmat(k, 967)
         mat(k, 969) = lmat(k, 969)
         mat(k, 973) = lmat(k, 973)
         mat(k, 975) = mat(k, 975) + lmat(k, 975)
         mat(k, 976) = mat(k, 976) + lmat(k, 976)
         mat(k, 977) = mat(k, 977) + lmat(k, 977)
         mat(k, 978) = mat(k, 978) + lmat(k, 978)
         mat(k, 979) = mat(k, 979) + lmat(k, 979)
         mat(k, 982) = mat(k, 982) + lmat(k, 982)
         mat(k, 983) = mat(k, 983) + lmat(k, 983)
         mat(k, 986) = lmat(k, 986)
         mat(k, 987) = lmat(k, 987)
         mat(k, 988) = lmat(k, 988)
         mat(k, 989) = mat(k, 989) + lmat(k, 989)
         mat(k, 990) = lmat(k, 990)
         mat(k, 991) = lmat(k, 991)
         mat(k, 993) = lmat(k, 993)
         mat(k, 997) = lmat(k, 997)
         mat(k, 998) = lmat(k, 998)
         mat(k, 999) = mat(k, 999) + lmat(k, 999)
         mat(k,1000) = lmat(k,1000)
         mat(k,1002) = mat(k,1002) + lmat(k,1002)
         mat(k,1006) = mat(k,1006) + lmat(k,1006)
         mat(k,1008) = lmat(k,1008)
         mat(k,1010) = mat(k,1010) + lmat(k,1010)
         mat(k,1011) = lmat(k,1011)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1041) = mat(k,1041) + lmat(k,1041)
         mat(k,1060) = mat(k,1060) + lmat(k,1060)
         mat(k,1076) = mat(k,1076) + lmat(k,1076)
         mat(k,1088) = mat(k,1088) + lmat(k,1088)
         mat(k,1105) = mat(k,1105) + lmat(k,1105)
         mat(k,1125) = mat(k,1125) + lmat(k,1125)
         mat(k,1140) = mat(k,1140) + lmat(k,1140)
         mat(k,1141) = mat(k,1141) + lmat(k,1141)
         mat(k,1144) = mat(k,1144) + lmat(k,1144)
         mat(k,1145) = mat(k,1145) + lmat(k,1145)
         mat(k,1149) = mat(k,1149) + lmat(k,1149)
         mat(k,1150) = mat(k,1150) + lmat(k,1150)
         mat(k,1152) = mat(k,1152) + lmat(k,1152)
         mat(k,1153) = mat(k,1153) + lmat(k,1153)
         mat(k,1154) = mat(k,1154) + lmat(k,1154)
         mat(k,1159) = lmat(k,1159)
         mat(k,1172) = mat(k,1172) + lmat(k,1172)
         mat(k,1188) = lmat(k,1188)
         mat(k,1205) = mat(k,1205) + lmat(k,1205)
         mat(k,1218) = mat(k,1218) + lmat(k,1218)
         mat(k,1229) = mat(k,1229) + lmat(k,1229)
         mat(k,1243) = lmat(k,1243)
         mat(k,1245) = mat(k,1245) + lmat(k,1245)
         mat(k,1249) = mat(k,1249) + lmat(k,1249)
         mat(k,1251) = mat(k,1251) + lmat(k,1251)
         mat(k,1252) = lmat(k,1252)
         mat(k,1269) = mat(k,1269) + lmat(k,1269)
         mat(k,1300) = mat(k,1300) + lmat(k,1300)
         mat(k,1314) = lmat(k,1314)
         mat(k,1316) = mat(k,1316) + lmat(k,1316)
         mat(k,1325) = mat(k,1325) + lmat(k,1325)
         mat(k,1335) = mat(k,1335) + lmat(k,1335)
         mat(k,1338) = mat(k,1338) + lmat(k,1338)
         mat(k,1340) = mat(k,1340) + lmat(k,1340)
         mat(k,1350) = mat(k,1350) + lmat(k,1350)
         mat(k,1394) = mat(k,1394) + lmat(k,1394)
         mat(k,1413) = mat(k,1413) + lmat(k,1413)
         mat(k,1416) = mat(k,1416) + lmat(k,1416)
         mat(k,1418) = lmat(k,1418)
         mat(k,1425) = mat(k,1425) + lmat(k,1425)
         mat(k,1432) = mat(k,1432) + lmat(k,1432)
         mat(k,1436) = mat(k,1436) + lmat(k,1436)
         mat(k,1468) = mat(k,1468) + lmat(k,1468)
         mat(k,1469) = lmat(k,1469)
         mat(k,1473) = mat(k,1473) + lmat(k,1473)
         mat(k,1505) = mat(k,1505) + lmat(k,1505)
         mat(k,1512) = mat(k,1512) + lmat(k,1512)
         mat(k,1561) = mat(k,1561) + lmat(k,1561)
         mat(k,1562) = mat(k,1562) + lmat(k,1562)
         mat(k,1563) = mat(k,1563) + lmat(k,1563)
         mat(k,1569) = mat(k,1569) + lmat(k,1569)
         mat(k,1570) = mat(k,1570) + lmat(k,1570)
         mat(k,1571) = mat(k,1571) + lmat(k,1571)
         mat(k,1590) = mat(k,1590) + lmat(k,1590)
         mat(k,1593) = mat(k,1593) + lmat(k,1593)
         mat(k,1594) = lmat(k,1594)
         mat(k,1595) = lmat(k,1595)
         mat(k,1599) = mat(k,1599) + lmat(k,1599)
         mat(k,1608) = mat(k,1608) + lmat(k,1608)
         mat(k,1622) = lmat(k,1622)
         mat(k,1632) = lmat(k,1632)
         mat(k,1742) = mat(k,1742) + lmat(k,1742)
         mat(k,1743) = mat(k,1743) + lmat(k,1743)
         mat(k,1747) = mat(k,1747) + lmat(k,1747)
         mat(k,1748) = mat(k,1748) + lmat(k,1748)
         mat(k,1756) = mat(k,1756) + lmat(k,1756)
         mat(k,1759) = mat(k,1759) + lmat(k,1759)
         mat(k,1765) = mat(k,1765) + lmat(k,1765)
         mat(k,1806) = mat(k,1806) + lmat(k,1806)
         mat(k,1811) = mat(k,1811) + lmat(k,1811)
         mat(k,1814) = mat(k,1814) + lmat(k,1814)
         mat(k,1819) = mat(k,1819) + lmat(k,1819)
         mat(k,1832) = mat(k,1832) + lmat(k,1832)
         mat(k,1849) = mat(k,1849) + lmat(k,1849)
         mat(k,1857) = mat(k,1857) + lmat(k,1857)
         mat(k,1858) = mat(k,1858) + lmat(k,1858)
         mat(k,1873) = mat(k,1873) + lmat(k,1873)
         mat(k,1879) = lmat(k,1879)
         mat(k,1899) = mat(k,1899) + lmat(k,1899)
         mat(k,1934) = mat(k,1934) + lmat(k,1934)
         mat(k,1937) = mat(k,1937) + lmat(k,1937)
         mat(k,1941) = mat(k,1941) + lmat(k,1941)
         mat(k,1942) = mat(k,1942) + lmat(k,1942)
         mat(k,1943) = mat(k,1943) + lmat(k,1943)
         mat(k,1959) = mat(k,1959) + lmat(k,1959)
         mat(k,1964) = lmat(k,1964)
         mat(k,1965) = mat(k,1965) + lmat(k,1965)
         mat(k,1985) = mat(k,1985) + lmat(k,1985)
         mat(k,1990) = mat(k,1990) + lmat(k,1990)
         mat(k,1993) = mat(k,1993) + lmat(k,1993)
         mat(k,2027) = mat(k,2027) + lmat(k,2027)
         mat(k,2089) = mat(k,2089) + lmat(k,2089)
         mat(k,2093) = mat(k,2093) + lmat(k,2093)
         mat(k,2096) = mat(k,2096) + lmat(k,2096)
         mat(k,2097) = mat(k,2097) + lmat(k,2097)
         mat(k,2099) = mat(k,2099) + lmat(k,2099)
         mat(k,2101) = mat(k,2101) + lmat(k,2101)
         mat(k,2102) = lmat(k,2102)
         mat(k,2103) = mat(k,2103) + lmat(k,2103)
         mat(k,2104) = lmat(k,2104)
         mat(k,2106) = mat(k,2106) + lmat(k,2106)
         mat(k,2107) = mat(k,2107) + lmat(k,2107)
         mat(k,2109) = mat(k,2109) + lmat(k,2109)
         mat(k,2111) = mat(k,2111) + lmat(k,2111)
         mat(k,2115) = lmat(k,2115)
         mat(k,2116) = mat(k,2116) + lmat(k,2116)
         mat(k,2117) = lmat(k,2117)
         mat(k,2122) = mat(k,2122) + lmat(k,2122)
         mat(k,2123) = lmat(k,2123)
         mat(k,2133) = mat(k,2133) + lmat(k,2133)
         mat(k,2141) = mat(k,2141) + lmat(k,2141)
         mat(k,2148) = lmat(k,2148)
         mat(k,2157) = mat(k,2157) + lmat(k,2157)
         mat(k,2159) = lmat(k,2159)
         mat(k,2161) = lmat(k,2161)
         mat(k,2166) = mat(k,2166) + lmat(k,2166)
         mat(k,2168) = mat(k,2168) + lmat(k,2168)
         mat(k, 144) = 0._r8
         mat(k, 145) = 0._r8
         mat(k, 244) = 0._r8
         mat(k, 325) = 0._r8
         mat(k, 327) = 0._r8
         mat(k, 340) = 0._r8
         mat(k, 379) = 0._r8
         mat(k, 382) = 0._r8
         mat(k, 397) = 0._r8
         mat(k, 495) = 0._r8
         mat(k, 497) = 0._r8
         mat(k, 532) = 0._r8
         mat(k, 533) = 0._r8
         mat(k, 538) = 0._r8
         mat(k, 539) = 0._r8
         mat(k, 542) = 0._r8
         mat(k, 554) = 0._r8
         mat(k, 556) = 0._r8
         mat(k, 558) = 0._r8
         mat(k, 561) = 0._r8
         mat(k, 562) = 0._r8
         mat(k, 566) = 0._r8
         mat(k, 591) = 0._r8
         mat(k, 593) = 0._r8
         mat(k, 594) = 0._r8
         mat(k, 596) = 0._r8
         mat(k, 598) = 0._r8
         mat(k, 618) = 0._r8
         mat(k, 620) = 0._r8
         mat(k, 621) = 0._r8
         mat(k, 623) = 0._r8
         mat(k, 626) = 0._r8
         mat(k, 634) = 0._r8
         mat(k, 636) = 0._r8
         mat(k, 637) = 0._r8
         mat(k, 639) = 0._r8
         mat(k, 641) = 0._r8
         mat(k, 643) = 0._r8
         mat(k, 658) = 0._r8
         mat(k, 675) = 0._r8
         mat(k, 676) = 0._r8
         mat(k, 678) = 0._r8
         mat(k, 694) = 0._r8
         mat(k, 695) = 0._r8
         mat(k, 697) = 0._r8
         mat(k, 704) = 0._r8
         mat(k, 705) = 0._r8
         mat(k, 706) = 0._r8
         mat(k, 720) = 0._r8
         mat(k, 723) = 0._r8
         mat(k, 727) = 0._r8
         mat(k, 735) = 0._r8
         mat(k, 739) = 0._r8
         mat(k, 741) = 0._r8
         mat(k, 746) = 0._r8
         mat(k, 753) = 0._r8
         mat(k, 797) = 0._r8
         mat(k, 806) = 0._r8
         mat(k, 832) = 0._r8
         mat(k, 834) = 0._r8
         mat(k, 842) = 0._r8
         mat(k, 849) = 0._r8
         mat(k, 858) = 0._r8
         mat(k, 860) = 0._r8
         mat(k, 868) = 0._r8
         mat(k, 875) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 880) = 0._r8
         mat(k, 884) = 0._r8
         mat(k, 885) = 0._r8
         mat(k, 886) = 0._r8
         mat(k, 888) = 0._r8
         mat(k, 904) = 0._r8
         mat(k, 916) = 0._r8
         mat(k, 927) = 0._r8
         mat(k, 930) = 0._r8
         mat(k, 931) = 0._r8
         mat(k, 932) = 0._r8
         mat(k, 933) = 0._r8
         mat(k, 934) = 0._r8
         mat(k, 939) = 0._r8
         mat(k, 947) = 0._r8
         mat(k, 948) = 0._r8
         mat(k, 949) = 0._r8
         mat(k, 951) = 0._r8
         mat(k, 952) = 0._r8
         mat(k, 956) = 0._r8
         mat(k, 959) = 0._r8
         mat(k, 980) = 0._r8
         mat(k, 984) = 0._r8
         mat(k, 992) = 0._r8
         mat(k, 994) = 0._r8
         mat(k, 995) = 0._r8
         mat(k,1001) = 0._r8
         mat(k,1017) = 0._r8
         mat(k,1018) = 0._r8
         mat(k,1020) = 0._r8
         mat(k,1021) = 0._r8
         mat(k,1022) = 0._r8
         mat(k,1023) = 0._r8
         mat(k,1026) = 0._r8
         mat(k,1027) = 0._r8
         mat(k,1028) = 0._r8
         mat(k,1030) = 0._r8
         mat(k,1033) = 0._r8
         mat(k,1042) = 0._r8
         mat(k,1043) = 0._r8
         mat(k,1046) = 0._r8
         mat(k,1047) = 0._r8
         mat(k,1049) = 0._r8
         mat(k,1052) = 0._r8
         mat(k,1057) = 0._r8
         mat(k,1058) = 0._r8
         mat(k,1059) = 0._r8
         mat(k,1061) = 0._r8
         mat(k,1062) = 0._r8
         mat(k,1066) = 0._r8
         mat(k,1067) = 0._r8
         mat(k,1069) = 0._r8
         mat(k,1072) = 0._r8
         mat(k,1085) = 0._r8
         mat(k,1090) = 0._r8
         mat(k,1091) = 0._r8
         mat(k,1096) = 0._r8
         mat(k,1111) = 0._r8
         mat(k,1112) = 0._r8
         mat(k,1118) = 0._r8
         mat(k,1120) = 0._r8
         mat(k,1122) = 0._r8
         mat(k,1123) = 0._r8
         mat(k,1124) = 0._r8
         mat(k,1126) = 0._r8
         mat(k,1127) = 0._r8
         mat(k,1128) = 0._r8
         mat(k,1133) = 0._r8
         mat(k,1134) = 0._r8
         mat(k,1136) = 0._r8
         mat(k,1148) = 0._r8
         mat(k,1157) = 0._r8
         mat(k,1164) = 0._r8
         mat(k,1165) = 0._r8
         mat(k,1166) = 0._r8
         mat(k,1167) = 0._r8
         mat(k,1168) = 0._r8
         mat(k,1169) = 0._r8
         mat(k,1171) = 0._r8
         mat(k,1173) = 0._r8
         mat(k,1175) = 0._r8
         mat(k,1180) = 0._r8
         mat(k,1181) = 0._r8
         mat(k,1182) = 0._r8
         mat(k,1184) = 0._r8
         mat(k,1187) = 0._r8
         mat(k,1191) = 0._r8
         mat(k,1194) = 0._r8
         mat(k,1195) = 0._r8
         mat(k,1198) = 0._r8
         mat(k,1199) = 0._r8
         mat(k,1201) = 0._r8
         mat(k,1202) = 0._r8
         mat(k,1203) = 0._r8
         mat(k,1206) = 0._r8
         mat(k,1207) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1213) = 0._r8
         mat(k,1214) = 0._r8
         mat(k,1215) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1220) = 0._r8
         mat(k,1227) = 0._r8
         mat(k,1230) = 0._r8
         mat(k,1235) = 0._r8
         mat(k,1236) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1241) = 0._r8
         mat(k,1246) = 0._r8
         mat(k,1250) = 0._r8
         mat(k,1253) = 0._r8
         mat(k,1254) = 0._r8
         mat(k,1255) = 0._r8
         mat(k,1258) = 0._r8
         mat(k,1259) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1263) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1268) = 0._r8
         mat(k,1276) = 0._r8
         mat(k,1278) = 0._r8
         mat(k,1281) = 0._r8
         mat(k,1302) = 0._r8
         mat(k,1304) = 0._r8
         mat(k,1307) = 0._r8
         mat(k,1309) = 0._r8
         mat(k,1312) = 0._r8
         mat(k,1328) = 0._r8
         mat(k,1329) = 0._r8
         mat(k,1336) = 0._r8
         mat(k,1337) = 0._r8
         mat(k,1341) = 0._r8
         mat(k,1343) = 0._r8
         mat(k,1352) = 0._r8
         mat(k,1354) = 0._r8
         mat(k,1356) = 0._r8
         mat(k,1358) = 0._r8
         mat(k,1360) = 0._r8
         mat(k,1369) = 0._r8
         mat(k,1397) = 0._r8
         mat(k,1399) = 0._r8
         mat(k,1400) = 0._r8
         mat(k,1401) = 0._r8
         mat(k,1403) = 0._r8
         mat(k,1407) = 0._r8
         mat(k,1422) = 0._r8
         mat(k,1424) = 0._r8
         mat(k,1427) = 0._r8
         mat(k,1428) = 0._r8
         mat(k,1431) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1434) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1438) = 0._r8
         mat(k,1474) = 0._r8
         mat(k,1510) = 0._r8
         mat(k,1514) = 0._r8
         mat(k,1517) = 0._r8
         mat(k,1519) = 0._r8
         mat(k,1526) = 0._r8
         mat(k,1532) = 0._r8
         mat(k,1540) = 0._r8
         mat(k,1541) = 0._r8
         mat(k,1544) = 0._r8
         mat(k,1547) = 0._r8
         mat(k,1558) = 0._r8
         mat(k,1559) = 0._r8
         mat(k,1560) = 0._r8
         mat(k,1564) = 0._r8
         mat(k,1566) = 0._r8
         mat(k,1567) = 0._r8
         mat(k,1568) = 0._r8
         mat(k,1572) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1576) = 0._r8
         mat(k,1582) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1588) = 0._r8
         mat(k,1589) = 0._r8
         mat(k,1592) = 0._r8
         mat(k,1597) = 0._r8
         mat(k,1603) = 0._r8
         mat(k,1604) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1606) = 0._r8
         mat(k,1609) = 0._r8
         mat(k,1611) = 0._r8
         mat(k,1663) = 0._r8
         mat(k,1682) = 0._r8
         mat(k,1692) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1696) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1731) = 0._r8
         mat(k,1757) = 0._r8
         mat(k,1774) = 0._r8
         mat(k,1777) = 0._r8
         mat(k,1780) = 0._r8
         mat(k,1785) = 0._r8
         mat(k,1789) = 0._r8
         mat(k,1790) = 0._r8
         mat(k,1791) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1794) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1799) = 0._r8
         mat(k,1801) = 0._r8
         mat(k,1816) = 0._r8
         mat(k,1821) = 0._r8
         mat(k,1823) = 0._r8
         mat(k,1824) = 0._r8
         mat(k,1825) = 0._r8
         mat(k,1827) = 0._r8
         mat(k,1828) = 0._r8
         mat(k,1829) = 0._r8
         mat(k,1833) = 0._r8
         mat(k,1835) = 0._r8
         mat(k,1836) = 0._r8
         mat(k,1837) = 0._r8
         mat(k,1839) = 0._r8
         mat(k,1840) = 0._r8
         mat(k,1848) = 0._r8
         mat(k,1852) = 0._r8
         mat(k,1855) = 0._r8
         mat(k,1856) = 0._r8
         mat(k,1860) = 0._r8
         mat(k,1863) = 0._r8
         mat(k,1864) = 0._r8
         mat(k,1865) = 0._r8
         mat(k,1875) = 0._r8
         mat(k,1877) = 0._r8
         mat(k,1883) = 0._r8
         mat(k,1890) = 0._r8
         mat(k,1901) = 0._r8
         mat(k,1904) = 0._r8
         mat(k,1906) = 0._r8
         mat(k,1915) = 0._r8
         mat(k,1918) = 0._r8
         mat(k,1921) = 0._r8
         mat(k,1924) = 0._r8
         mat(k,1925) = 0._r8
         mat(k,1926) = 0._r8
         mat(k,1930) = 0._r8
         mat(k,1931) = 0._r8
         mat(k,1932) = 0._r8
         mat(k,1936) = 0._r8
         mat(k,1939) = 0._r8
         mat(k,1946) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1948) = 0._r8
         mat(k,1951) = 0._r8
         mat(k,1952) = 0._r8
         mat(k,1953) = 0._r8
         mat(k,1954) = 0._r8
         mat(k,1955) = 0._r8
         mat(k,1956) = 0._r8
         mat(k,1958) = 0._r8
         mat(k,1960) = 0._r8
         mat(k,1961) = 0._r8
         mat(k,1962) = 0._r8
         mat(k,1963) = 0._r8
         mat(k,1966) = 0._r8
         mat(k,1967) = 0._r8
         mat(k,1968) = 0._r8
         mat(k,1969) = 0._r8
         mat(k,1984) = 0._r8
         mat(k,1987) = 0._r8
         mat(k,1988) = 0._r8
         mat(k,1992) = 0._r8
         mat(k,1995) = 0._r8
         mat(k,1997) = 0._r8
         mat(k,2016) = 0._r8
         mat(k,2017) = 0._r8
         mat(k,2018) = 0._r8
         mat(k,2030) = 0._r8
         mat(k,2047) = 0._r8
         mat(k,2052) = 0._r8
         mat(k,2053) = 0._r8
         mat(k,2054) = 0._r8
         mat(k,2056) = 0._r8
         mat(k,2058) = 0._r8
         mat(k,2066) = 0._r8
         mat(k,2071) = 0._r8
         mat(k,2087) = 0._r8
         mat(k,2090) = 0._r8
         mat(k,2105) = 0._r8
         mat(k,2112) = 0._r8
         mat(k,2113) = 0._r8
         mat(k,2120) = 0._r8
         mat(k,2126) = 0._r8
         mat(k,2127) = 0._r8
         mat(k,2128) = 0._r8
         mat(k,2132) = 0._r8
         mat(k,2134) = 0._r8
         mat(k,2136) = 0._r8
         mat(k,2138) = 0._r8
         mat(k,2140) = 0._r8
         mat(k,2147) = 0._r8
         mat(k,2149) = 0._r8
         mat(k,2150) = 0._r8
         mat(k,2151) = 0._r8
         mat(k,2152) = 0._r8
         mat(k,2153) = 0._r8
         mat(k,2154) = 0._r8
         mat(k,2155) = 0._r8
         mat(k,2156) = 0._r8
         mat(k,2158) = 0._r8
         mat(k,2160) = 0._r8
         mat(k,2162) = 0._r8
         mat(k,2163) = 0._r8
         mat(k,2164) = 0._r8
         mat(k,2165) = 0._r8
         mat(k,2167) = 0._r8
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
         mat(k, 78) = mat(k, 78) - dti(k)
         mat(k, 85) = mat(k, 85) - dti(k)
         mat(k, 91) = mat(k, 91) - dti(k)
         mat(k, 95) = mat(k, 95) - dti(k)
         mat(k, 100) = mat(k, 100) - dti(k)
         mat(k, 102) = mat(k, 102) - dti(k)
         mat(k, 111) = mat(k, 111) - dti(k)
         mat(k, 118) = mat(k, 118) - dti(k)
         mat(k, 123) = mat(k, 123) - dti(k)
         mat(k, 127) = mat(k, 127) - dti(k)
         mat(k, 130) = mat(k, 130) - dti(k)
         mat(k, 140) = mat(k, 140) - dti(k)
         mat(k, 148) = mat(k, 148) - dti(k)
         mat(k, 153) = mat(k, 153) - dti(k)
         mat(k, 156) = mat(k, 156) - dti(k)
         mat(k, 161) = mat(k, 161) - dti(k)
         mat(k, 164) = mat(k, 164) - dti(k)
         mat(k, 167) = mat(k, 167) - dti(k)
         mat(k, 170) = mat(k, 170) - dti(k)
         mat(k, 174) = mat(k, 174) - dti(k)
         mat(k, 178) = mat(k, 178) - dti(k)
         mat(k, 182) = mat(k, 182) - dti(k)
         mat(k, 186) = mat(k, 186) - dti(k)
         mat(k, 192) = mat(k, 192) - dti(k)
         mat(k, 198) = mat(k, 198) - dti(k)
         mat(k, 201) = mat(k, 201) - dti(k)
         mat(k, 207) = mat(k, 207) - dti(k)
         mat(k, 213) = mat(k, 213) - dti(k)
         mat(k, 216) = mat(k, 216) - dti(k)
         mat(k, 221) = mat(k, 221) - dti(k)
         mat(k, 226) = mat(k, 226) - dti(k)
         mat(k, 231) = mat(k, 231) - dti(k)
         mat(k, 236) = mat(k, 236) - dti(k)
         mat(k, 242) = mat(k, 242) - dti(k)
         mat(k, 247) = mat(k, 247) - dti(k)
         mat(k, 252) = mat(k, 252) - dti(k)
         mat(k, 260) = mat(k, 260) - dti(k)
         mat(k, 268) = mat(k, 268) - dti(k)
         mat(k, 274) = mat(k, 274) - dti(k)
         mat(k, 280) = mat(k, 280) - dti(k)
         mat(k, 286) = mat(k, 286) - dti(k)
         mat(k, 292) = mat(k, 292) - dti(k)
         mat(k, 298) = mat(k, 298) - dti(k)
         mat(k, 304) = mat(k, 304) - dti(k)
         mat(k, 310) = mat(k, 310) - dti(k)
         mat(k, 316) = mat(k, 316) - dti(k)
         mat(k, 324) = mat(k, 324) - dti(k)
         mat(k, 330) = mat(k, 330) - dti(k)
         mat(k, 337) = mat(k, 337) - dti(k)
         mat(k, 343) = mat(k, 343) - dti(k)
         mat(k, 346) = mat(k, 346) - dti(k)
         mat(k, 351) = mat(k, 351) - dti(k)
         mat(k, 358) = mat(k, 358) - dti(k)
         mat(k, 362) = mat(k, 362) - dti(k)
         mat(k, 369) = mat(k, 369) - dti(k)
         mat(k, 378) = mat(k, 378) - dti(k)
         mat(k, 385) = mat(k, 385) - dti(k)
         mat(k, 393) = mat(k, 393) - dti(k)
         mat(k, 400) = mat(k, 400) - dti(k)
         mat(k, 407) = mat(k, 407) - dti(k)
         mat(k, 413) = mat(k, 413) - dti(k)
         mat(k, 418) = mat(k, 418) - dti(k)
         mat(k, 423) = mat(k, 423) - dti(k)
         mat(k, 431) = mat(k, 431) - dti(k)
         mat(k, 439) = mat(k, 439) - dti(k)
         mat(k, 447) = mat(k, 447) - dti(k)
         mat(k, 455) = mat(k, 455) - dti(k)
         mat(k, 459) = mat(k, 459) - dti(k)
         mat(k, 467) = mat(k, 467) - dti(k)
         mat(k, 475) = mat(k, 475) - dti(k)
         mat(k, 482) = mat(k, 482) - dti(k)
         mat(k, 493) = mat(k, 493) - dti(k)
         mat(k, 502) = mat(k, 502) - dti(k)
         mat(k, 506) = mat(k, 506) - dti(k)
         mat(k, 515) = mat(k, 515) - dti(k)
         mat(k, 522) = mat(k, 522) - dti(k)
         mat(k, 530) = mat(k, 530) - dti(k)
         mat(k, 537) = mat(k, 537) - dti(k)
         mat(k, 549) = mat(k, 549) - dti(k)
         mat(k, 560) = mat(k, 560) - dti(k)
         mat(k, 571) = mat(k, 571) - dti(k)
         mat(k, 579) = mat(k, 579) - dti(k)
         mat(k, 590) = mat(k, 590) - dti(k)
         mat(k, 601) = mat(k, 601) - dti(k)
         mat(k, 608) = mat(k, 608) - dti(k)
         mat(k, 619) = mat(k, 619) - dti(k)
         mat(k, 635) = mat(k, 635) - dti(k)
         mat(k, 646) = mat(k, 646) - dti(k)
         mat(k, 655) = mat(k, 655) - dti(k)
         mat(k, 664) = mat(k, 664) - dti(k)
         mat(k, 674) = mat(k, 674) - dti(k)
         mat(k, 682) = mat(k, 682) - dti(k)
         mat(k, 691) = mat(k, 691) - dti(k)
         mat(k, 702) = mat(k, 702) - dti(k)
         mat(k, 709) = mat(k, 709) - dti(k)
         mat(k, 713) = mat(k, 713) - dti(k)
         mat(k, 718) = mat(k, 718) - dti(k)
         mat(k, 729) = mat(k, 729) - dti(k)
         mat(k, 745) = mat(k, 745) - dti(k)
         mat(k, 754) = mat(k, 754) - dti(k)
         mat(k, 763) = mat(k, 763) - dti(k)
         mat(k, 770) = mat(k, 770) - dti(k)
         mat(k, 782) = mat(k, 782) - dti(k)
         mat(k, 801) = mat(k, 801) - dti(k)
         mat(k, 810) = mat(k, 810) - dti(k)
         mat(k, 820) = mat(k, 820) - dti(k)
         mat(k, 833) = mat(k, 833) - dti(k)
         mat(k, 859) = mat(k, 859) - dti(k)
         mat(k, 881) = mat(k, 881) - dti(k)
         mat(k, 892) = mat(k, 892) - dti(k)
         mat(k, 899) = mat(k, 899) - dti(k)
         mat(k, 907) = mat(k, 907) - dti(k)
         mat(k, 917) = mat(k, 917) - dti(k)
         mat(k, 929) = mat(k, 929) - dti(k)
         mat(k, 944) = mat(k, 944) - dti(k)
         mat(k, 961) = mat(k, 961) - dti(k)
         mat(k, 966) = mat(k, 966) - dti(k)
         mat(k, 976) = mat(k, 976) - dti(k)
         mat(k, 989) = mat(k, 989) - dti(k)
         mat(k,1002) = mat(k,1002) - dti(k)
         mat(k,1006) = mat(k,1006) - dti(k)
         mat(k,1019) = mat(k,1019) - dti(k)
         mat(k,1041) = mat(k,1041) - dti(k)
         mat(k,1060) = mat(k,1060) - dti(k)
         mat(k,1076) = mat(k,1076) - dti(k)
         mat(k,1088) = mat(k,1088) - dti(k)
         mat(k,1105) = mat(k,1105) - dti(k)
         mat(k,1125) = mat(k,1125) - dti(k)
         mat(k,1141) = mat(k,1141) - dti(k)
         mat(k,1153) = mat(k,1153) - dti(k)
         mat(k,1172) = mat(k,1172) - dti(k)
         mat(k,1205) = mat(k,1205) - dti(k)
         mat(k,1229) = mat(k,1229) - dti(k)
         mat(k,1249) = mat(k,1249) - dti(k)
         mat(k,1269) = mat(k,1269) - dti(k)
         mat(k,1300) = mat(k,1300) - dti(k)
         mat(k,1316) = mat(k,1316) - dti(k)
         mat(k,1335) = mat(k,1335) - dti(k)
         mat(k,1350) = mat(k,1350) - dti(k)
         mat(k,1394) = mat(k,1394) - dti(k)
         mat(k,1425) = mat(k,1425) - dti(k)
         mat(k,1505) = mat(k,1505) - dti(k)
         mat(k,1563) = mat(k,1563) - dti(k)
         mat(k,1599) = mat(k,1599) - dti(k)
         mat(k,1748) = mat(k,1748) - dti(k)
         mat(k,1811) = mat(k,1811) - dti(k)
         mat(k,1832) = mat(k,1832) - dti(k)
         mat(k,1857) = mat(k,1857) - dti(k)
         mat(k,1899) = mat(k,1899) - dti(k)
         mat(k,1942) = mat(k,1942) - dti(k)
         mat(k,1965) = mat(k,1965) - dti(k)
         mat(k,1993) = mat(k,1993) - dti(k)
         mat(k,2089) = mat(k,2089) - dti(k)
         mat(k,2116) = mat(k,2116) - dti(k)
         mat(k,2141) = mat(k,2141) - dti(k)
         mat(k,2168) = mat(k,2168) - dti(k)
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
      call nlnmat10( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
