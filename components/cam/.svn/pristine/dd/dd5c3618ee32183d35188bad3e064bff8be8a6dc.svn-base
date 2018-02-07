      module mo_indprd
      use shr_kind_mod, only : r8 => shr_kind_r8
      private
      public :: indprd
      contains
      subroutine indprd( class, prod, nprod, y, extfrc, rxt, chnkpnts )
      use chem_mods, only : gas_pcnst, extcnt, rxntot
      implicit none
!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: class
      integer, intent(in) :: chnkpnts
      integer, intent(in) :: nprod
      real(r8), intent(in) :: y(chnkpnts,gas_pcnst)
      real(r8), intent(in) :: rxt(chnkpnts,rxntot)
      real(r8), intent(in) :: extfrc(chnkpnts,extcnt)
      real(r8), intent(inout) :: prod(chnkpnts,nprod)
!--------------------------------------------------------------------
! ... "independent" production for Explicit species
!--------------------------------------------------------------------
      if( class == 1 ) then
         prod(:,1) = 0._r8
         prod(:,2) = 0._r8
         prod(:,3) = 0._r8
         prod(:,4) = 0._r8
         prod(:,5) = 0._r8
         prod(:,6) = 0._r8
         prod(:,7) = 0._r8
         prod(:,8) = 0._r8
         prod(:,9) = 0._r8
         prod(:,10) = 0._r8
         prod(:,11) = 0._r8
         prod(:,12) = 0._r8
         prod(:,13) = 0._r8
         prod(:,14) =.100_r8*rxt(:,350)*y(:,132)*y(:,28)
         prod(:,15) = 0._r8
         prod(:,16) = 0._r8
         prod(:,17) = (rxt(:,307)*y(:,61) +rxt(:,309)*y(:,86) +rxt(:,317)*y(:,61) + &
                 rxt(:,337)*y(:,49) +.500_r8*rxt(:,338)*y(:,50) + &
                 .800_r8*rxt(:,343)*y(:,73) +rxt(:,344)*y(:,74) + &
                 .500_r8*rxt(:,393)*y(:,108) +1.800_r8*rxt(:,503)*y(:,175))*y(:,217) &
                  + (2.000_r8*rxt(:,333)*y(:,192) +.900_r8*rxt(:,334)*y(:,193) + &
                 rxt(:,336)*y(:,121) +2.000_r8*rxt(:,383)*y(:,205) + &
                 rxt(:,407)*y(:,201) +rxt(:,432)*y(:,225))*y(:,192) &
                  + (.200_r8*rxt(:,350)*y(:,28) +.100_r8*rxt(:,394)*y(:,110) + &
                 .270_r8*rxt(:,482)*y(:,5) +.270_r8*rxt(:,485)*y(:,109))*y(:,132) &
                  + (rxt(:,384)*y(:,193) +.450_r8*rxt(:,385)*y(:,199) + &
                 2.000_r8*rxt(:,386)*y(:,205))*y(:,205) &
                  + (.500_r8*rxt(:,492)*y(:,193) +.900_r8*rxt(:,494)*y(:,121)) &
                 *y(:,222) +rxt(:,38)*y(:,50) +.400_r8*rxt(:,61)*y(:,137) +rxt(:,66) &
                 *y(:,171) +.800_r8*rxt(:,70)*y(:,175)
         prod(:,18) = 0._r8
         prod(:,19) = 0._r8
         prod(:,20) = 0._r8
         prod(:,21) = 0._r8
         prod(:,22) =rxt(:,193)*y(:,122)*y(:,111)
         prod(:,23) = 0._r8
         prod(:,24) = 0._r8
         prod(:,25) =rxt(:,520)*y(:,217)*y(:,119) +rxt(:,535)*y(:,120)
         prod(:,26) = (rxt(:,454)*y(:,194) +rxt(:,457)*y(:,204) +rxt(:,460)*y(:,206) + &
                 rxt(:,464)*y(:,139))*y(:,122) +.500_r8*rxt(:,393)*y(:,217)*y(:,108) &
                  +.200_r8*rxt(:,489)*y(:,212)*y(:,121) +.500_r8*rxt(:,501)*y(:,174) &
                 *y(:,123)
!--------------------------------------------------------------------
! ... "independent" production for Implicit species
!--------------------------------------------------------------------
      else if( class == 4 ) then
         prod(:,124) = 0._r8
         prod(:,126) = 0._r8
         prod(:,1) = + extfrc(:,6)
         prod(:,2) = + extfrc(:,7)
         prod(:,154) = 0._r8
         prod(:,48) = 0._r8
         prod(:,85) = 0._r8
         prod(:,49) = 0._r8
         prod(:,86) = 0._r8
         prod(:,96) = 0._r8
         prod(:,71) = 0._r8
         prod(:,119) = 0._r8
         prod(:,77) = 0._r8
         prod(:,62) = 0._r8
         prod(:,83) = 0._r8
         prod(:,185) =rxt(:,80)*y(:,33) +rxt(:,81)*y(:,34) +2.000_r8*rxt(:,87)*y(:,40) &
                  +rxt(:,88)*y(:,42) +3.000_r8*rxt(:,91)*y(:,54) +2.000_r8*rxt(:,99) &
                 *y(:,77)
         prod(:,64) = 0._r8
         prod(:,194) = 0._r8
         prod(:,111) = 0._r8
         prod(:,65) = 0._r8
         prod(:,80) = 0._r8
         prod(:,72) = 0._r8
         prod(:,112) = 0._r8
         prod(:,67) = 0._r8
         prod(:,81) = 0._r8
         prod(:,73) = 0._r8
         prod(:,161) = 0._r8
         prod(:,90) = 0._r8
         prod(:,40) = 0._r8
         prod(:,68) = 0._r8
         prod(:,201) =.180_r8*rxt(:,41)*y(:,53)
         prod(:,163) = 0._r8
         prod(:,39) = 0._r8
         prod(:,156) = 0._r8
         prod(:,176) = 0._r8
         prod(:,113) = 0._r8
         prod(:,105) = 0._r8
         prod(:,142) = 0._r8
         prod(:,91) = 0._r8
         prod(:,190) =4.000_r8*rxt(:,79)*y(:,32) +rxt(:,80)*y(:,33) &
                  +2.000_r8*rxt(:,82)*y(:,35) +2.000_r8*rxt(:,83)*y(:,36) &
                  +2.000_r8*rxt(:,84)*y(:,37) +rxt(:,85)*y(:,38) +2.000_r8*rxt(:,86) &
                 *y(:,39) +3.000_r8*rxt(:,89)*y(:,43) +rxt(:,90)*y(:,45) +rxt(:,101) &
                 *y(:,81) +rxt(:,102)*y(:,82) +rxt(:,103)*y(:,83)
         prod(:,51) = 0._r8
         prod(:,37) = 0._r8
         prod(:,198) = 0._r8
         prod(:,158) = 0._r8
         prod(:,166) = (rxt(:,42) +rxt(:,110))*y(:,62) +.380_r8*rxt(:,41)*y(:,53) &
                  + extfrc(:,3)
         prod(:,41) =rxt(:,80)*y(:,33) +rxt(:,81)*y(:,34) +rxt(:,83)*y(:,36) &
                  +2.000_r8*rxt(:,84)*y(:,37) +2.000_r8*rxt(:,85)*y(:,38) +rxt(:,86) &
                 *y(:,39) +2.000_r8*rxt(:,99)*y(:,77) +rxt(:,102)*y(:,82) +rxt(:,103) &
                 *y(:,83)
         prod(:,57) =rxt(:,82)*y(:,35) +rxt(:,83)*y(:,36) +rxt(:,101)*y(:,81)
         prod(:,54) = 0._r8
         prod(:,70) = 0._r8
         prod(:,3) = 0._r8
         prod(:,4) = 0._r8
         prod(:,5) = 0._r8
         prod(:,6) = 0._r8
         prod(:,42) = 0._r8
         prod(:,136) =rxt(:,81)*y(:,34) +rxt(:,85)*y(:,38)
         prod(:,162) = 0._r8
         prod(:,152) = 0._r8
         prod(:,193) = (rxt(:,40) +.330_r8*rxt(:,41))*y(:,53)
         prod(:,172) =1.440_r8*rxt(:,41)*y(:,53)
         prod(:,116) = 0._r8
         prod(:,43) = 0._r8
         prod(:,146) = 0._r8
         prod(:,184) = 0._r8
         prod(:,52) = 0._r8
         prod(:,141) = 0._r8
         prod(:,60) = 0._r8
         prod(:,197) = 0._r8
         prod(:,100) = 0._r8
         prod(:,135) = 0._r8
         prod(:,147) = 0._r8
         prod(:,165) = 0._r8
         prod(:,61) = 0._r8
         prod(:,167) = 0._r8
         prod(:,74) = 0._r8
         prod(:,44) = 0._r8
         prod(:,149) = 0._r8
         prod(:,120) = 0._r8
         prod(:,109) = 0._r8
         prod(:,174) = 0._r8
         prod(:,89) = 0._r8
         prod(:,128) = 0._r8
         prod(:,35) = 0._r8
         prod(:,175) = 0._r8
         prod(:,75) = 0._r8
         prod(:,107) = 0._r8
         prod(:,76) = 0._r8
         prod(:,115) = 0._r8
         prod(:,153) = 0._r8
         prod(:,180) = 0._r8
         prod(:,145) = (.800_r8*rxt(:,112) +rxt(:,115) +rxt(:,116) + &
                 .800_r8*rxt(:,118)) + extfrc(:,15)
         prod(:,69) = 0._r8
         prod(:,84) = 0._r8
         prod(:,160) = 0._r8
         prod(:,7) = 0._r8
         prod(:,8) = 0._r8
         prod(:,9) = 0._r8
         prod(:,38) = 0._r8
         prod(:,10) = 0._r8
         prod(:,188) = + extfrc(:,1)
         prod(:,196) = + extfrc(:,2)
         prod(:,189) = 0._r8
         prod(:,148) = 0._r8
         prod(:,87) = 0._r8
         prod(:,11) = + extfrc(:,12)
         prod(:,12) = + extfrc(:,13)
         prod(:,13) = 0._r8
         prod(:,14) = + extfrc(:,14)
         prod(:,195) = (rxt(:,42) +rxt(:,110))*y(:,62) +.180_r8*rxt(:,41)*y(:,53)
         prod(:,187) = 0._r8
         prod(:,192) = 0._r8
         prod(:,78) = 0._r8
         prod(:,82) = 0._r8
         prod(:,63) = 0._r8
         prod(:,98) = 0._r8
         prod(:,45) = 0._r8
         prod(:,99) = 0._r8
         prod(:,50) = 0._r8
         prod(:,79) = 0._r8
         prod(:,15) = + extfrc(:,8)
         prod(:,16) = + extfrc(:,9)
         prod(:,110) = 0._r8
         prod(:,88) = 0._r8
         prod(:,130) = 0._r8
         prod(:,183) = 0._r8
         prod(:,157) = + extfrc(:,4)
         prod(:,66) = 0._r8
         prod(:,17) = + extfrc(:,10)
         prod(:,18) = + extfrc(:,11)
         prod(:,19) = 0._r8
         prod(:,20) = 0._r8
         prod(:,21) = 0._r8
         prod(:,22) = 0._r8
         prod(:,23) = 0._r8
         prod(:,24) = 0._r8
         prod(:,25) = 0._r8
         prod(:,26) = 0._r8
         prod(:,27) = 0._r8
         prod(:,28) = 0._r8
         prod(:,29) = 0._r8
         prod(:,30) = 0._r8
         prod(:,31) = 0._r8
         prod(:,32) = 0._r8
         prod(:,33) = 0._r8
         prod(:,34) = 0._r8
         prod(:,36) = + extfrc(:,5)
         prod(:,55) = 0._r8
         prod(:,117) = 0._r8
         prod(:,122) = 0._r8
         prod(:,101) = 0._r8
         prod(:,159) = 0._r8
         prod(:,164) = 0._r8
         prod(:,118) = 0._r8
         prod(:,53) = 0._r8
         prod(:,56) = 0._r8
         prod(:,58) = 0._r8
         prod(:,129) = 0._r8
         prod(:,59) = 0._r8
         prod(:,92) = 0._r8
         prod(:,106) = 0._r8
         prod(:,155) = 0._r8
         prod(:,102) = 0._r8
         prod(:,93) = 0._r8
         prod(:,150) = 0._r8
         prod(:,144) = 0._r8
         prod(:,123) = 0._r8
         prod(:,182) = 0._r8
         prod(:,186) =rxt(:,88)*y(:,42) +rxt(:,90)*y(:,45) +rxt(:,40)*y(:,53)
         prod(:,134) = 0._r8
         prod(:,140) = (rxt(:,113) +rxt(:,114) +rxt(:,115) +rxt(:,116) +rxt(:,117) + &
                 rxt(:,119)) + extfrc(:,22)
         prod(:,114) = 0._r8
         prod(:,97) = 0._r8
         prod(:,137) = 0._r8
         prod(:,199) = 0._r8
         prod(:,94) = 0._r8
         prod(:,177) = 0._r8
         prod(:,178) = 0._r8
         prod(:,179) = 0._r8
         prod(:,131) = 0._r8
         prod(:,181) = 0._r8
         prod(:,151) = 0._r8
         prod(:,127) = 0._r8
         prod(:,108) = (1.200_r8*rxt(:,112) +rxt(:,113) +rxt(:,117) + &
                 1.200_r8*rxt(:,118)) + extfrc(:,21)
         prod(:,125) = (rxt(:,114) +rxt(:,119)) + extfrc(:,19)
         prod(:,139) = 0._r8
         prod(:,103) = (rxt(:,113) +rxt(:,115) +rxt(:,116) +rxt(:,117)) + extfrc(:,20)
         prod(:,170) = 0._r8
         prod(:,200) =rxt(:,12)*y(:,112)
         prod(:,46) = 0._r8
         prod(:,47) = 0._r8
         prod(:,138) = + extfrc(:,18)
         prod(:,191) =.330_r8*rxt(:,41)*y(:,53) + extfrc(:,16)
         prod(:,121) = + extfrc(:,17)
         prod(:,95) = 0._r8
         prod(:,143) = 0._r8
         prod(:,171) = 0._r8
         prod(:,169) = 0._r8
         prod(:,168) = 0._r8
         prod(:,132) = 0._r8
         prod(:,173) = 0._r8
         prod(:,133) = 0._r8
         prod(:,104) = 0._r8
         prod(:,202) =.050_r8*rxt(:,41)*y(:,53)
      end if
      end subroutine indprd
      end module mo_indprd
