module chem_prod_loss_diags
  use shr_kind_mod, only : r8 => shr_kind_r8
  use chem_mods, only : clscnt4, gas_pcnst, clsmap, permute
  use ppgrid, only : pver
  use chem_mods, only : rxntot
  use cam_history, only : addfld, outfld
  use mo_tracname, only : solsym

  implicit none

  private
  public :: chem_prod_loss_diags_init
  public :: chem_prod_loss_diags_out

contains

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  subroutine chem_prod_loss_diags_init

    integer :: i,j

    do i = 1,clscnt4
       j = clsmap(i,4)
       call addfld( trim(solsym(j))//'_CHMP', (/ 'lev' /), 'I', '/cm3/s', 'chemical production rate' )
       call addfld( trim(solsym(j))//'_CHML', (/ 'lev' /), 'I', '/cm3/s', 'chemical loss rate' )
    enddo

    call addfld('H_PEROX_CHMP', (/ 'lev' /), 'I', '/cm3/s', 'total ROOH production rate' ) !PJY changed "RO2" to "ROOH"

  end subroutine chem_prod_loss_diags_init

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  subroutine chem_prod_loss_diags_out( ncol, lchnk, base_sol, reaction_rates, prod_in, loss_in, xhnm )

    integer,  intent(in) :: ncol, lchnk
    real(r8), intent(in) :: base_sol(ncol,pver,gas_pcnst)
    real(r8), intent(in) :: reaction_rates(ncol,pver,max(1,rxntot))
    real(r8), intent(in) :: prod_in(ncol,pver,max(1,clscnt4))
    real(r8), intent(in) :: loss_in(ncol,pver,max(1,clscnt4))
    real(r8), intent(in) :: xhnm(ncol,pver)

    real(r8), dimension(ncol,pver,max(1,clscnt4)) :: prod_out, loss_out
    real(r8), dimension(ncol,pver) :: prod_hydrogen_peroxides_out
    integer :: lev, i, k, j, m

    level_loop : do lev = 1,pver
       column_loop : do i = 1,ncol

          !-----------------------------------------------------------------------
          ! ... Prod/Loss history buffers...
          !-----------------------------------------------------------------------
          cls_loop2: do k = 1,clscnt4
             j = clsmap(k,4)
             m = permute(k,4)
             prod_out(i,lev,k) = prod_in(i,lev,m)
             loss_out(i,lev,k) = loss_in(i,lev,m)
          end do cls_loop2
       end do column_loop
    end do level_loop

    prod_hydrogen_peroxides_out(:,:) = 0._r8

    do i = 1,clscnt4
       j = clsmap(i,4)
       prod_out(:,:,i) = prod_out(:,:,i)*xhnm(:,:)
       loss_out(:,:,i) = loss_out(:,:,i)*xhnm(:,:)
       call outfld( trim(solsym(j))//'_CHMP', prod_out(:,:,i), ncol, lchnk )
       call outfld( trim(solsym(j))//'_CHML', loss_out(:,:,i), ncol, lchnk )
       !
       ! added code for ROOH production !PJY not "RO2 production"
       !
       if ( trim(solsym(j)) == 'ALKOOH' &
            .or.trim(solsym(j)) == 'C2H5OOH' &
            .or.trim(solsym(j)) == 'CH3OOH' & !PJY added this
            .or.trim(solsym(j)) == 'CH3COOH' &
            .or.trim(solsym(j)) == 'CH3COOOH' &
            .or.trim(solsym(j)) == 'C3H7OOH' & !PJY corrected this (from CH3H7OOH)
            .or.trim(solsym(j)) == 'EOOH' &
            .or.trim(solsym(j)) == 'ISOPOOH' &
            .or.trim(solsym(j)) == 'MACROOH' &
            .or.trim(solsym(j)) == 'MEKOOH' &
            .or.trim(solsym(j)) == 'POOH' &
            .or.trim(solsym(j)) == 'ROOH' &
            .or.trim(solsym(j)) == 'TERPOOH' &
            .or.trim(solsym(j)) == 'TOLOOH' &
            .or.trim(solsym(j)) == 'XOOH' ) then
          prod_hydrogen_peroxides_out(:,:) = prod_hydrogen_peroxides_out(:,:) + prod_out(:,:,i)
       end if
    enddo

    call outfld( 'H_PEROX_CHMP', prod_hydrogen_peroxides_out(:,:), ncol, lchnk )

  end subroutine chem_prod_loss_diags_out

end module chem_prod_loss_diags

