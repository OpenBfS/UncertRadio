!#######################################################################
module sym1

    use ur_params,   only: rn

    interface

        !-----------------------------------------------------------------------
        module subroutine symbol1()
            implicit none
        end subroutine symbol1

        module subroutine pointnach(mfall)
            implicit none
            integer, intent(in)    :: mfall    ! 1: called from symbol1;   2: called from rechw1
        end subroutine pointnach

        module subroutine readj_knetto()
        end subroutine readj_knetto

        module subroutine readj_kbrutto()
            implicit none
        end subroutine readj_kbrutto

        module subroutine rs_numbers
        end subroutine rs_numbers

    end interface

end module sym1
