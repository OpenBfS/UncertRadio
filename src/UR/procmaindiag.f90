
module PMD

    interface

        module subroutine ProcMainDiag(ncitem)
            integer(4),intent(in)            :: ncitem   ! index of widget in the list of clobj
        end subroutine ProcMainDiag


        module subroutine GamSymList
        end subroutine GamSymList


        module subroutine GamPeakvals
        end subroutine GamPeakvals


        module subroutine AdjustRemoveTVRows(numrows_marked)
            use, intrinsic :: iso_c_binding,        only: c_int
            integer(c_int),intent(in) ::numrows_marked
        end subroutine AdjustRemoveTVRows

    end interface

end module PMD
