!-------------------------------------------------------------------------------------------------!
! This file is part of UncertRadio.
!
!    UncertRadio is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    UncertRadio is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with UncertRadio. If not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!
module URdate
    use UR_types
    implicit none

contains
    !#######################################################################
    !
    !!  datdif6 calculates the time difference between two (date and time)
    !  integer arrays, which is returned as a real(rn) number in the unit
    !  of days

    !  array ad : 1. date and time (integer*4)
    !  array ed : 2. date and time (integer*4)
    !  Meaning:      ad(1): day
    !                ad(2): month
    !                ad(3): year (given as <100 or >1900,
    !                       since 2000 : given as <30 or >=2000)
    !                ad(4): hour
    !                ad(5): minutes
    !                ad(6): seconds
    !  The same applies to the array ed.
    !-----------------------------------------------------------------------
    real(rn) function DATDIF6 (AD,ED)

        implicit none

        integer, intent(in)  :: ad(6)
        integer, intent(in)  :: ed(6)

        integer              :: k

        real(rn)             :: F(2)
        real(rn)             :: X(2,6)
        !-----------------------------------------------------------------------
        x(1,1:6) = real(ad(1:6),rn)
        x(2,1:6) = real(ed(1:6),rn)
        do k=1,2
            if(x(k,3) < 100._rn) then
                if(x(k,3) >= 30._rn) then
                    x(k,3) = x(k,3) + 1900._rn
                else
                    x(k,3) = x(k,3) + 2000._rn
                end if
            end if
            f(k) = 365._rn*x(k,3) + x(k,1) + 31._rn*(x(k,2)-1._rn)
            if(x(k,2) >= 3._rn) then
                f(k) = f(k) - real(int(.4_rn*x(k,2)+2.3_rn,4),rn)
                f(k) = f(k) + real(int(x(k,3)/4._rn,4),rn)
                f(k) = f(k) - real(int(.75_rn*real(int(x(k,3)/100._rn,4)+1._rn,rn),4),rn)
            else
                f(k) = f(k) + real(int((x(k,3)-1._rn)/4._rn,4),rn)
                f(k) = f(k) - real(int(.75_rn*real(int((x(k,3)-1._rn)/100._rn,4)+1._rn,rn),4),rn)
            end if
        end do
        datdif6 = f(2) - f(1) + (x(2,4)-x(1,4))/24._rn  &    ! hours
            + (x(2,5)-x(1,5))/1440._rn &                     ! minutes
            + (x(2,6)-x(1,6))/1440._rn/60._rn                ! seconds
        return
        !------------------------------------------------------------------------------------------
        ! do i=1,6
        !     x(1,i) = real(ad(i),rn)
        !     x(2,i) = real(ed(i),rn)
        ! end do

        ! xx = x(1,3)
        ! if(x(1,3) < 100._rn) then
        !     if(x(1,3) >= 30._rn) xx = x(1,3) + 1900._rn
        !     if(x(1,3) < 30._rn)  xx = x(1,3) + 2000._rn
        ! end if
        ! x(1,3) = xx

        ! xx = x(2,3)
        ! if(x(2,3) < 100._rn) then
        !     if(x(2,3) >= 30._rn) xx = x(2,3) + 1900._rn
        !     if(x(2,3) < 30._rn)  xx = x(2,3) + 2000._rn
        ! end if
        ! x(2,3) = xx

        ! do k=1,2
        !     if(x(k,2) >= 3._rn) then
        !         f(k) = 365._rn*x(k,3) + x(k,1) + 31._rn*(x(k,2)-1._rn) - real(int(.4_rn*x(k,2)+2.3_rn,4),rn)
        !         f(k) = f(k) + real(int(x(k,3)/4._rn,4),rn)
        !         f(k) = f(k) - real(int(.75_rn*real(int(x(k,3)/100._rn,4)+1._rn,rn),4),rn)
        !     else
        !         f(k) = 365._rn*x(k,3) + x(k,1) + 31._rn*(x(k,2)-1._rn) + real(int((x(k,3)-1._rn)/4._rn,4),rn)
        !         f(k) = f(k) - real(int(.75_rn*real(int((x(k,3)-1._rn)/100._rn,4)+1._rn,rn),4),rn)
        !     end if
        ! end do

        ! datdif6 = f(2) - f(1) + (x(2,4)-x(1,4))/24._rn  &    ! Stunden
        !     + (x(2,5)-x(1,5))/1440._rn &                     ! minuten
        !     + (x(2,6)-x(1,6))/1440._rn/60._rn                ! sekunden
        !-----------------------------------------------------------------------
        ! return
    end function datdif6
    !
    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    function get_formated_date_time() result(date_time)
        implicit none
        character(len=19) :: date_time
        integer           :: val(8)

        call date_and_time(values=val)

        write(date_time,'(2(i2.2,''.''),i4.4,1X,2(i2.2,'':''),i2.2)') &
              val(3), val(2), val(1), val(5), val(6), val(7)

    end function get_formated_date_time

    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ! subroutine datim(z)

    !     implicit none

    !     integer, intent(out)       :: z(9)

    !     integer  :: z8, zz
    !     integer  :: vals(8)
    !     !-----------------------------------------------------------------------
    !     !     z(3) = Sekunden
    !     !     z(4) = Minuten
    !     !     z(5) = Stunden
    !     !     z(6) = Tag
    !     !     z(7) = Monat
    !     !     z(8) = Jahr
    !     !
    !     !     Diese Routine simuliert die entsprechende Datum/Zeit-Routine des
    !     !     FTN-Fortrans der R30.
    !     !
    !     !------------------------------------------------------------------------
    !     call date_and_time(values=vals)

    !     z(8) = int(vals(1),4)    ! Jahr
    !     z(7) = int(vals(2),4)    ! Monat
    !     z(6) = int(vals(3),4)    ! Tag

    !     z(5) = int(vals(5),4)    ! Stunde
    !     z(4) = int(vals(6),4)    ! Minute
    !     z(3) = int(vals(7),4)    ! Sekunde

    !     ! if(z(8).lt.100) z(8) = z(8) + 1900    ! Bis Ende 1999
    !     z8 = z(8)                               ! Ab 2000
    !     if(z(8) < int(100,4)) then
    !         if(z(8) >= int(30,4)) then
    !             z8 = z8 + int(1900,4)
    !         else
    !             z8 = z8 + int(2000,4)
    !         end if
    !     end if
    !     z(8) = z8
    !     return
    ! end subroutine datim

!############################################################################

    ! subroutine Clockm(seconds)
    !     use UR_params,     only: rn

    !     implicit none

    !     real(rn),intent(out)     :: seconds

    !     integer     :: z(8), zz
    !     integer     :: vals(8)

    !     call date_and_time(values=vals)

    !     z(8) = int(vals(1),4)    ! Jahr
    !     z(7) = int(vals(2),4)    ! Monat
    !     z(6) = int(vals(3),4)    ! Tag

    !     z(5) = int(vals(5),4)    ! Stunde
    !     z(4) = int(vals(6),4)    ! Minute
    !     z(3) = int(vals(7),4)    ! Sekunde
    !     zz   = int(vals(8),4)    ! Millisekunden

    !     seconds = real( z(5)*3600 + z(4)*60 + z(3) + int(real(zz, rn)/1000._rn, 4) , rn)

    ! end subroutine Clockm

!#######################################################################
end module URdate
