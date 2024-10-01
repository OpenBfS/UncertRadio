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

subroutine InitTreeViews()

    !  Initiates the length of tables (GTK TreeViews), which is done by initiating
    !  the "models" behind the treeviews, the GTK liststores.
    !
    !  Copyright (C) 2014-2024  Günter Kanisch

    use, intrinsic :: iso_c_binding
    use gtk,                     only: gtk_list_store_clear,gtk_list_store_append, &
                                       gtk_list_store_set_value,gtk_cell_renderer_toggle_get_active
    use g,                       only: g_value_set_long,g_value_init, g_value_set_string, &
                                       g_value_set_boolean
    use gtk_sup,                 only: clear_gtktreeiter,c_f_logical
    use gtk_hl,                  only: hl_gtk_listn_set_cell

    use UR_types
    use UR_gtk_variables,        only: iter
    use UR_Variables,            only: progstart_on
    use UR_Gleich,               only: ncovmx,missingval,charv
    use UR_Linft,                only: ndatmax
    use UR_gini
    use Rout,                    only: WTreeViewPutIntArray,WTreeViewPutStrArray,     &
                                       WTreeViewPutDoubleArray,WTreeViewPutCheckArray

    use gui_functions,           only: idpt

    use UR_Gspk1Fit,             only: kdatmax
    use color_theme

    implicit none
    type(c_ptr)                  :: liststore,renderer
    integer                      :: rowmax,k,icol
    integer   ,allocatable       :: num(:),iuval(:)
    real(rn),allocatable         :: xval(:)
    type(charv),allocatable      :: sval(:)
    integer(c_int)               :: kcol
    integer(c_long)              :: i
    logical                      :: bcheck

    rowmax = 200

    !tree: treeview1
    liststore = idpt('liststore_symtable')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1,rowmax
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))
        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,10
            kcol = k - 1
            if(k <= 5) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else

                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        end do
    end do

    !tree: treeview2
    liststore = idpt('liststore_valunc')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1,rowmax
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))
        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,22
            if(.not.progstart_on .and. k > 11) exit
            ! columns 1-11:  valunc fields
            ! columns 12-18:  background color
            ! columns 19-22: full-precision Felder der real(8)-Werte;  <-- not used
            kcol = k - 1
            if(k >= 12 .and. k <= 22) then

                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do

    !tree: treeview3
    liststore = idpt('liststore_covtable')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1, ncovmx
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))
        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2, 12
            if(.not.progstart_on .and. k > 6) exit
            kcol = k - 1
            if(k <= 6) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else
                call g_value_set_string(pstring, get_color_string('table_bg') //c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do

    !tree: treeview4
    liststore = idpt('liststore_budget')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1,rowmax
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))
        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,16
            if(.not.progstart_on .and. k > 8) exit
            kcol = k - 1
            if(k  <= 16) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else

                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do

    if(allocated(sval)) deallocate(sval)
    allocate(sval(ndatmax))
    if(allocated(num)) deallocate(num)
    allocate(num(ndatmax))
    if(allocated(iuval)) deallocate(iuval)
    allocate(iuval(ndatmax))
    if(allocated(xval)) deallocate(xval)
    allocate(xval(ndatmax))

    liststore = idpt('liststore_Decay')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1,ndatmax            ! 60
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))

        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,24
            if(.not.progstart_on .and. k > 12) exit
            kcol = k - 1
            if(k <= 12) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else
                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do
    do i=1,ndatmax         !  60
        num(i) = i
        sval(i)%s = ' '
        xval(i) = missingval
    enddo
    call WTreeViewPutIntArray('treeview5', 1, ndatmax, num)
    call WTreeViewPutStrArray('treeview5', 2, ndatmax, sval)
    do icol=3,12
        call WTreeViewPutDoubleArray('treeview5', icol, ndatmax,xval)
    enddo

    liststore = idpt('liststore_gspk1')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)

    if(allocated(sval)) deallocate(sval)
    allocate(sval(kdatmax))
    if(allocated(num)) deallocate(num)
    allocate(num(kdatmax))
    if(allocated(iuval)) deallocate(iuval)
    allocate(iuval(kdatmax))
    if(allocated(xval)) deallocate(xval)
    allocate(xval(kdatmax))
    renderer = idpt('cellrenderertext64')
    do i=1, kdatmax
        call clear_gtktreeiter(iter)
        call gtk_list_store_append(Liststore, c_loc(iter))
        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)


        bcheck = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
        do k=3,30
            if(.not.progstart_on .and. k > 15) exit
            kcol = k - 1
            if(k <= 15) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else

                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do
    do i=1,kdatmax
        iuval(i) = 0
        num(i) = i
        xval(i) = missingval
    enddo
    call WTreeViewPutIntArray('treeview6', 1, kdatmax, num)
    call WTreeViewPutCheckArray('treeview6', 2, kdatmax, iuval)
    do icol=3,15
        call WTreeViewPutDoubleArray('treeview6', icol, kdatmax,xval)
    enddo

    liststore = idpt('liststore_kalfit')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    if(allocated(sval)) deallocate(sval)
    allocate(sval(40))
    if(allocated(num)) deallocate(num)
    allocate(num(40))
    if(allocated(iuval)) deallocate(iuval)
    allocate(iuval(40))
    if(allocated(xval)) deallocate(xval)
    allocate(xval(40))

    do i=1,40
        call gtk_list_store_append(Liststore, c_loc(iter))

        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,14
            if(.not.progstart_on .and. k > 7) exit
            kcol = k - 1
            if(k <= 7) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else
                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do

    liststore = idpt('liststore_mean')
    call gtk_list_store_clear(liststore)
    call clear_gtktreeiter(iter)
    do i=1,200
        call gtk_list_store_append(Liststore, c_loc(iter))

        call g_value_set_long(dintval, i)
        call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, dintval)
        do k=2,4
            if(.not.progstart_on .and. k > 2) exit
            kcol = k - 1
            if(k <= 2) then
                call g_value_set_string(pstring,'   '//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            else
                call g_value_set_string(pstring, get_color_string('table_bg') // c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), kcol, pstring)
            endif
        enddo
    end do

    if(allocated(sval)) deallocate(sval)
    if(allocated(num)) deallocate(num)
    if(allocated(iuval)) deallocate(iuval)
    if(allocated(xval)) deallocate(xval)


end subroutine InitTreeViews
