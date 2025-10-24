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
module LSTfillT
    use UR_types, only: rn

contains


    subroutine WDListstoreFill_table(listname, mode, ugr)

        ! stores arrays of various data into columns of GTK liststores with name listname.

        !     Copyright (C) 2014-2023  Günter Kanisch

        !           Liststore:
        ! Mode:  1: symbol table;
        !        2: values/unc table;
        !        3: budget table,
        !        4: covar table;
        !        5: decay curve data
        !        6: Gspk1-Daten
        !        7: Liststore_kalfit
        !        8: Liststore_sumeval


        use, intrinsic :: iso_c_binding,    only:   c_ptr, c_int, c_long, c_null_char, c_loc, c_f_pointer
        use gtk,                            only:   gtk_list_store_clear, gtk_list_store_set_value, &
                                                    gtk_list_store_append, gtk_cell_renderer_toggle_get_active
        use UR_gtk_globals,                 only:   iter,item_setintern,consoleout_gtk,tvnames,ntvs,tv_colwidth_digits, &
                                                    dintval, pstring
        use g,                              only:   g_value_init, g_value_set_string, g_value_set_double, &
                                                    g_value_set_long, g_value_set_boolean

        use gtk_sup,                        only:   c_f_logical, clear_gtktreeiter

        use UR_Gleich_globals,              only:   ngrs,symbole,symtyp,einheit,bedeutung,missingval,ncov,ncovmx,messwert, &
                                                    stdunc,ivtl,sdformel,sdwert,hbreite,iar,sensi,perc,isymba, &
                                                    isymbb,icovtyp,ucontyp,vdopt,absrel,ucontrib,cvformel,covarval,vcovcor, &
                                                    nparts,FormeltextFit
        use UR_Linft,                       only:   ndatmax,numd,cstartzeit,dmesszeit,dbimpulse,dbzrate,sdbzrate, &
                                                    d0messzeit,d0impulse,d0zrate,sd0zrate,dnetrate,sdnetrate,  &
                                                    nkalpts,xkalib,ykalib,uxkalib, uykalib

        use UR_Gspk1Fit
        use gtk_hl,             only: hl_gtk_listn_set_cell,hl_gtk_listn_get_n_rows

        use Rout,               only: pending_events, clobj
        use top,                only: idpt, finditems
        use UR_params,          only: EPS1MIN
        use ur_general_globals, only: progstart_on
        use color_theme

        implicit none

        character(len=*),intent(in)  :: listname           ! GTK liststore name as string
        integer, intent(in)          :: mode               ! see below
        logical, intent(in)          :: ugr                ! TRUE if val_unc defined, otherwise False


        type(c_ptr)                  :: liststore_widget_ptr, renderer, tree
        integer                      :: nmax,ibc, nmaxk,k,ngrsx,kk,kkx,ncolorcols,itv
        integer                      :: ncitem
        integer(c_long)              :: i, ival
        integer(c_int)               :: kcol
        character(len=50)            :: vstr,frmtk
        character(len=7)             :: vstring
        character(len=250)           :: str1
        logical                      :: lmiss, bcheck, cycle_symbt
        integer, pointer             :: fp0
        !---------------------------------------------------
        item_setintern = .true.

        frmtk = '(ES15.6E2)'
        ncolorcols = 1

        !---------------------------------------------
        if(consoleout_gtk) call pending_events()

        call finditems(listname, ncitem)
        liststore_widget_ptr = clobj%id_ptr(ncitem)

        call gtk_list_store_clear(liststore_widget_ptr)
        call clear_gtktreeiter(iter)
        if(consoleout_gtk) write(0,*) 'ListstoreFill: mode=',mode,' listname=',trim(listname)
        ! if(consoleout_gtk) write(0,*) 'clear iter: iter%intv=',iter%intv, &
        !     'p0=',iter%p0  ! ,'p1=',iter%p1

        select case (mode)

          case (1,2,3)

            if(mode == 1)  tree = idpt('treeview1')
            if(mode == 2)  tree = idpt('treeview2')
            if(mode == 3)  tree = idpt('treeview4')

            cycle_symbt = .false.
            nmax = 150
            if(mode <= 3) nmax = ngrs + ncov + numd + 40

            ngrsx = ngrs
            if(mode == 3) ngrsx = ngrs+ncov+numd + 40

            nmaxk = ngrs+ncov+numd + 40
            if(mode < 3) nmaxk = ngrs + 40
            if(mode == 3) nmax = ngrsx

            if(consoleout_gtk) write(0,*) 'Fill_table Anf:  iter%intv=',iter%intv, &
                'nmax=',nmax,' mode=',mode
            do i=1,nmax

                lmiss = .false.           ! indicates missing values, e.g.for empty entries
                ! at the end of the liststore
                if(i > nmaxk)  lmiss = .true.
                if(i > nmaxk .and. mode < 3) cycle
                if(mode == 3 .and. i > ngrs+ncov+numd) lmiss = .true.
                if(mode == 2 .and. i > ngrs) lmiss = .true.
                if(mode == 1 .and. i > ngrsx) lmiss = .true.

                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))
                if(consoleout_gtk) then
                    ! write(0,*) 'Fill_table: (<=3): i=',i,' iter%intv=',iter%intv, &
                    !     'p0=',iter%p0  ! ,'p1=',iter%p1
                    call c_f_pointer(iter%p0,fp0)
                    write(0,*) 'fp0=',fp0
                end if

                if(i <= ngrsx) then
                    if(i < ubound(Symbole,dim=1)) then
                        if(len_trim(Symbole(i)%s) >= 8) then
                            if(mode == 1 .and. Symbole(i)%s(1:8) == 'cov(Fitp') cycle_symbt = .true.
                            if(cycle_symbt) then
                                symbole(i)%s = '   '
                                symtyp(i)%s = '   '
                                einheit(i)%s = '   '
                                bedeutung(i)%s = '   '
                            end if
                        end if
                    end if
                else
                    if(i <= ubound(Symbole,dim=1)) then
                        symbole(i)%s = '   '
                        symtyp(i)%s = '   '
                        einheit(i)%s = '   '
                        bedeutung(i)%s = '   '
                    end if
                end if
                ! 4 columns:
                call g_value_set_long(dintval,i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)
                if(consoleout_gtk) write(0,*)  'Column 1 ready'

                kkx = ubound(Symbole,dim=1)
                if(i <= kkx) call g_value_set_string(pstring,max('  ',Symbole(i)%s)//c_null_char)
                if(i > kkx)  call g_value_set_string(pstring,'  '//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 1_c_int, pstring)

                if(consoleout_gtk) write(0,*) 'Column 2 ready'

                kkx = ubound(symtyp,dim=1)
                if(i <= kkx) call g_value_set_string(pstring,max('  ',symtyp(i)%s)//c_null_char)
                if(i > kkx) call g_value_set_string(pstring,'  '//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 2_c_int, pstring)
                if(consoleout_gtk) write(0,*) 'Column 3 ready'

                kkx = ubound(einheit,dim=1)
                if(i <= kkx) then
                    str1 = einheit(i)%s
                else
                    str1 = '  '
                end if
                if(consoleout_gtk) write(0,*) 'Column 4:  einheit=',trim(str1)
                call g_value_set_string(pstring,max('  ',trim(str1))//c_null_char)
                if(consoleout_gtk)  write(0,*) '          after Gvalue set string'
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 3_c_int, pstring)
                if(consoleout_gtk) write(0,*) 'Column 4 ready'

                if(mode == 1) Then
                    kkx = ubound(Bedeutung,dim=1)
                    if(i <= kkx) then
                        str1 = Bedeutung(i)%s
                    else
                        str1 = ' '
                    end if

                    call g_value_set_string(pstring,max('  ', trim(str1))//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 4_c_int, pstring)
                    if(consoleout_gtk) write(0,*) 'Column 5 ready'

                end if

                if(mode == 2 .and. ugr) then

                    kkx = ubound(Messwert,dim=1)
                    if(i <= kkx) then
                        write(vstr,*) Messwert(i)
                        if(abs(Messwert(i) - missingval) < EPS1MIN) vstr = ' '
                    else
                        vstr = ' '
                    end if
                    if(lmiss) vstr = ' '
                    call g_value_set_string(pstring,max('  ',trim(adjustL(vstr)))//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 4_c_int, pstring)

                    kkx = ubound(IVTL,dim=1)
                    if(i <= kkx) vstr = vdopt(max(1,IVTL(i)))%s
                    if(i > kkx) vstr = '  '
                    call g_value_set_string(pstring,max('  ',vstr)//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr,c_loc(iter), 5_c_int, pstring)
                    if(consoleout_gtk) write(0,*) 'Column 6 ready'

                    kkx = ubound(SDFormel,dim=1)
                    ! if(i <= kkx) write(66,*) 'Fill_table:  sdformel(i):  i=',i,'  sdformel=',SDFormel(i)%s
                    if(i <= kkx) call g_value_set_string(pstring,max('  ',SDFormel(i)%s)//c_null_char)
                    if(i > kkx) call g_value_set_string(pstring,'  '//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 6_c_int, pstring)
                    if(consoleout_gtk) write(0,*) 'Column 7 ready'

                    kkx = ubound(SDWert,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 8, SDWert(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 8, missingval, lmiss)

                    kkx = ubound(HBreite,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 9, HBreite(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 9, missingval, lmiss)

                    ! write(0,*) 'i=',int(i,2),' ngrs=',int(ngrs,2)
                    if(i <= ngrs) vstr = absrel(max(1,IAR(i)))%s
                    ! if(i <= ngrs .and. ubound(iar,dim=1) == i) vstr = absrel(max(1,IAR(i)))%s
                    if(i > ngrs) vstr = '  '
                    call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 9_c_int, pstring)
                    if(consoleout_gtk) write(0,*) 'Column 10 ready'

                    kkx = ubound(StdUnc,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 11, StdUnc(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 11, missingval, lmiss)
                    if(consoleout_gtk) write(0,*) 'Column 11 ready'

                end if

                if(mode == 3 .and. i > ngrsx) cycle

                if(mode == 3) then

                    ! uncertainty budget:
                    kkx = ubound(Messwert,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 5, Messwert(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 5, missingval, lmiss)
                    if(consoleout_gtk) write(0,*) 'Spalte 6 fertig'

                    kkx = ubound(StdUnc,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 6, StdUnc(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 6, missingval, lmiss)

                    kkx = ubound(Sensi,dim=1)
                    if(i <= kkx) call StoreStrDouble(ncitem, 7, Sensi(i), lmiss)
                    if(i > kkx) call StoreStrDouble(ncitem, 7, missingval, lmiss)

                    if(Ucontyp == 1) then
                        kkx = ubound(Perc,dim=1)
                        if(i <= kkx) call StoreStrDouble(ncitem, 8, Perc(i), lmiss)
                        if(i > kkx) call StoreStrDouble(ncitem, 8, missingval, lmiss)
                    elseif(Ucontyp == 2) then
                        kkx = ubound(Ucontrib,dim=1)
                        if(i <= kkx) call StoreStrDouble(ncitem, 8, Ucontrib(i), lmiss)
                        if(i > kkx) call StoreStrDouble(ncitem, 8, missingval, lmiss)
                    end if
                    if(consoleout_gtk) write(0,*) 'Spalte 9 fertig'
                end if

                if(mode == 1) ncolorcols = 5
                if(mode == 2) ncolorcols = 11
                if(mode == 3) ncolorcols = 8
                if(progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if

            end do       ! i=1,nmax

          case (4)

            ! Liststore_covtable:
            nmax = ncovmx
            tree = idpt('treeview3')

            do i=1,ncov+25

                lmiss = .false.
                if(i > ncov) lmiss = .true.

                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))

                call g_value_set_long(dintval, i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)

                if(i <= ncov) then
                    if(ISymbA(i) > 0 .and. ISymbA(i) <= ubound(Symbole,dim=1)) then
                        vstr = Symbole(ISymbA(i))%s
                    else
                        vstr = '  '
                    end if
                end if
                if(i > ncov) vstr = '  '
                call g_value_set_string(pstring,trim(vstr)//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 1_c_int, pstring)

                if(i <= ncov) then
                    if(ISymbB(i) > 0 .and. ISymbB(i) <= ubound(Symbole,dim=1)) then
                        vstr = Symbole(ISymbB(i))%s
                    else
                        vstr = '  '
                    end if
                end if
                if(i > ncov) vstr = '  '
                call g_value_set_string(pstring,trim(vstr)//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 2_c_int, pstring)

                if(i <= ncov) then
                    if(icovtyp(i) > 0) then
                        vstr = vcovcor(Icovtyp(i))%s
                    else
                        vstr = '  '
                    end if
                else
                    vstr = '  '
                end if
                call g_value_set_string(pstring,vstr//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 3_c_int, pstring)

                if(i <= ncov) then
                    vstr = max('  ',CVformel(i)%s)
                else
                    vstr = '  '
                end if
                call g_value_set_string(pstring,trim(vstr)//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 4_c_int, pstring)

                if(i <= ncov) then
                    call StoreStrDouble(ncitem, 6, CovarVal(i), lmiss)
                else
                    call StoreStrDouble(ncitem, 6, missingval, lmiss)
                end if

                ncolorcols = 6
                if(progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if

            end do
            goto 9000 ! return

          case (5)

            ! Liststore_decay:
            nmax = ndatmax
            do i=1,ntvs
                if('treeview5' == tvnames(i)%s) itv = int(i)
            end do
            call gtk_list_store_clear(liststore_widget_ptr)

            do i=1, ndatmax         ! rows

                lmiss = .false.
                if(numd > 0 .and. i > numd) lmiss = .true.

                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))

                call g_value_set_long(dintval, i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)

                if(i <= numd) then
                    str1 = CStartzeit(i)%s
                else
                    str1= ' '
                end if

                call g_value_set_string(pstring, trim(str1)//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 1_c_int, pstring)

                if(i <= numd) then
                    call StoreStrDouble(ncitem, 3, dmesszeit(i), lmiss)
                    call StoreStrDouble(ncitem, 4, dbimpulse(i), lmiss)
                    call StoreStrDouble(ncitem, 5, dbzrate(i), lmiss)
                    call StoreStrDouble(ncitem, 6, sdbzrate(i), lmiss)
                    call StoreStrDouble(ncitem, 7, d0messzeit(i), lmiss)
                    call StoreStrDouble(ncitem, 8, d0impulse(i), lmiss)
                    call StoreStrDouble(ncitem, 9, d0zrate(i), lmiss)
                    call StoreStrDouble(ncitem, 10, sd0zrate(i), lmiss)
                    call StoreStrDouble(ncitem, 11, dnetrate(i), lmiss)
                    call StoreStrDouble(ncitem, 12, sdnetrate(i), lmiss)
                else
                    call StoreStrDouble(ncitem, 3, missingval, lmiss)
                    call StoreStrDouble(ncitem, 4, missingval, lmiss)
                    call StoreStrDouble(ncitem, 5, missingval, lmiss)
                    call StoreStrDouble(ncitem, 6, missingval, lmiss)
                    call StoreStrDouble(ncitem, 7, missingval, lmiss)
                    call StoreStrDouble(ncitem, 8, missingval, lmiss)
                    call StoreStrDouble(ncitem, 9, missingval, lmiss)
                    call StoreStrDouble(ncitem, 10, missingval, lmiss)
                    call StoreStrDouble(ncitem, 11, missingval, lmiss)
                    call StoreStrDouble(ncitem, 12, missingval, lmiss)
                end if

                ncolorcols = 12
                if(.true. .or. progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        kk = k - ncolorcols
                        if(kk <= 4 .or. (kk == 7 .or. kk == 8)) then
                            vstring = trim(vstr)
                        else
                            vstring = get_color_string('orange_bg')
                        end if
                        call g_value_set_string(pstring,max('  ',trim(vstring))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if

            end do
            goto 9000   ! return

          case (6)

            ! Liststore_Gspk1,   treeview6:
            nmax = kdatmax
            itv = 0
            do i=1,ntvs
                if('treeview6' == trim(tvnames(i)%s)) itv = int(i)
            end do
            renderer = idpt('cellrenderertext64')
            do i=1,nmax         ! rows

                lmiss = .false.
                if(numd > 0 .and. i > numd/5) lmiss = .true.

                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))

                call g_value_set_long(dintval, i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)

                ival = 0
                if(i <= ubound(guse,dim=1)) ival = guse(i)

                bcheck = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
                ibc = 0
                if(bcheck) ibc = 1
                bcheck= .false.
                if(ival == 1) bcheck = .true.

                call g_value_set_long(dintval, ival)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 1_c_int, dintval)


                if(.false.) then
                    write(vstr,'(f7.2)') erg(i)
                    if(abs(erg(i) - missingval) < EPS1MIN) vstr=' '
                    if(lmiss) vstr = ' '
                    call g_value_set_string(pstring,trim(adjustL(vstr))//c_null_char)
                    call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 2_c_int, pstring)

                    tv_colwidth_digits(itv,3) = max(tv_colwidth_digits(itv,3), len_trim(vstr))
                end if
                if(.true.) then
                    if(.not.lmiss) call StoreStrDouble(ncitem, 3, erg(i), lmiss)
                    if(lmiss) call StoreStrDouble(ncitem, 3, missingval, lmiss)
                end if

                if(.not.lmiss) call StoreStrDouble(ncitem, 4, GnetRate(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 4, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 5, RateCB(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 5, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 6, RateBG(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 6,missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 7, SDRateBG(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 7, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 8, effi(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 8, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 9, SDeffi(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 9, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 10, pgamm(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 10, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 11, SDpgamm(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 11, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 12, fatt(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 12, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 13, SDfatt(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 13, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 14, fcoinsu(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 14, missingval, lmiss)
                if(.not.lmiss) call StoreStrDouble(ncitem, 15, SDfcoinsu(i), lmiss)
                if(lmiss) call StoreStrDouble(ncitem, 15, missingval, lmiss)

                ncolorcols = 15
                if(progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if

            end do
            goto 9000   ! return

          case (7)

            ! Liststore_kalfit:
            nmax = 40
            itv = 0
            do i=1,ntvs
                if('treeview7' == tvnames(i)%s) itv = int(i)
            end do
            do i=1,nmax         ! rows

                lmiss = .false.
                if(nkalpts > 0 .and. i > nkalpts) lmiss = .true.

                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))

                call g_value_set_long(dintval, i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)
                if(i <= nkalpts) then
                    call StoreStrDouble(ncitem, 2, xkalib(i), lmiss)
                    call StoreStrDouble(ncitem, 3, uxkalib(i), lmiss)
                    call StoreStrDouble(ncitem, 4, ykalib(i), lmiss)
                    call StoreStrDouble(ncitem, 5, uykalib(i), lmiss)
                end if
                ncolorcols = 7
                if(progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if

            end do
            goto 9000   ! return

          case (8)
            ! Liststore_sumeval       (treeview9):
            nmax = nparts
            itv = 0
            do i=1,ntvs
                if('treeview7' == tvnames(i)%s) itv = int(i)
            end do
            do i=1,nmax         ! rows
                lmiss = .false.
                call clear_gtktreeiter(iter)
                call gtk_list_store_append(liststore_widget_ptr, c_loc(iter))

                call g_value_set_long(dintval, i)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 0_c_int, dintval)

                call g_value_set_string(pstring,max('  ',FormeltextFit(i)%s)//c_null_char)
                call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), 1_c_int, pstring)

                ncolorcols = 2
                if(progstart_on) then
                    !vstr = "#FFFFFF"
                    !if(contrast_mode) vstr = colors%table_bg
                    vstr = get_color_string('table_bg')
                    do k=ncolorcols+1,2*ncolorcols
                        kcol = k - 1_c_int
                        call g_value_set_string(pstring,max('  ',trim(vstr))//c_null_char)
                        call gtk_list_store_set_value(liststore_widget_ptr, c_loc(iter), kcol, pstring)
                    end do
                end if
            end do

            goto 9000   ! return

          case default
        end select
9000    continue
        if(consoleout_gtk) call pending_events
        if(consoleout_gtk) write(0,*) 'Ende von WDListStoreFill_table'
        item_setintern = .false.

    end subroutine WDListstoreFill_table

!######################################################################################

    subroutine StoreStrDouble(ncitem, fcol, dvalue, lmiss)

        ! writes real(rn) values dvalue with a specific format into the column
        ! number fcol of the liststore with name Listname and treview number itv.
        ! The format for writing the value depends on the liststore.

        !     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_null_char, c_loc
        use UR_gtk_globals,     only: iter, pstring
        use g,                  only: g_value_init, g_value_set_string
        use UR_Gleich_globals,  only: missingval
        use gtk,                only: gtk_list_store_set_value
        use ur_general_globals, only: frmt, frmt_min1, frmtc, sDecimalPoint

        use Rout,               only: clobj

        use UR_params,          only: EPS1MIN
        use CHF,                only: FormatNumStr

        implicit none

        integer, intent(in)           :: ncitem

        integer   ,intent(in)         :: fcol
        real(rn),intent(in)           :: dvalue
        logical, intent(in)           :: lmiss

        integer(c_int)                :: icol
        character(len=30)             :: vstr
        character(len=15)             :: frmt_w
        character(len=:), allocatable :: Listname
        type(c_ptr)                   :: Liststore
        !-----------------------------------------------------
        icol = fcol - 1
        listname = clobj%name(ncitem)%s
        Liststore = clobj%id_ptr(ncitem)

        if(.not.lmiss) then
            frmt_w = trim(frmt)
            if(dvalue < 0.10_rn) frmt_w = trim(frmt_min1)
            if(Listname == 'liststore_gspk1' .and. fcol == 2) frmt_w = '(f7.2)'
            if(Listname == 'liststore_covcor' .and. fcol == 6) frmt_w = trim(frmtc)
            write(vstr, frmt_w) real(dvalue,8)
            vstr = FormatNumStr(trim(vstr), sDecimalPoint)
            if(abs(dvalue - missingval) < EPS1MIN) vstr = ' '
        else
            vstr = ' '
        end if

        call g_value_set_string(pstring, trim(vstr)//'  '//c_null_char)
        call gtk_list_store_set_value(Liststore, c_loc(iter), icol, pstring)

    end subroutine StoreStrDouble

!######################################################################################

end module LSTfillT
