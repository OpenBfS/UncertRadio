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
! other module parts required:
!    type(c_ptr)     :: builder
!
!    type window
!        ! private
!        type(c_ptr) :: window_ptr
!    end type
!
!   integer   , parameter             :: nclmax = 1250
!
!   type :: charv                   ! ca. Mai 2020
!     character(:),allocatable  :: s
!   end type charv
!
!   type Wclobj
!     type(charv),allocatable  :: name(:)            ! e.g. 'GtkButton' in  Glade
!     type(charv),allocatable  :: idd(:)             ! identifier of the widget as string
!     type(charv),allocatable  :: label(:)           ! identifier of the widget label as string
!     type(c_ptr),allocatable  :: id_ptr(:)          ! the C-pointer associated with the widegt
!     type(c_ptr),allocatable  :: label_ptr(:)       ! the C-pointer associated with the widgets label
!     type(charv),allocatable  :: signal(:)          ! the name of the widget's signal as string
!     integer   ,allocatable   :: idparent(:)        ! the name of the widget's parent as string
!     type(charv),allocatable  :: handler(:)         ! not used
!   end type
!--------------------------------------------
!  type(window), target           :: UR_win
!  type(Wclobj), target           :: clobj
!  integer                        :: nclobj
!--------------------------------------------

!   type(charv)  : declares an allocatable variable-length character variable
!                  or an allocatable character array with string elements of
!                  variable length


module Rout

!     Copyright (C) 2014-2024  Günter Kanisch


    ! wstr     :    widget name string
    ! idpt     :    returns the C-pointer of the widget with name wstr

    ! pstring       : Gvalue, defined (once) globally in Uncw_init

    use, intrinsic :: iso_c_binding,  only: c_int, c_null_char, &
                                            c_null_ptr,c_associated, &
                                            c_f_pointer

    use UR_params,          only: rn, EPS1MIN, win_title
    use gtk_sup
    use top,                only: idpt, FindItemP, FindItemS
    use UR_gtk_variables,   only: clobj, item_setintern

    ! logical item_setintern: if set .true. in a routine xxx:
    ! helps to prevent from reacting to the signal just emitted by xxx

    ! FindItemP: find the index number ncitem from the widget's c-pointer
    ! FindItemS: find the index number ncitem from the widget's idd-string

contains


! following routines:

!  WDPutLabelString               WDGetLabelString
!  WDPutEntryString               WDGetEntryString
!  WDPutEntryDouble               WDGetEntryDouble
!  WDPutEntryInt                  WDGetEntryInt
!  WDPutSelRadio                  WDGetSelRadio
!  WDPutSelRadioMenu              WDGetSelRadioMenu
!  WDSetComboboxAct               WDGetComboboxAct
!  WDPutTextviewString            WDGetTextviewString
!  WDSetCheckButton               WDGetCheckButton
!  WDSetCheckMenuItem             WDGetCheckMenuItem
!  WDListstoreClearCell           WDListstoreFill_1
!                                 WTreeViewRemoveRow
!  WTreeViewPutStrArray           WTreeViewGetStrArray
!  WTreeViewPutIntArray           WTreeViewGetIntArray
!  WTreeViewPutDoubleArray        WTreeViewGetDoubleArray
!  WTreeViewPutDoubleCell         WTreeViewGetDoubleCell
!  WTreeViewPutStrCell            WTreeViewGetStrCell
!  WTreeViewPutComboArray         WTreeViewGetComboArray
!  WTreeViewPutComboCell          WTreeViewGetComboCell
!  WTreeViewPutCheckArray         WTreeViewGetCheckArray
!  WTreeViewSetCursorCell
!  FOpen                          WSelectFile
!  WDNotebookSetCurrPage          WDNotebookGetCurrPage
!  WTreeViewScrollRow             WTreeviewVisibleRows
!  WTreeViewSetColorCell          WTreeViewSetColorRow
!  WDPutLabelColorF               WDPutLabelColorB
!  WDPutLabelStringBold           WDPutTreeViewColumnLabel
!  WDGetTreeViewColumnLabel       WDPutTextviewEditor
!                                 MessageShow
!  SetTooltipText                 UpdateProName
!  NumRowsTV                      SetMenuEGr
!  ClearMCfields
!  ClearPEfields                  EraseNWGfields
!  ExpandTV2Col7                  hl_gtk_text_view_get_text_GK   !

!
!#####################################################################################################

    subroutine WDPutLabelString(wstr, string)

        use, intrinsic :: iso_c_binding, only: c_ptr

        use gtk,              only: gtk_label_set_text, gtk_label_set_markup, gtk_label_set_label, &
                                    gtk_label_get_text, gtk_button_set_label, gtk_menu_item_set_label, &
                                    gtk_menu_item_get_type,GTK_STATE_FLAG_NORMAL
        use gtk_hl,           only: hl_gtk_menu_item_set_label_markup
        use file_io,          only: logger
        use UR_gtk_variables, only: clobj, item_setintern
        use color_theme

        implicit none

        character(len=*), intent(in)        :: wstr               ! widget-string
        character(len=*), intent(in)        :: string             ! Ausgabetext

        type(c_ptr)                         :: widget
        integer                             :: ncitem, i1
        character(len=len_trim(string)+20)  :: str
        !------------------------------------------------------------

        item_setintern = .false.
        call FindItemS(wstr, ncitem)
        widget = clobj%id_ptr(ncitem)
        if(ncitem == 0) then
            call logger(66, 'warning: widget="' // wstr //'" is not connected!')
            return
        end if

        str = string

        if(trim(clobj%name(ncitem)%s) == 'GtkButton' .or.   &
            trim(clobj%name(ncitem)%s) == 'GtkCheckButton'  .or.  &
            trim(clobj%name(ncitem)%s) == 'GtkRadioButton'   ) then

            call gtk_button_set_label(widget, trim(str) // c_null_char)
            if(trim(clobj%name(ncitem)%s) == 'GtkCheckButton') &
            call WDPutLabelColorF(wstr, GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))

        elseif(trim(clobj%name(ncitem)%s) == 'GtkRadioMenuItem' .or. trim(clobj%name(ncitem)%s) == 'GtkMenuItem' .or. &
            trim(clobj%name(ncitem)%s) == 'GtkCheckMenuItem' .or. trim(clobj%name(ncitem)%s) == 'GtkImageMenuItem'  ) then
            call gtk_menu_item_set_label(widget, trim(str) // c_null_char)
        else
            call gtk_label_set_text(widget, trim(str) // c_null_char)
            call WDPutLabelColorF(wstr, GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))
        end if

        item_setintern = .false.

    end subroutine WDPutLabelString

    !-----------------------------------------------------------------------------------------

    subroutine WDGetLabelString(wstr, string)

        use gtk,                only: gtk_label_get_text,gtk_menu_item_get_label,gtk_button_get_label

        implicit none

        character(len=*),intent(in)     :: wstr               ! widget-string
        character(len=*),intent(out)    :: string             ! Ausgabetext

        integer             :: ncitem
        type(c_ptr)         :: cptxt
        type(c_ptr)         :: widget

        !------------------------------------------------------------

        call FindItemS(wstr, ncitem)
        widget = clobj%id_ptr(ncitem)
        if(trim(clobj%name(ncitem)%s) == 'GtkButton' .or.   &
            trim(clobj%name(ncitem)%s) == 'GtkCheckButton'  .or.  &
            trim(clobj%name(ncitem)%s) == 'GtkRadioButton'   ) then

            cptxt = gtk_button_get_label(widget)

        elseif(trim(clobj%name(ncitem)%s) == 'GtkRadioMenuItem' .or. trim(clobj%name(ncitem)%s) == 'GtkMenuItem' .or. &
            trim(clobj%name(ncitem)%s) == 'GtkCheckMenuItem'  ) then
            cptxt = gtk_menu_item_get_label(widget)
        else
            cptxt = gtk_label_get_text(widget)
        end if

        string = ' '
        if(c_associated(cptxt)) then
            call c_f_string(cptxt,string)
        end if

    end subroutine WDGetLabelString

    !-----------------------------------------------------------------------------------------

    subroutine WDPutEntryString(wstr, string)

        use gtk,                only: gtk_entry_set_text

        implicit none

        character(len=*),intent(in)         :: wstr    ! widget name string
        character(len=*),intent(in)         :: string  ! Ausgabetext

        ! ---------------------------------------------------------
        item_setintern = .true.

        call gtk_entry_set_text(idpt(wstr), trim(string)//c_null_char)
        item_setintern = .false.

    end subroutine WDPutEntryString

    !-----------------------------------------------------------------------------------------

    subroutine WDGetEntryString(wstr, string)

        use gtk,                only: gtk_entry_get_text

        implicit none

        character(len=*),intent(in)          :: wstr               ! widget-string
        character(len=*),intent(out)         :: string             ! entry output string

        type(c_ptr)                :: cptxt
        !------------------------------------------------------------
        cptxt = gtk_entry_get_text(idpt(wstr))
        string = ' '
        if(c_associated(cptxt)) then
            call c_f_string(cptxt,string)
        end if

    end subroutine WDGetEntryString

    !-----------------------------------------------------------------------------------------

    subroutine WDPutEntryDouble(wstr, value, dform)

        use gtk,                  only: gtk_entry_set_text
        use CHF,                  only: FormatNumStr
        use ur_variables,         only: sDecimalPoint

        implicit none

        character(len=*),intent(in)          :: wstr
        real(rn),intent(in)                  :: value
        character(len=*),intent(in),optional :: dform                     ! e.g.:   '(1pG17.7E2)'

        character(len=50)                    :: string

        !------------------------------------------------------------
        item_setintern = .true.

        ! convert value into  string:
        if(present(dform)) then
            write(string,dform) real(value,8)
        else
            write(string,*) real(value,8)
        end if
        string = adjustl(string)
        string = FormatNumStr(trim(string), sDecimalPoint)

        call gtk_entry_set_text(idpt(wstr), trim(string)//c_null_char)

        item_setintern = .false.

    end subroutine WDPutEntryDouble

    !-----------------------------------------------------------------------------------------

    subroutine WDGetEntryDouble(wstr, dvalue)

        use gtk,                only: gtk_entry_get_text

        implicit none

        character(len=*),intent(in)          :: wstr
        real(rn),intent(out)                 :: dvalue

        type(c_ptr)                :: cptxt
        character(len=50)          :: string
        integer                    :: i1, ios
        !------------------------------------------------------------

        cptxt = gtk_entry_get_text(idpt(wstr))
        !write(66,*) 'Get text passed','   cptxt=',cptxt
        if(c_associated(cptxt)) call c_f_string(cptxt,string)
        !write(66,*) 'text read from entryEGR: ',trim(symb)
        i1 = index(string,',')
        if(i1 > 0) THEN
            string(i1:i1) = '.'
        end if
        read(string,*,iostat=ios) dvalue
        if(ios /= 0) dvalue = 0._rn

    end subroutine WDGetEntryDouble

    !----------------------------------------------------------------------------------------------

    subroutine WDPutEntryInt(wstr, ivalue, dform)

        use gtk,                only:   gtk_entry_set_text, GTK_STATE_FLAG_NORMAL
        use UR_gtk_variables,   only:   item_setintern

        implicit none

        character(len=*),intent(in)          :: wstr
        integer   ,intent(in)                :: ivalue
        character(len=*),intent(in),optional :: dform                     ! e.g.:   '(1pG17.7E2)'
        !------------------------------------------------------------------------------------------
        character(len=25) :: string
        !------------------------------------------------------------------------------------------
        item_setintern = .true.
        if(present(dform)) then
            write(string,dform) ivalue
        else
            write(string,*) ivalue
        end if
        string = adjustl(string)

        call gtk_entry_set_text(idpt(wstr), trim(string)//c_null_char)

        ! Entries must not be colored with the following statements,
        ! routines WDPutLabelColorB and/or WDPutLabelColorF,
        ! otherwise they loose the ability that their content strings can
        ! be highlighted/marked.
        ! 20.9.2024  GK
        item_setintern = .false.
        !------------------------------------------------------------------------------------------
    end subroutine WDPutEntryInt

    !----------------------------------------------------------------------------------------------

    subroutine WDGetEntryInt(wstr, ivalue)

        use gtk,                only: gtk_entry_get_text

        implicit none

        character(len=*),intent(in)          :: wstr
        integer   ,intent(out)               :: ivalue

        type(c_ptr)                :: cptxt
        character(len=25)          :: string
        integer                    :: ios
        !---------------------------------------------------------
        cptxt = gtk_entry_get_text(idpt(wstr))
        !write(66,*) 'Get text passed','   cptxt=',cptxt
        if(c_associated(cptxt)) call c_f_string(cptxt,string)
        !write(66,*) 'text read from entryEGR: ',trim(symb)
        read(string,*,iostat=ios) ivalue
        if(ios /= 0) ivalue = 0

    end subroutine WDGetEntryInt

    !-----------------------------------------------------------------------------------------

    subroutine WDPutSelRadio(wstr, k)

        use gtk_hl_button,      only:   hl_gtk_radio_group_set_select, gtk_radio_button_get_group, &
                                        hl_gtk_radio_group_get_select

        implicit none

        character(len=*),intent(in)   :: wstr
        integer   ,intent(in)         :: k

        type(c_ptr)                :: group
        integer(c_int)             :: indx
!------------------------------------------------------------
        item_setintern = .true.
        indx = k - 1
        group = gtk_radio_button_get_group(idpt(wstr))
        call hl_gtk_radio_group_set_select(group, indx)

        item_setintern = .false.

    end subroutine WDPutSelRadio

    !------------------------------------------------------------
    subroutine WDGetSelRadio(wstr, k)

        use gtk,                only: gtk_radio_button_get_group
        use gtk_hl_button,      only: hl_gtk_radio_group_get_select

        implicit none

        character(len=*),intent(in)  :: wstr
        integer   ,intent(out)       :: k

        type(c_ptr)                :: group
        integer(c_int)             :: index
        !------------------------------------------------------------


        group = gtk_radio_button_get_group(idpt(wstr))
        index = hl_gtk_radio_group_get_select(group)
        k = index + 1
        ! write(66,*) 'WDSelRadio:   widget_str=',wstr,' group=',group,' index=',index

    end subroutine WDGetSelRadio

!-----------------------------------------------------------------------------------------

    subroutine WDPutSelRadioMenu(wstr, k)

        use gtk,                only: gtk_radio_menu_item_get_group
        use gtk_hl_menu,        only: hl_gtk_radio_menu_group_set_select

        implicit none

        character(len=*),intent(in)   :: wstr
        integer   ,intent(in)         :: k

        type(c_ptr)                :: group
        integer(c_int)             :: indx
!------------------------------------------------------------
        item_setintern = .true.
        indx = k
        group = gtk_radio_menu_item_get_group(idpt(wstr))

        call hl_gtk_radio_menu_group_set_select(group, indx)

        item_setintern = .false.

    end subroutine WDPutSelRadioMenu

!-----------------------------------------------------------------------------------------

    subroutine WDGetSelRadioMenu(wstr, k)

        use gtk,                only: gtk_radio_menu_item_get_group
        use gtk_hl_menu,        only: hl_gtk_radio_menu_group_get_select

        implicit none

        character(len=*),intent(in)   :: wstr
        integer   ,intent(out)        :: k

        type(c_ptr)                :: group
        integer(c_int)             :: index
!------------------------------------------------------------
        item_setintern = .true.

        group = gtk_radio_menu_item_get_group(idpt(wstr))

        index = hl_gtk_radio_menu_group_get_select(group)
        k = index

        item_setintern = .false.

    end subroutine WDGetSelRadioMenu

!########################################################################################

    subroutine WDSetComboboxAct(wstr, kopt)

        use gtk,                only: gtk_combo_box_set_active
        use gtk_hl,             only: hl_gtk_combo_box_get_active

        implicit none

        character(len=*),intent(in)   :: wstr         ! widget-string
        integer   ,intent(in)         :: kopt         ! zu selektierender Index (zählt ab 1)

        type(c_ptr)                :: cbox
        integer(c_int)             :: indx
!--------------------------------------------------------------------
        item_setintern = .true.
        cbox = idpt(wstr)
        indx = kopt - 1
        call gtk_combo_box_set_active(cbox, indx)

        item_setintern = .false.

    end subroutine WDSetComboboxAct

!#####################################################################################

    subroutine WDGetComboboxAct(wstr, kopt)

        use gtk_hl_combobox,    only: hl_gtk_combo_box_get_active

        implicit none

        character(len=*),intent(in)   :: wstr         ! widget-string
        integer   ,intent(out)        :: kopt         ! selektierter Index

        type(c_ptr)                :: cbox
        integer(c_int)             :: indx
        !--------------------------------------------------------------------
        cbox = idpt(wstr)
        indx = hl_gtk_combo_box_get_active(cbox)
        kopt = indx + 1

    end subroutine WDGetComboboxAct

!#####################################################################################

    subroutine WDPutTextviewString(wstr, carray)

        use gtk_hl,             only:   hl_gtk_text_view_insert, hl_gtk_text_view_delete,  &
                                        hl_gtk_text_view_insert_single
        use pango,              only:   pango_font_description_new, pango_font_description_set_size, &
                                        pango_font_description_free, pango_font_description_get_size, &
                                        pango_font_description_to_string, pango_font_description_from_string
        use gtk,                only:   gtk_widget_override_font,True,gtk_text_view_set_cursor_visible,False
        use UR_Gleich,          only:   charv
        use file_io,            only:   logger
        implicit none

        character(len=*),intent(in)            :: wstr
        type(charv),intent(in),allocatable     :: carray(:)

        type(c_ptr)                      :: widget
        integer(c_int)                   :: cline, ccol
        integer                          :: i,nrec
        character(len=512)               :: log_str
        logical                          :: prout

        !------------------------------------------------------------
        item_setintern = .true.
        prout = .false.
        widget = idpt(wstr)
        nrec = size(carray)

        if(nrec == 0 .or. .not.allocated(carray)) then
            call hl_gtk_text_view_delete(widget, line=0_c_int, column=0_c_int, n_lines=200)
            item_setintern = .false.
            return
        end if

        call hl_gtk_text_view_delete(widget, line=0_c_int, column=0_c_int, n_lines=200)
        cline = 0
        ccol  = 0
        cline = -1_c_int
        do i=1,min(nrec,ubound(carray,dim=1))
            cline = cline + 1
            if(cline >= 0_c_int) then
                ! within the textview, the end-of-line character is LF:
                call hl_gtk_text_view_insert_single(widget,carray(i)%s//char(10), line=cline,column=ccol, replace = False)
            end if
        end do
! append another record with many blank characters:        ! important!!!
        call hl_gtk_text_view_insert_single(widget, &
            '                                                              '//char(10), line=cline+1,column=ccol, replace = False)

        if(prout) then
!             write(66,*)
            call logger(66, ' ')
!             write(66,*) 'WDPutTextViewString====================       wstr=',trim(wstr)
            write(log_str, '(*(g0))') 'WDPutTextViewString====================       wstr=',trim(wstr)
            call logger(66, log_str)
!             write(66,*) '  ----------  nrec=',nrec
            write(log_str, '(*(g0))') '  ----------  nrec=',nrec
            call logger(66, log_str)
            do i=1,nrec
!                 write(66,*) trim(carray(i)%s)
                call logger(66, trim(carray(i)%s))
            end do
!             write(66,*) 'WDPutTextViewString===================='
            call logger(66, 'WDPutTextViewString====================')
!             write(66,*)
            call logger(66, ' ')
        end if

        item_setintern = .false.
        call gtk_text_view_set_cursor_visible(widget,1_c_int)

!         if(prout) write(66,*) 'End WDPutTextviewString'
        if(prout)  then
            write(log_str, '(*(g0))') 'End WDPutTextviewString'
            call logger(66, log_str)
        end if

    end subroutine WDPutTextviewString

!#####################################################################################

    subroutine WDGetTextviewString(wstr, carray)

        use UR_gtk_variables,   only: item_setintern
        use top,                only: CharModA1
        use file_io,           only: logger
        use UR_Gleich,          only: charv

        implicit none

        character(len=*),intent(in)   :: wstr               ! widget-string
        type(charv),intent(out),allocatable  :: carray(:)            ! array of output text lines

        type(c_ptr)                       :: widget
        integer                           :: i,j,ic

        character(len=512)                :: log_str
        logical                           :: prout
        !------------------------------------------------------------
        item_setintern = .true.
        ! crlf = char(13) // char(10)
        prout = .false.
        ! prout = .true.

        widget = idpt(wstr)

        call hl_gtk_text_view_get_text_GK(widget, carray, start_line=0_c_int,  hidden = TRUE)
!         if(prout) write(66,*) 'GetTextView: size(carray)=',size(carray),' ubound(carray,dim=1)=',ubound(carray,dim=1)
        if(prout)  then
            write(log_str, '(*(g0))') 'GetTextView: size(carray)=',size(carray),' ubound(carray,dim=1)=',ubound(carray,dim=1)
            call logger(66, log_str)
        end if

        ! write(66,*) 'size(carray)=',size(carray), ' carray='
        ! write(66,*) (carray(i)%s,'; ',i=1,size(carray))
        ! write(66,*) '--'

        do i=size(carray),1,-1
            ! write(66,*) 'get: i=',int(i,2),' len_trim(carray(i))=',len_trim(carray(i)%s),' carray(i)=',carray(i)%s

            if(len_trim(carray(i)%s) > 2) then
                call Charmoda1(carray,i)
                exit
            end if
            if(i == 2) then
                ! leave one empty row
                call Charmoda1(carray,1)
                exit
            end if
        end do

        ic = size(carray)
15      continue
        do i=1,ic
            exit
            if(i < ic .and. len_trim(carray(i)%s) == 0) then
                do j=i,ic-1
                    carray(j)%s = carray(j+1)%s
                end do
                ic = ic - 1
                call Charmoda1(carray,ic)
                ! exit
                goto 15
            end if
        end do

!         if(prout) write(66,*) '------------------- WDGetTextViewString '
        if(prout)  then
            write(log_str, '(*(g0))') '------------------- WDGetTextViewString '
            call logger(66, log_str)
        end if
!         if(prout) write(66,*)
        if(prout)  then
            write(log_str, '(*(g0))')
            call logger(66, log_str)
        end if

        item_setintern = .false.

    end subroutine WDGetTextviewString

!#####################################################################################

    subroutine WDSetCheckButton(wstr, kopt)

        use gtk,                only: gtk_toggle_button_set_active

        implicit none

        character(len=*),intent(in)    :: wstr         ! widget-string
        integer   ,intent(in)          :: kopt         ! 0: de-activated; 1: activated

        type(c_ptr)                :: cbut
        integer(c_int)             :: indx
        !--------------------------------------------------------------------
        item_setintern = .true.
        cbut = idpt(wstr)
        indx = kopt
        call gtk_toggle_button_set_active(cbut, indx)
        item_setintern = .false.

    end subroutine WDSetCheckButton

!#####################################################################################

    subroutine WDGetCheckButton(wstr, kopt)

        use gtk,                only: True, &
                                      gtk_toggle_button_get_active
        use UR_gtk_variables,   only: consoleout_gtk

        implicit none

        character(len=*),intent(in) :: wstr         ! widget-string
        integer   ,intent(out)      :: kopt         ! 0: de-activated; 1: activated

        type(c_ptr)                 :: cbut
        integer(c_int)              :: isactive
        !--------------------------------------------------------------------
        cbut = idpt(wstr)

        kopt = 0
        isactive = gtk_toggle_button_get_active(cbut)
        if(isactive == TRUE) kopt = 1

        if(consoleout_gtk) write(0,*) '  WDGetCheckbutton:  ',wstr,'  kopt=',kopt,'    cbut=',cbut

    end subroutine WDGetCheckButton

!#####################################################################################

    subroutine WDSetCheckMenuItem(wstr, kopt)

        use gtk,                only: gtk_check_menu_item_set_active, gtk_toggle_button_set_active

        implicit none

        character(len=*),intent(in)    :: wstr         ! widget-string
        integer   ,intent(in)          :: kopt         ! 0: de-activated; 1: activated

        type(c_ptr)                :: cbut
        integer(c_int)             :: indx
!--------------------------------------------------------------------
        item_setintern = .true.
        cbut = idpt(wstr)
        indx = kopt
        ! write(66,*) '  WDSetCheckMenuItem:  ',wstr,'  kopt=',kopt,'    cbut=',cbut
        call gtk_check_menu_item_set_active(cbut, indx)
        ! write(66,*) '  WDSetCheckMenuItem:  ',wstr,'  kopt=',kopt,'    cbut=',cbut   ! ,' indx after readout:' ,indx
        item_setintern = .false.

    end subroutine WDSetCheckMenuItem

    !#####################################################################################

    subroutine WDGetCheckMenuItem(wstr, kopt)

        use gtk,                only: gtk_check_menu_item_get_active, gtk_toggle_button_get_active

        implicit none

        character(len=*),intent(in)  :: wstr         ! widget-string
        integer   ,intent(out)       :: kopt         ! 0: de-activated; 1: activated

        type(c_ptr)                :: cbut
        integer(c_int)             :: indx
    !--------------------------------------------------------------------
        cbut = idpt(wstr)
        indx = kopt
        ! write(66,*) '  WDSetCheckMenuItem:  ',wstr,'  kopt=',kopt,'    cbut=',cbut
        indx = gtk_check_menu_item_get_active(cbut)
        kopt = indx
        ! write(66,*) '  WDSetCheckMenuItem:  ',wstr,'  kopt=',kopt,'    cbut=',cbut   ! ,' indx after reading:' ,indx

    end subroutine WDGetCheckMenuItem

    !#####################################################################################

    subroutine WDListstoreClearCell(treename, ncol, nrow)

        use gtk_hl,             only: hl_gtk_listn_set_cell

        implicit none

        character(len=*),intent(in)  :: treename               ! treeview name as string
        integer   , intent(in)       :: nrow, ncol

        type(c_ptr)              :: tree
    !--------------------------------------------------------------------- ------
        tree = idpt(trim(treename))
        call hl_gtk_listn_set_cell(tree, col=ncol-1, row=nrow-1, svalue=' ')

    end subroutine WDListstoreClearCell

    !#####################################################################################

    subroutine WDListstoreFill_1(liststr, nvals, strgarr)

        use, intrinsic :: iso_c_binding
        use gtk,                only: gtk_list_store_clear, gtk_list_store_set_value, gtk_list_store_append, &
                                      gtk_tree_model_iter_nth_child, gtk_tree_model_get_value, &
                                      gtk_widget_override_font
        use UR_gtk_variables,   only: iter, list_filling_on, item_setintern, consoleout_gtk
        use UR_gini
        use top,                only: CharModA1
        use g,                  only: g_value_set_string
        use UR_Gleich,          only: refdataMD,meanID,rinflu_known,charv,ngrs,ncov
        use UR_Linft,           only: numd
        use CHF,                only: FindLocT
        use gtk_hl,             only: hl_gtk_listn_get_n_rows
        use pango,              only: pango_font_description_from_string,pango_font_description_free

        implicit none

        character(len=*),intent(in)     :: liststr     ! liststore name as string (singl-column-liatstore)
        integer   ,intent(in)           :: nvals       ! number of values to be loaded into the Liststore
        type(charv),allocatable         :: strgarr(:)  ! arrray of values to be loaded into the Liststore

        type(c_ptr)                     :: liststore, font_desc
        integer                         :: i,ksmax,i1
        character(:),allocatable        :: str1
        character(len=60)               :: refnameold

    !---------------------------------------------------
        item_setintern = .true.
        list_filling_on = .true.

        if(consoleout_gtk) write(0,*) 'LSTFill_1  lststr=',trim(liststr)
        ! write(66,*) 'LSTFill_1  lststr=',trim(liststr)
        if(trim(liststr) == 'liststore_MDvars' .and. rinflu_known) then
            if(refdataMD > 0) refnameold = meanID(refdataMD)%s
        end if

        liststore = idpt(trim(liststr))
        call gtk_list_store_clear(Liststore)
        call clear_gtktreeiter(iter)
        allocate(character(len=500) :: str1)

        i1 = ubound(strgarr,dim=1)

        do i=1, nvals
            call gtk_list_store_append(Liststore, c_loc(iter))

            if(i <= i1) then

                str1 = strgarr(i)%s

                ksmax = max(1,len_trim(str1))
                if(len_trim(str1) > 0) then
                    call g_value_set_string(pstring,trim(str1)//c_null_char)
                else
                    call g_value_set_string(pstring,' '//c_null_char)
                end if
            else
                call g_value_set_string(pstring,'  '//c_null_char)
            end if
            if(consoleout_gtk) write(0,*) 'LSTFill_1: before set_value:  i=',int(i,2),' nvals=',int(nvals,2)
            call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, pstring)
            if(consoleout_gtk) write(0,*) 'LSTFill_1: after set_value:  i=',int(i,2)
        end do

        if(trim(liststr) == 'liststore_MDvars' .and. i1 == nvals) then
            ! 12.8.2023:
            call gtk_list_store_append(Liststore, c_loc(iter))  ! 19.8.2023
            call g_value_set_string(pstring,' '//c_null_char)
            call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, pstring)
        end if
        !----------------------------------

        if(trim(liststr) == 'liststore_MDvars' .and. rinflu_known .and. nvals >= refdataMD) then
            refnameold = meanID(refdataMD)%s
            i = FindlocT(strgarr,trim(refnameold))
            refdataMD = i
            call WDSetComboboxAct('combobox_RefMD',refdataMD)
            ! write(55,*) 'ListstoreFill_1: refdataMD=',refdataMD,' refnameold=',trim(refnameold)
            ! write(log_str, '(*(g0))') 'ListstoreFill_1: refdataMD=',refdataMD,' refnameold=',trim(refnameold)
            ! call logger(55, log_str)
            ! write(55,*) (trim(Strgarr(i)),' ',i=1,nvals)
        end if


        ! special code added, only for 'liststore_symbols' :

        if(trim(liststr) == 'liststore_symbols') then
            ! write(66,*) ' Liststore pointer=',liststore
            ! write(66,*) ' Liststore_symbols: number of rows = ',hl_gtk_listn_get_n_rows(liststore)

            liststore = idpt('liststore_symbols_combo')
            if(consoleout_gtk) write(0,*) 'LSTFill_1: lst_combo:'

            if(nvals < 30) font_desc = pango_font_description_from_string('Sans 11' // c_null_char)
            if(nvals >= 30 .and. nvals < 50) font_desc = pango_font_description_from_string('Sans 10' // c_null_char)
            if(nvals >= 50 .and. nvals < 70) font_desc = pango_font_description_from_string('Sans 9' // c_null_char)
            if(nvals >= 70) font_desc = pango_font_description_from_string('Sans 8' // c_null_char)
            call gtk_widget_override_font(idpt('comboboxNetRate'), font_desc)
            call gtk_widget_override_font(idpt('comboboxGrossRate'), font_desc)
            call pango_font_description_free(font_desc)

            call gtk_list_store_clear(Liststore)
            call clear_gtktreeiter(iter)
            do i=1,nvals
                if(i > ngrs+ncov+numd) exit
                call clear_gtktreeiter(iter)
                call gtk_list_store_append(Liststore, c_loc(iter))
                str1 = strgarr(i)%s

                ksmax = max(1,len_trim(str1))
                if(len_trim(str1) == 0) str1 = ' '
                ! write(66,*) 'liststore_symbols-combo:  i=',int(i,2),"ksmax=",ksmax,' ',strgarr(i)%s

                call g_value_set_string(pstring,str1(1:ksmax)//c_null_char)
                call gtk_list_store_set_value(Liststore, c_loc(iter), 0_c_int, pstring)
                if(consoleout_gtk) write(0,*) 'LSTFill_1: after set_value:  i=',i,' nvals=',int(nvals,2), &
                    ' str1=',trim(str1)
                if(.false.) then
                    ! str1 = RGBA_BG_ITEMS_hex
                    str1 = "#AdAdAd"
                    call g_value_set_string(pstring,trim(str1)//c_null_char)
                    ! write(66,*) '::::::::::::: i=',i,' symbol = ',strgarr(i)
                    call gtk_list_store_set_value(Liststore, c_loc(iter), 1_c_int, pstring)
                end if

            end do
            !  write(66,*) ' Liststore_symbols_combo: number of rows = ',hl_gtk_listn_get_n_rows(liststore)

        end if
        deallocate(str1)
        list_filling_on = .false.
        item_setintern = .false.

    end subroutine WDListstoreFill_1

    !###############################################################################


    subroutine WTreeViewRemoveRow(treename, nrow)

        use gtk_hl,               only: hl_gtk_listn_rem

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: nrow                  ! number of row which is to be removed

        type(c_ptr)              :: tree
        integer(c_int)           :: irow1
        integer                  :: i
        !---------------------------------------------------

        ! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        irow1 = nrow - 1
        call hl_gtk_listn_rem(tree, row=irow1)

    end subroutine WTreeViewRemoveRow

    !###############################################################################

    subroutine WTreeViewPutStrArray(treename, ncol, nvals, stringarr)

        use gtk_hl,                   only: hl_gtk_listn_set_cell
        use UR_gtk_variables,         only: ntvs,tvnames,tv_colwidth_digits
        use UR_gleich,                only: charv

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! number of the column into which the array is to be stored
        integer   ,intent(in)         :: nvals                 ! number of array elements to be stored

        type(charv),intent(in)  :: stringarr(:)            ! array of strings to be stored

        type(c_ptr)                          :: tree
        integer(c_int)                       :: irow1,icol1
        integer                              :: i,itv
        character(:),allocatable  :: str,xstr
        !---------------------------------------------------
        item_setintern = .true.
        ! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do

        allocate(character(len=2000) :: str,xstr )           ! 11.8.2023

        do i = 1, nvals
            irow1 = i - 1
            str = max(' ',stringarr(i)%s)

            xstr = trim(str)
            call hl_gtk_listn_set_cell(list=tree, row=irow1, col=icol1,  svalue=trim(xstr))
            if(itv > 0) then
                tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(xstr))
            end if

        end do
        deallocate(str,xstr)

        item_setintern = .false.

    end subroutine WTreeViewPutStrArray

!###############################################################################

    subroutine WTreeViewGetStrArray(treename, ncol, nvals, stringarr)

        use gtk_hl,               only: hl_gtk_listn_get_cell,hl_gtk_tree_get_cell  ! ,gtk_tree_view_get_model
        use top,                  only: CharModA1
        use UR_gtk_variables,     only: ntvs,tvnames,tv_colwidth_digits
        use UR_Gleich,            only: charv

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! column-number from the array is loaded
        integer   ,intent(in)            :: nvals                 ! number of values to be loaded
        type(charv),allocatable          :: stringarr(:)          ! array of strings to be loaded

        type(c_ptr)                     :: tree ! ,store
        integer(c_int)                  :: irow1,icol1
        integer                         :: i,itv,is,ios

        character(:),allocatable        :: string
        type(charv),allocatable         :: dd2(:)
        !---------------------------------------------------
        ! write(66,*)  'treename = ',trim(treename),'  ncol=',ncol,'  nvals=',nvals

        ! get c_ptr tree from treename:
        tree = idpt(trim(treename))

        ! store = gtk_tree_view_get_model(tree)

        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do
        icol1 = ncol - 1

        is = ubound(stringarr,dim=1)
        if(nvals > is) call CharModA1(stringarr,nvals)
        is = ubound(stringarr,dim=1)
        ! write(66,*) 'is=',is

        do i = 1, nvals
            irow1 = i - 1
            if(allocated(string)) deallocate(string)
            allocate(character(len=2000) :: string)       ! 12.8.2023
            call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)

            if(i > is) then
                if(allocated(dd2)) deallocate(dd2)
                allocate(dd2(i), stat=ios)
                if(is == 0 .and. i > 1) then
                    dd2(1:i-1) = stringarr
                    call move_alloc(dd2, stringarr)
                end if
                if(is == 0 .and. i == 1 .and. .not.allocated(stringarr)) allocate(stringarr(1))
                if(is == 0 .and. i == 1) stringarr(1)%s = trim(string)
                if(is > 0) call CharModA1(stringarr,i)
                cycle
            end if
            if(len_trim(string) > 0) then
                stringarr(i)%s = trim(string)
            else
                stringarr(i)%s = ' '
            end if
            if(trim(stringarr(i)%s) == 'invalid') then
                stringarr(i)%s = ' '
            end if
            if(itv > 0) then
                tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(stringarr(i)%s))
            end if
        end do

    end subroutine WTreeViewGetStrArray

!###############################################################################


    subroutine WTreeViewPutIntArray(treename, ncol, nvals, iarray)

        use gtk_hl,                   only: hl_gtk_listn_set_cell

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! number of the column from which the array is to be stored
        integer   ,intent(in)         :: nvals                 ! number of values to be stored
        integer   ,intent(in)         :: iarray(nvals)         ! array of integers to be stored

        type(c_ptr)          :: tree
        integer(c_int)       :: irow1,icol1
        integer              :: i
!---------------------------------------------------

        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        do i = 1, nvals
            irow1 = i - 1
            call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  ivalue=iarray(i))
        end do
        item_setintern = .false.

    end subroutine WTreeViewPutIntArray

!###############################################################################

    subroutine WTreeViewGetIntArray(treename, ncol, nvals, iarray)

        use gtk_hl,                   only: hl_gtk_listn_get_cell

        implicit none

        character(len=*),intent(in)    :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)          :: ncol                  ! number of the column from which the array is to be loaded
        integer   ,intent(in)          :: nvals                 ! number of values to be loaded
        integer   ,intent(out)         :: iarray(nvals)         ! array of integers to be loaded

        type(c_ptr)         :: tree
        integer(c_int)      :: irow1,icol1
        integer             :: i
!---------------------------------------------------

! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        do i = 1, nvals
            irow1 = i - 1
            call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  ivalue=iarray(i))
        end do

    end subroutine WTreeViewGetIntArray

!###############################################################################

    subroutine WTreeViewPutDoubleArray(treename, ncol, nvals, darray)

        use gtk_hl,                   only: hl_gtk_listn_set_cell,hl_gtk_listn_get_cell
        Use UR_Gleich,                only: missingval,ngrs
        use UR_Variables,             only: frmt,frmtg,frmt_min1,frmtc, sDecimalPoint
        use UR_gtk_variables,         only: ntvs,tvnames
        use CHF,                      only: FormatNumStr

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! number of the column into which the array is to be stored
        integer   ,intent(in)            :: nvals                 ! number of values to be stored
        real(rn),allocatable             :: darray(:)             ! array of real(rn) of values to be stored

        type(c_ptr)                  :: tree
        integer(c_int)               :: irow1,icol1
        integer                      :: i,itv,nadd
        character(len=50)            :: string
        character(len=50)            :: frmtv
!---------------------------------------------------
        item_setintern = .true.

! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        frmtv = frmt
        if(trim(treename) == 'treeview6') frmtv = frmtg
        if(trim(treename) == 'treeview3') frmtv = frmtc

        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do
        icol1 = ncol - 1
        nadd = 20
        if(trim(treename) == 'treeview5') nadd = 30
        if(trim(treename) == 'treeview6') nadd = 5

        do i = 1, nvals+nadd
            irow1 = i - 1

            if(trim(treename) == 'treeview2' .and. i > ngrs) cycle
            if(i <= min(nvals,ubound(darray,dim=1))) then
                frmtv = frmt
                if(darray(i) < 0.10_rn .and. trim(treename) /= 'treeview3') frmtv = frmt_min1
                write(string,frmtv) real(darray(i),8)
                string = FormatNumStr(trim(string), sDecimalPoint)
                ! if(trim(treename) == 'treeview6') write(66,*) 'i=',int(i,2),' string=',trim(string),' frmtv=',trim(frmtv)
                if(abs(darray(i)-missingval) < EPS1MIN) string = '  '
            else
                string = '  '
            end if
            ! call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=string(1:max(2,len_trim(string))))
            call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=string)     !(1:max(2,len_trim(string))))

            if(trim(treename) == 'treeview6') then
                ! call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)
                !   write(66,*) ' read after put: string=',string
            end if

        end do
        item_setintern = .false.

    end subroutine WTreeViewPutDoubleArray

!###############################################################################

    subroutine WTreeViewGetDoubleArray(treename, ncol, nvals, darray)

        use gtk_hl,               only: hl_gtk_listn_get_cell
        Use UR_Gleich,            only: missingval
        use top,                  only: RealModA1
        use UR_gtk_variables,     only: ntvs,tvnames,tv_colwidth_digits

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! number of the column from which the array is loaded
        integer   ,intent(in)            :: nvals                 ! number of values to be loaded
        real(rn),allocatable             :: darray(:)             ! array of real(rn) values to be loaded

        type(c_ptr)                  :: tree
        integer(c_int)               :: irow1,icol1
        integer                      :: i,i1,ios,itv,is,k
        character(len=50)            :: string
        real(rn)                     :: dummy
!---------------------------------------------------
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do

        is = ubound(darray,dim=1)
        if(nvals > is .or. abs(is) > 1E+7) then
            call RealModA1(darray,nvals)
            darray(1:nvals) = missingval
        end if

        do i = 1, nvals
            irow1 = i - 1
            call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)
            if(string(1:1) == '@') string = string(2:)
            i1 = index(string,',')
            if(i1 > 0) string(i1:i1) = '.'

            ! write(66,*) '############### GEtDoubleArr:    i=',int(i,2),'  string=',trim(string)

            read(string,*,iostat=ios) dummy

            is = ubound(darray,dim=1)
            if(i <= is) then
                if(ios == 0) darray(i) = dummy
                if(ios /= 0) darray(i) = missingval
            else
                call RealModA1(darray,i)
                darray(i) = missingval
            end if
            ! write(66,*) '############### GEtDoubleArr:    i=',int(i,2),'  darray(i)=',sngl(darray(i))

            if(itv > 0) then
                tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(string))
            end if
        end do

    end subroutine WTreeViewGetDoubleArray

!###############################################################################

    subroutine WTreeViewPutDoubleCell(treename, ncol, nrow, dval)

        use gtk_hl,                   only: hl_gtk_listn_set_cell,hl_gtk_listn_get_cell
        Use UR_Gleich,                only: missingval,ngrs
        use UR_Variables,             only: frmt,frmtg,frmt_min1,frmtc, sDecimalPoint
        use UR_gtk_variables,         only: ntvs,tvnames,tv_colwidth_digits
        use CHF,                      only: FormatNumStr

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! column-number
        integer   ,intent(in)            :: nrow                  ! row-number
        real(rn),intent(in)              :: dval                  ! value to be stored

        type(c_ptr)                  :: tree
        integer(c_int)               :: irow1,icol1
        integer                      :: i,itv
        character(len=50)            :: str
        character(len=50)            :: frmtv
        real(rn),allocatable         :: rdummy(:)
        character(:),allocatable     :: str2
!---------------------------------------------------
        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1
        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do

        frmtv = frmt
        if(dval < 0.10_rn) frmtv = frmt_min1
        if(trim(treename) == 'treeview6') frmtv = frmtg
        if(trim(treename) == 'treeviewELI') frmtv = frmtc
        if(trim(treename) == 'treeview3') frmtv = frmtc

        if(trim(treename) == 'treeview2' .and. nrow > ngrs) goto 999
        if(.not.allocated(str2)) allocate(character(len=40) :: str2)
        write(str,frmtv) dval

        str2 = FormatNumStr(trim(str), sDecimalPoint)
        if(abs(dval-missingval) < EPS1MIN) str2 = '  '
        call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,svalue=str2)

        if(itv > 0) then
            tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(str))
        end if
999     continue
        if(allocated(str2)) deallocate(str2)
        if(allocated(rdummy)) deallocate(rdummy)

        item_setintern = .false.

    end subroutine WTreeViewPutDoubleCell

!###############################################################################

    subroutine WTreeViewGetDoubleCell(treename, ncol, nrow, dval)

        use gtk_hl,               only: hl_gtk_listn_get_cell
        Use UR_Gleich,            only: missingval

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! column-number
        integer   ,intent(in)            :: nrow                  ! row-number
        real(rn),intent(out)             :: dval                  ! value to be read

        type(c_ptr)                  :: tree
        integer(c_int)               :: irow1,icol1
        integer                      :: ios,i1
        character(len=50)            :: str
!---------------------------------------------------
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1
        call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=str)
        i1 = index(str,',')
        if(i1 > 0) THEN
            str(i1:i1) = '.'
        end if

        read(str,*,iostat=ios) dval

        if(ios /= 0) dval = missingval

    end subroutine WTreeViewGetDoubleCell

!###############################################################################

    subroutine WTreeViewPutStrCell(treename, ncol, nrow, string)

        use gtk_hl,                   only: hl_gtk_listn_set_cell
        use UR_gtk_variables,         only: ntvs,tvnames,tv_colwidth_digits

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! column-number from the array is loaded
        integer   ,intent(in)         :: nrow                  ! row-number
        character(len=*),intent(in)   :: string                ! value to be stored

        type(c_ptr)                         :: tree
        integer(c_int)                      :: irow1,icol1
        integer                             :: i,itv
        character(len=len_trim(string)+20)  :: str, xstr
!---------------------------------------------------
        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1
        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do
        str = string
        if(len_trim(str) == 0) str = ' '

        xstr = max(' ',trim(str))
        call hl_gtk_listn_set_cell(list=tree, row=irow1, col=icol1,  svalue=xstr)
        item_setintern = .false.
        if(itv > 0) then
            tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(xstr))
        end if

    end subroutine WTreeViewPutStrCell

!###############################################################################

    subroutine WTreeViewGetStrCell(treename, ncol, nrow, string)

        use gtk_hl,                   only: hl_gtk_listn_get_cell
        use UR_gtk_variables,         only: ntvs,tvnames,tv_colwidth_digits

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! column-number
        integer   ,intent(in)            :: nrow                  ! row-number
        character(:),allocatable, intent(out)  :: string          ! string to be read

        type(c_ptr)                 :: tree
        integer(c_int)              :: irow1,icol1
        integer                     :: i,itv
        character(len=:),allocatable :: str1
!---------------------------------------------------
        item_setintern = .true.

! allocate(character(len=200) :: string,str1)
        allocate(character(len=2000) :: string,str1)         ! 12.8.2023

! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1
        itv = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) itv = i
        end do

        call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)
        if(len_trim(string) > 0) then
            string = trim(string)
            if(string(1:1) == char(0)) string = ' '
        else
            string = ' '
        end if

        if(itv > 0) then
            tv_colwidth_digits(itv,ncol) = max(tv_colwidth_digits(itv,ncol), len_trim(string))
        end if
        item_setintern = .false.

    end subroutine WTreeViewGetStrCell

!###############################################################################

    subroutine WTreeViewPutComboArray(treename, ncol, nvals, iarray)

        use gtk_hl,                   only: hl_gtk_listn_set_cell
        use UR_Gleich,                only: symbole,absrel,vcovcor,vdopt

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! column-number
        integer   ,intent(in)         :: nvals                 ! number of values to be stored
        integer   ,intent(in)         :: iarray(nvals)         ! array of integers to be stored

        type(c_ptr)             :: tree
        integer(c_int)          :: irow1,icol1
        integer                 :: i, k,ixx
        character(len=40)       :: string
!---------------------------------------------------
        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        do i = 1, nvals
            irow1 = i - 1
            select case (trim(treename))
              case ('treeview2')
                if(ncol == 6) then
                    ixx = iarray(i)
                    if(ixx > 11) ixx = 1       !
                    if(i <= size(Symbole)) then
                        string = vdopt(max(1,ixx))%s
                    end if
                else if(ncol == 10) then
                    ixx = iarray(i)
                    if(ixx > 2) ixx = 1
                    if(i <= size(Symbole)) then
                        string = ' '
                        if(iarray(i) > 0) string = absrel(ixx)%s
                    end if
                end if

              case ('treeview3')
                if(ncol == 2 .or. ncol == 3) then
                    if(i <= ubound(Symbole,dim=1)) then
                        string = ' '
                        if(iarray(i) > 0) then
                            string = Symbole(iarray(i))%s
                        else
                            cycle
                        end if
                    end if
                else if(ncol == 4) then
                    if(i <= ubound(Symbole,dim=1)) then
                        string = ' '
                        if(iarray(i) > 0) string = vcovcor(iarray(i))%s
                    end if
                end if
              case default
            end select
            call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=trim(string))

        end do
        item_setintern = .false.

    end subroutine WTreeViewPutComboArray

!###############################################################################

    subroutine WTreeViewGetComboArray(treename, ncol, nvals, iarray)

        use gtk_hl,                   only: hl_gtk_listn_get_cell,hl_gtk_tree_get_cell
        use UR_Gleich,                only: symbole,absrel,vcovcor,vdopt,ndopt

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! column-number
        integer   ,intent(in)         :: nvals                 ! number of values to be loaded
        integer   ,allocatable        :: iarray(:)             ! array of integers to be loaded

        type(c_ptr)             :: tree
        integer(c_int)          :: irow1,icol1
        integer                 :: i, k
        character(len=40)       :: string
        integer   ,allocatable   :: ii4(:)
!---------------------------------------------------

! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        if(.not.allocated(iarray)) allocate(iarray(1))

        do i = 1, nvals
            irow1 = i - 1
            call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)
            if(i > ubound(iarray,dim=1)) then
                allocate(ii4(i))
                ii4(1:i-1) = iarray
                call move_alloc(ii4, iarray)
            end if
            select case (trim(treename))
              case ('treeview2')
                if(ncol == 6) then
                    do k=1,ndopt
                        if(trim(vdopt(k)%s) == trim(string)) iarray(i) = k
                    end do
                else if(ncol == 10) then
                    do k=1,2
                        if(trim(absrel(k)%s) == trim(strinG)) iarray(i) = k
                    end do
                end if
              case ('treeview3')
                if(ncol == 2 .or. ncol == 3) then
                    iarray(i) = 0
                    do k=1,ubound(Symbole,dim=1)
                        if(len_trim(symbole(k)%s) == 0) exit
                        if(trim(Symbole(k)%s) == trim(string)) then
                            iarray(i) = k
                            exit
                        end if
                    end do
                else if(ncol == 4) then
                    do k=1,2
                        if(trim(vcovcor(k)%s) == trim(string)) iarray(i) = k
                    end do
                end if
              case default
            end select
        end do

    end subroutine WTreeViewGetComboArray

!###############################################################################


    subroutine WTreeViewPutComboCell(treename, ncol, nrow, ival)

        use gtk_hl,                   only: hl_gtk_listn_set_cell
        use UR_Gleich,                only: symbole,absrel,vcovcor,vdopt

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! cell column-number
        integer   ,intent(in)         :: nrow                  ! cell row-number
        integer   ,intent(in)         :: ival                  ! option index to be stored

        type(c_ptr)           :: tree
        integer(c_int)        :: irow1,icol1
        character(len=40)     :: string
!---------------------------------------------------
        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1

        select case (trim(treename))
          case ('treeview2')
            if(ncol == 6) then
                string = vdopt(ival)%s
            else if(ncol == 10) then
                string = absrel(ival)%s
            end if

          case ('treeview3')
            if(ncol == 2 .or. ncol == 3) then
                string = Symbole(ival)%s
            else if(ncol == 4) then
                string = vcovcor(ival)%s
            end if
          case default
        end select
        call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=trim(string))
        item_setintern = .false.

    end subroutine WTreeViewPutComboCell

    !###############################################################################

    subroutine WTreeViewGetComboCell(treename, ncol, nrow, ival)

        use gtk_hl,                   only: hl_gtk_listn_get_cell
        use UR_Gleich,                only: symbole,absrel,vcovcor,vdopt

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! cell column-number
        integer   ,intent(in)         :: nrow                  ! cell row-number
        integer   ,intent(out)        :: ival                  ! option-index to be read

        type(c_ptr)           :: tree
        integer(c_int)        :: irow1,icol1
        integer               :: k
        character(len=40)     :: string
        !---------------------------------------------------
        item_setintern = .true.
        ! get c_ptr tree from treename:
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1

        select case (trim(treename))
          case ('treeview2')
            ! noch ändern
            if(ncol == 6) then
                string = vdopt(ival)%s
            else if(ncol == 10) then
                string = absrel(ival)%s
            end if

          case ('treeview3')
            if(ncol == 2 .or. ncol == 3) then
                call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=string)
                ival = 0
                do k=1,ubound(Symbole,dim=1)
                    if(len_trim(string) > 0 .and. trim(Symbole(k)%s) == trim(string)) then
                        ival = k
                        exit
                    end if
                end do
            else if(ncol == 4) then
                ! noch ändern (?)
                string = vcovcor(ival)%s
            end if
          case default
        end select
        item_setintern = .false.

    end subroutine WTreeViewGetComboCell

!###############################################################################

    subroutine WTreeViewPutCheckArray(treename, ncol, nvals, intarr)

        use gtk,                      only: gtk_tree_view_get_model, gtk_tree_store_set_value, &
            gtk_cell_renderer_toggle_set_active, &
            gtk_cell_renderer_toggle_get_active
        use gtk_hl,                   only: hl_gtk_listn_set_cell, hl_gtk_listn_get_cell, &
            hl_gtk_tree_row_iter
        use g,                        only: g_value_init, g_value_set_boolean
        use UR_gtk_variables,         only: toggleTypeGTK

        implicit none

        character(len=*),intent(in)    :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)          :: ncol                  ! number of the column into which the array is to be stored
        integer   ,intent(in)          :: nvals                 ! number of values to be stored
        integer   ,intent(in)          :: intarr(nvals)         ! array of integers to be stored

        type(c_ptr)           :: tree
        integer(c_int)        :: irow1,icol1
        integer               :: i
        character(len=1)      :: str
        logical               :: bcheck

!---------------------------------------------------
        item_setintern = .true.
! get c_ptr tree from treename:
        tree = idpt(trim(treename))

        icol1 = ncol - 1
        do i = 1, nvals
            irow1 = i - 1
            write(str,'(i1)') intarr(i)
            if(intarr(i) == 0) bcheck = .false.
            if(intarr(i) == 1) bcheck = .true.

            select case (toggleTypeGTK)

              case ('text')
                if(intarr(i) == 0) call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue='F')
                if(intarr(i) == 1) call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue='T')
                str = ' '
                call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=str)

              case ('bool')
                bcheck = .false.
                if(intarr(i) == 1) bcheck = .true.
                call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  logvalue=bcheck)

              case default
            end select
        end do
        item_setintern = .false.

    end subroutine WTreeViewPutCheckArray

!###############################################################################

    subroutine WTreeViewGetCheckArray(treename, ncol, nvals, intarr)

        use gtk_hl,                   only: hl_gtk_listn_get_cell
        use UR_gtk_variables,         only: toggleTypeGTK

        implicit none

        character(len=*),intent(in)   :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)         :: ncol                  ! number of the column from which the array is to be loaded
        integer   ,intent(in)         :: nvals                 ! number of values to be loaded
        integer   ,intent(out)        :: intarr(nvals)         ! array of integers to be stored

        type(c_ptr)           :: tree
        integer(c_int)        :: irow1,icol1
        integer               :: i
        logical               :: bcheck
        character(len=1)      :: str

!---------------------------------------------------
! get c_ptr tree from treename:
        tree = idpt(trim(treename))

        icol1 = ncol - 1
        do i = 1, nvals
            irow1 = i - 1

            select case (toggleTypeGTK)

              case ('text')
                str = ' '
                call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=str)
                if(str == 'F') intarr(i) = 0
                if(str == 'T') intarr(i) = 1

              case ('bool')
                call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  logvalue=bcheck)
                if(.not.bcheck) intarr(i) = 0
                if(bcheck) intarr(i) = 1

              case default
            end select

        end do

    end subroutine WTreeViewGetCheckArray

!###############################################################################

    subroutine WTreeViewSetCursorCell(treename, ncol, nrow, edit)

        use gtk,                only:   gtk_tree_view_set_cursor_on_cell, gtk_tree_path_new_from_string, &
                                        gtk_widget_grab_focus,gtk_tree_view_set_cursor, &
                                        gtk_tree_view_row_activated,gtk_tree_view_get_column
        use UR_gtk_variables,   only:   ntvs,tvcolindex,tvnames

        implicit none

        character(len=*),intent(in)    :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)          :: ncol                  ! column-number for setting cursor
        integer   ,intent(in)          :: nrow                  ! row-number for setting cursor
        logical,intent(in),optional    :: edit

        type(c_ptr)                  :: tree
        type(c_ptr)                  :: focus_column, focus_cell,path
        integer(c_int)               :: irow1,icol1,openfield
        integer                      :: i, nt
        character(len=10)            :: fpath
        character(len=3)             :: chcol,chrow

        item_setintern = .true.
        tree = idpt(trim(treename))
        icol1 = ncol - 1
        irow1 = nrow - 1
        ! write(fpath,'(i1,a1,i1)') irow1,':',icol1
        write(chcol,'(i3)') icol1
        write(chrow,'(i3)') irow1
        fpath = trim(adjustL(chrow)) // ':' // trim(adjustL(chcol))
        if(trim(treename) == 'treeview8') fpath = trim(chrow)
        path = gtk_tree_path_new_from_string(trim(fpath)//c_null_char)
        !write(66,*) 'SetCursorCell:  path = ',path,'  tree=',tree,' nrow=',int(nrow,2),' fpath=',fpath

        if(.not.present(edit)) then
            openfield = 0_c_int
        else
            openfield = 0_c_int
            if(edit) openfield = 1_c_int
        end if

        nt = 0
        do i=1,ntvs
            if(trim(treename) == tvnames(i)%s) then
                nt = i
                exit
            end if
        end do
        write(chcol,'(i3)') tvcolindex(nt, icol1+1)

        focus_column = gtk_tree_view_get_column(tree, icol1)
        focus_cell = idpt('cellrenderertext'//trim(adjustl(chcol)))
        if(trim(treename) == 'treeview8') focus_cell = idpt('cellrenderertext88')

        item_setintern = .false.

        call gtk_tree_view_set_cursor_on_cell(tree, path, focus_column, focus_cell, openfield)

        item_setintern = .false.

    end subroutine WTreeViewSetCursorCell

!###############################################################################

    subroutine FOpen(ifehl, create, Hinweis)
        !
        !   This subroutine processes Open selection
        !
        use gtk,          only: GTK_BUTTONS_OK, &
                                GTK_BUTTONS_Yes_NO, &
                                gtk_RESPONSE_YES, &
                                gtk_RESPONSE_No, &
                                gtk_RESPONSE_CANCEL, &
                                GTK_RESPONSE_APPLY, &
                                GTK_MESSAGE_WARNING
        USE UR_Variables
        use UR_MCC,       only: iopt_copygr
        use UR_Gleich,    only: GrFormat
        use Top,          only: FieldUpdate
        use CHF,          only: ucase
        use translation_module, only: T => get_translation

        implicit none

        integer, intent(out)           :: ifehl
        logical, intent(in)            :: create
        character(len=*),intent(inout) :: hinweis

        integer                :: mift
        integer                :: resp
        CHARACTER(LEN=256)     :: fstr1

        integer                :: nfilt
        character(len=15)      :: filtergtk(5)
        character(len=30)      :: filternames(5)
        character(len=4)       :: grf
        logical                :: okay
        !----------------------------------------------------------------------
        ifehl = 0

        !// IF (Filetyp == 'P' .AND. SAVEP .and. .not.saveas) THEN
        IF (Filetyp == 'P' .AND. SAVEP .and. .not.saveas .and. len_trim(fname) > 0) THEN
            fstr1 = T("The present file is yet unsaved!") // CHAR(13) // &
                    T("Do you wish to continue anyhow?")
            call MessageShow(trim(fstr1), GTK_BUTTONS_Yes_NO, T("Open file:"), &
                             resp, mtype=0_c_int)

            select case (resp)
            case (gtk_RESPONSE_YES, gtk_RESPONSE_APPLY)
                ! answer is yes
            case (gtk_RESPONSE_NO, gtk_RESPONSE_CANCEL)
                fstr1 = T("Please first save the present file!")
                call MessageShow(trim(fstr1), GTK_BUTTONS_OK, T("Open file:"), resp, &
                                 mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                goto 9000
            end select

        end if

        nfilt = 3
        filtergtk(1) = '*.txp'
        filtergtk(2) = '*.csv'
        filtergtk(3) = '*.*'

        select case (FileTyp)

          case ('P')

            SAVEP  = .FALSE.

            ! FILTER = 'TXP files|*.txp|CSV files|*.csv|All files|*.*|'
            filternames(1) = 'txp ' // T('Files')
            filternames(2) = 'csv ' // T('Files')
            filternames(3) = 'all ' // T('Files')

          case ('F')
            nfilt = 2
            filtergtk(1) = '*.txt'
            filtergtk(2) = '*.*'

            ! FILTER = 'TXP files|*.txp|CSV files|*.csv|All files|*.*|'
            filternames(1) = 'txt ' // T('Files')
            filternames(2) = 'all ' // T('Files')

          case ('G')                     ! graphics output
            ! SAVEP  = .FALSE.                  ! wenn, dann savef ?

            ! FILTER = 'TXP files|*.txp|CSV files|*.csv|All files|*.*|'
            filternames(1) = GrFormat(iopt_copygr)%s
            nfilt = 2
            if(filternames(1)(1:3) == 'PNG') grf = 'png'
            if(filternames(1)(1:3) == 'BMP') grf = 'bmp'
            if(filternames(1)(1:4) == 'JPEG') grf = 'jpeg'
            if(filternames(1)(1:3) == 'PDF') grf = 'pdf'
            filtergtk(1) = '*.'//trim(grf)
            filtergtk(2) = '*.*'
          case ('D')
            nfilt = 2
            filtergtk(1) = '*.csv'
            filtergtk(2) = '*.*'

            ! FILTER = 'TXP files|*.txp|CSV files|*.csv|All files|*.*|'
            filternames(1) = 'csv ' // T('Files')
            filternames(2) = 'all ' // T('Files')

        end select

15      CONTINUE

        mift = 1   ! entspricht *.txp

        CALL WSelectFile(Hinweis, create, nfilt, filtergtk, filternames, okay)
        if ( .not. okay) then
            ifehl = 1
            goto 9000
        end if

        CALL FieldUpdate()

        fstr1 = trim(ucase(fname))
        if(FileTyp == 'P') then
            mift = 0
            fstr1 = ucase(FNAME)
            if(INDEX(fstr1, '.TXP') > 0) mift = 1
            if(INDEX(fstr1, '.CSV') > 0) mift = 2
            ! write(66,*)'Open: fname=',trim(fname)

            IF(mift == 0 .and. .not. create) THEN
                call MessageShow(T("The file is not a project file!"), &
                                 GTK_BUTTONS_OK, "Open file:"//c_null_char, &
                                 resp, mtype=GTK_MESSAGE_WARNING)

                GOTO 15
            end if
        end if
        if(FileTyp == 'F') then
            mift = 0
            fstr1 = ucase(EditorFileName)
            if(INDEX(fstr1, '.TXT') > 0) mift = 1
            if(INDEX(fstr1, '.CSV') > 0) mift = 2
            if(mift == 0) mift = 1

        end if
        if(FileTyp == 'G') then
            if(mift == 0) mift = 1
        end if

        if(FileTyp == 'D') then
            mift = 0
            fstr1 = ucase(EditorFileName)
            if(INDEX(fstr1, '.CSV') > 0) mift = 1
            ! if(INDEX(fstr1, '.*') > 0) mift = 2
            if(mift == 0) mift = 1

        end if

        ifehl = 0

9000    continue

    END SUBROUTINE FOpen


    !#############################################################################################

    subroutine WSelectFile(Hinweis, createf, nfilt, filtergtk, filternames, okay)

        use gtk,          only: GTK_BUTTONS_YES_NO, GTK_RESPONSE_NO, GTK_BUTTONS_OK, &
                                gtk_recent_manager_has_item, gtk_recent_manager_add_item, &
                                gtk_recent_manager_remove_item,gtk_recent_manager_get_default, &
                                gtk_recent_chooser_get_items,GTK_MESSAGE_INFO

        use gtk_hl,       only: hl_gtk_file_chooser_show, false, true

        use UR_VARIABLES, only: work_path, fname, FileTyp, &
                                EditorFileName, fname_grout, &
                                serial_csvinput, filtname, dir_sep

        use CHF,          only: ucase
        use translation_module, only: T => get_translation

        implicit none


        character(len=*),intent(IN)      :: hinweis
        logical,intent(in)               :: createf
        integer   ,intent(in)            :: nfilt
        character(len=*), intent(in)     :: filtergtk(nfilt)
        character(len=*), intent(in)     :: filternames(nfilt)
        logical,intent(out)              :: okay

        character(len=256),allocatable      :: filenames(:)
        integer(c_int)                      :: isel,ccreate,cint
        character(kind=c_char),allocatable  :: ctitle(:)
        character(kind=c_char),allocatable  :: cinidir(:)
        logical                             :: lexist
        character(len=256)                  :: str1,xhinweis,xfname,filnam1
        character(len=255)                  :: gwork_path,fnamex
        integer                             :: resp
        type(c_ptr)                         :: recentmanager
        integer                             :: i,i1,kloop,i0
        character(len=6)                    :: cfext
        !------------------------------------------------------------------------------------------
        !isel = function hl_gtk_file_chooser_show(files, cdir, directory, create = False, &
        !       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
        !       & initial_dir, current, initial_file, filter, filter_name, parent, &
        !       & all, wsize, edit_filters) result(isel)
        okay = .true.
        ccreate = FALSE
        if(createf) ccreate = True
        xhinweis = trim(hinweis)

        gwork_path = work_path

        call convert_f_string_s(gwork_path, cinidir)
        call convert_f_string_s(xhinweis, ctitle)

        ! write(0,*) 'WSElectFile: initial_dir=',trim(work_path)
        ! write(0,*) 'inidir=',cinidir
        ! write(0,*) 'Hinweis=',trim(hinweis)
        ! write(0,*) 'ccreate=',ccreate,'  FileTyp=',FileTyp,' kloop=',int(kloop,2)

        kloop = 0
10      continue
        kloop = kloop + 1
        if(FileTyp == 'P') then

            if(kloop == 1) then
                if(allocated(filenames)) deallocate(filenames)
                allocate(filenames(1))
                ! filenames(1) = trim(fname)   ! no: filenames only on output
                if(createf) then
                    i1 = 0
                    do i=len_trim(fname),1,-1
                        if(fname(i:i) == '.') then
                            i1 = i
                            exit
                        end if
                    end do
                    xfname = trim(fname)
                    if(i1 > 0) xfname = trim(fname(1:i1-1)) ! // '.txp'
                    do i=len_trim(xfname),1,-1
                        if(xfname(i:i) == dir_sep) then
                            xfname = xfname(i+1:)           ! 23.6.2023
                        end if
                    end do
                end if
            end if

            if(.not.createf) then
                isel = hl_gtk_file_chooser_show(files=filenames, create = ccreate, &
                & allow_uri = False,                &
                ! & allow_uri = True,                &
                & show_hidden = True,   &      ! False,  &
                & confirm_overwrite=True,           &
                & title = ctitle, filter = filtergtk,  &
                & filter_name=filternames, initial_dir = cinidir, &
                & wsize=(/760_c_int, 500_c_int/))

                if(isel == 0_C_INT .or. ubound(filternames,dim=1) < 1) goto 100
                i0 = index(ucase(filenames(1)),'.TXP')
                i1 = index(filtname,' ')
                if(i1 > 3) then
                    cfext = '.' // adjustL(filtname(1:i1-1))
                    if(trim(ucase(cfext)) /= '.TXP' .and. i0 == 0) then
                        if(index(ucase(filenames(1)),ucase(trim(cfext))) == 0) then
                            filenames(1) = trim(filenames(1)) // trim(cfext)
                        end if
                    end if
                    if(trim(ucase(cfext)) == '.TXP' .and. i0 == 0) then
                        if(index(ucase(filenames(1)),ucase(trim(cfext))) == 0) then
                            filenames(1) = trim(filenames(1)) // trim(cfext)
                        end if
                    end if

                end if

            else
                isel = hl_gtk_file_chooser_show(files=filenames, create = ccreate, &
                & allow_uri = False,                &
                ! & allow_uri = True,                &
                & show_hidden = False,  &
                & confirm_overwrite=True,           &
                & title = ctitle,    &
                & filter = filtergtk,  &
                !  & filter_name=filternames, initial_dir = cinidir,  &
                & filter_name=filternames, current = 1_c_int,  &          ! 23.6.2023
                & initial_file=trim(xfname)//c_null_char,  &
                & wsize=(/760_c_int, 500_c_int/))
                if(isel == 0_C_INT) goto 100
            end if

        elseif(FileTyp == 'F') then
            isel = hl_gtk_file_chooser_show(files=filenames, create = ccreate, &
            & allow_uri = False,                &
            ! & allow_uri = True,                &
            & confirm_overwrite=True,           &
            & title = ctitle, filter = filtergtk,  &
            & filter_name=filternames, initial_dir = cinidir, &
            & initial_file=trim(EditorFileName)//c_null_char )
            if(isel == 0_C_INT) goto 100
            EditorFileName = filenames(1)
            !!%%  call LFU(EditorFileName)
            EditorFileName = EditorFileName

        elseif(FileTyp == 'G') then
            isel = hl_gtk_file_chooser_show(files=filenames, create = ccreate, &
            & allow_uri = False,                &
            ! & allow_uri = True,                &
            & confirm_overwrite=True,           &
            & title = ctitle, filter = filtergtk,  &
            & filter_name=filternames, initial_dir = cinidir, &
            & initial_file='*.pdf'//c_null_char )
            if(isel == 0_C_INT) goto 100
        elseif(FileTyp == 'D') then
            isel = hl_gtk_file_chooser_show(files=filenames, create = ccreate, &
            & allow_uri = False,                &
            ! & allow_uri = True,                &
            & confirm_overwrite=True,           &
            & title = ctitle, filter = filtergtk,  &
            & filter_name=filternames, initial_dir = cinidir, &
            & initial_file=trim(EditorFileName)//c_null_char )
            if(isel == 0_C_INT) goto 100
            serial_csvinput = filenames(1)
        end if

        if(isel == False) then
            okay = .false.
            return
        end if

        if(size(filenames) == 0) then
            okay = .false.
            return
        end if

        if(kloop == 1) then

            filnam1 = ucase(filenames(1))
            i1 = index(filnam1, '.TXP')
            if(i1 == 0 .and. ccreate == TRUE .and. FileTyp == 'P') then
                ! add the project file ext
                i1 = len_trim(filnam1) - 1
                filenames(1) = trim(filenames(1)) // '.txp'
            end if
            if(i1 == 0) i1 = index(filnam1, '.CSV')
            if(i1 == 0) i1 = index(filnam1, '.TXT')

            if(i1 == 0 .and. FileTyp == 'G')  i1 = index(filnam1, '.PDF')
            if(i1 == 0 .and. FileTyp == 'G')  i1 = index(filnam1, '.EPS')
            if(i1 == 0 .and. FileTyp == 'G')  i1 = index(filnam1, '.JPEG')
            if(i1 == 0 .and. FileTyp == 'G')  i1 = index(filnam1, '.PNG')
            if(i1 == 0 .and. FileTyp == 'G')  i1 = index(filnam1, '.BMP')
            if(i1 == 0) then

                call MessageShow(T("Please check the file extension!"), &
                                 GTK_BUTTONS_OK,'WselectF:', resp, &
                                 mtype=GTK_MESSAGE_INFO)
                kloop = 0
                goto 10
            end if
        end if

        if(isel == True) then
            ! Yes:
            if(FileTyp == 'P') fname = trim(filenames(1))
            if(FileTyp == 'F') EditorFilename = trim(filenames(1))
            if(FileTyp == 'G') fname_grout = trim(filenames(1))
            if(FileTyp == 'D') serial_csvinput = trim(filenames(1))

            Inquire(file=filenames(1), exist=lexist)
            if(ccreate == True .and. lexist) then
                str1 = T("The file already exists!") // CHAR(13) // T("Do you wish to over-write this file?")


                call MessageShow(trim(str1), GTK_BUTTONS_Yes_NO, "Over-write file:"// c_null_char, resp, mtype=GTK_MESSAGE_INFO)
                if(resp == GTK_RESPONSE_NO) goto 10

            end if
            if(FileTyp == 'P') then

                ! Note: RecentManager works with a file named recently-used.xbel on drive C.
                recentmanager = gtk_recent_manager_get_default()

                fnamex = fname
                do i=1,len_trim(fnamex)
                    if(fnamex(i:i) == '\') fnamex(i:i) = dir_sep
                end do
                fnamex = 'file:///' // trim(fnamex)
                cint   = gtk_recent_manager_add_item(recentmanager,trim(fnamex)//c_null_char)
            end if

        elseif(isel == 0) then
            ! No:
            okay = .false.
        end if
        return

100     continue
        okay = .false.

    end subroutine WSelectFile

!#####################################################################################

    subroutine WDNotebookSetCurrPage(nbstring, ipage)

        use UR_Loadsel,            only: NBcurrentPage, NBpreviousPage
        use gtk,                   only: gtk_notebook_set_current_page,gtk_notebook_get_current_page, &
                                         gtk_label_set_markup,gtk_widget_show,gtk_notebook_get_nth_page, &
                                         gtk_widget_show_all,gtk_notebook_prev_page
        use UR_gtk_variables,      only: NBsoftSwitch, Nbook2

        implicit none

        character(len=*),intent(in)    :: nbstring       ! GUI name of the notebook
        integer   ,intent(in)          :: ipage

        type(c_ptr)                :: widget
        integer(c_int)             :: curp
        integer                    :: ncp

        item_setintern = .true.
        NBsoftSwitch = .true.

        if(trim(nbstring) == 'notebook1') ncp = NBcurrentPage
        curp = ipage
        curp = curp - 1_c_int
        if(trim(nbstring) == 'notebook1') widget = idpt(trim(nbstring))
        if(trim(nbstring) == 'notebook2') widget = nbook2
        call gtk_notebook_set_current_page(widget,curp)         !

        NBcurrentPage = ipage

        if(trim(nbstring) == 'notebook1' .and. ipage /= ncp) then
            NBpreviousPage = ncp
            NBcurrentPage = ipage
            call NBlabelmodify()
        end if

        if(trim(nbstring) == 'notebook1') NBsoftSwitch = .false.
        item_setintern = .false.

    end subroutine WDNotebookSetCurrPage

!#####################################################################################

    subroutine WDNotebookGetCurrPage(nbstring, ipage)

        use UR_gtk_variables,      only: clobj
        use UR_Loadsel,            only: NBcurrentPage, NBpreviousPage
        use gtk,                   only: gtk_notebook_get_current_page,gtk_notebook_get_nth_page

        implicit none

        character(len=*),intent(in)    :: nbstring       ! NAme des Notebooks
        integer   ,intent(out)         :: ipage

        type(c_ptr)                :: nbk,cptr
        integer(c_int)             :: curp
        integer                    :: ncp,nci
        character(len=50)          :: childname

        if(trim(nbstring) == 'notebook1') then
            ncp = NBcurrentPage
            nbk = idpt(trim(nbstring))
            curp = gtk_notebook_get_current_page(nbk)
            ipage = curp + 1
        end if

        cptr = gtk_notebook_get_nth_page(idpt(nbstring), curp)
        call FindItemP(cptr, nci)
        if(nci > 0) childname = clobj%idd(nci)%s

        if(ipage /= ncp) then
            NBpreviousPage = ncp
            NBcurrentPage = ipage
            call NBlabelmodify()
        end if

    end subroutine WDNotebookGetCurrPage

!#####################################################################################

    subroutine NBlabelmodify

        use UR_gtk_variables,         only: Notebook_labelid,Notebook_labeltext,item_setintern,NBsoftSwitch
        use UR_Loadsel,               only: NBcurrentPage, NBpreviousPage
        use gtk,                      only: gtk_label_set_markup

        implicit none

        type(c_ptr)        :: ptr
        integer            :: i

        item_setintern = .true.
        NBsoftSwitch = .true.

        if(i == 0) goto 100
        i = NBcurrentPage
        ptr = idpt(trim(Notebook_labelid(i)))
        call gtk_label_set_markup(ptr,  &
            '<span foreground="blue">' // trim(Notebook_labeltext(i)) // '</span>'//c_null_char)
        i = NBpreviousPage
        ptr = idpt(trim(Notebook_labelid(i)))
        call gtk_label_set_markup(ptr,  &
            '<span foreground="black">' // trim(Notebook_labeltext(i)) // '</span>'//c_null_char)

100     continue
        item_setintern = .false.
        NBsoftSwitch = .false.

    end subroutine NBlabelmodify

!#####################################################################################

    subroutine WTreeViewScrollRow(treename,krow)

        use Gtk,                only: gtk_cell_renderer_get_preferred_size,gtk_cell_renderer_get_alignment, &
            gtk_tree_view_scroll_to_point,gtk_tree_view_get_model, &
            gtk_builder_get_object,gtk_tree_view_scroll_to_cell,   &
            gtk_tree_path_new_from_string,gtk_scrollable_get_vadjustment, &
            gtk_adjustment_get_value,gtk_adjustment_set_value,   &
            gtk_scrollable_set_vadjustment
        use UR_gtk_variables,   only: ntvs,tvnames,tvcolindex

        implicit none

        character(len=*),intent(in)   :: treename
        integer   ,intent(in)         :: krow

        integer                      :: i,i1,nt,ntvind,nrows
        integer(c_int)               :: irow1
        character(len=3)             :: chcol,chrow
        type(c_ptr)                  :: trvw,cellrend,focus_col,model,path,pvadj
        type(GtkRequisition),target  :: minsize,natsize
        real(c_double)               :: vadj
        character(len=7)             :: fpath
!-------------------------------------------------------------------------------------
        i1 = index(treename,'treeview') + len('treeview')
        read(treename(i1:),*)  ntvind
        trvw = idpt(trim(treename))

        model = gtk_tree_view_get_model(trvw)
        nt = 0
        do i=1,ntvs
            if(trim(treename) == tvnames(i)%s) then
                nt = i
                exit
            end if
        end do
        write(chcol,'(i3)') tvcolindex(nt, 2)     !   ,1)
        focus_col = idpt('treeviewcolumn'//trim(adjustl(chcol)))
        cellrend = idpt('cellrenderertext'//trim(adjustl(chcol)))

        call WTreeviewVisibleRows(trvw, nrows) ! visible rows

        call gtk_cell_renderer_get_preferred_size (cellrend,trvw, c_loc(minsize),c_loc(natsize))
        irow1 = krow - 1
        write(chcol,'(i3)') 4_c_int
        write(chrow,'(i3)') irow1
        fpath = trim(adjustL(chrow)) // ':' // trim(adjustL(chcol))
        path = gtk_tree_path_new_from_string(trim(fpath)//c_null_char)

        call gtk_tree_view_scroll_to_cell(trvw,path,focus_col,1_c_int,0.0_c_float, 0.5_c_float)

        pvadj = gtk_scrollable_get_vadjustment(trvw)
        vadj = gtk_adjustment_get_value(pvadj)
        vadj = krow*(natsize%height+2_c_int)

        call gtk_adjustment_set_value(pvadj, vadj)
        call gtk_scrollable_set_vadjustment(trvw,pvadj)

        call pending_events()

    end subroutine WTreeViewScrollRow

!###############################################################################

    subroutine WTreeviewVisibleRows(trvw, nrows)

        use file_io,            only: logger
        use Gtk,                only: gtk_tree_path_new, gtk_tree_view_get_visible_range, &
            gtk_tree_path_to_string

        implicit none

        type(c_ptr),intent(in)     :: trvw
        integer   ,intent(out)     :: nrows

        logical                    :: prout
        integer                    :: ntop,nbottom
        type(c_ptr), target        :: start_path,end_path,cpp
        integer(c_int)             :: cres
        character(len=512)         :: log_str
        character(len=10),pointer  :: stext,etext

        prout = .false.
        ! prout = .true.

        nrows = 1
        start_path = gtk_tree_path_new()
        end_path = gtk_tree_path_new()

        cres = gtk_tree_view_get_visible_range(trvw,c_loc(start_path),c_loc(end_path))
!         if(prout) write(66,*) 'cres=',cres ,' start_path=',start_path,'  end_path=',end_path
        if(prout)  then
            write(log_str, '(*(g0))') 'cres=',cres ,' start_path=',start_path,'  end_path=',end_path
            call logger(66, log_str)
        end if
        if(cres == 0_c_int) return

        cpp =  gtk_tree_path_to_string(start_path)
        !         if(prout) write(66,*) 'start:  c_associated(cpp)=',c_associated(cpp)
        if(prout)  then
            write(log_str, '(*(g0))') 'start:  c_associated(cpp)=',c_associated(cpp)
            call logger(66, log_str)
        end if
        if(.not.c_associated(cpp)) return
        call c_f_pointer(cpp,stext)

        read(stext,'(i3)') ntop
        ntop = ntop + 1

        cpp =  gtk_tree_path_to_string(end_path)
        if(.not.c_associated(cpp)) return
        call c_f_pointer(cpp,etext)
        read(etext,'(i3)') nbottom
        nbottom = nbottom + 1

        nrows = nbottom - ntop
        !         if(prout) write(66,*) 'nbottom',nbottom,'  nrows_visible=',nrows,' Zeilen voll sichtbar'
        if(prout)  then
            write(log_str, '(*(g0))') 'nbottom',nbottom,'  nrows_visible=',nrows,' Zeilen voll sichtbar'
            call logger(66, log_str)
        end if

    end subroutine WTreeviewVisibleRows

!###############################################################################


    subroutine WTreeViewSetColorCell(treename, ncol, nrow, bcolorname)

        use gtk,                     only: gtk_tree_view_get_model
        use gtk_hl,                  only: hl_gtk_listn_set_cell

        implicit none

        character(len=*),intent(in)      :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)            :: ncol                  ! column-number for setting cursor
        integer   ,intent(in)            :: nrow                  ! row-number for setting cursor
        character(len=*),intent(in)      :: bcolorname            ! background-color name

        type(c_ptr)          :: tree,store
        integer(c_int)       :: irow1,icol1
!--------------------------------------------------------------------------------------
        item_setintern = .true.
        tree = idpt(trim(treename))
        store = gtk_tree_view_get_model(tree)
        !if(trim(treename) /= 'treeview2') then
        !  icol1 = ncol + 7 - 1
        !  irow1 = nrow - 1
        Select case (trim(treename))
          case ('treeview1')
            icol1 = ncol + 5 - 1
            irow1 = nrow - 1

          case ('treeview2')
            icol1 = ncol + 11 - 1
            irow1 = nrow - 1

          case ('treeview3')
            icol1 = ncol + 6 - 1
            irow1 = nrow - 1

          case ('treeview4')
            icol1 = ncol + 8 - 1
            irow1 = nrow - 1

          case ('treeview5')
            icol1 = ncol + 12 - 1
            irow1 = nrow - 1

          case ('treeview6')
            icol1 = ncol + 15 - 1
            irow1 = nrow - 1

          case ('treeview7')
            icol1 = ncol + 7 - 1
            irow1 = nrow - 1

          case ('treeview8')
            icol1 = ncol + 2 - 1
            irow1 = nrow - 1

          case ('treeviewELI')
            icol1 = ncol + 4 - 1
            irow1 = nrow - 1

        end select

        call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=bcolorname)
        item_setintern = .false.

    end subroutine WTreeViewSetColorCell

    !-----------------------------------------------------------------------------------------

    subroutine WTreeViewSetColorRow(treename, nrow, bcolorname)

        use gtk,                     only: gtk_tree_view_get_model
        use gtk_hl,                  only: hl_gtk_listn_set_cell

        implicit none

        character(len=*),intent(in)  :: treename              ! name of GTK-TreeView name as string
        integer   ,intent(in)        :: nrow                  ! row-number for setting cursor
        character(len=*),intent(in)  :: bcolorname            ! background-color name

        type(c_ptr)                  :: tree !,store
        integer(c_int)               :: icol1
        integer                      :: i,irr(6)
        !--------------------------------------------------------------------------------------
        item_setintern = .true.
        tree = idpt(treename)

        if(treename == 'treeview1') then
            do i=1,5
                icol1 = (i + 5) - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        if(treename == 'treeview2') then
            !do i=5,11
            !  icol1 = (i + 7) - 1
            do i=1,11
                icol1 = (i + 11) - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)

            end do
            do i=1,4
                icol1 = (i + 18) - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do

        end if

        if(treename == 'treeview3') then
            do i=1,6
                icol1 = 6 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        if(treename == 'treeview4') then
            do i=1,8
                icol1 = 8 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if
        if(treename == 'treeview5') then
            irr = [5,6,9,10,11,12]
            do i=1,12
                icol1 = 12 + i - 1
                if(Findloc(irr,i,dim=1) == 0) then
                    call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
                else
                    call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname) !svalue=orange_bg)
                end if
            end do
        end if
        if(treename == 'treeview6') then
            do i=1,15
                icol1 = 15 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        if(treename == 'treeviewELI') then
            do i=1,4
                icol1 = 4 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        if(treename == 'treeview7') then
            do i=1,7
                icol1 = 7 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        if(treename == 'treeview8') then
            do i=1,2
                icol1 = 2 + i - 1
                call hl_gtk_listn_set_cell(tree, row=nrow-1, col=icol1,  svalue=bcolorname)
            end do
        end if

        item_setintern = .false.

    end subroutine WTreeViewSetColorRow

    !###############################################################################

    subroutine WDPutLabelColorF(labelid, gtkstate, colorname)

        use gtk,                   only: gtk_label_set_markup, gtk_label_get_text, &
                                         gtk_entry_get_text, &
                                         gtk_widget_override_color, &
                                         gtk_label_set_use_markup
        use file_io,               only: logger
        use UR_gtk_variables,      only: URcolor

        implicit none

        character(len=*),intent(in)   :: labelid, colorname
        integer(c_int),intent(in)     :: gtkstate

        type(c_ptr)                   :: cstring
        character(len=:),allocatable  :: fstring
        character(len=512)            :: log_str
        integer                       :: ncitem,i1,i2,i3
        real(c_double)                :: rgba(4)
        !-----------------------------------------------------------------------------
        item_setintern = .true.
        ncitem = 0

        allocate(character(len=400) :: fstring)

        call FindItemS(labelid, ncitem)
        if(ncitem == 0) then
!             write(66,*) 'WDPutLabelColorF:  labelid=',trim(labelid),'  existiert nicht: ncitem=0'
            write(log_str, '(*(g0))') 'WDPutLabelColorF:  labelid=',trim(labelid),'  existiert nicht: ncitem=0'
            call logger(66, log_str)
            return
        end if
        if(trim(clobj%name(ncitem)%s) == 'GtkButton') return

        if(.true. .and. (trim(clobj%name(ncitem)%s) == 'GtkLabel')) then    !  .and. trim(clobj%name(ncitem)%s) == 'GtkCheckButton')) then
            call gtk_label_set_use_markup(clobj%id_ptr(ncitem), 1_c_int)
            cstring = gtk_label_get_text(clobj%id_ptr(ncitem))
            if(c_associated(cstring)) call c_f_string(cstring, fstring)
            call gtk_label_set_markup(clobj%id_ptr(ncitem),  &
                '<span foreground="' // trim(colorname) // '">' // trim(fstring) // '</span>'//c_null_char)
        else
            rgba = (/0._c_double, 0._c_double, 0._c_double, 1._c_double /)    ! black

            if(colorname(1:1) == '#') then
                read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
                URcolor%red = dble(i1)/dble(256)
                URcolor%green = dble(i2)/dble(256)
                URcolor%blue = dble(i3)/dble(256)
                URcolor%alpha = dble(1.)
            else
                if(trim(colorname) == 'red')  rgba = (/1._c_double, 0._c_double, 0._c_double, 1._c_double /)
                if(trim(colorname) == 'green') rgba = (/0._c_double, 1._c_double, 0._c_double, 1._c_double /)
                if(trim(colorname) == 'yellow') rgba = (/0._c_double, 1._c_double, 1._c_double, 1._c_double /)
                Urcolor%red = rgba(1)
                Urcolor%green = rgba(2)
                Urcolor%blue = rgba(3)
                Urcolor%alpha = rgba(4)
            end if
            call gtk_widget_override_color(clobj%id_ptr(ncitem), gtkstate, c_loc(URColor))
        end if
        item_setintern = .false.

    end subroutine WDPutLabelColorF

    !###############################################################################

    subroutine WDPutLabelColorB(labelid, gtkstate, colorname)

        use gtk,                   only: gtk_label_set_markup, gtk_label_get_text, gtk_entry_get_text, &
                                         gtk_widget_override_background_color
        use file_io,               only: logger
        use UR_gtk_variables,      only: URcolor

        implicit none

        character(len=*),intent(in)   :: labelid, colorname
        integer(c_int),intent(in)     :: gtkstate

        type(c_ptr)                  :: cstring
        character(len=:),allocatable :: fstring
        integer                      :: ncitem,i1,i2,i3
        character(len=512)           :: log_str
        real(c_double)               :: rgba(4)
        !-----------------------------------------------------------------------------
        item_setintern = .true.
        ncitem = 0
        call FindItemS(trim(labelid), ncitem)
        !         if(ncitem == 0) write(66,*) 'WDPutLabelColorF:  labelid=',trim(labelid),'  existiert nicht: ncitem=0'
        if(ncitem == 0)  then
            write(log_str, '(*(g0))') 'WDPutLabelColorF:  labelid=',trim(labelid),'  existiert nicht: ncitem=0'
            call logger(66, log_str)
        end if

        allocate(character(len=400) :: fstring)
        if(trim(clobj%name(ncitem)%s) == 'GtkLabel') then
            cstring = gtk_label_get_text(clobj%id_ptr(ncitem))
            if(c_associated(cstring)) call c_f_string(cstring, fstring)
            call gtk_label_set_markup(clobj%id_ptr(ncitem),  &
                '<span background="' // trim(colorname) // '">' // trim(fstring) // '</span>'//c_null_char)
        else
            rgba = (/0._c_double, 0._c_double, 0._c_double, 1._c_double /)    ! black
            if(colorname(1:1) == '#') then
                read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
                URcolor%red = dble(i1)/dble(256)
                URcolor%green = dble(i2)/dble(256)
                URcolor%blue = dble(i3)/dble(256)
                URcolor%alpha = dble(1.)
            else
                if(trim(colorname) == 'red')  rgba = (/1._c_double, 0._c_double, 0._c_double, 1._c_double /)
                if(trim(colorname) == 'green') rgba = (/0._c_double, 1._c_double, 0._c_double, 1._c_double /)
                if(trim(colorname) == 'yellow') rgba = (/0._c_double, 1._c_double, 1._c_double, 1._c_double /)
                Urcolor%red = rgba(1)
                Urcolor%green = rgba(2)
                Urcolor%blue = rgba(3)
                Urcolor%alpha = rgba(4)
            end if
            call gtk_widget_override_background_color(clobj%id_ptr(ncitem), gtkstate, c_loc(URColor))
        end if
        item_setintern = .false.

    end subroutine WDPutLabelColorB

    !###############################################################################

    subroutine WDPutLabelStringBold(labelid, labeltext, color_fg)

        use gtk,      only: gtk_label_set_markup
        use file_io,  only: logger

        implicit none

        character(len=*), intent(in)     :: labelid, labeltext
        character(len=*), intent(in)     :: color_fg

        character(len=len_trim(labeltext)+20)  :: str1

        character(len=512)  :: log_str
        integer             :: ncitem

        ncitem = 0
        call FindItemS(trim(labelid), ncitem)
        if(ncitem == 0)  then
            write(log_str, '(*(g0))') 'WDPutLabelStringBold:  labelid=',trim(labelid),'  existiert nicht: ncitem=0'
            call logger(66, log_str)
        end if

        ! if(ncitem > 0) then
        !     if(trim(clobj%name(ncitem)%s) == 'GtkFrame' ) color_fg = colors%frame_fg
        ! end if

        item_setintern = .true.
        str1 = labeltext

        call gtk_label_set_markup(clobj%id_ptr(ncitem),  &
            trim('<span foreground="' // trim(color_fg) // '"><b>' // trim(str1) // '</b></span>' &
            //c_null_char  ) )

        item_setintern = .false.
    end subroutine WDPutLabelStringBold

!###############################################################################

    subroutine WDPutTreeViewColumnLabel(treename, ncol, string)

        use, intrinsic :: iso_c_binding,      only: c_ptr,c_int,c_null_char
        use gtk,                only: gtk_tree_view_column_set_title
        use UR_gtk_variables,   only: tvnames,tvcolindex,ntvs

        implicit none

        character(len=*),intent(in)    :: treename           ! widget-string
        integer   ,intent(in)          :: ncol               ! Nummer der Spalte
        character(len=*),intent(in)    :: string             ! Ausgabetext

        character(len=50)              :: treecol
        integer                        :: ncitem,kt,i
        character(len=2)               :: mcol

!------------------------------------------------------------
        item_setintern = .true.
        kt = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) kt = i
        end do
        if(kt == 0) then
            goto 999     ! return
        end if
        treecol = 'treeviewcolumn'
        write(mcol,'(i2)') tvcolindex(kt,ncol)
        treecol = trim(treecol) // adjustl(mcol)


        call FindItemS(treecol, ncitem)

        if(ncitem == 0) goto 999  ! return

        call gtk_tree_view_column_set_title(clobj%id_ptr(ncitem), trim(string) // c_null_char)
999     continue
        item_setintern = .false.

    end subroutine WDPutTreeViewColumnLabel

!-------------------------------------------------------------------

    subroutine WDGetTreeViewColumnLabel(treename, ncol, string)

        use gtk,                only: gtk_tree_view_column_get_title
        use UR_gtk_variables,   only: tvnames,tvcolindex,ntvs

        implicit none

        character(len=*),intent(in)    :: treename           ! widget-string
        integer   ,intent(in)          :: ncol               ! Number of column
        character(len=30),intent(out)  :: string             ! text of label

        type(c_ptr)                        :: cptr
        character(len=50)                  :: treecol
        integer                            :: ncitem,kt,i
        character(len=2)                   :: mcol
!------------------------------------------------------------
        item_setintern = .true.
        kt = 0
        do i=1,ntvs
            if(trim(treename) == trim(tvnames(i)%s)) kt = i
        end do
        if(kt == 0) then
            goto 999     ! return
        end if
        treecol = 'treeviewcolumn'
        write(mcol,'(i2)') tvcolindex(kt,ncol)
        treecol = trim(treecol) // adjustl(mcol)


        call FindItemS(treecol, ncitem)
        if(ncitem == 0) goto 999  ! return
        cptr = gtk_tree_view_column_get_title(clobj%id_ptr(ncitem))
        call convert_c_string(cptr, string)

999     continue
        item_setintern = .false.

    end subroutine WDGetTreeViewColumnLabel

!-----------------------------------------------------------------------------------------

    recursive subroutine pending_events ()

        use gtk,             only: gtk_events_pending, gtk_main_iteration_do, &
                                   gtk_main_iteration, TRUE, FALSE
        use UR_VARIABLES,    only: batest_on,autoreport,bat_mcmc
        use UR_Loadsel,      only: NBcurrentPage

        implicit none

        integer(c_int) :: run_status = TRUE
        integer(c_int) :: boolresult

        if(batest_on .and. NBcurrentPage > 1 .and. .not.bat_mcmc) return
        if(autoreport) return

        ! gtk_events_pending:   returns: True falls ein Ereignis auf die
        !                       Verarbeitung wartet, andernfalls False

        ! gtk_main_iteration_do(blocking):
        !                       blocking:  TRUE if you want GTK+ to
        !                       block if no events are pending

        do while(IAND(gtk_events_pending(), run_status) /= FALSE)       ! wait until True
            boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
        end do

    end subroutine pending_events

!#####################################################################################

    subroutine WDPutTextviewEditor(wstr, ReportFile, ifehl)

        use gtk_hl,             only: hl_gtk_text_view_insert, hl_gtk_text_view_delete
        use gtk,                only: gtk_widget_override_font
        use pango,              only: pango_font_description_from_string,pango_font_description_free
        use UR_VARIABLES,       only: work_path, dir_sep
        use chf,                only: flfu
        use g,                  only: g_path_is_absolute

        implicit none

        character(len=*),intent(in)    :: wstr
        character(len=*),intent(in)    :: ReportFile
        integer   , intent(out)        :: ifehl

        type(c_ptr)             :: widget, font_desc
        integer(c_int)          :: cline,ccol
        integer                 :: ios ,i1,k
        logical                 :: prout
        character(len=2)        :: crlf = char(13)//char(10)
        character(len=200)      :: textline(1)
        !------------------------------------------------------------
        item_setintern = .true.
        prout = .false.
        ! prout = .true.

        call WDPutLabelString('TElabelFname', trim(ReportFile))

        widget = idpt(wstr)
        ifehl = 0
        close(15)

        if (g_path_is_absolute(ReportFile) == 0) then
            open(15, file=flfu(work_path) // ReportFile, status='old', iostat=ios)
        else
            open(15, file=flfu(ReportFile), status='old', iostat=ios)
        end if

        if(ios /= 0) then
            close (15)
            ifehl = 1
            item_setintern = .false.
            return
        end if

        call hl_gtk_text_view_delete(widget, line=0_c_int, column=0_c_int, n_lines=500)

        ! font_desc = pango_font_description_from_string('Courier New 10' // c_null_char)
        font_desc = pango_font_description_from_string('Consolas 11' // c_null_char)
        call gtk_widget_override_font(widget, font_desc)
        call pango_font_description_free(font_desc)

        cline = -1
        do while(.true.)
            read(15,'(a)',iostat=ios) textline(1)
            if(is_iostat_end(ios)) exit

            cline = cline + 1

            k = len_trim(textline(1))
            if(k > 0) then
                if(textline(1)(k:k) == char(13)) then
                    textline(1) = trim(textline(1)(1:k-1)) // crlf
                else
                    textline(1) = trim(textline(1)) // crlf
                end if
            else
                textline(1) = ' ' // crlf
            end if

            ccol  = 0
            call hl_gtk_text_view_insert(widget,textline)
        end do
        close (15)
        item_setintern = .false.

    end subroutine WDPutTextviewEditor

!#####################################################################################

    subroutine MessageShow(message, button_set, title, resp, mtype)

        use gtk_hl, only: hl_gtk_message_dialog_show

        implicit none

        character(len=*),intent(in)         :: message
        integer(c_int),intent(in)           :: button_set
        character(len=*),intent(in)         :: title
        integer(c_int),intent(out)          :: resp
        integer(c_int),intent(in),optional  :: mtype

        integer                               :: i,i1,nret
        character(len=len_trim(message)+20)   :: xmessage
        character(len=len_trim(title)+10)     :: str2
        character(len=200),allocatable        :: rmessage(:)

        nret = 0
        xmessage = trim(message)
        do i=1,len_trim(xmessage)
            if(xmessage(i:i) == char(13)) nret = nret + 1
        end do
        allocate(rmessage(nret+1+1))
        ! write(66,*) 'nret (1)=',nret
        nret = 0
        do
            i1 = index(xmessage(1:),char(13))
            if(i1 > 0) then
                nret = nret + 1
                rmessage(nret) = xmessage(1:i1-1)
                xmessage = xmessage(i1+1:)
                ! write(66,*) 'MESHOW: nret=',nret,' rmessage(nret)=',rmessage(nret)
            else
                nret = nret + 1                           !  24.2.2024
                rmessage(nret) = trim(xmessage)     !
                ! write(66,*) 'MESHOW: nret=',nret,' rmessage(nret)=',rmessage(nret)
                exit
            end if
        end do
        ! nret = nret + 1
        ! rmessage(nret) = trim(xmessage)

        str2 = title
        ! add one empty record to obtain a certain distance to the button shown below the
        ! messge records:
        nret = nret + 1
        rmessage(nret) = ' '


        if(.not.Present(mtype) .or. (Present(mtype) .and. mtype == 0_c_int)) then
            resp = hl_gtk_message_dialog_show(message=rmessage, button_set=button_set, &
                title=trim(str2)//c_null_char, parent=idpt('window1'))
        else
            resp = hl_gtk_message_dialog_show(message=rmessage, button_set=button_set,  &
                title=trim(str2)//c_null_char, type=mtype, parent=idpt('window1'))
        end if

    end subroutine MessageShow

!#####################################################################################

    subroutine SetTooltipText(wstring,tooltiptext)

        use gtk,                 only: gtk_widget_set_tooltip_text
        implicit none

        character(len=*),intent(in)  :: wstring
        character(len=*),intent(in)  :: tooltiptext

        call gtk_widget_set_tooltip_text(idpt(wstring), tooltiptext // c_null_char)

    end subroutine SetTooltipText
!-----------------------------------------------------------------------------------------


    subroutine UpdateProName(proname)

        use UR_variables,     only: dir_sep
        use gtk,              only: gtk_window_set_title
        use top,              only: WrStatusbar
        use translation_module, only: T => get_translation

        implicit none

        character(len=*),intent(in)   :: proname
        integer                       :: i1, i
        character(len=len(proname))   :: prstr

        i1 = 1
        if(LEN_TRIM(proname) > 59) i1 = LEN_TRIM(proname) - 59 + 1

        call WrStatusBar(1,T('Project') // ': '//TRIM(proname(i1:)))

        prstr = trim(proname)
        do i=len_trim(proname),1,-1
            if(proname(i:i) == dir_sep) then
                prstr = trim(proname(i+1:))
                exit
            end if
        end do

        call gtk_window_set_title(idpt('window1'), trim(win_title)// '   -   ' // trim(prstr) // c_null_char)
        call pending_events()

    end subroutine UpdateProName

!------------------------------------------------------------------------------------------

    subroutine WTreeViewAppend(treeview)

        use gtk,              only: gtk_tree_view_get_model, &
                                    gtk_tree_model_iter_n_children, &
                                    gtk_list_store_append
        use UR_gtk_variables, only: iter
        use UR_Gleich,        only: ngrs

        implicit none

        character(len=*),intent(in)   :: treeview

        integer           :: nrows, nmax,i
        type(c_ptr)       :: Liststore

        Liststore = gtk_tree_view_get_model(idpt(trim(treeview)))
        nrows = gtk_tree_model_iter_n_children(Liststore, C_NULL_PTR)
        nrows = nrows + 1
        call clear_gtktreeiter(iter)
        if(trim(treeview) =='treeview1') then
            nmax = ngrs + 30
            nmax = min(nmax,150)
            if(nrows < nmax) then
                do i=nrows+1,nmax
                    call gtk_list_store_append(Liststore, c_loc(iter))
                end do
            end if
        end if

    end subroutine WTreeViewAppend

!#####################################################################################

    integer function NumRowsTV(treeview_str)

        use gtk,              only: gtk_tree_view_get_model, &
                                    gtk_tree_model_iter_n_children

        implicit none

        character(len=*),intent(in)  :: treeview_str

        type(c_ptr)        :: liststore
        integer(c_int)     :: nrows

        NumRowsTV = 0
        Liststore = gtk_tree_view_get_model(idpt(trim(treeview_str)))
        nrows = gtk_tree_model_iter_n_children(Liststore, C_NULL_PTR)
        NumRowsTV = nrows + 0_c_int
        return

    end function NumRowsTV

!#########################################################################################

    subroutine SetMenuEGr(knumEGr)

        use gtk,            only: gtk_widget_set_visible, gtk_widget_set_sensitive
        use UR_gleich,      only: Symbole

        implicit none

        integer, intent(in)     :: knumEGr

        ! write symbol names to menu:
        call WDPutLAbelString('QFirst', TRIM(symbole(1)%s))
        call gtk_widget_set_visible(idpt('QSecond'), 0_c_int)
        call gtk_widget_set_visible(idpt('QThird'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('QSecond'),0_c_int)
        call gtk_widget_set_sensitive(idpt('QThird'),0_c_int)

        IF(knumEGR >= 2) then
            call gtk_widget_set_visible(idpt('QSecond'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('QSecond'),1_c_int)
            call WDPutLAbelString('QSecond', TRIM(symbole(2)%s))
        END IF
        IF(knumEGR >= 3) then
            call gtk_widget_set_visible(idpt('QThird'),1_c_int)
            call gtk_widget_set_sensitive(idpt('QThird'),1_c_int)
            call WDPutLAbelString('QThird', TRIM(symbole(3)%s))
        END IF

    end subroutine SetMenuEGr

!#####################################################################################

    subroutine ClearMCfields(mode)

        USE UR_Variables, only: frmtres

        implicit none
        integer, intent(in)    :: mode        !  0: clear labels and entries; 1: only clear entries

        call WDPutEntryDouble('TRentryMCvalPE', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCvalUPE', 0._rn, frmtres)

        call WDPutEntryDouble('TRentryMCValue', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCunc', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCuncrel', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMClq', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCuq', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCdt', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCdl', 0._rn, frmtres)
        call WDPutEntryInt('TRentryMCit', ivalue=0)

        call WDPutEntryDouble('TRentryMCvalPERSD', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCvalUPERSD', 0._rn, frmtres)

        call WDPutEntryDouble('TRentryMCValueRSD', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCuncRSD', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMClqRSD', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCuqRSD', 0._rn, frmtres )
        call WDPutEntryDouble('TRentryMCdtRSD', 0._rn, frmtres)
        call WDPutEntryDouble('TRentryMCdlRSD', 0._rn, frmtres)
        if(mode == 1) return

        call WDPutLabelString('TRLBUnit22', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit21', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit9', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit10', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit11', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit12', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit13', ' ')   ! einheit(1)%s)
        call WDPutLabelString('TRLBUnit14', ' ')   ! einheit(1)%s)

    end subroutine ClearMCfields

!#####################################################################################

    subroutine ClearPEfields()

        implicit none

        call WDPutEntryString('TRentryValue',' ')
        call WDPutEntryString('TRentryUnc',' ')
        call WDPutEntryString('TRentryUncPC',' ')
        call WDPutLabelString('TRLBUnit1',' ')
        call WDPutLabelString('TRLBUnit2',' ')
        call WDPutLabelString('TRLBUnit3',' ')
        call WDPutLabelString('TRLBUnit4',' ')
        call WDPutLabelString('TRLBUnit5',' ')
        call WDPutLabelString('TRLBUnit6',' ')
        call WDPutLabelString('TRLBUnit7',' ')
        call WDPutLabelString('TRLBUnit8',' ')
        call WDPutLabelString('TRLBUnit9',' ')
        call WDPutLabelString('TRLBUnit10',' ')
        call WDPutLabelString('TRLBUnit11',' ')
        call WDPutLabelString('TRLBUnit12',' ')
        call WDPutLabelString('TRLBUnit13',' ')
        call WDPutLabelString('TRLBUnit14',' ')

        call WDPutLabelString('TrlabUfitUnit',' ')
        call WDPutLabelString('TRlabUpropUnit',' ')

        call WDPutEntryString('TRentryCoverf',' ')
        call WDPutEntryString('TRentryDT',' ')
        call WDPutEntryString('TRentryDL',' ')
        call WDPutEntryString('TRentryDT',' ')

        call WDPutLabelString('TRlabDTIteras',' ')
        call WDPutLabelString('TRlabDLIteras',' ')
        call WDPutLabelString('TRlabMessage',' ')
        call WDPutLabelString('TRlabMethod',' ')

        call WDPutEntryString('TRentryValueBy',' ')
        call WDPutEntryString('TRentryUncBy',' ')
        call WDPutEntryString('TRentryLQBy',' ')
        call WDPutEntryString('TRentryUQBy',' ')
        call WDPutEntryString('TRentryGamma',' ')

        call WDPutEntryString('TRentryChisqr',' ')
        call WDPutEntryString('TRentryUfit',' ')
        call WDPutEntryString('TRentryUprop',' ')

    end subroutine ClearPEfields

!#####################################################################################

    subroutine EraseNWGfields()

        use UR_Gleich,     only: missingval
        use UR_VARIABLES,  only: frmtres
        use translation_module, only: T => get_translation
        implicit none

        character(len=100)  :: str1,xstr

        call WDPutEntryDouble('TRentryDT', missingval, frmtres)
        call WDPutEntryDouble('TRentryDL', missingval, frmtres)

        write(str1,'(a,i3)') T('Iterations') // ': ', 0

        xstr = max(' ',trim(str1))
        call WDPutLabelString('TRlabDTIteras', xstr)
        call WDPutLabelString('TRlabDLIteras', xstr)
        call WDPutLabelString('TRlabMessage', ' ')

    end subroutine EraseNWGfields

    !#######################################################################

    subroutine ExpandTV2Col7(xpnd)

        ! Expand column 7 (SD formulae) in treeview2 (TV2):
        ! if TV2 is visible: expand it;
        ! if (TV2 is not visible, limit column width and windows width


        use gtk,              only: gtk_tree_view_column_set_min_width, gtk_tree_view_column_set_max_width, &
                                    gtk_tree_view_columns_autosize,gtk_window_resize
        use UR_gtk_variables, only: tvnames,tvcolindex,tv_colwidth_digits
        use Top,              only: PixelPerString
        use CHF,              only: FindlocT
        implicit none

        logical,intent(in)  :: xpnd     ! expand: yes or no

        integer           :: kwd,kht,pixel_per_char,n,cwidth,nt
        character(len=3)  :: chcol


        ! limit the column width:
        nt = FindlocT(tvnames,'treeview2')
        call PixelPerString(idpt(tvnames(nt)%s), '123456789E-02123456789E-02',kwd,kht)
        pixel_per_char = int(real(kwd)/real(len_trim('123456789E-02123456789E-02'))) + 1

        n = 7
        write(chcol,'(i0)') tvcolindex(nt,n)
        if(.not.xpnd) cwidth = (4 + 2) * pixel_per_char
        ! if(xpnd) cwidth = (tv_colwidth_digits(nt,n) + 2) * pixel_per_char
        if(xpnd) then       ! 12.8.2023
            if(tv_colwidth_digits(nt,n) < 60) then
                cwidth = (tv_colwidth_digits(nt,n) + 2) * pixel_per_char
            else
                cwidth = (70+2) * pixel_per_char
            end if
        end if
        call gtk_tree_view_column_set_max_width(idpt('treeviewcolumn' // trim(adjustL(chcol))),cwidth )


    end subroutine ExpandTV2Col7


    ! modified version of hl_gtk_text_view_get_text:
    subroutine hl_gtk_text_view_get_text_GK(view, text, start_line, start_column, &
                                            end_line, end_column, hidden, buffer)

        use Top,              only: IntModA1,CharModA1
        use gtk_sup,          only: gtktextiter
        use gtk,              only: gtk_text_view_get_buffer,gtk_text_buffer_get_text, &
                                    gtk_text_iter_get_offset,TRUE,                     &
                                    gtk_text_buffer_get_iter_at_line_offset,           &
                                    gtk_text_buffer_get_iter_at_line,gtk_text_buffer_get_start_iter, &
                                    gtk_text_buffer_get_end_iter,gtk_text_iter_forward_to_line_end, &
                                    gtk_text_iter_forward_to_end,gtk_text_iter_forward_to_line_end, &
                                    gtk_text_buffer_insert,gtk_text_buffer_get_end_iter, &
                                    gtk_text_buffer_get_char_count
        use UR_gleich,        only: charv

        implicit none

        type(c_ptr), intent(in)                   :: view
        type(charv),allocatable,intent(out)       :: text(:)
        integer(kind=c_int), intent(in), optional :: start_column, start_line, &
        & end_line, end_column
        integer(kind=c_int), intent(in), optional :: hidden
        type(c_ptr), intent(in), optional         :: buffer

        integer                                   :: nrecs_mod

        ! Get text from s text view.
        !
        ! VIEW: c_ptr: required: The text view to read.
        ! TEXT: string(): required: A variable to contain the output text.
        ! START_LINE: c_int: optional: The first line to read.
        ! START_COLUMN: c_int: optional: The column at which to start reading.
        ! END_LINE: c_int: optional: The last line to read.
        ! END_COLUMN: c_int: optional: The column at which to stop reading.
        ! HIDDEN: boolean: optional: If set to FALSE, then do not get hidden
        ! 		characters
        ! BUFFER: c_ptr: optional: The text buffer from which to read. If this
        ! 		is given, then VIEW is ignored, useful for signal handlers
        ! 		attached to the buffer.
        !
        ! Note the rules for selection.
        !
        ! * If no selection arguments are present, the whole text is returned.
        ! * If either start_column or end_column is absent, but the matching line
        ! is present, then selection is by line.
        ! * If end_line is absent, but both columns are present, then the selection
        ! is within start_line
        ! * If neither start_line nor start_column is present, then the selection is
        ! from the start of the buffer
        ! * If neither end_line nor end_column is present, then the selection is
        ! to the end of the buffer.
        !-

        type(c_ptr)                                   :: tbuf, ctext0
        character(kind=c_char), dimension(:), pointer :: ftext0
        type(gtktextiter), target                     :: s_iter, e_iter
        integer(kind=c_int)                           :: ihid
        integer                                       :: nchars_r,endgo
        integer                                       :: i,k,nrecs,ilast,ijk,j
        logical                                       :: newr,  retry
        character(:),allocatable                      :: btext

        if (present(buffer)) then
            tbuf = buffer
        else
            tbuf = gtk_text_view_get_buffer(view)
        end if

        ! Fully specified
        if (present(start_line) .and. present(start_column) .and. &
        & present(end_line) .and. present(end_column)) then
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & end_line, end_column)

            ! Not Fully specified xxxxxxxxxxxxxxxxxxxxx GK
        else if (present(start_line) .and. present(start_column) .and. &
        & present(end_line) .and. .not. present(end_column)) then
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

            ! Both columns only start line
        else if (present(start_line) .and. present(start_column) .and. &
        &  present(end_column)) then
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

            ! Both lines, at least one column not given
        else if (present(start_line) .and. present(start_column)) then
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

            ! Fully specified start, no end
        else if (present(start_line) .and. present(start_column)) then
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
            call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

            ! Start line only
        else if (present(start_line)) then
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
            call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

            ! Fully specified end, no start
        else if (present(end_line) .and. present(end_column)) then
            call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
            call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

            ! End line only
        else if (present(end_line)) then
            call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
            call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

            ! Should only get here with nothing specified
        else
            call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
            call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))
        end if

        if (present(hidden)) then
            ihid = hidden
        else
            ihid = TRUE
        end if
        retry = .false.

        endgo = gtk_text_iter_forward_to_line_end(c_loc(e_iter))
        call gtk_text_buffer_get_end_iter(tbuf,c_loc(e_iter))

        nchars_r = gtk_text_buffer_get_char_count(tbuf)
        ! write(66,*) 'first nchars_r: ',nchars_r
        ctext0 = c_null_ptr
        if(associated(ftext0)) then
            ftext0 => Null()
            !do i=1,size(ftext0) + 30
            !  ftext0(i) = char(0)
            !end do
        end if
        ctext0 = gtk_text_buffer_get_text(tbuf, c_loc(s_iter), c_loc(e_iter), ihid)
        call c_f_pointer(ctext0, ftext0, (/ nchars_r + 30 /))

        do i=nchars_r,1,-1
            ! this loop: 31.3.2022 afternoon; include extra number of chars,
            ! accumulated from extra UTF-8 chars für ä,ö,ü, ß.
            ! bit delete the remaining NUL chars.
            if(ichar(ftext0(i)) > 0) then
                nchars_r = i      ! real number of characters!
                exit
            end if
        end do

        ijk = nchars_r

        nrecs = 0
        allocate(character(len=2500) :: btext)
        allocate(text(1))
        ilast = 1
22      continue
        do i=ilast,nchars_r
            newr = .false.
            if(ichar(ftext0(i)) == 10) newr = .true.
            if(i == nchars_r) then
                nrecs = nrecs + 1
                call charmoda1(text,nrecs)
                ! write(btext,'(2500a)') (ftext0(k),k=ilast,i-1)
                write(btext,'(2500a)') (ftext0(k),k=ilast,i)             !  corrected on 22.6.2022:  (,i-1 deleted the last significant character))

                text(nrecs)%s = trim(btext)
                ! write(66,*) 'i == nchars_r:  nrecs=',nrecs,' text(nrecs)%s=',text(nrecs)%s
                exit
            end if
            if(newr) then
                !if(i - ilast + 1 > 0) then
                nrecs = nrecs + 1
                call charmoda1(text,nrecs)
                write(btext,'(2500a)') (ftext0(k),k=ilast,i-1)

                text(nrecs)%s = trim(btext)
                if(ftext0(i) == char(10)) ilast = i+1

                if(ilast < nchars_r) goto 22
                !end if
            end if
        end do

        nrecs_mod = nrecs

        do i=nrecs,1,-1
            if(len_trim(text(i)%s) == 0) then
                nrecs_mod = nrecs_mod - 1
            elseif(len_trim(text(i)%s) == 1 .and. text(i)%s(1:1) == ' ') then
                nrecs_mod = nrecs_mod - 1
            else
                nrecs = nrecs_mod
                call CharModA1(text,nrecs)
                exit
            end if
        end do
        nrecs = nrecs_mod
        if(nrecs > 0) then
            ijk = len_trim(text(nrecs)%s)
            if(ijk > 1) then
                if(ichar(text(nrecs)%s(ijk:ijk)) <= 20) text(nrecs)%s = text(nrecs)%s(1:ijk-1)
            end if
            ijk = len_trim(text(nrecs)%s)
            if(ijk > 1) then
                if(ichar(text(nrecs)%s(ijk:ijk)) <= 20) text(nrecs)%s = text(nrecs)%s(1:ijk-1)
            end if
        end if

        deallocate (btext)

    end subroutine hl_gtk_text_view_get_text_GK

    !#######################################################################

end module Rout
