subroutine URGladeSys()

    ! this routine prepares a structure clobj which contains arrays of idd_names,
    ! labels, signals and so on for each widget contained in the Glade
    ! file.
    ! It goes through each line of the Glade file of XML format, searching for a list
    ! of 46 keywords, packed into the array oclass (object class).
    !
    ! It looks for the following specfic names in the XML file, which helps to find
    ! how they depend, one on another:
    !
    !    CName = 'ClassObject'
    !    Cidd  = 'ID='
    !    Clabel = 'Label'
    !    Csignal = 'Signal'
    !    Chandler = 'handler'

    ! The idd names and the label names of the widgets are later used in the routine
    ! gui_functions to obtain  with gtk_builder_get_object the associated C-pointers,
    ! which are still missing here.
    !
    ! This routine calls the routine PrintGladeSys for printing the clobj System;
    ! printing cane be invoked by the statement
    !     ! prout_gldsys = .true.           ! output von URGladeSys?
    ! by de-commenting this line.
    !
    !     Copyright (C) 2014-2024  Günter Kanisch

    use UR_gtk_variables
    use, intrinsic :: iso_c_binding
    use UR_interfaces,     only: PrintGladeSys
    use CHF,               only: ucase
    use UR_gtk_window,     only: nclmax
    use Top,               only: CharModA1
    use UR_Gleich,         only: ifehl

    implicit none

    character(len=120)  :: str
    character(len=230)  :: text, textSV, search, tarr(5), textin
    character(len=40)   :: CName,CIdd,csignal,modelname,submodel,Chandler
    character(len=150)  :: CLabel,Messg
    integer(4)          :: i1,ios,i,j,nlb,nlast,ntcols,jstore,jntcols,i2,i10
    integer(4)          :: kk,kx,kkmax,k, nnn,iddparent, i0, jj,ipar,maxk,finfo(13)
    integer(4)          :: klen,maxtvc,maxcrc
    integer(4)          :: name_max,idd_max,label_max,signal_max,handler_max

    character(len=40)   :: oclass(50),strid,strocl
    character(len=130)  :: vlabel,vlabelG
    logical             :: try_sub
    !----------------------------------------------------------------------------
    if(prout_gldsys) write(66,*) 'Begin GladeSys:'

    if(.not.allocated(clobj%name)) allocate(clobj%name(nclmax))
    if(.not.allocated(clobj%idd)) allocate(clobj%idd(nclmax))
    if(.not.allocated(clobj%label)) allocate(clobj%label(nclmax))
    if(.not.allocated(clobj%id_ptr)) allocate(clobj%id_ptr(nclmax))
    if(.not.allocated(clobj%label_ptr)) allocate(clobj%label_ptr(nclmax))
    if(.not.allocated(clobj%signal)) allocate(clobj%signal(nclmax))
    if(.not.allocated(clobj%idparent)) allocate(clobj%idparent(nclmax))
    if(.not.allocated(clobj%handler)) allocate(clobj%handler(nclmax))

    clobj%id_ptr = c_null_ptr
    clobj%label_ptr = c_null_ptr
    clobj%idparent = 0

    allocate(storename(1))
    allocate(tvmodel(1))
    allocate(tvnames(1))
    allocate(lsgtype(nstmax,ncolmax))
    ncolsmx = ncolmax    ! must be used for the 2nd index in CharmodA2 !

    Notebook_labelid(1) = 'NBProcedure'
    Notebook_labelid(2) = 'NBEquations'
    Notebook_labelid(3) = 'NBValUnc'
    Notebook_labelid(4) = 'NBBudget'
    Notebook_labelid(5) = 'NBResults'
    Notebook_labelid(6) = 'NBEditor'

    kkmax = 46                 ! 21.5.2024 GK
    do kk=1, kkmax
        oclass(kk) = ''
    end do

    jstore = 0
    jntcols = 0
    tvcols = 0

    oclass(1) =  'GtkMenu'
    oclass(2) =  'GtkImageMenuItem'
    oclass(3) =  'GtkMenuItem'
    oclass(4) =  'GtkToolbar'
    oclass(5) =  'GtkToolButton'
    oclass(6) =  'GtkNotebook'
    oclass(7) =  'GtkLabel'
    oclass(8) =  'GtkButton'
    oclass(9) =  'GtkComboBox'
    oclass(10) = 'GtkStatusbar'
    oclass(11) = 'GtkRadioButton'
    oclass(12) = 'GtkEntry'
    oclass(13) = 'GtkComboBoxText'
    oclass(14) = 'GtkCheckButton'
    oclass(15) = 'GtkFileChooserDialog'
    oclass(16) = 'GtkTextView'
    oclass(17) = 'GtkRadioMenuItem'
    oclass(18) = 'GtkListStore'
    oclass(19) = 'GtkTreeViewColumn'
    oclass(20) = 'GtkTreeView'
    oclass(21) = 'GtkCellRendererText'
    oclass(22) = 'GtkCellRendererCombo'
    oclass(23) = 'GtkFontButton'
    oclass(24) = 'GtkCellRendererToggle'
    oclass(25) = 'GtkBox'
    oclass(26) = 'GtkGrid'
    oclass(27) = 'GtkCheckMenuItem'
    oclass(28) = 'GtkTreeSelection'
    oclass(29) = 'GtkProgressBar'
    oclass(30) = 'GtkDrawingArea'
    oclass(31) = 'GtkTextBuffer'
    oclass(32) = 'GtkColorSelection'
    oclass(33) = 'GtkRecentManager'
    oclass(34) = 'GtkFrame'
    oclass(35) = 'GtkPaned'
    oclass(36) = 'GtkColorButton'
    oclass(37) = 'GtkScrolledWindow'
    oclass(38) = 'GtkViewport'
    oclass(39) = 'GtkContainer'
    oclass(40) = 'GtkWindow'
    oclass(41) = 'GtkDialog'
    oclass(42) = 'GtkMenuBar'
    oclass(43) = 'GtkAlignment'
    oclass(44) = 'GtkImage'
    oclass(45) = 'GtkFileChooserButton'
    oclass(46) = 'GtkLinkButton'

    maxtvc = 0
    maxcrc = 0

    CName = 'ClassObject'
    Cidd  = 'ID='
    Clabel = 'Label'
    Csignal = 'Signal'
    Chandler = 'handler'

    call stat(Gladeorg_file, finfo)

    open(18, file=gladeorg_file, status='old', iostat=ios, iomsg=messg)

    if(ios /= 0) then
        ifehl = 1
        write(66,*) 'Error on opening the Glade file ', trim(gladeorg_file), ' ios=',int(ios,2), ' ',trim(Messg)
        return
    end if

    klen = size(keya)
    keystrg = ' '
    do i=1,klen
        keystrg = trim(keystrg) // char(keya(i))
    end do

    maxk = 0
    do i=1, 100000
        read(18,'(a)',iostat=ios) textin
        if(ios /= 0) then
            close (19)
            close (20)
            exit
        end if
        text = trim(textin)
        if(i > 3) then
            tarr(1) = tarr(2)
            tarr(2) = tarr(3)
            tarr(3) = text
        end if

        if(prout_gldsys) then
            if(index(text,'"box2"') > 0) write(65,*) '  box2:'
            if(index(text,'"box3"') > 0) write(65,*) '  box3:'
            if(index(text,'"box4"') > 0) write(65,*) '  box4:'
            if(index(text,'"box5"') > 0) write(65,*) '  box5:'
            if(index(text,'"grid5"') > 0) write(65,*) '  grid5:'
            if(index(text,'"grid10"') > 0) write(65,*) '  grid10:'
        end if

        if(index(text,'"height_request"') == 0) then
            ! if(i <= 3000) write(65,*)'height-fail: ',trim(text)
            cycle
        end if

        i1 = index(text,'>')
        if(i1 > 0) then
            text = text(i1+1:)
        end if
        i1 = index(text,'<')
        if(i1 > 0) then
            read(text(1:i1-1),*,iostat=ios) k
            if(ios == 0 .and. k > 60) then
                do j=1,3
                    ! write(65,'(a,i4,a,i4,a,a)') 'Zeile=',i,' height value=',k,' : ',trim(adjustl(textorg))
                    if(prout_gldsys) write(65,*) trim(adjustl(tarr(j)))
                end do
                if(prout_gldsys) write(65,'(a,i4,a,i4,a,a)') 'Zeile=',i,' height value=',k
                if(prout_gldsys) write(65,*)
                maxk = max(k, maxk)
            end if
        end if
    end do
    if(prout_gldsys) then
        write(65,*) '------------------ max.Wert = ',maxk
        write(65,*)
    end if
    rewind 18

    nstores = 0
    nclobj = 0
    ntvs = 0
    TVcolindex = 0
    nlb = 0

    do i=1,100000
        read(18,'(a)',iostat=ios) textin
        if(ios /= 0) then
            ! write(65,*) 'End of loop at line ',i
            exit
        end if
        text = trim(textin)
        i1 = index(text,'<object class=')
        i10 = i1

        if(i1 == 3) then
            nclobj = nclobj + 1
            call CharModA1(clobj%name,nclobj)
            call CharModA1(clobj%idd,nclobj)
            call CharModA1(clobj%label,nclobj)
            call CharModA1(clobj%signal,nclobj)
            call CharModA1(clobj%handler,nclobj)
            clobj%signal(nclobj)%s = ' '
            nlb = 0
            text = text(i1+14:)
            i1 = index(text,'"')
            if(i1 > 0) then
                text = text(i1+1:)
            end if
            i1 = index(text,'"')
            clobj%name(nclobj)%s = text(1:i1-1)

            i1 = index(text,'id="')
            text =text(i1+4:)
            i1 = index(text,'"')
            clobj%idd(nclobj)%s = text(1:i1-1)
            clobj%id_ptr(nclobj) = c_null_ptr
            clobj%signal(nclobj)%s = ' '
            clobj%handler(nclobj)%s = ' '
            !write(65,'(10x,i4,10(2x,a))') j,clobj%name(j),clobj%idd(j)

            if(i10 == 3 .and. trim(clobj%idd(nclobj)%s) /= 'window1' ) then
                iddparent = nclobj
                clobj%idparent(nclobj) = iddparent
            end if

            if(trim(clobj%name(nclobj)%s) == 'GtkListStore') then
                nstores = nstores + 1
                call CharModA1(storename, nstores)
                storename(nstores)%s = clobj%idd(nclobj)%s
                lscolnums(nstores) = 0
                ! write(65,*) '    nstores=',nstores,'   store=',trim(storename(nstores)%s)
            end if

        else if(nclobj > 0) then

            if(trim(clobj%name(nclobj)%s) == 'GtkListStore') then
                str = '<column type="'
                i1 = index(text,trim(str))
                !write(65,*) 'column type:  i1=',i1
                if(i1 > 1) then
                    text = text(i1+len_trim(str):)
                    i1 = index(text,'"/>')
                    if(nstores > 0) then
                        lscolnums(nstores) = lscolnums(nstores) + 1
                        nnn = lscolnums(nstores)
                        lsgtype(nstores, nnn)%s = text(1:i1-1)
                        ! write(65,*) '   lscolnums=',int(lscolnums(nstores),2),' nnn=',int(nnn,2),' gtype=',trim(lsgtype(nstores,nnn)%s)
                        cycle
                    end if
                end if
            end if

            str = '<property name="label">'
            i1 = index(text,trim(str))                ! gtk-ok</property>
            if(i1 > 0) then
                text = text(i1+len_trim(str):)
                i1 = index(text,'</')
                if(trim(clobj%name(nclobj)%s) /= 'GtkDialog' .and. &
                    trim(clobj%name(nclobj)%s) /= 'GtkWindow') then
                    if(nlb <= 0) then
                        clobj%label(nclobj)%s = text(1:i1-1)
                        clobj%label_ptr(nclobj) = c_null_ptr
                        clobj%signal(nclobj)%s = ' '
                        nlb = nlb + 1
                    else
                        nclobj = nclobj + 1
                        call CharModA1(clobj%name,nclobj)
                        call CharModA1(clobj%idd,nclobj)
                        call CharModA1(clobj%label,nclobj)
                        call CharModA1(clobj%signal,nclobj)
                        call CharModA1(clobj%handler,nclobj)

                        clobj%label(nclobj)%s = text(1:i1-1)
                        clobj%name(nclobj)%s = trim(clobj%name(nclobj-1)%s)
                        clobj%idd(nclobj)%s = trim(clobj%idd(nclobj-1)%s)
                        clobj%id_ptr(nclobj) =c_null_ptr
                        clobj%label_ptr(nclobj) = c_null_ptr
                        clobj%signal(nclobj)%s = ' '
                        nlb = nlb + 1
                    end if
                end if
                ! write(65,*) '    nlb=',nlb
                cycle
            end if

            !-----
            str = '<signal name="'
            i1 = index(text,trim(str))
            if(trim(clobj%name(nclobj)%s) /= 'GtkDialog' .and.  &
                !!!!!!  (trim(clobj%name(nclobj)%s) /= 'GtkWindow' .or.   &   ! 16.8.2023
                ( trim(clobj%name(nclobj)%s) == 'GtkWindow' .or.  &
                trim(clobj%idd(nclobj)%s) == 'window_graphs') ) then
                if(i1 > 0) then
                    text = text(i1+len_trim(str):)
                    i1 = index(text,'"')
                    if(len_trim(clobj%signal(nclobj)%s) == 0) clobj%signal(nclobj)%s = text(1:i1-1)
                    !  write(65,*) '  nclobj=',nclobj,'  signal text=',trim(clobj%signal(nclobj))
                    str = 'handler="'
                    i2 = index(text,trim(str))
                    if(i2 > 0) then
                        text = text(i2+len_trim(str):)
                        i2 = index(text,'"')
                        !  write(65,*) '     signal text=',trim(text(1:i1-1))
                        if(len_trim(clobj%handler(nclobj)%s) == 0) clobj%handler(nclobj)%s = text(1:i2-1)
                    end if
                    cycle
                end if
            end if
            !-----------
        end if
    end do
    ! close (18)

    if(prout_gldsys) then
        write(65,'(5x,a,2x,a,2x,a,2x,a)') CName,Cidd,Clabel,Csignal
        write(65,'(5x,125a1)') ('-',i=1,125)
        do i=1,nclobj
            write(65,'(i4,2x,a,T28,2x,a,2x,T50,a,2x,T70,a,3x,a)') i,clobj%name(i)%s,clobj%idd(i)%s,  &
                clobj%label(i)%s,clobj%signal(i)%s,clobj%handler(i)%s
        end do
        write(65,*)
    end if
    nlast = nclobj
    !--------------------------------
    if(prout_gldsys) then
        write(65,*)
        write(65,*) 'Begin of part 2:'
        write(65,*)
    end if
    ! nstores = 0
    ntcols = 0
    modelname = ''
    submodel = ''
    try_sub = .false.
    kx = 0
    rewind (18)
    nlb = 0
    iddparent = 0
    do i=1,100000
        read(18,'(a)',iostat=ios) textin
        if(ios /= 0) then
            if(nclobj > 0) then
                str = clobj%name(nclobj)%s
                if(nclobj-nlast >  2 .and. nlb == 0 .and. (kx /= 9 .and. trim(str) /= 'GtkStatusbar'   &
                    .and. kx /= 19 .and. kx /= 20 .and. kx /= 22 )) nclobj = nclobj - 1
            end if
            exit
        end if
        text = trim(textin)
        textSV = trim(text)
        search = '<object class="'
        i0 = index(text,trim(search))
        ! if(i0 == 3) then
        if(i0 >= 3) then
            i1 = i0 + len_trim(search)
            text = text(i1:)
            i1 = index(text,'" id="')
            strocl = text(1:i1-1)
            text = text(i1+6:)
            i1 = index(text,'">')
            strid = text(1:i1-1)
            if(i0 == 3) then
                do jj=1,nlast
                    if(trim(strocl) == trim(clobj%name(jj)%s) .and.   &
                        trim(strid) == trim(clobj%idd(jj)%s)  ) then
                        iddparent = jj
                        exit
                    end if
                end do
            end if
            ! write(65,*) 'strocl=',trim(strocl),'  strid=',trim(strid),' jj=',jj,'  textsv=',trim(textsv),' i0=',i0
            text = trim(textSV)
        end if

        k = 0
        kx = 0
        do kk=1,kkmax
            search = '<object class="' // trim(oclass(kk)) // '" id="'
            i0 = index(text,trim(search))
            if(i0 > 0) then
                kx = kk
                exit
            end if
        end do

        if(kx > 0) then
            if(kx < 18) then
                modelname = ''
                ntcols = 0
            end if
            nclobj = nclobj + 1
            call CharModA1(clobj%name,nclobj)
            call CharModA1(clobj%idd,nclobj)
            call CharModA1(clobj%label,nclobj)
            call CharModA1(clobj%signal,nclobj)
            call CharModA1(clobj%handler,nclobj)

            clobj%name(nclobj)%s = trim(oclass(kx))
            clobj%idparent(nclobj) = iddparent
            i1 = index(text,'id="')
            if(i1 > 0) then
                text = text(i1+4:)
            end if
            i1 = index(text,'"')
            clobj%idd(nclobj)%s = text(1:i1-1)
            clobj%id_ptr(nclobj) = c_null_ptr
            clobj%signal(nclobj)%s = ''
            clobj%handler(nclobj)%s = ''

            do j=1,nlast
                if( trim(clobj%name(j)%s) == trim(clobj%name(nclobj)%s) .and.   &
                    trim(clobj%idd(j)%s) == trim(clobj%idd(nclobj)%s) .and.   &
                    ( trim(clobj%name(j)%s) == 'GtkListStore' .or. &
                    trim(clobj%name(j)%s) == 'GtkTextBuffer')          )  then
                    nclobj = nclobj - 1
                    goto 55
                end if
            end do

            if(trim(oclass(kx)) == 'GtkTreeView' .or. trim(oclass(kx)) == 'GtkComboBox') then
                !!!!!!!!! Warning: the treeviews can be sorted in a different way than the liststores
                ntvs = ntvs + 1
                call CharModA1(tvnames,ntvs)
                call CharModA1(tvmodel,ntvs)
                tvnames(ntvs)%s = clobj%idd(nclobj)%s
                ntcols = 0
                modelname = ''
                submodel = ''
                tvmodel(ntvs)%s = ' '
                cycle
            end if
            if(trim(oclass(kx)) == 'GtkTreeViewColumn') then
                if(ntvs > 0) then
                    if(tvnames(ntvs)%s(1:8) == 'treeview') then
                        ntcols = ntcols + 1
                        tvcols(ntvs) = ntcols
                        read(clobj%idd(nclobj)%s(15:),*) tvcolindex(ntvs,ntcols)
                        if(tvcolindex(ntvs,ntcols) < 700) maxtvc = max(maxtvc,tvcolindex(ntvs,ntcols))
                        cycle
                    end if
                end if
            end if
            if(trim(oclass(kx)) == 'GtkCellRendererText') then
                if(ntvs > 0) then
                    if(tvnames(ntvs)%s(1:8) /= 'treeview') then
                        ntcols = ntcols + 1
                        read(clobj%idd(nclobj)%s(17:),*) tvcolindex(ntvs,ntcols)
                        if(prout_gldsys) write(65,'(a,a,T35,a,a,T70,a,i2,2x,a,i2,a,i3,a,a)') 'TVCol: TV=',trim(tvnames(ntvs)%s), &
                            '  column: ',clobj%idd(nclobj)%s,  &
                            '  ntvs=',ntvs,'  ntcols=',ntcols,'  colnum=',tvcolindex(ntvs,ntcols), &
                            '  model=',trim(tvmodel(ntvs)%s)
                        if(tvcolindex(ntvs,ntcols) < 700) maxcrc = max(maxcrc,tvcolindex(ntvs,ntcols))
                        cycle
                    end if
                end if
            end if
            if(len_trim(modelname) > 0 .and. nstores > 0 .and. ntcols > 0) then
                do j=1,nstores
                    if(lscolnums(j) == 1) cycle

                    if(trim(modelname) == trim(storename(j)%s)) then
                        if(trim(oclass(kx)) == 'GtkCellRendererText')  lstype(j,ntcols) = 1
                        if(trim(oclass(kx)) == 'GtkCellRendererCombo') then
                            lstype(j,ntcols) = 2
                            try_sub = .true.
                            jstore = j
                            jntcols = ntcols
                        end if
                        if(trim(oclass(kx)) == 'GtkCellRendererToggle') lstype(j,ntcols) = 1
                        exit
                    end if
                end do
            end if

            nlb = 0
            j = nclobj
            if(kx == 10) cycle

        else if(nclobj-nlast > 0) then

            str = '<property name="label"'
            i1 = index(text,trim(str))                ! gtk-ok</property>
            if(i1 > 0) then
                i1 = index(text,'>')
                text = text(i1+1:)
                i1 = index(text,'</')

                if(nlb <= 0) then
                    clobj%label(nclobj)%s = text(1:i1-1)
                    clobj%id_ptr(nclobj) = c_null_ptr
                    clobj%signal(nclobj)%s = ''
                    nlb = nlb + 1
                else
                    nclobj = nclobj + 1
                    call CharModA1(clobj%name,nclobj)
                    call CharModA1(clobj%idd,nclobj)
                    call CharModA1(clobj%label,nclobj)
                    call CharModA1(clobj%signal,nclobj)
                    call CharModA1(clobj%handler,nclobj)

                    clobj%label(nclobj)%s = text(1:i1-1)
                    clobj%name(nclobj)%s = trim(clobj%name(nclobj-1)%s)
                    clobj%idparent(nclobj) = clobj%idparent(nclobj-1)
                    clobj%idd(nclobj)%s = trim(clobj%idd(nclobj-1)%s)
                    clobj%id_ptr(nclobj) = c_null_ptr
                    clobj%id_ptr(nclobj) = c_null_ptr
                    clobj%signal(nclobj)%s = ''
                    nlb = nlb + 1
                end if
                !j = nclobj
                !write(65,'(i4,10(2x,a))') j,clobj%name(j),clobj%idd(j),clobj%label(j),clobj%signal(j)
                cycle
            end if
            str = '<signal name="'
            i1 = index(text,trim(str))
            if(i1 > 0) then
                text = text(i1+len_trim(str):)
                i1 = index(text,'"')
                clobj%signal(nclobj)%s = text(1:i1-1)
                str = 'handler="'
                i2 = index(text,trim(str))
                if(i2 > 0) then
                    text = text(i2+len_trim(str):)
                    i2 = index(text,'"')
                    clobj%handler(nclobj)%s = text(1:i2-1)
                end if

                cycle
            end if

            if(len_trim(modelname) == 0)  then
                str = '<property name="model"'
                i1 = index(text,trim(str))
                if(i1 > 0) then
                    text = text(i1+len_trim(str)+1:)
                    i1 = index(text,'<')
                    modelname = text(1:i1-1)

                    if(ntvs > 0)then
                        if(len_trim(tvmodel(ntvs)%s) == 0) tvmodel(ntvs)%s = modelname
                    end if
                end if
            end if

            if(try_sub)  then
                str = '<property name="model"'
                i1 = index(text,trim(str))
                if(i1 > 0) then
                    text = text(i1+len_trim(str)+1:)
                    i1 = index(text,'<')
                    submodel = text(1:i1-1)
                    try_sub = .false.
                end if
            end if

        end if
55      continue
    end do        ! do i=1,100000
    close (18)

    if(prout_gldsys) then
        write(65,*)
        write(65,*) ' Repeating with existing signal'

        do i=nlast+1,nclobj
            ipar = clobj%idparent(i)
            if(ipar == 0) write(65,*) 'ipar=0:  i=',i,' idd(i)=',clobj%idd(i)%s
            if(ipar > 0) then
                str = clobj%name(ipar)%s
                write(65,'(i4,5(3x,a),2x,i4)') i,str(1:20),trim(clobj%name(i)%s),clobj%idd(i)%s, &
                    clobj%label(i)%s,clobj%signal(i)%s,clobj%idparent(i)
            end if
        end do
    end if

    name_max = 0
    idd_max = 0
    label_max = 0
    signal_max = 0
    handler_max = 0

    do i=1,nclobj
        name_max = max(name_max,len_trim(clobj%name(i)%s))
        idd_max = max(idd_max,len_trim(clobj%idd(i)%s))
        label_max = max(label_max,len_trim(clobj%label(i)%s))
        signal_max = max(signal_max,len_trim(clobj%signal(i)%s))
        handler_max = max(handler_max,len_trim(clobj%handler(i)%s))
    end do

    if(prout_gldsys) then
        write(65,*) 'maximum lenght values:'
        write(65,'(a,i0)') 'name:    ',name_max
        write(65,'(a,i0)') 'idd:     ',idd_max
        write(65,'(a,i0)') 'label:   ',label_max
        write(65,'(a,i0)') 'signal:  ',signal_max
        write(65,'(a,i0)') 'handler: ',handler_max
    end if

    close (18)
    if(.false.) then
        open(18,file='PrepTranslate.txt',status='unknown')
        do i=1,nclobj
            vlabel = adjustl(clobj%label(i)%s)
            vlabelG = ucase(vlabel)

            if(len_trim(vlabel) == 0) cycle
            if(vlabel(1:4) == 'gtk-') cycle
            if(index(vlabel,'oolbutton') > 0) cycle

            if(index(vlabelG, 'REL') == 1) cycle
            if(index(vlabelG, 'ABS') == 1) cycle
            if(index(vlabelG, '%') == 1) cycle
            if(index(vlabelG, 'CPS') == 1) cycle
            if(index(vlabelG, 'CPM') == 1) cycle
            if(index(vlabelG, 'BQ') == 1) cycle
            if(index(vlabelG, 'RSD%') == 1) cycle
            if(index(vlabelG, 'IT:') == 1) cycle
            if(index(vlabelG, 'K-ALPHA') == 1) cycle
            if(index(vlabelG, 'K-BETA') == 1) cycle
            if(index(vlabelG, 'ALPHA') == 1) cycle
            if(index(vlabelG, 'BETA') == 1) cycle
            if(index(vlabelG, 'CHISQR') == 1) cycle

            write(18,'(a)') "      IF(langg == 'DE') call WDPutLabelString('" // trim(adjustl(clobj%idd(i)%s))  &
                // "', '" // trim(adjustl(vlabel)) // "')"
            write(18,'(a)') "      IF(langg == 'EN') call WDPutLabelString('" // trim(adjustl(clobj%idd(i)%s))  &
                // "', '" // trim(adjustl(vlabel)) // "')"
            write(18,*)
        end do
        close (18)
    end if
    write(66,*) 'max treeviewcol=',int(maxtvc,2),'  max cellrenderer=',int(maxcrc,2)


    if(prout_gldsys) call PrintGladeSys(65)

end subroutine URGladeSys

!#######################################################################################

subroutine PrintGladeSys(kunit)

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_gtk_variables, only: nstores, storename, tvmodel, &
        lscolnums, lstype, lsgtype,  &
        tvcolindex, ntvs
    implicit none

    integer(4),intent(in)  :: kunit

    integer(4)             :: i, k, kkst
    character(len=5)       :: addr

    kkst = 0
    write(kunit,*)
    do i=1,nstores
        do k=1,ntvs
            if(trim(storename(i)%s) == trim(tvmodel(k)%s)) kkst = k
        end do
        if(lscolnums(i) == 1) cycle
        write(kunit,*) storename(i)%s,'   lscolnums(i)=',int(lscolnums(i),2)
        do k=1,lscolnums(i)
            write(addr,'(i2,a1,i2)') i,'/',k
            write(kunit,*) 'i,k=',addr,'   lstype = ',int(lstype(i,k),2),'  lsgtype = ',  &
                lsgtype(i,k)%s,'  colindex=',int(tvcolindex(kkst,k),2)
        end do
    end do
    write(kunit,*)

end subroutine PrintGladeSys

!#######################################################################################
