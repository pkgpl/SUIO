! SUIO: Seismic Un*x input/output routines.
! 2014.9.2
! wansooha@gmail.com

module suio_file
    use iso_c_binding
    use suio_types
    use suio_binding
    use suio_trace
    use suio_xdr_default
    implicit none
    private
    integer,parameter :: TRACE_HEADER_BYTES=240

    type:: SuFile
        private
        type(c_ptr):: file_p
        character(c_char):: xdr_t(XDR_SIZE)
        logical:: xdr=XDR_DEFAULT,std=.false.,readonly=.true.,use_itr=.false.
        integer(c_int):: nsize=0
        integer(c_int):: ns_=0,ntr_=0
        contains
            !! file IO
            !! subroutines (read/write, overloaded):: read (trace, header), write (trace)
            !! functions (read only):: header, trace, fread (logical return value)
            !! etc:: ntr (file input only), ns (both file & std IO), close
            private
            procedure:: move_to
            procedure,public:: close
            ! write subroutine
            procedure,public:: write => write_trace
            ! read subroutines
            procedure:: sread_header
            procedure:: sread_trace
            generic,public :: read => sread_trace, sread_header
            ! read functions
            procedure:: fread_trace
            procedure:: fread_header
            generic,public :: fread => fread_trace, fread_header
            procedure,public:: trace => read_trace
            procedure,public:: header => read_header
            ! ns,ntr
            procedure,public:: ntr => get_ntr
            procedure,public:: ns => get_ns
    end type SuFile

    public:: SuFile, su_input, su_output, su_close

    interface su_input
        module procedure input_file
        module procedure input_std
    end interface

    interface su_output
        module procedure output_file
        module procedure output_std
    end interface

    interface SuFile
        module procedure open_file
        module procedure open_std
    end interface

contains

!! initialization
    subroutine su_init(sf)
    type(SuFile),intent(out):: sf
    sf%xdr=XDR_DEFAULT
    sf%std=.false.
    sf%readonly=.true.
    sf%ns_=0
    sf%nsize=0
    sf%ntr_=0
    end subroutine

!! open file/std for input/output
    type(SuFile) function open_file(filename,mode,xdr,ns,ntr) result(sf)
    character(len=*),intent(in):: filename
    character,intent(in):: mode
    logical,intent(in):: xdr
    integer,intent(out),optional:: ns,ntr
    select case(mode)
    case('r','R')
        sf=su_input(trim(filename),xdr)
    case('w','W')
        sf=su_output(trim(filename),xdr)
    case default
        stop 'mode should be (r|R|w|W)'
    end select
    if(present(ns)) ns=sf%ns()
    if(present(ntr)) ntr=sf%ntr()
    end function

    type(SuFile) function open_std(mode,xdr,ns) result(sf)
    character,intent(in):: mode
    logical,intent(in):: xdr
    integer,intent(out),optional:: ns
    select case(mode)
    case('r','R')
        sf=su_input(xdr)
    case('w','W')
        sf=su_output(xdr)
    case default
        stop 'mode should be (r|R|w|W)'
    end select
    if(present(ns)) ns=sf%ns()
    end function

!! open file for input
    type(SuFile) function input_file(filename,xdr,ns,ntr) result(sf)
    character(len=*),intent(in):: filename ! su file name
    logical,intent(in),optional:: xdr
    integer,intent(out),optional:: ns,ntr
    call su_init(sf)
    sf%readonly=.true.
    sf%std=.false.
    call c_file_open_read(trim(filename)//c_null_char,sf%file_p)
    if(present(xdr)) sf%xdr=xdr
    if(sf%xdr) then
        call c_xdr_init_read(sf%xdr_t,sf%file_p)
        sf%ns_=c_xdr_get_ns(sf%xdr_t)
    else
        sf%ns_=c_get_ns(sf%file_p)
    endif
    sf%nsize=get_nsize(sf%ns_)
    sf%ntr_=su_get_ntr(trim(filename),sf%ns_)
    if(present(ns)) ns=sf%ns()
    if(present(ntr)) ntr=sf%ntr()
    end function

    type(SuFile) function input_std(xdr,ns) result(sf)
    logical,intent(in),optional:: xdr
    integer,intent(out),optional:: ns
    call su_init(sf)
    sf%readonly=.true.
    sf%std=.true.
    call c_std_open_read(sf%file_p)
    if(present(xdr)) sf%xdr=xdr
    if(sf%xdr) then
        call c_xdr_init_read(sf%xdr_t,sf%file_p)
        sf%ns_=c_xdr_get_ns(sf%xdr_t)
    else
        sf%ns_=c_get_ns(sf%file_p)
    endif
    sf%nsize=get_nsize(sf%ns_)
    sf%ntr_=0
    if(present(ns)) ns=sf%ns()
    end function

!! open file for output
    type(SuFile) function output_file(filename,xdr) result(sf)
    character(len=*),intent(in):: filename ! su file name
    logical,intent(in),optional:: xdr
    call su_init(sf)
    sf%readonly=.false.
    sf%std=.false.
    call c_file_open_write(trim(filename)//c_null_char,sf%file_p)
    if(present(xdr)) sf%xdr=xdr
    if(sf%xdr) call c_xdr_init_write(sf%xdr_t,sf%file_p)
    end function

    type(SuFile) function output_std(xdr) result(sf)
    logical,intent(in),optional:: xdr
    call su_init(sf)
    sf%readonly=.false.
    sf%std=.true.
    call c_std_open_write(sf%file_p)
    if(present(xdr)) sf%xdr=xdr
    if(sf%xdr) call c_xdr_init_write(sf%xdr_t,sf%file_p)
    end function

!! Close su file
    subroutine close(sf)
    class(SuFile),intent(inout):: sf ! SuFile to close
    if(sf%xdr) call c_xdr_finalize(sf%xdr_t)
    if(.not.sf%std) call c_file_close(sf%file_p)
    end subroutine

!! write SuTrace
    subroutine write_trace(sf,su)
    class(SuFile),intent(inout):: sf
    type(SuTrace),intent(in):: su
    type(SuTrace_c):: trc
    integer val
    if(sf%readonly) stop "cannot read from output file"
    if(sf%nsize==0) sf%nsize=get_nsize(int(su%key("ns")))
    call su%get(trc)
    if(sf%xdr) then
        val=c_xdr_write_trc(sf%xdr_t,trc)
    else
        val=c_write_trc(trc,sf%nsize,sf%file_p)
    endif
    end subroutine

!! read SuTrace
    subroutine sread_trace(sf,su,itr,eof)
    class(SuFile),intent(inout):: sf
    type(SuTrace),intent(out):: su
    integer,intent(in),optional:: itr
    logical,intent(out),optional:: eof
    type(SuTrace_c):: trc
    integer val
    if(.not.sf%readonly) stop "cannot write to input file"
    if(present(itr)) then
        sf%use_itr=.true.
        call sf%move_to(itr)
    else
        if(sf%use_itr) stop "you should continue to use itr option for this file"
    endif
    if(sf%xdr) then
        val=c_xdr_read_trc(sf%xdr_t,trc)
    else
        val=c_read_trc(trc,sf%nsize,sf%file_p)
    endif
    call su%set(trc)
    if(present(eof)) eof = (val==0)
    end subroutine

    type(SuTrace) function read_trace(sf,itr) result(su)
    class(SuFile),intent(inout):: sf
    integer,intent(in),optional:: itr
    if(present(itr)) then
        call sread_trace(sf,su,itr)
    else
        call sread_trace(sf,su)
    endif
    end function

    logical function fread_trace(sf,su,itr) result(val)
    class(SuFile),intent(inout):: sf
    type(SuTrace),intent(out):: su
    integer,intent(in),optional:: itr
    logical:: eof
    if(present(itr)) then
        call sf%read(su,itr,eof)
    else
        call sf%read(su,eof=eof)
    endif
    val=.not.eof
    end function

!! read SuHeader
    subroutine sread_header(sf,sh,itr,eof)
    class(SuFile),intent(inout):: sf
    type(SuHeader),intent(out):: sh
    integer,intent(in):: itr
    logical,intent(out),optional:: eof
    type(SuTrace_c):: trc
    integer val
    if(.not.sf%readonly) stop "cannot write to input file"
    call sf%move_to(itr)
    if(sf%xdr) then
        val=c_xdr_read_trch(sf%xdr_t,trc)
    else
        val=c_read_trch(trc,sf%file_p)
    endif
    call sh%set_cheader(trc)
    if(present(eof)) eof = (val==0)
    end subroutine

    type(SuHeader) function read_header(sf,itr) result(sh)
    class(SuFile),intent(inout):: sf
    integer,intent(in):: itr
    call sread_header(sf,sh,itr)
    end function

    logical function fread_header(sf,su,itr) result(val)
    class(SuFile),intent(inout):: sf
    type(SuHeader),intent(out):: su
    integer,intent(in):: itr
    logical:: eof
    call sf%read(su,itr,eof)
    val=.not.eof
    end function

!! utils
    integer function get_ntr(sf) result(ntr)
    class(SuFile),intent(in):: sf
    if(sf%std .or. (.not.sf%readonly)) stop "ntr: file input only"
    ntr = sf%ntr_
    end function

    integer function get_ns(sf) result(ns)
    class(SuFile),intent(in):: sf
    if(.not.sf%readonly) stop "ns: input only"
    ns=sf%ns_
    end function

    integer(kind=8) function su_get_ntr(filename,ns) result(ntr)
    character(len=*),intent(in):: filename
    integer,intent(in):: ns
    ntr=c_filesize(trim(filename)//c_null_char)/(ns+60)/4
    end function

    integer function get_nsize(ns)
    integer,intent(in):: ns
    get_nsize=TRACE_HEADER_BYTES + 4*ns
    end function

    subroutine move_to(sf,itr)
    class(SuFile),intent(inout):: sf
    integer,intent(in):: itr
    if(sf%std) stop 'stdin does not support: move_to i-th trace'
    if(itr<=0 .or. itr>sf%ntr()) stop 'wrong position'
    if(sf%xdr) then
        call c_xdr_fseek_trc(itr,sf%nsize,sf%xdr_t)
    else
        call c_fseek_trc(itr,sf%nsize,sf%file_p)
    endif
    end subroutine

    subroutine su_close(s0,s1,s2,s3,s4,s5,s6,s7,s8,s9)
    type(SuFile),intent(inout),optional:: s0,s1,s2,s3,s4,s5,s6,s7,s8,s9
    if(present(s0)) call s0%close()
    if(present(s1)) call s1%close()
    if(present(s2)) call s2%close()
    if(present(s3)) call s3%close()
    if(present(s4)) call s4%close()
    if(present(s5)) call s5%close()
    if(present(s6)) call s6%close()
    if(present(s7)) call s7%close()
    if(present(s8)) call s8%close()
    if(present(s9)) call s9%close()
    end subroutine

end module suio_file

