! Program :
! Author  : wansooha@gmail.com
! Date    :

module suio_binding
    !! c functions
    integer,parameter :: XDR_SIZE=48 !! sizeof(XDR)=48
    interface

! standard IO
        subroutine c_std_open_read(file_p) bind(c,name='c_std_open_read')
        use iso_c_binding
        type(c_ptr),intent(out) :: file_p
        end subroutine

        subroutine c_std_open_write(file_p) bind(c,name='c_std_open_write')
        use iso_c_binding
        type(c_ptr),intent(out) :: file_p
        end subroutine

! file IO
        subroutine c_file_open_read(filename,file_p) bind(c,name='c_file_open_read')
        use iso_c_binding
        character(c_char),dimension(*),intent(in):: filename
        type(c_ptr),intent(out) :: file_p
        end subroutine

        subroutine c_file_open_write(filename,file_p) bind(c,name='c_file_open_write')
        use iso_c_binding
        character(c_char),dimension(*),intent(in):: filename
        type(c_ptr),intent(out) :: file_p
        end subroutine

        subroutine c_file_close(file_p) bind(c,name='c_file_close')
        use iso_c_binding
        type(c_ptr),intent(inout) :: file_p
        end subroutine

! XDR IO
        subroutine c_xdr_init_read(xdr_t,file_p) bind(c,name='c_xdr_init_read')
        use iso_c_binding
        import XDR_SIZE
        character(c_char),intent(out):: xdr_t(XDR_SIZE)
        type(c_ptr),intent(in)       :: file_p
        end subroutine

        subroutine c_xdr_init_write(xdr_t,file_p) bind(c,name='c_xdr_init_write')
        use iso_c_binding
        import XDR_SIZE
        character(c_char),intent(out):: xdr_t(XDR_SIZE)
        type(c_ptr),intent(in)      :: file_p
        end subroutine

        subroutine c_xdr_finalize(xdr_t) bind(c,name='c_xdr_finalize')
        use iso_c_binding
        import XDR_SIZE
        character(c_char),intent(inout) :: xdr_t(XDR_SIZE)
        end subroutine


! trace IO
        function c_read_trc(su,nsize,file_p) bind(c,name='c_read_trc')
        use iso_c_binding
        use suio_types
        integer(c_int) :: c_read_trc
        type(SuTrace_c),intent(out):: su
        integer(c_int),intent(in)  :: nsize
        type(c_ptr),intent(inout)  :: file_p
        end function

        function c_write_trc(su,nsize,file_p) bind(c,name='c_write_trc')
        use iso_c_binding
        use suio_types
        integer(c_int) :: c_write_trc
        type(SuTrace_c),intent(in):: su
        integer(c_int),intent(in) :: nsize
        type(c_ptr),intent(inout) :: file_p
        end function

        function c_xdr_read_trc(xdr_t,su) bind(c,name='c_xdr_read_trc')
        use iso_c_binding
        use suio_types
        import XDR_SIZE
        integer(c_int)   :: c_xdr_read_trc
        character(c_char),intent(inout):: xdr_t(XDR_SIZE)
        type(SuTrace_c),intent(out)    :: su
        end function

        function c_xdr_write_trc(xdr_t,su) bind(c,name='c_xdr_write_trc')
        use iso_c_binding
        use suio_types
        import XDR_SIZE
        integer(c_int)   :: c_xdr_write_trc
        character(c_char),intent(inout):: xdr_t(XDR_SIZE)
        type(SuTrace_c),intent(in)     :: su
        end function

! header IO
        function c_read_trch(su,file_p) bind(c,name='c_read_trch')
        use iso_c_binding
        use suio_types
        integer(c_int) :: c_read_trch
        type(SuTrace_c),intent(inout):: su
        type(c_ptr),intent(inout)    :: file_p
        end function

        function c_xdr_read_trch(xdr_t,su) bind(c,name='c_xdr_read_trch')
        use iso_c_binding
        use suio_types
        import XDR_SIZE
        integer(c_int)   :: c_xdr_read_trch
        character(c_char),intent(inout):: xdr_t(XDR_SIZE)
        type(SuTrace_c),intent(inout)  :: su
        end function

! move file pointer
        subroutine c_fseek_trc(itr,nsize,file_p) bind(c,name='c_fseek_trc')
        use iso_c_binding
        integer(c_int),intent(in) :: itr,nsize
        type(c_ptr),intent(inout) :: file_p
        end subroutine

        subroutine c_xdr_fseek_trc(itr,nsize,xdr_t) bind(c,name='c_xdr_fseek_trc')
        use iso_c_binding
        import XDR_SIZE
        integer(c_int),intent(in)      :: itr,nsize
        character(c_char),intent(inout):: xdr_t(XDR_SIZE)
        end subroutine

! get ns
        function c_get_ns(file_p) bind(c,name='c_get_ns')
        use iso_c_binding
        integer(c_short) :: c_get_ns
        type(c_ptr),intent(inout) :: file_p
        end function

        function c_xdr_get_ns(xdr_t) bind(c,name='c_xdr_get_ns')
        use iso_c_binding
        import XDR_SIZE
        integer(c_short) :: c_xdr_get_ns
        character(c_char),intent(inout) :: xdr_t(XDR_SIZE)
        end function

! get file size to calculate ntr (file IO only)
        function c_filesize(filename) bind(c,name='c_filesize')
        use iso_c_binding
        integer(c_int64_t) :: c_filesize
        character(c_char),dimension(*),intent(in):: filename
        end function

    end interface
end module

