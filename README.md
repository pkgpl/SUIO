# SUIO
A Fortran library for input/output of SU files, independent from the [Seismic Unix package](http://www.cwp.mines.edu/cwpcodes/index.html)

## Install
```sh
make
make install
```

## Compile
```sh
your_fortran_compiler -o main.e main.f90 -I/path/to/include -L/path/to/lib -lsuio
```

## SU IO module

### SuTrace and SuFile handle
SU IO handles su files using two derived data types: SuFile and SuTrace.

```fortran
use suio, only: SuFile, SuTrace, su_input, su_output
type(SuFile):: sf
type(SuTrace):: trc
```

### Input

#### Opening a file
We use `su_input` function or `SuFile` generator to open an SU file for input.

```fortran
type(SuFile):: sf
! use su_input function
sf = su_input(filename,xdr,ns,ntr)

! or the generator
sf = SuFile(filename,mode,xdr,ns,ntr)
```

Their interfaces are

```fortran
!! file input
type(SuFile) function su_input(filename, xdr, ns, ntr)
character(len=*),intent(in):: filename
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns, ntr
end function

!! standard input
type(SuFile) function su_input(xdr, ns)
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns
end function

!! file input/output
type(SuFile) function SuFile(filename, mode, xdr, ns, ntr)
character(len=*),intent(in):: filename
character,intent(in):: mode
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns, ntr
end function

!! standard input/output
type(SuFile) function SuFile(mode, xdr, ns)
character,intent(in):: mode
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns
end function
```

- All arguments are optional.
- `filename` is an su file name to read. If we omit `fileanme`, the module reads traces from the standard input.
- `xdr=.false.` is a logical value. Default value is `.false.` and it means that the su file is in native binary format. Use `xdr=.true.` to read XDR-formatted su file.
- `ns` is an integer output and it is the number of samples in a trace.
- `ntr` is an integer output and it is the number of traces in the su file. `ntr` is not available for su files from the standard input.
- `mode` should be 'r' or 'R' for input, 'w' or 'W' for output.

#### Reading traces
Once we open an SU file as above, we can read data trace by trace using `su%read`, `su%fread`, and `su%trace`. Their interfaces are

```fortran
subroutine read(sf,trc,itr,eof)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(out):: trc
integer,intent(in),optional:: itr
logical,intent(out),optional:: eof
end subroutine

logical function fread(sf,trc,itr)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(out):: trc
integer,intent(in),optional:: itr
end function

type(SuTrace) function trace(sf,itr)
type(SuFile),intent(inout):: sf
integer,intent(in),optional:: itr
end function
```
- Optional input `itr` is the trace number. We can read a specific trace using `itr`. If we omit `itr`, the module reads traces sequentially from the first trace.
- Optional output `eof` is .true. at the end of a file, while `fread` is .false. at the end of a file.

Examples:

```fortran
type(SuTrace):: trc

! we can use sf%read subroutine
! ntr is from su_input or SuFile generator
do itr=1,ntr
    call sf%read(trc)
enddo

! or su%trace function
do itr=1,ntr
    trc = sf%trace()
enddo

! or sf%fread function which returns .true. after success and .false. at the end of file. This function can be used with su files from standard input.
do while( sf%fread(trc) )
    !! process
enddo
```

#### Reading trace headers only
We can use `SuHeader` type to read header only.

```fortran
type(SuHeader):: sh
```

Routines for reading header are similar to those for reading traces. But we use `SuHeader` instead of `SuTrace` and the trace number `itr` is not optional.

```fortran
subroutine read(sf,sh,itr,eof)
type(SuFile),intent(inout):: sf
type(SuHeader),intent(out):: sh
integer,intent(in):: itr
logical,intent(out),optional:: eof
end subroutine

logical function fread(sf,sh,itr)
type(SuFile),intent(inout):: sf
type(SuHeader),intent(out):: sh
integer,intent(in):: itr
end function

type(SuHeader) function header(sf,itr)
type(SuFile),intent(inout):: sf
integer,intent(in):: itr
end function
```

#### Reading header keywords
We use `get` subroutine and functions with keyword name to obtain header values

```fortran
type(SuTrace):: trc
type(SuHeader):: sh
integer:: fldr, trace, sx
real:: f1

! read trace
! ...

! using 'get' subroutine
call trc%get('fldr',fldr)
call trc%get('f1',f1)

! We can read multiple header values at once. This one is convenient.
call trc%get(fldr=fldr, tracf=tracf, sx=sx, f1=f1)

! Or we can use functions named by the keywords
fldr = trc%fldr()
f1 = trc%f1()

! If we want whole header,
sh = trc%header()
```



