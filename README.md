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

### Quick reference
```fortran
use suio, only: SuFile, SuTrace, SuHeader, su_input, su_output
type(SuFile):: sf, so
type(SuTrace):: trc
type(SuHeader):: sh
integer:: fldr, tracf, ns, dt, ntr
real,allocatable:: array(:)
! set ns and allocate data array
allocate(array(ns))

! open: read
sf = su_input('input.su',ns=ns,ntr=ntr)
sf = SuFile('input.su','r')
ns = sf%ns()
ntr= sf%ntr() ! for file input only (not for stdin input)

! open: write
so = su_output(xdr=.true.) ! stdout, XDR format
so = SuFile('output.su','w')

! close
call sf%close()

! read trace
do itr=1,ntr
    call sf%read(trc)
enddo

! read header
do itr=1,ntr
    call sf%read(sh,itr)
enddo

! write trace
do itr=1,ntr
    call sf%write(trc)
enddo

! read header, keyword, data
call trc%get(fldr=fldr, tracf=tracf, header=sh, data=array)

! write header, keyword, data
call trc%set(fldr=fldr, tracf=tracf, header=sh, data=array)

! trace, header generator
trc = SuTrace(header=sh, fldr=fldr, ns=ns, dt=dt, data=array)
sh = SuHeader(fldr=fldr, ns=ns, dt=dt)
```


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
sf = SuFile(filename,mode='r',xdr,ns,ntr)
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
Once we open an SU file, we can read data trace by trace using `su%read`, `su%fread`, and `su%trace`. Their interfaces are

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
We can use `SuHeader` type to read a trace header only.

```fortran
type(SuHeader):: sh
```

Routines for reading header are similar to those for reading traces. But we use `SuHeader` type instead of `SuTrace` and the trace number `itr` is not optional.

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

#### Reading header keywords and data
We use `get` subroutine and functions with keyword name to obtain header values

```fortran
type(SuTrace):: trc
type(SuHeader):: sh
integer:: fldr, trace, sx
real:: f1
real,allocatable:: arr(:)

! read trace
! ...

allocate(arr(ns))

! using 'get' subroutine
call trc%get('fldr',fldr)
call trc%get('f1',f1)

! We can use 'get' subroutine to read trace data (without header)
call trc%get(arr)
! or we can use 'data' function
arr = trc%data()

! We can read multiple header values at once. This one is convenient.
call trc%get(fldr=fldr, tracf=tracf, sx=sx, f1=f1, data=arr)

! Or we can use functions named by the keywords
fldr = trc%fldr()
f1 = trc%f1()

! If we want whole header,
sh = trc%header()
call trc%get(header=sh)
```


### Output

#### Generating an SU trace
We need traces to write su files.

```fortran
type(SuTrace):: trc
type(SuHeader):: sh
integer:: ns=3000, dt=4000, sx=10
real:: arr(ns)

! we can use SuHeader type
sh = SuHeader(ns=ns, dt=dt)
trc = SuTrace(header=sh, data=arr)

! or we can generate the SuTrace without SuHeader
trc = SuTrace(ns=ns, dt=dt, sx=sx, data=arr)
```

#### Writing header keywords and data
We use `set` subroutine to set header, keywords, and data.

```fortran
call sh%set('sx',sx)
call sh%set(sx=sx, dt=dt)
call trc%set('sx',sx)
call trc%set(sx=sx, dt=dt, data=arr)
call trc%set(header=sh)
```

#### Opening a file
We open a file for writing to write traces.

```fortran
type(SuFile):: sf
! use su_output function
sf = su_output(filename,xdr)

! or the generator
sf = SuFile(filename,mode='w',xdr)
```

Interfaces are

```fortran
!! file output
type(SuFile) function su_output(filename, xdr)
character(len=*),intent(in),optional:: filename
logical,intent(in),optional:: xdr
end function

!! standard output
type(SuFile) function su_output(xdr)
logical,intent(in),optional:: xdr
end function
```

#### Writing traces
Writing is simple. Always write sequentially using `so%write`.

```fortran
type(SuFile):: sf, so, sd
type(SuTrace):: trc
! open ...
do while( sf%fread(trc) )
    call so%write(trc)
enddo
```

The interface is

```fortran
subroutine write(sf,trc)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(in):: su
end subroutine
```

### Close
After we are done with a SuFile, we close it.

```fortran
call sf%close()

! we can close multiple files (up to 10) at once.
call su_close(sf1,sf2,sf3...)
```

### Examples

#### suwind
```fortran
! suwind key=fldr min=10 max=20 < input.su > output.su
program suwind
use suio
type(SuFile):: sf, so
type(SuTrace):: trc
character(len=4):: keyword=“fldr”
integer:: keymin=10, keymax=20, keyval

sf = su_input()
so = su_output()
do while( sf%fread(trc) )
       call trc%get(keyword,keyval)
       if(keymin <= keyval .and. keyval <= keymax) call so%write(trc)
enddo
call su_close(sf, so)
end program
```

#### supaste
```fortran
! sustrip head=header < input_header.su > /dev/null
! sustrip < input_data.su > data.bin
! supaste ns=1000 head=header < data.bin > output.su

program supaste
use suio
type(SuFile):: sh, sd, so
type(SuTrace):: trc
integer:: itr, ntr, ns

sh = su_input(“input_header.su”, ntr=ntr)
sd = su_input(“input_data.su”, ns=ns)
so = su_output(“output.su”)

do itr = 1,ntr
    trc = sd%trace(itr)
    call trc%set(header=sh%header(itr), ns=ns)
    call so%write(trc)
enddo

call su_close(sh, sd, so)
end program
```

#### sunewtoold (XDR to native binary)
```
program suNewToOld
use suio
type(SuFile):: sf, so
type(SuTrace):: trc

sf = su_input(xdr=.true.)
so = su_output(xdr=.false.)
do while( sf%fread(trc) )
    call so%write(trc)
enddo
call su_close(sf,so)
end program
```
