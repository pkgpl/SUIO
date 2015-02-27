! Program :
! Author  : wansooha@gmail.com
! Date    :

module suio_types
    use iso_c_binding
    implicit none
    private
    integer,parameter :: stderr=0
    integer,parameter :: MAX_INT2=32767

! SU Trace Header & Data
    type,bind(c):: SuHeader_c
        integer(c_int)   tracl, tracr, fldr, tracf, ep, cdp, cdpt !28
        integer(c_short) trid, nvs, nhs, duse !28+8=36
        integer(c_int)   offset, gelev, selev, sdepth, gdel, sdel, swdep, gwdep !36+32=68
        integer(c_short) scalel, scalco  !68+4=72
        integer(c_int)   sx,sy,gx,gy     !72+16=88
        integer(c_short) counit, wevel, swevel, sut, gut, sstat, gstat, tstat, laga, lagb !88+20=108
        integer(c_short) delrt, muts, mute, ns, dt, gain, igc, igi !108+16=124
        integer(c_short) corr, sfs, sfe, slen, styp, stas, stae, tatyp !124+16=140
        integer(c_short) afilf, afils, nofilf, nofils, lcf, hcf, lcs, hcs !140+16=156
        integer(c_short) year, day, hour, minute, sec, timbas ! 156+12=168
        integer(c_short) trwf, grnors, grnofr, grnlof, gaps, otrav ! 168+12=180
        !! Seismic Un*x extension
        real   (c_float) d1, f1, d2, f2, ungpow, unscale ! 180+24=204
        integer(c_int)   ntr ! 204 + 4 = 208
        integer(c_short) mark, shortpad ! 208+4=212
        integer(c_short) unass(14) ! 212+28=240
    end type SuHeader_c

    type,bind(c):: SuTrace_c
        type(SuHeader_c):: th
        real(c_float) :: td(MAX_INT2)
    end type SuTrace_c

    public:: SuTrace_c, su_header_init, su_trace_init
    public:: su_get_header, su_get_data, su_get_int, su_get_real
    public:: su_set_header, su_set_data, su_set_int, su_set_real

contains

! initialization
    subroutine su_header_init(th)
    type(SuHeader_c),intent(out):: th
    th%tracl=0; th%tracr=0; th%fldr=0; th%tracf=0; th%ep=0; th%cdp=0; th%cdpt=0 !28
    th%trid=0; th%nvs=0; th%nhs=0; th%duse=0 !28+8=36
    th%offset=0; th%gelev=0; th%selev=0; th%sdepth=0
    th%gdel=0; th%sdel=0; th%swdep=0; th%gwdep=0 !36+32=68
    th%scalel=0; th%scalco=0  !68+4=72
    th%sx=0; th%sy=0; th%gx=0; th%gy=0     !72+16=88
    th%counit=0; th%wevel=0; th%swevel=0; th%sut=0; th%gut=0 
    th%sstat=0; th%gstat=0; th%tstat=0; th%laga=0; th%lagb=0 !88+20=108
    th%delrt=0; th%muts=0; th%mute=0
    th%ns=0; th%dt=0; th%gain=0; th%igc=0; th%igi=0 !108+16=124
    th%corr=0; th%sfs=0; th%sfe=0; th%slen=0
    th%styp=0; th%stas=0; th%stae=0; th%tatyp=0 !124+16=140
    th%afilf=0; th%afils=0; th%nofilf=0; th%nofils=0
    th%lcf=0; th%hcf=0; th%lcs=0; th%hcs=0 !140+16=156
    th%year=0; th%day=0; th%hour=0; th%minute=0; th%sec=0; th%timbas=0 ! 156+12=168
    th%trwf=0; th%grnors=0; th%grnofr=0; th%grnlof=0; th%gaps=0; th%otrav=0 ! 168+12=180
    th%d1=0.; th%f1=0.; th%d2=0.; th%f2=0.; th%ungpow=0.; th%unscale=0. ! 180+24=204
    th%ntr=0; th%mark=0; th%shortpad=0 ! 204+4+2*2=212
    th%unass(1:14)=0 ! 212+28=240
    end subroutine

    subroutine su_trace_init(trc)
    type(SuTrace_c),intent(out):: trc
    call su_header_init(trc%th)
    trc%td=0.
    end subroutine

! trace header IO
    subroutine su_get_header(trc,trch)
    type(SuTrace_c),intent(in):: trc
    type(SuHeader_c),intent(out):: trch
    trch=trc%th
    end subroutine

    subroutine su_set_header(trc,trch)
    type(SuTrace_c),intent(inout):: trc
    type(SuHeader_c),intent(in):: trch
    trc%th=trch
    end subroutine

! trace data IO
    subroutine su_get_data(trc,arr)
    type(SuTrace_c),intent(in):: trc
    real(kind=4),intent(out):: arr(trc%th%ns)
    arr(1:trc%th%ns)=trc%td(1:trc%th%ns)
    end subroutine

    subroutine su_set_data(trc,arr)
    type(SuTrace_c),intent(inout):: trc
    real(kind=4),intent(in):: arr(trc%th%ns)
    trc%td(1:trc%th%ns)=arr(1:trc%th%ns)
    end subroutine

! header keyword IO - integer & real
    subroutine su_get_int(trc,attr,val)
    type(SuTrace_c),intent(in):: trc
    character(len=*),intent(in):: attr ! attribute name
    integer,intent(out):: val
    select case(trim(attr))
    case('tracl');    val=trc%th%tracl
    case('tracr');    val=trc%th%tracr
    case('fldr');     val=trc%th%fldr
    case('tracf');    val=trc%th%tracf
    case('ep');       val=trc%th%ep
    case('cdp');      val=trc%th%cdp
    case('cdpt');     val=trc%th%cdpt
    case('trid');     val=trc%th%trid
    case('nvs');      val=trc%th%nvs
    case('nhs');      val=trc%th%nhs
    case('duse');     val=trc%th%duse
    case('offset');   val=trc%th%offset
    case('gelev');    val=trc%th%gelev
    case('selev');    val=trc%th%selev
    case('sdepth');   val=trc%th%sdepth
    case('gdel');     val=trc%th%gdel
    case('sdel');     val=trc%th%sdel
    case('swdep');    val=trc%th%swdep
    case('gwdep');    val=trc%th%gwdep
    case('scalel');   val=trc%th%scalel
    case('scalco');   val=trc%th%scalco
    case('sx');       val=trc%th%sx
    case('sy');       val=trc%th%sy
    case('gx');       val=trc%th%gx
    case('gy');       val=trc%th%gy
    case('counit');   val=trc%th%counit
    case('wevel');    val=trc%th%wevel
    case('swevel');   val=trc%th%swevel
    case('sut');      val=trc%th%sut
    case('gut');      val=trc%th%gut
    case('sstat');    val=trc%th%sstat
    case('gstat');    val=trc%th%gstat
    case('tstat');    val=trc%th%tstat
    case('laga');     val=trc%th%laga
    case('lagb');     val=trc%th%lagb
    case('delrt');    val=trc%th%delrt
    case('muts');     val=trc%th%muts
    case('mute');     val=trc%th%mute
    case('ns');       val=trc%th%ns
    case('dt');       val=trc%th%dt
    case('gain');     val=trc%th%gain
    case('igc');      val=trc%th%igc
    case('igi');      val=trc%th%igi
    case('corr');     val=trc%th%corr
    case('sfs');      val=trc%th%sfs
    case('sfe');      val=trc%th%sfe
    case('slen');     val=trc%th%slen
    case('styp');     val=trc%th%styp
    case('stas');     val=trc%th%stas
    case('stae');     val=trc%th%stae
    case('tatyp');    val=trc%th%tatyp
    case('afilf');    val=trc%th%afilf
    case('afils');    val=trc%th%afils
    case('nofilf');   val=trc%th%nofilf
    case('nofils');   val=trc%th%nofils
    case('lcf');      val=trc%th%lcf
    case('hcf');      val=trc%th%hcf
    case('lcs');      val=trc%th%lcs
    case('hcs');      val=trc%th%hcs
    case('year');     val=trc%th%year
    case('day');      val=trc%th%day
    case('hour');     val=trc%th%hour
    case('minute');   val=trc%th%minute
    case('sec');      val=trc%th%sec
    case('timbas');   val=trc%th%timbas
    case('trwf');     val=trc%th%trwf
    case('grnors');   val=trc%th%grnors
    case('grnofr');   val=trc%th%grnofr
    case('grnlof');   val=trc%th%grnlof
    case('gaps');     val=trc%th%gaps
    case('otrav');    val=trc%th%otrav

    case('ntr');      val=trc%th%ntr
    case('mark');     val=trc%th%mark
    case default
        write(stderr,*) 'unknown attr: '//trim(attr)
        stop
    end select
    end subroutine

    subroutine su_get_real(trc,attr,val)
    type(SuTrace_c),intent(in):: trc
    character(len=*),intent(in):: attr ! attribute name
    real,intent(out):: val
    select case(trim(attr))
    case('d1');       val=trc%th%d1
    case('f1');       val=trc%th%f1
    case('d2');       val=trc%th%d2
    case('f2');       val=trc%th%f2
    case('ungpow');   val=trc%th%ungpow
    case('unscale');  val=trc%th%unscale
    case default
        write(stderr,*) 'unknown attr: '//trim(attr)
        stop
    end select
    end subroutine

    subroutine su_set_int(trc,attr,val)
    type(SuTrace_c),intent(inout):: trc
    character(len=*),intent(in):: attr
    integer,intent(in) :: val
    select case(trim(attr))
    case('tracl');    trc%th%tracl=val
    case('tracr');    trc%th%tracr=val
    case('fldr');     trc%th%fldr=val
    case('tracf');    trc%th%tracf=val
    case('ep');       trc%th%ep=val
    case('cdp');      trc%th%cdp=val
    case('cdpt');     trc%th%cdpt=val
    case('trid');     trc%th%trid=val
    case('nvs');      trc%th%nvs=val
    case('nhs');      trc%th%nhs=val
    case('duse');     trc%th%duse=val
    case('offset');   trc%th%offset=val
    case('gelev');    trc%th%gelev=val
    case('selev');    trc%th%selev=val
    case('sdepth');   trc%th%sdepth=val
    case('gdel');     trc%th%gdel=val
    case('sdel');     trc%th%sdel=val
    case('swdep');    trc%th%swdep=val
    case('gwdep');    trc%th%gwdep=val
    case('scalel');   trc%th%scalel=val
    case('scalco');   trc%th%scalco=val
    case('sx');       trc%th%sx=val
    case('sy');       trc%th%sy=val
    case('gx');       trc%th%gx=val
    case('gy');       trc%th%gy=val
    case('counit');   trc%th%counit=val
    case('wevel');    trc%th%wevel=val
    case('swevel');   trc%th%swevel=val
    case('sut');      trc%th%sut=val
    case('gut');      trc%th%gut=val
    case('sstat');    trc%th%sstat=val
    case('gstat');    trc%th%gstat=val
    case('tstat');    trc%th%tstat=val
    case('laga');     trc%th%laga=val
    case('lagb');     trc%th%lagb=val
    case('delrt');    trc%th%delrt=val
    case('muts');     trc%th%muts=val
    case('mute');     trc%th%mute=val
    case('ns');       trc%th%ns=val
    case('dt');       trc%th%dt=val
    case('gain');     trc%th%gain=val
    case('igc');      trc%th%igc=val
    case('igi');      trc%th%igi=val
    case('corr');     trc%th%corr=val
    case('sfs');      trc%th%sfs=val
    case('sfe');      trc%th%sfe=val
    case('slen');     trc%th%slen=val
    case('styp');     trc%th%styp=val
    case('stas');     trc%th%stas=val
    case('stae');     trc%th%stae=val
    case('tatyp');    trc%th%tatyp=val
    case('afilf');    trc%th%afilf=val
    case('afils');    trc%th%afils=val
    case('nofilf');   trc%th%nofilf=val
    case('nofils');   trc%th%nofils=val
    case('lcf');      trc%th%lcf=val
    case('hcf');      trc%th%hcf=val
    case('lcs');      trc%th%lcs=val
    case('hcs');      trc%th%hcs=val
    case('year');     trc%th%year=val
    case('day');      trc%th%day=val
    case('hour');     trc%th%hour=val
    case('minute');   trc%th%minute=val
    case('sec');      trc%th%sec=val
    case('timbas');   trc%th%timbas=val
    case('trwf');     trc%th%trwf=val
    case('grnors');   trc%th%grnors=val
    case('grnofr');   trc%th%grnofr=val
    case('grnlof');   trc%th%grnlof=val
    case('gaps');     trc%th%gaps=val
    case('otrav');    trc%th%otrav=val

    case('ntr');      trc%th%ntr=val
    case('mark');     trc%th%mark=val
    case default
        write(stderr,*) 'unknown attr: '//trim(attr)
        stop
    end select
    end subroutine

    subroutine su_set_real(trc,attr,val)
    type(SuTrace_c),intent(inout):: trc
    character(len=*),intent(in):: attr
    real,intent(in) :: val
    select case(trim(attr))
    case('d1');       trc%th%d1=val
    case('f1');       trc%th%f1=val
    case('d2');       trc%th%d2=val
    case('f2');       trc%th%f2=val
    case('ungpow');   trc%th%ungpow=val
    case('unscale');  trc%th%unscale=val
    case default
        write(stderr,*) 'unknown attr: '//trim(attr)
        stop
    end select
    end subroutine

end module

