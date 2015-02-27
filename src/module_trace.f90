! SUIO: Seismic Un*x input/output routines.
! 2014.9.2
! wansooha@gmail.com

module suio_trace
    use iso_c_binding
    use suio_types
    use suio_binding
    implicit none
    private

    type:: SuHeader
        private
        type(SuTrace_c):: trc
        contains
            !! keyword IO
            !! subroutines (read/write, overloaded):: get, set
            !! functions (read only):: key (integer), key_real, keyword_names (tracl, tracr, fldr ...)
            !! etc::: init
            private
            !! set subroutines
            procedure:: set_int
            procedure:: set_real
            procedure:: set_key
            generic,public :: set => set_int, set_real, set_key
            procedure,public:: set_cheader
            !! get subroutines
            procedure:: get_int
            procedure:: get_real
            procedure:: get_key
            generic,public :: get => get_int, get_real, get_key
            procedure,public:: get_cheader
            !! get functions
            procedure,public:: key => key_int
            procedure,public:: key_real

            ! keyword functions
            procedure,public:: tracl  
            procedure,public:: tracr  
            procedure,public:: fldr   
            procedure,public:: tracf  
            procedure,public:: ep     
            procedure,public:: cdp    
            procedure,public:: cdpt   
            procedure,public:: trid   
            procedure,public:: nvs    
            procedure,public:: nhs    
            procedure,public:: duse   
            procedure,public:: offset 
            procedure,public:: gelev  
            procedure,public:: selev  
            procedure,public:: sdepth 
            procedure,public:: gdel   
            procedure,public:: sdel   
            procedure,public:: swdep  
            procedure,public:: gwdep  
            procedure,public:: scalel 
            procedure,public:: scalco 
            procedure,public:: sx     
            procedure,public:: sy     
            procedure,public:: gx     
            procedure,public:: gy     
            procedure,public:: counit 
            procedure,public:: wevel  
            procedure,public:: swevel 
            procedure,public:: sut    
            procedure,public:: gut    
            procedure,public:: sstat  
            procedure,public:: gstat  
            procedure,public:: tstat  
            procedure,public:: laga   
            procedure,public:: lagb   
            procedure,public:: delrt  
            procedure,public:: muts   
            procedure,public:: mute   
            procedure,public:: ns     
            procedure,public:: dt     
            procedure,public:: gain   
            procedure,public:: igc    
            procedure,public:: igi    
            procedure,public:: corr   
            procedure,public:: sfs    
            procedure,public:: sfe    
            procedure,public:: slen   
            procedure,public:: styp   
            procedure,public:: stas   
            procedure,public:: stae   
            procedure,public:: tatyp  
            procedure,public:: afilf  
            procedure,public:: afils  
            procedure,public:: nofilf 
            procedure,public:: nofils 
            procedure,public:: lcf    
            procedure,public:: hcf    
            procedure,public:: lcs    
            procedure,public:: hcs    
            procedure,public:: year   
            procedure,public:: day    
            procedure,public:: hour   
            procedure,public:: minute 
            procedure,public:: sec    
            procedure,public:: timbas 
            procedure,public:: trwf   
            procedure,public:: grnors 
            procedure,public:: grnofr 
            procedure,public:: grnlof 
            procedure,public:: gaps   
            procedure,public:: otrav  

            procedure,public:: ntr    
            procedure,public:: mark   

            procedure,public:: d1     
            procedure,public:: f1     
            procedure,public:: d2     
            procedure,public:: f2     
            procedure,public:: ungpow 
            procedure,public:: unscale
    end type 

    type,extends(SuHeader):: SuTrace
        contains
            !! header/data IO
            !! subroutines (read/write, overloaded):: get, set
            !! functions (read only):: header, data
            private
            !! set subroutines
            procedure:: set_header
            procedure:: set_data
            procedure:: set_ctrace
            procedure:: set_all
            generic,public :: set => set_header, set_data, set_ctrace, set_all
            !! get subroutines
            procedure:: sget_header
            procedure:: sget_data
            procedure:: sget_ctrace
            procedure:: get_all
            generic,public :: get => sget_header, sget_data, sget_ctrace, get_all
            !! get functions
            procedure,public:: header => get_header
            procedure,public:: data => get_data
    end type SuTrace

    public:: SuHeader, SuTrace

    interface SuHeader
        module procedure suheader_init
    end interface
    interface SuTrace
        module procedure sutrace_init
    end interface

contains

!! SuHeader - SuTrace_c%SuHeader_c IO
    subroutine set_cheader(su,trc)
    class(SuHeader),intent(inout):: su
    type(SuTrace_c),intent(in):: trc
    call su_set_header(su%trc,trc%th)
    end subroutine

    subroutine get_cheader(su,trc)
    class(SuHeader),intent(in):: su
    type(SuTrace_c),intent(out):: trc
    call su_get_header(su%trc,trc%th)
    end subroutine

!! SuTrace - SuTrace_c IO
    subroutine sget_ctrace(su,trc)
    class(SuTrace),intent(in):: su
    type(SuTrace_c),intent(out):: trc
    trc=su%trc
    end subroutine

    subroutine set_ctrace(su,trc)
    class(SuTrace),intent(inout):: su
    type(SuTrace_c),intent(in):: trc
    su%trc=trc
    end subroutine

!! SuTrace - SuHeader IO
    subroutine sget_header(su,header)
    class(SuTrace),intent(in):: su
    type(SuHeader),intent(out):: header
    header=su%SuHeader
    end subroutine

    type(SuHeader) function get_header(su) result(header)
    class(SuTrace),intent(in):: su
    call sget_header(su,header)
    end function

    subroutine set_header(su,header)
    class(SuTrace),intent(inout):: su
    type(SuHeader),intent(in):: header
    call su%set_cheader(header%trc)
    end subroutine

!! SuTrace - real data IO
    subroutine sget_data(su,arr)
    class(SuTrace),intent(in):: su
    real(kind=4):: arr(su%trc%th%ns)
    call su_get_data(su%trc,arr)
    end subroutine

    function get_data(su) result(arr)
    class(SuTrace),intent(in):: su
    real(kind=4),dimension(su%trc%th%ns) :: arr(su%trc%th%ns)
    call sget_data(su,arr)
    end function

    subroutine set_data(su,arr)
    class(SuTrace),intent(inout):: su
    real(kind=4),intent(in):: arr(:)
    if(size(arr).ne.su%trc%th%ns) stop "ns and array size do not match"
    call su_set_data(su%trc,arr)
    end subroutine

!! SuHeader - get/set key (attribute) value
    integer function key_int(su,attr) result(val)
    class(SuHeader),intent(in):: su
    character(len=*),intent(in):: attr ! attribute name
    call su%get(trim(attr),val)
    end function

    real function key_real(su,attr) result(val)
    class(SuHeader),intent(in):: su
    character(len=*),intent(in):: attr
    call su%get(trim(attr),val)
    end function

    subroutine get_int(su,attr,val)
    class(SuHeader),intent(in):: su
    character(len=*),intent(in):: attr
    integer,intent(out):: val
    call su_get_int(su%trc,trim(attr),val)
    end subroutine

    subroutine get_real(su,attr,val)
    class(SuHeader),intent(in):: su
    character(len=*),intent(in):: attr
    real(kind=4),intent(out):: val
    call su_get_real(su%trc,trim(attr),val)
    end subroutine

    subroutine set_int(su,attr,val)
    class(SuHeader),intent(inout):: su
    character(len=*),intent(in):: attr
    integer,intent(in):: val
    call su_set_int(su%trc,trim(attr),val)
    end subroutine

    subroutine set_real(su,attr,val)
    class(SuHeader),intent(inout):: su
    character(len=*),intent(in):: attr
    real(kind=4),intent(in):: val
    call su_set_real(su%trc,trim(attr),val)
    end subroutine

    integer function tracl  (su); class(SuHeader),intent(in):: su; call su%get('tracl'  ,tracl  ); end function
    integer function tracr  (su); class(SuHeader),intent(in):: su; call su%get('tracr'  ,tracr  ); end function
    integer function fldr   (su); class(SuHeader),intent(in):: su; call su%get('fldr'   ,fldr   ); end function
    integer function tracf  (su); class(SuHeader),intent(in):: su; call su%get('tracf'  ,tracf  ); end function
    integer function ep     (su); class(SuHeader),intent(in):: su; call su%get('ep'     ,ep     ); end function
    integer function cdp    (su); class(SuHeader),intent(in):: su; call su%get('cdp'    ,cdp    ); end function
    integer function cdpt   (su); class(SuHeader),intent(in):: su; call su%get('cdpt'   ,cdpt   ); end function
    integer function trid   (su); class(SuHeader),intent(in):: su; call su%get('trid'   ,trid   ); end function
    integer function nvs    (su); class(SuHeader),intent(in):: su; call su%get('nvs'    ,nvs    ); end function
    integer function nhs    (su); class(SuHeader),intent(in):: su; call su%get('nhs'    ,nhs    ); end function
    integer function duse   (su); class(SuHeader),intent(in):: su; call su%get('duse'   ,duse   ); end function
    integer function offset (su); class(SuHeader),intent(in):: su; call su%get('offset' ,offset ); end function
    integer function gelev  (su); class(SuHeader),intent(in):: su; call su%get('gelev'  ,gelev  ); end function
    integer function selev  (su); class(SuHeader),intent(in):: su; call su%get('selev'  ,selev  ); end function
    integer function sdepth (su); class(SuHeader),intent(in):: su; call su%get('sdepth' ,sdepth ); end function
    integer function gdel   (su); class(SuHeader),intent(in):: su; call su%get('gdel'   ,gdel   ); end function
    integer function sdel   (su); class(SuHeader),intent(in):: su; call su%get('sdel'   ,sdel   ); end function
    integer function swdep  (su); class(SuHeader),intent(in):: su; call su%get('swdep'  ,swdep  ); end function
    integer function gwdep  (su); class(SuHeader),intent(in):: su; call su%get('gwdep'  ,gwdep  ); end function
    integer function scalel (su); class(SuHeader),intent(in):: su; call su%get('scalel' ,scalel ); end function
    integer function scalco (su); class(SuHeader),intent(in):: su; call su%get('scalco' ,scalco ); end function
    integer function sx     (su); class(SuHeader),intent(in):: su; call su%get('sx'     ,sx     ); end function
    integer function sy     (su); class(SuHeader),intent(in):: su; call su%get('sy'     ,sy     ); end function
    integer function gx     (su); class(SuHeader),intent(in):: su; call su%get('gx'     ,gx     ); end function
    integer function gy     (su); class(SuHeader),intent(in):: su; call su%get('gy'     ,gy     ); end function
    integer function counit (su); class(SuHeader),intent(in):: su; call su%get('counit' ,counit ); end function
    integer function wevel  (su); class(SuHeader),intent(in):: su; call su%get('wevel'  ,wevel  ); end function
    integer function swevel (su); class(SuHeader),intent(in):: su; call su%get('swevel' ,swevel ); end function
    integer function sut    (su); class(SuHeader),intent(in):: su; call su%get('sut'    ,sut    ); end function
    integer function gut    (su); class(SuHeader),intent(in):: su; call su%get('gut'    ,gut    ); end function
    integer function sstat  (su); class(SuHeader),intent(in):: su; call su%get('sstat'  ,sstat  ); end function
    integer function gstat  (su); class(SuHeader),intent(in):: su; call su%get('gstat'  ,gstat  ); end function
    integer function tstat  (su); class(SuHeader),intent(in):: su; call su%get('tstat'  ,tstat  ); end function
    integer function laga   (su); class(SuHeader),intent(in):: su; call su%get('laga'   ,laga   ); end function
    integer function lagb   (su); class(SuHeader),intent(in):: su; call su%get('lagb'   ,lagb   ); end function
    integer function delrt  (su); class(SuHeader),intent(in):: su; call su%get('delrt'  ,delrt  ); end function
    integer function muts   (su); class(SuHeader),intent(in):: su; call su%get('muts'   ,muts   ); end function
    integer function mute   (su); class(SuHeader),intent(in):: su; call su%get('mute'   ,mute   ); end function
    integer function ns     (su); class(SuHeader),intent(in):: su; call su%get('ns'     ,ns     ); end function
    integer function dt     (su); class(SuHeader),intent(in):: su; call su%get('dt'     ,dt     ); end function
    integer function gain   (su); class(SuHeader),intent(in):: su; call su%get('gain'   ,gain   ); end function
    integer function igc    (su); class(SuHeader),intent(in):: su; call su%get('igc'    ,igc    ); end function
    integer function igi    (su); class(SuHeader),intent(in):: su; call su%get('igi'    ,igi    ); end function
    integer function corr   (su); class(SuHeader),intent(in):: su; call su%get('corr'   ,corr   ); end function
    integer function sfs    (su); class(SuHeader),intent(in):: su; call su%get('sfs'    ,sfs    ); end function
    integer function sfe    (su); class(SuHeader),intent(in):: su; call su%get('sfe'    ,sfe    ); end function
    integer function slen   (su); class(SuHeader),intent(in):: su; call su%get('slen'   ,slen   ); end function
    integer function styp   (su); class(SuHeader),intent(in):: su; call su%get('styp'   ,styp   ); end function
    integer function stas   (su); class(SuHeader),intent(in):: su; call su%get('stas'   ,stas   ); end function
    integer function stae   (su); class(SuHeader),intent(in):: su; call su%get('stae'   ,stae   ); end function
    integer function tatyp  (su); class(SuHeader),intent(in):: su; call su%get('tatyp'  ,tatyp  ); end function
    integer function afilf  (su); class(SuHeader),intent(in):: su; call su%get('afilf'  ,afilf  ); end function
    integer function afils  (su); class(SuHeader),intent(in):: su; call su%get('afils'  ,afils  ); end function
    integer function nofilf (su); class(SuHeader),intent(in):: su; call su%get('nofilf' ,nofilf ); end function
    integer function nofils (su); class(SuHeader),intent(in):: su; call su%get('nofils' ,nofils ); end function
    integer function lcf    (su); class(SuHeader),intent(in):: su; call su%get('lcf'    ,lcf    ); end function
    integer function hcf    (su); class(SuHeader),intent(in):: su; call su%get('hcf'    ,hcf    ); end function
    integer function lcs    (su); class(SuHeader),intent(in):: su; call su%get('lcs'    ,lcs    ); end function
    integer function hcs    (su); class(SuHeader),intent(in):: su; call su%get('hcs'    ,hcs    ); end function
    integer function year   (su); class(SuHeader),intent(in):: su; call su%get('year'   ,year   ); end function
    integer function day    (su); class(SuHeader),intent(in):: su; call su%get('day'    ,day    ); end function
    integer function hour   (su); class(SuHeader),intent(in):: su; call su%get('hour'   ,hour   ); end function
    integer function minute (su); class(SuHeader),intent(in):: su; call su%get('minute' ,minute ); end function
    integer function sec    (su); class(SuHeader),intent(in):: su; call su%get('sec'    ,sec    ); end function
    integer function timbas (su); class(SuHeader),intent(in):: su; call su%get('timbas' ,timbas ); end function
    integer function trwf   (su); class(SuHeader),intent(in):: su; call su%get('trwf'   ,trwf   ); end function
    integer function grnors (su); class(SuHeader),intent(in):: su; call su%get('grnors' ,grnors ); end function
    integer function grnofr (su); class(SuHeader),intent(in):: su; call su%get('grnofr' ,grnofr ); end function
    integer function grnlof (su); class(SuHeader),intent(in):: su; call su%get('grnlof' ,grnlof ); end function
    integer function gaps   (su); class(SuHeader),intent(in):: su; call su%get('gaps'   ,gaps   ); end function
    integer function otrav  (su); class(SuHeader),intent(in):: su; call su%get('otrav'  ,otrav  ); end function
                                                                   
    integer function ntr    (su); class(SuHeader),intent(in):: su; call su%get('ntr'    ,ntr    ); end function
    integer function mark   (su); class(SuHeader),intent(in):: su; call su%get('mark'   ,mark   ); end function
                                                                   
    real    function d1     (su); class(SuHeader),intent(in):: su; call su%get('d1'     ,d1     ); end function
    real    function f1     (su); class(SuHeader),intent(in):: su; call su%get('f1'     ,f1     ); end function
    real    function d2     (su); class(SuHeader),intent(in):: su; call su%get('d2'     ,d2     ); end function
    real    function f2     (su); class(SuHeader),intent(in):: su; call su%get('f2'     ,f2     ); end function
    real    function ungpow (su); class(SuHeader),intent(in):: su; call su%get('ungpow' ,ungpow ); end function
    real    function unscale(su); class(SuHeader),intent(in):: su; call su%get('unscale',unscale); end function

!! SuHeader initialization
    type(SuHeader) function suheader_init(tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale ) result(su)

    integer,intent(in),optional::         tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(in),optional::            d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    call su_header_init(su%trc%th)
    if(present(tracl  )) su%trc%th%tracl  =tracl  
    if(present(tracr  )) su%trc%th%tracr  =tracr  
    if(present(fldr   )) su%trc%th%fldr   =fldr   
    if(present(tracf  )) su%trc%th%tracf  =tracf  
    if(present(ep     )) su%trc%th%ep     =ep     
    if(present(cdp    )) su%trc%th%cdp    =cdp    
    if(present(cdpt   )) su%trc%th%cdpt   =cdpt   
    if(present(trid   )) su%trc%th%trid   =trid   
    if(present(nvs    )) su%trc%th%nvs    =nvs    
    if(present(nhs    )) su%trc%th%nhs    =nhs    
    if(present(duse   )) su%trc%th%duse   =duse   
    if(present(offset )) su%trc%th%offset =offset 
    if(present(gelev  )) su%trc%th%gelev  =gelev  
    if(present(selev  )) su%trc%th%selev  =selev  
    if(present(sdepth )) su%trc%th%sdepth =sdepth 
    if(present(gdel   )) su%trc%th%gdel   =gdel   
    if(present(sdel   )) su%trc%th%sdel   =sdel   
    if(present(swdep  )) su%trc%th%swdep  =swdep  
    if(present(gwdep  )) su%trc%th%gwdep  =gwdep  
    if(present(scalel )) su%trc%th%scalel =scalel 
    if(present(scalco )) su%trc%th%scalco =scalco 
    if(present(sx     )) su%trc%th%sx     =sx     
    if(present(sy     )) su%trc%th%sy     =sy     
    if(present(gx     )) su%trc%th%gx     =gx     
    if(present(gy     )) su%trc%th%gy     =gy     
    if(present(counit )) su%trc%th%counit =counit 
    if(present(wevel  )) su%trc%th%wevel  =wevel  
    if(present(swevel )) su%trc%th%swevel =swevel 
    if(present(sut    )) su%trc%th%sut    =sut    
    if(present(gut    )) su%trc%th%gut    =gut    
    if(present(sstat  )) su%trc%th%sstat  =sstat  
    if(present(gstat  )) su%trc%th%gstat  =gstat  
    if(present(tstat  )) su%trc%th%tstat  =tstat  
    if(present(laga   )) su%trc%th%laga   =laga   
    if(present(lagb   )) su%trc%th%lagb   =lagb   
    if(present(delrt  )) su%trc%th%delrt  =delrt  
    if(present(muts   )) su%trc%th%muts   =muts   
    if(present(mute   )) su%trc%th%mute   =mute   
    if(present(ns     )) su%trc%th%ns     =ns     
    if(present(dt     )) su%trc%th%dt     =dt     
    if(present(gain   )) su%trc%th%gain   =gain   
    if(present(igc    )) su%trc%th%igc    =igc    
    if(present(igi    )) su%trc%th%igi    =igi    
    if(present(corr   )) su%trc%th%corr   =corr   
    if(present(sfs    )) su%trc%th%sfs    =sfs    
    if(present(sfe    )) su%trc%th%sfe    =sfe    
    if(present(slen   )) su%trc%th%slen   =slen   
    if(present(styp   )) su%trc%th%styp   =styp   
    if(present(stas   )) su%trc%th%stas   =stas   
    if(present(stae   )) su%trc%th%stae   =stae   
    if(present(tatyp  )) su%trc%th%tatyp  =tatyp  
    if(present(afilf  )) su%trc%th%afilf  =afilf  
    if(present(afils  )) su%trc%th%afils  =afils  
    if(present(nofilf )) su%trc%th%nofilf =nofilf 
    if(present(nofils )) su%trc%th%nofils =nofils 
    if(present(lcf    )) su%trc%th%lcf    =lcf    
    if(present(hcf    )) su%trc%th%hcf    =hcf    
    if(present(lcs    )) su%trc%th%lcs    =lcs    
    if(present(hcs    )) su%trc%th%hcs    =hcs    
    if(present(year   )) su%trc%th%year   =year   
    if(present(day    )) su%trc%th%day    =day    
    if(present(hour   )) su%trc%th%hour   =hour   
    if(present(minute )) su%trc%th%minute =minute 
    if(present(sec    )) su%trc%th%sec    =sec    
    if(present(timbas )) su%trc%th%timbas =timbas 
    if(present(trwf   )) su%trc%th%trwf   =trwf   
    if(present(grnors )) su%trc%th%grnors =grnors 
    if(present(grnofr )) su%trc%th%grnofr =grnofr 
    if(present(grnlof )) su%trc%th%grnlof =grnlof 
    if(present(gaps   )) su%trc%th%gaps   =gaps   
    if(present(otrav  )) su%trc%th%otrav  =otrav  

    if(present(ntr    )) su%trc%th%ntr    =ntr    
    if(present(mark   )) su%trc%th%mark   =mark   

    if(present(d1     )) su%trc%th%d1     =d1     
    if(present(f1     )) su%trc%th%f1     =f1     
    if(present(d2     )) su%trc%th%d2     =d2     
    if(present(f2     )) su%trc%th%f2     =f2     
    if(present(ungpow )) su%trc%th%ungpow =ungpow 
    if(present(unscale)) su%trc%th%unscale=unscale
    end function

!! SuTrace initialization
    type(SuTrace) function sutrace_init(  tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale,header,data) result(su)

    integer,intent(in),optional::         tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(in),optional::            d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    type(SuHeader),intent(in),optional:: header
    real,intent(in),optional:: data(:)
    call su_trace_init(su%trc)
    if(present(header )) call su%set(header)

    if(present(tracl  )) su%trc%th%tracl  =tracl  
    if(present(tracr  )) su%trc%th%tracr  =tracr  
    if(present(fldr   )) su%trc%th%fldr   =fldr   
    if(present(tracf  )) su%trc%th%tracf  =tracf  
    if(present(ep     )) su%trc%th%ep     =ep     
    if(present(cdp    )) su%trc%th%cdp    =cdp    
    if(present(cdpt   )) su%trc%th%cdpt   =cdpt   
    if(present(trid   )) su%trc%th%trid   =trid   
    if(present(nvs    )) su%trc%th%nvs    =nvs    
    if(present(nhs    )) su%trc%th%nhs    =nhs    
    if(present(duse   )) su%trc%th%duse   =duse   
    if(present(offset )) su%trc%th%offset =offset 
    if(present(gelev  )) su%trc%th%gelev  =gelev  
    if(present(selev  )) su%trc%th%selev  =selev  
    if(present(sdepth )) su%trc%th%sdepth =sdepth 
    if(present(gdel   )) su%trc%th%gdel   =gdel   
    if(present(sdel   )) su%trc%th%sdel   =sdel   
    if(present(swdep  )) su%trc%th%swdep  =swdep  
    if(present(gwdep  )) su%trc%th%gwdep  =gwdep  
    if(present(scalel )) su%trc%th%scalel =scalel 
    if(present(scalco )) su%trc%th%scalco =scalco 
    if(present(sx     )) su%trc%th%sx     =sx     
    if(present(sy     )) su%trc%th%sy     =sy     
    if(present(gx     )) su%trc%th%gx     =gx     
    if(present(gy     )) su%trc%th%gy     =gy     
    if(present(counit )) su%trc%th%counit =counit 
    if(present(wevel  )) su%trc%th%wevel  =wevel  
    if(present(swevel )) su%trc%th%swevel =swevel 
    if(present(sut    )) su%trc%th%sut    =sut    
    if(present(gut    )) su%trc%th%gut    =gut    
    if(present(sstat  )) su%trc%th%sstat  =sstat  
    if(present(gstat  )) su%trc%th%gstat  =gstat  
    if(present(tstat  )) su%trc%th%tstat  =tstat  
    if(present(laga   )) su%trc%th%laga   =laga   
    if(present(lagb   )) su%trc%th%lagb   =lagb   
    if(present(delrt  )) su%trc%th%delrt  =delrt  
    if(present(muts   )) su%trc%th%muts   =muts   
    if(present(mute   )) su%trc%th%mute   =mute   
    if(present(ns     )) su%trc%th%ns     =ns     
    if(present(dt     )) su%trc%th%dt     =dt     
    if(present(gain   )) su%trc%th%gain   =gain   
    if(present(igc    )) su%trc%th%igc    =igc    
    if(present(igi    )) su%trc%th%igi    =igi    
    if(present(corr   )) su%trc%th%corr   =corr   
    if(present(sfs    )) su%trc%th%sfs    =sfs    
    if(present(sfe    )) su%trc%th%sfe    =sfe    
    if(present(slen   )) su%trc%th%slen   =slen   
    if(present(styp   )) su%trc%th%styp   =styp   
    if(present(stas   )) su%trc%th%stas   =stas   
    if(present(stae   )) su%trc%th%stae   =stae   
    if(present(tatyp  )) su%trc%th%tatyp  =tatyp  
    if(present(afilf  )) su%trc%th%afilf  =afilf  
    if(present(afils  )) su%trc%th%afils  =afils  
    if(present(nofilf )) su%trc%th%nofilf =nofilf 
    if(present(nofils )) su%trc%th%nofils =nofils 
    if(present(lcf    )) su%trc%th%lcf    =lcf    
    if(present(hcf    )) su%trc%th%hcf    =hcf    
    if(present(lcs    )) su%trc%th%lcs    =lcs    
    if(present(hcs    )) su%trc%th%hcs    =hcs    
    if(present(year   )) su%trc%th%year   =year   
    if(present(day    )) su%trc%th%day    =day    
    if(present(hour   )) su%trc%th%hour   =hour   
    if(present(minute )) su%trc%th%minute =minute 
    if(present(sec    )) su%trc%th%sec    =sec    
    if(present(timbas )) su%trc%th%timbas =timbas 
    if(present(trwf   )) su%trc%th%trwf   =trwf   
    if(present(grnors )) su%trc%th%grnors =grnors 
    if(present(grnofr )) su%trc%th%grnofr =grnofr 
    if(present(grnlof )) su%trc%th%grnlof =grnlof 
    if(present(gaps   )) su%trc%th%gaps   =gaps   
    if(present(otrav  )) su%trc%th%otrav  =otrav  

    if(present(ntr    )) su%trc%th%ntr    =ntr    
    if(present(mark   )) su%trc%th%mark   =mark   

    if(present(d1     )) su%trc%th%d1     =d1     
    if(present(f1     )) su%trc%th%f1     =f1     
    if(present(d2     )) su%trc%th%d2     =d2     
    if(present(f2     )) su%trc%th%f2     =f2     
    if(present(ungpow )) su%trc%th%ungpow =ungpow 
    if(present(unscale)) su%trc%th%unscale=unscale

    if(present(data   )) call su%set(data(:))
    end function

!! SuHeader - multiple keywords IO
    subroutine get_key(su                ,tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale)

    class(SuHeader),intent(in):: su
    integer,intent(out),optional::        tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(out),optional::           d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    if(present(tracl  )) tracl  =su%trc%th%tracl  
    if(present(tracr  )) tracr  =su%trc%th%tracr  
    if(present(fldr   )) fldr   =su%trc%th%fldr   
    if(present(tracf  )) tracf  =su%trc%th%tracf  
    if(present(ep     )) ep     =su%trc%th%ep     
    if(present(cdp    )) cdp    =su%trc%th%cdp    
    if(present(cdpt   )) cdpt   =su%trc%th%cdpt   
    if(present(trid   )) trid   =su%trc%th%trid   
    if(present(nvs    )) nvs    =su%trc%th%nvs    
    if(present(nhs    )) nhs    =su%trc%th%nhs    
    if(present(duse   )) duse   =su%trc%th%duse   
    if(present(offset )) offset =su%trc%th%offset 
    if(present(gelev  )) gelev  =su%trc%th%gelev  
    if(present(selev  )) selev  =su%trc%th%selev  
    if(present(sdepth )) sdepth =su%trc%th%sdepth 
    if(present(gdel   )) gdel   =su%trc%th%gdel   
    if(present(sdel   )) sdel   =su%trc%th%sdel   
    if(present(swdep  )) swdep  =su%trc%th%swdep  
    if(present(gwdep  )) gwdep  =su%trc%th%gwdep  
    if(present(scalel )) scalel =su%trc%th%scalel 
    if(present(scalco )) scalco =su%trc%th%scalco 
    if(present(sx     )) sx     =su%trc%th%sx     
    if(present(sy     )) sy     =su%trc%th%sy     
    if(present(gx     )) gx     =su%trc%th%gx     
    if(present(gy     )) gy     =su%trc%th%gy     
    if(present(counit )) counit =su%trc%th%counit 
    if(present(wevel  )) wevel  =su%trc%th%wevel  
    if(present(swevel )) swevel =su%trc%th%swevel 
    if(present(sut    )) sut    =su%trc%th%sut    
    if(present(gut    )) gut    =su%trc%th%gut    
    if(present(sstat  )) sstat  =su%trc%th%sstat  
    if(present(gstat  )) gstat  =su%trc%th%gstat  
    if(present(tstat  )) tstat  =su%trc%th%tstat  
    if(present(laga   )) laga   =su%trc%th%laga   
    if(present(lagb   )) lagb   =su%trc%th%lagb   
    if(present(delrt  )) delrt  =su%trc%th%delrt  
    if(present(muts   )) muts   =su%trc%th%muts   
    if(present(mute   )) mute   =su%trc%th%mute   
    if(present(ns     )) ns     =su%trc%th%ns     
    if(present(dt     )) dt     =su%trc%th%dt     
    if(present(gain   )) gain   =su%trc%th%gain   
    if(present(igc    )) igc    =su%trc%th%igc    
    if(present(igi    )) igi    =su%trc%th%igi    
    if(present(corr   )) corr   =su%trc%th%corr   
    if(present(sfs    )) sfs    =su%trc%th%sfs    
    if(present(sfe    )) sfe    =su%trc%th%sfe    
    if(present(slen   )) slen   =su%trc%th%slen   
    if(present(styp   )) styp   =su%trc%th%styp   
    if(present(stas   )) stas   =su%trc%th%stas   
    if(present(stae   )) stae   =su%trc%th%stae   
    if(present(tatyp  )) tatyp  =su%trc%th%tatyp  
    if(present(afilf  )) afilf  =su%trc%th%afilf  
    if(present(afils  )) afils  =su%trc%th%afils  
    if(present(nofilf )) nofilf =su%trc%th%nofilf 
    if(present(nofils )) nofils =su%trc%th%nofils 
    if(present(lcf    )) lcf    =su%trc%th%lcf    
    if(present(hcf    )) hcf    =su%trc%th%hcf    
    if(present(lcs    )) lcs    =su%trc%th%lcs    
    if(present(hcs    )) hcs    =su%trc%th%hcs    
    if(present(year   )) year   =su%trc%th%year   
    if(present(day    )) day    =su%trc%th%day    
    if(present(hour   )) hour   =su%trc%th%hour   
    if(present(minute )) minute =su%trc%th%minute 
    if(present(sec    )) sec    =su%trc%th%sec    
    if(present(timbas )) timbas =su%trc%th%timbas 
    if(present(trwf   )) trwf   =su%trc%th%trwf   
    if(present(grnors )) grnors =su%trc%th%grnors 
    if(present(grnofr )) grnofr =su%trc%th%grnofr 
    if(present(grnlof )) grnlof =su%trc%th%grnlof 
    if(present(gaps   )) gaps   =su%trc%th%gaps   
    if(present(otrav  )) otrav  =su%trc%th%otrav  
                                 
    if(present(ntr    )) ntr    =su%trc%th%ntr    
    if(present(mark   )) mark   =su%trc%th%mark   
                                 
    if(present(d1     )) d1     =su%trc%th%d1     
    if(present(f1     )) f1     =su%trc%th%f1     
    if(present(d2     )) d2     =su%trc%th%d2     
    if(present(f2     )) f2     =su%trc%th%f2     
    if(present(ungpow )) ungpow =su%trc%th%ungpow 
    if(present(unscale)) unscale=su%trc%th%unscale
    end subroutine

    subroutine set_key(su                ,tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale )

    class(SuHeader),intent(inout):: su
    integer,intent(in),optional::         tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(in),optional::            d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    if(present(tracl  )) su%trc%th%tracl  =tracl  
    if(present(tracr  )) su%trc%th%tracr  =tracr  
    if(present(fldr   )) su%trc%th%fldr   =fldr   
    if(present(tracf  )) su%trc%th%tracf  =tracf  
    if(present(ep     )) su%trc%th%ep     =ep     
    if(present(cdp    )) su%trc%th%cdp    =cdp    
    if(present(cdpt   )) su%trc%th%cdpt   =cdpt   
    if(present(trid   )) su%trc%th%trid   =trid   
    if(present(nvs    )) su%trc%th%nvs    =nvs    
    if(present(nhs    )) su%trc%th%nhs    =nhs    
    if(present(duse   )) su%trc%th%duse   =duse   
    if(present(offset )) su%trc%th%offset =offset 
    if(present(gelev  )) su%trc%th%gelev  =gelev  
    if(present(selev  )) su%trc%th%selev  =selev  
    if(present(sdepth )) su%trc%th%sdepth =sdepth 
    if(present(gdel   )) su%trc%th%gdel   =gdel   
    if(present(sdel   )) su%trc%th%sdel   =sdel   
    if(present(swdep  )) su%trc%th%swdep  =swdep  
    if(present(gwdep  )) su%trc%th%gwdep  =gwdep  
    if(present(scalel )) su%trc%th%scalel =scalel 
    if(present(scalco )) su%trc%th%scalco =scalco 
    if(present(sx     )) su%trc%th%sx     =sx     
    if(present(sy     )) su%trc%th%sy     =sy     
    if(present(gx     )) su%trc%th%gx     =gx     
    if(present(gy     )) su%trc%th%gy     =gy     
    if(present(counit )) su%trc%th%counit =counit 
    if(present(wevel  )) su%trc%th%wevel  =wevel  
    if(present(swevel )) su%trc%th%swevel =swevel 
    if(present(sut    )) su%trc%th%sut    =sut    
    if(present(gut    )) su%trc%th%gut    =gut    
    if(present(sstat  )) su%trc%th%sstat  =sstat  
    if(present(gstat  )) su%trc%th%gstat  =gstat  
    if(present(tstat  )) su%trc%th%tstat  =tstat  
    if(present(laga   )) su%trc%th%laga   =laga   
    if(present(lagb   )) su%trc%th%lagb   =lagb   
    if(present(delrt  )) su%trc%th%delrt  =delrt  
    if(present(muts   )) su%trc%th%muts   =muts   
    if(present(mute   )) su%trc%th%mute   =mute   
    if(present(ns     )) su%trc%th%ns     =ns     
    if(present(dt     )) su%trc%th%dt     =dt     
    if(present(gain   )) su%trc%th%gain   =gain   
    if(present(igc    )) su%trc%th%igc    =igc    
    if(present(igi    )) su%trc%th%igi    =igi    
    if(present(corr   )) su%trc%th%corr   =corr   
    if(present(sfs    )) su%trc%th%sfs    =sfs    
    if(present(sfe    )) su%trc%th%sfe    =sfe    
    if(present(slen   )) su%trc%th%slen   =slen   
    if(present(styp   )) su%trc%th%styp   =styp   
    if(present(stas   )) su%trc%th%stas   =stas   
    if(present(stae   )) su%trc%th%stae   =stae   
    if(present(tatyp  )) su%trc%th%tatyp  =tatyp  
    if(present(afilf  )) su%trc%th%afilf  =afilf  
    if(present(afils  )) su%trc%th%afils  =afils  
    if(present(nofilf )) su%trc%th%nofilf =nofilf 
    if(present(nofils )) su%trc%th%nofils =nofils 
    if(present(lcf    )) su%trc%th%lcf    =lcf    
    if(present(hcf    )) su%trc%th%hcf    =hcf    
    if(present(lcs    )) su%trc%th%lcs    =lcs    
    if(present(hcs    )) su%trc%th%hcs    =hcs    
    if(present(year   )) su%trc%th%year   =year   
    if(present(day    )) su%trc%th%day    =day    
    if(present(hour   )) su%trc%th%hour   =hour   
    if(present(minute )) su%trc%th%minute =minute 
    if(present(sec    )) su%trc%th%sec    =sec    
    if(present(timbas )) su%trc%th%timbas =timbas 
    if(present(trwf   )) su%trc%th%trwf   =trwf   
    if(present(grnors )) su%trc%th%grnors =grnors 
    if(present(grnofr )) su%trc%th%grnofr =grnofr 
    if(present(grnlof )) su%trc%th%grnlof =grnlof 
    if(present(gaps   )) su%trc%th%gaps   =gaps   
    if(present(otrav  )) su%trc%th%otrav  =otrav  

    if(present(ntr    )) su%trc%th%ntr    =ntr    
    if(present(mark   )) su%trc%th%mark   =mark   

    if(present(d1     )) su%trc%th%d1     =d1     
    if(present(f1     )) su%trc%th%f1     =f1     
    if(present(d2     )) su%trc%th%d2     =d2     
    if(present(f2     )) su%trc%th%f2     =f2     
    if(present(ungpow )) su%trc%th%ungpow =ungpow 
    if(present(unscale)) su%trc%th%unscale=unscale
    end subroutine

!! SuTrace - multiple keywords IO
    subroutine get_all(su                ,tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale,header,data)

    class(SuTrace),intent(in):: su
    integer,intent(out),optional::        tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(out),optional::           d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    type(SuHeader),intent(out),optional:: header
    real,intent(out),optional:: data(su%trc%th%ns)

    if(present(header )) header =su%header()

    if(present(tracl  )) tracl  =su%trc%th%tracl  
    if(present(tracr  )) tracr  =su%trc%th%tracr  
    if(present(fldr   )) fldr   =su%trc%th%fldr   
    if(present(tracf  )) tracf  =su%trc%th%tracf  
    if(present(ep     )) ep     =su%trc%th%ep     
    if(present(cdp    )) cdp    =su%trc%th%cdp    
    if(present(cdpt   )) cdpt   =su%trc%th%cdpt   
    if(present(trid   )) trid   =su%trc%th%trid   
    if(present(nvs    )) nvs    =su%trc%th%nvs    
    if(present(nhs    )) nhs    =su%trc%th%nhs    
    if(present(duse   )) duse   =su%trc%th%duse   
    if(present(offset )) offset =su%trc%th%offset 
    if(present(gelev  )) gelev  =su%trc%th%gelev  
    if(present(selev  )) selev  =su%trc%th%selev  
    if(present(sdepth )) sdepth =su%trc%th%sdepth 
    if(present(gdel   )) gdel   =su%trc%th%gdel   
    if(present(sdel   )) sdel   =su%trc%th%sdel   
    if(present(swdep  )) swdep  =su%trc%th%swdep  
    if(present(gwdep  )) gwdep  =su%trc%th%gwdep  
    if(present(scalel )) scalel =su%trc%th%scalel 
    if(present(scalco )) scalco =su%trc%th%scalco 
    if(present(sx     )) sx     =su%trc%th%sx     
    if(present(sy     )) sy     =su%trc%th%sy     
    if(present(gx     )) gx     =su%trc%th%gx     
    if(present(gy     )) gy     =su%trc%th%gy     
    if(present(counit )) counit =su%trc%th%counit 
    if(present(wevel  )) wevel  =su%trc%th%wevel  
    if(present(swevel )) swevel =su%trc%th%swevel 
    if(present(sut    )) sut    =su%trc%th%sut    
    if(present(gut    )) gut    =su%trc%th%gut    
    if(present(sstat  )) sstat  =su%trc%th%sstat  
    if(present(gstat  )) gstat  =su%trc%th%gstat  
    if(present(tstat  )) tstat  =su%trc%th%tstat  
    if(present(laga   )) laga   =su%trc%th%laga   
    if(present(lagb   )) lagb   =su%trc%th%lagb   
    if(present(delrt  )) delrt  =su%trc%th%delrt  
    if(present(muts   )) muts   =su%trc%th%muts   
    if(present(mute   )) mute   =su%trc%th%mute   
    if(present(ns     )) ns     =su%trc%th%ns     
    if(present(dt     )) dt     =su%trc%th%dt     
    if(present(gain   )) gain   =su%trc%th%gain   
    if(present(igc    )) igc    =su%trc%th%igc    
    if(present(igi    )) igi    =su%trc%th%igi    
    if(present(corr   )) corr   =su%trc%th%corr   
    if(present(sfs    )) sfs    =su%trc%th%sfs    
    if(present(sfe    )) sfe    =su%trc%th%sfe    
    if(present(slen   )) slen   =su%trc%th%slen   
    if(present(styp   )) styp   =su%trc%th%styp   
    if(present(stas   )) stas   =su%trc%th%stas   
    if(present(stae   )) stae   =su%trc%th%stae   
    if(present(tatyp  )) tatyp  =su%trc%th%tatyp  
    if(present(afilf  )) afilf  =su%trc%th%afilf  
    if(present(afils  )) afils  =su%trc%th%afils  
    if(present(nofilf )) nofilf =su%trc%th%nofilf 
    if(present(nofils )) nofils =su%trc%th%nofils 
    if(present(lcf    )) lcf    =su%trc%th%lcf    
    if(present(hcf    )) hcf    =su%trc%th%hcf    
    if(present(lcs    )) lcs    =su%trc%th%lcs    
    if(present(hcs    )) hcs    =su%trc%th%hcs    
    if(present(year   )) year   =su%trc%th%year   
    if(present(day    )) day    =su%trc%th%day    
    if(present(hour   )) hour   =su%trc%th%hour   
    if(present(minute )) minute =su%trc%th%minute 
    if(present(sec    )) sec    =su%trc%th%sec    
    if(present(timbas )) timbas =su%trc%th%timbas 
    if(present(trwf   )) trwf   =su%trc%th%trwf   
    if(present(grnors )) grnors =su%trc%th%grnors 
    if(present(grnofr )) grnofr =su%trc%th%grnofr 
    if(present(grnlof )) grnlof =su%trc%th%grnlof 
    if(present(gaps   )) gaps   =su%trc%th%gaps   
    if(present(otrav  )) otrav  =su%trc%th%otrav  
                                 
    if(present(ntr    )) ntr    =su%trc%th%ntr    
    if(present(mark   )) mark   =su%trc%th%mark   
                                 
    if(present(d1     )) d1     =su%trc%th%d1     
    if(present(f1     )) f1     =su%trc%th%f1     
    if(present(d2     )) d2     =su%trc%th%d2     
    if(present(f2     )) f2     =su%trc%th%f2     
    if(present(ungpow )) ungpow =su%trc%th%ungpow 
    if(present(unscale)) unscale=su%trc%th%unscale

    if(present(data   )) data   =su%data()
    end subroutine

    subroutine set_all(su                ,tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   ,d1     ,f1     ,d2     ,f2     ,ungpow ,unscale,header,data)

    class(SuTrace),intent(inout):: su
    integer,intent(in),optional::         tracl  ,tracr  ,fldr   ,tracf  ,ep     ,cdp    ,cdpt   ,trid   ,nvs    ,nhs    &
                                         ,duse   ,offset ,gelev  ,selev  ,sdepth ,gdel   ,sdel   ,swdep  ,gwdep  ,scalel &
                                         ,scalco ,sx     ,sy     ,gx     ,gy     ,counit ,wevel  ,swevel ,sut    ,gut    &
                                         ,sstat  ,gstat  ,tstat  ,laga   ,lagb   ,delrt  ,muts   ,mute   ,ns     ,dt     &
                                         ,gain   ,igc    ,igi    ,corr   ,sfs    ,sfe    ,slen   ,styp   ,stas   ,stae   &
                                         ,tatyp  ,afilf  ,afils  ,nofilf ,nofils ,lcf    ,hcf    ,lcs    ,hcs    ,year   &
                                         ,day    ,hour   ,minute ,sec    ,timbas ,trwf   ,grnors ,grnofr ,grnlof ,gaps   &
                                         ,otrav  ,ntr    ,mark   
    real,intent(in),optional::            d1     ,f1     ,d2     ,f2     ,ungpow ,unscale
    type(SuHeader),intent(in),optional:: header
    real,intent(in),optional:: data(:)

    if(present(header )) call su%set(header) !! set header first so that the following keywords can modify header keys

    if(present(tracl  )) su%trc%th%tracl  =tracl  
    if(present(tracr  )) su%trc%th%tracr  =tracr  
    if(present(fldr   )) su%trc%th%fldr   =fldr   
    if(present(tracf  )) su%trc%th%tracf  =tracf  
    if(present(ep     )) su%trc%th%ep     =ep     
    if(present(cdp    )) su%trc%th%cdp    =cdp    
    if(present(cdpt   )) su%trc%th%cdpt   =cdpt   
    if(present(trid   )) su%trc%th%trid   =trid   
    if(present(nvs    )) su%trc%th%nvs    =nvs    
    if(present(nhs    )) su%trc%th%nhs    =nhs    
    if(present(duse   )) su%trc%th%duse   =duse   
    if(present(offset )) su%trc%th%offset =offset 
    if(present(gelev  )) su%trc%th%gelev  =gelev  
    if(present(selev  )) su%trc%th%selev  =selev  
    if(present(sdepth )) su%trc%th%sdepth =sdepth 
    if(present(gdel   )) su%trc%th%gdel   =gdel   
    if(present(sdel   )) su%trc%th%sdel   =sdel   
    if(present(swdep  )) su%trc%th%swdep  =swdep  
    if(present(gwdep  )) su%trc%th%gwdep  =gwdep  
    if(present(scalel )) su%trc%th%scalel =scalel 
    if(present(scalco )) su%trc%th%scalco =scalco 
    if(present(sx     )) su%trc%th%sx     =sx     
    if(present(sy     )) su%trc%th%sy     =sy     
    if(present(gx     )) su%trc%th%gx     =gx     
    if(present(gy     )) su%trc%th%gy     =gy     
    if(present(counit )) su%trc%th%counit =counit 
    if(present(wevel  )) su%trc%th%wevel  =wevel  
    if(present(swevel )) su%trc%th%swevel =swevel 
    if(present(sut    )) su%trc%th%sut    =sut    
    if(present(gut    )) su%trc%th%gut    =gut    
    if(present(sstat  )) su%trc%th%sstat  =sstat  
    if(present(gstat  )) su%trc%th%gstat  =gstat  
    if(present(tstat  )) su%trc%th%tstat  =tstat  
    if(present(laga   )) su%trc%th%laga   =laga   
    if(present(lagb   )) su%trc%th%lagb   =lagb   
    if(present(delrt  )) su%trc%th%delrt  =delrt  
    if(present(muts   )) su%trc%th%muts   =muts   
    if(present(mute   )) su%trc%th%mute   =mute   
    if(present(ns     )) su%trc%th%ns     =ns     
    if(present(dt     )) su%trc%th%dt     =dt     
    if(present(gain   )) su%trc%th%gain   =gain   
    if(present(igc    )) su%trc%th%igc    =igc    
    if(present(igi    )) su%trc%th%igi    =igi    
    if(present(corr   )) su%trc%th%corr   =corr   
    if(present(sfs    )) su%trc%th%sfs    =sfs    
    if(present(sfe    )) su%trc%th%sfe    =sfe    
    if(present(slen   )) su%trc%th%slen   =slen   
    if(present(styp   )) su%trc%th%styp   =styp   
    if(present(stas   )) su%trc%th%stas   =stas   
    if(present(stae   )) su%trc%th%stae   =stae   
    if(present(tatyp  )) su%trc%th%tatyp  =tatyp  
    if(present(afilf  )) su%trc%th%afilf  =afilf  
    if(present(afils  )) su%trc%th%afils  =afils  
    if(present(nofilf )) su%trc%th%nofilf =nofilf 
    if(present(nofils )) su%trc%th%nofils =nofils 
    if(present(lcf    )) su%trc%th%lcf    =lcf    
    if(present(hcf    )) su%trc%th%hcf    =hcf    
    if(present(lcs    )) su%trc%th%lcs    =lcs    
    if(present(hcs    )) su%trc%th%hcs    =hcs    
    if(present(year   )) su%trc%th%year   =year   
    if(present(day    )) su%trc%th%day    =day    
    if(present(hour   )) su%trc%th%hour   =hour   
    if(present(minute )) su%trc%th%minute =minute 
    if(present(sec    )) su%trc%th%sec    =sec    
    if(present(timbas )) su%trc%th%timbas =timbas 
    if(present(trwf   )) su%trc%th%trwf   =trwf   
    if(present(grnors )) su%trc%th%grnors =grnors 
    if(present(grnofr )) su%trc%th%grnofr =grnofr 
    if(present(grnlof )) su%trc%th%grnlof =grnlof 
    if(present(gaps   )) su%trc%th%gaps   =gaps   
    if(present(otrav  )) su%trc%th%otrav  =otrav  

    if(present(ntr    )) su%trc%th%ntr    =ntr    
    if(present(mark   )) su%trc%th%mark   =mark   

    if(present(d1     )) su%trc%th%d1     =d1     
    if(present(f1     )) su%trc%th%f1     =f1     
    if(present(d2     )) su%trc%th%d2     =d2     
    if(present(f2     )) su%trc%th%f2     =f2     
    if(present(ungpow )) su%trc%th%ungpow =ungpow 
    if(present(unscale)) su%trc%th%unscale=unscale

    if(present(data   )) call su%set(data(:)) !! set data here so that it can use new ns if ns is given
    end subroutine

end module suio_trace
