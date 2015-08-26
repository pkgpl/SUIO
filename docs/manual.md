# 사용법

라이브러리의 기본적인 사용 과정은 다음과 같습니다.

1. 선언: 사용할 SuFile, SuTrace 타입(클래스 객체) 선언
2. 열기: SuFile 타입을 이용해 읽거나 쓸 SU 파일 열기
3. 읽기/쓰기 루프: SU 파일의 각각의 트레이스별로 루프를 돌면서 SuTrace를 읽거나 쓰기
4. 닫기: SU 파일 닫기

열기, 읽기/쓰기, 닫기에는 동일한 역할을 하는 함수(서브루틴)들이 있으니 상황에 따라 편리한 것을 사용하시면 됩니다.

## SU 파일 열기

SU 파일을 열 때에는 SuFile 변수(객체)를 이용합니다. 파일을 열 때 읽기를 위해 열 것인지 쓰기를 위해 열 것인지, 표준입출력을 통해 읽고 쓸 것인지 파일을 통해 읽고 쓸 것인지, native binary 형식인지 XDR 형식인지 등을 설정합니다. 파일을 열 때 사용하는 함수는 `su_input`, `su_output`, `SuFile`이 있습니다.

### 읽기 위해 열기

읽기 위해 여는 함수 `su_input`의 인터페이스는 다음과 같습니다.

```
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
```

- 위 함수에서 `filename`은 열고 싶은 SU 파일 이름으로, 생략하면 표준입력에서 파일을 읽습니다.
- `xdr`은 `.true.`일 경우 XDR 형식의 SU 파일로, `.false.`일 경우 native binary 형식의 SU 파일로 인식합니다. 생략하면 `module_xdr_default.f90` 파일에 선언되어 있는 `XDR_DEFAULT` 형식으로 엽니다. 기본값은 `.false.`인데 컴파일하기 전에 자신의 시스템에 맞게 바꾸시면 되겠습니다.
- `ns`는 SU 파일의 샘플 개수입니다. `ns`를 넣으면 첫 번째 트레이스 헤더의 `ns` 값을 반환합니다.
- `ntr`은 SU 파일의 트레이스 개수입니다. 파일 크기로부터 트레이스 개수를 알아내는데, 표준입출력을 사용할 경우에는 사용할 수 없습니다.

#### 예제

```
type(SuFile):: sf
integer:: ns,ntr
!! file
sf = su_input('input.su')
sf = su_input('input.su', xdr=.true.)
sf = su_input('input.su', xdr=.false., ns=ns, ntr=ntr)
sf = su_input('intput.su', ntr=ntr)
!! 물론, 위의 경우들 중 하나를 선택해서 사용해야겠죠.

!! standard input
sf = su_input()
sf = su_input(xdr=.true.)
sf = su_input(ns=ns)
```


### 쓰기 위해 열기

쓰기 위해 여는 함수 `su_output`의 인터페이스는 다음과 같습니다.

```
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

변수들은 앞의 경우와 같습니다.

#### 예제

```
type(SuFile):: so
!! file
so = su_output('output.su')
so = su_output('output.su',xdr=.true.)

!! standard output
so = su_output()
so = su_output(xdr=.false.)
```


### 읽기 또는 쓰기를 지정해서 열기

위의 함수들 외에도 `SuFile` 생성자 함수를 이용할 수 있습니다. 인터페이스는 다음과 같습니다.

```
! file input/output
type(SuFile) function SuFile(filename, mode, xdr, ns, ntr)
character(len=*),intent(in):: filename
character,intent(in):: mode
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns, ntr
end function

! standard input/output
type(SuFile) function SuFile(mode, xdr, ns)
character,intent(in):: mode
logical,intent(in),optional:: xdr
integer,intent(out),optional:: ns
end function
```

- `mode`는 읽기인지 쓰기인지 지정해줍니다. `'r'` 또는 `'R'`이면 읽기, `'w'` 또는 `'W'`이면 쓰기입니다. 

나머지 변수들은 앞의 경우와 같습니다. 읽기 전용 파일과 쓰기 전용 파일은 구분됩니다. 읽기 전용으로 연 파일에 쓰거나 쓰기 전용으로 연 파일에서 읽을 수는 없습니다.


## SU trace 읽기

SU 파일을 읽기용으로 연 후에는 SU 파일로부터 트레이스들을 읽을 수 있습니다. 트레이스를 읽을 때에는 `SuFile`의 type-bound procedure인 `read`, `fread`, `trace` 중 하나와 `SuTrace` 변수(객체)를 이용합니다. 함수들의 인터페이스는 다음과 같습니다.

```
subroutine read(sf,trc,itr,eof)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(out):: su
integer,intent(in),optional:: itr
logical,intent(out),optional:: eof
end subroutine
```

- `sf`는 SuFile 객체로, 위의 서브루틴은 `call sf%read(trc,itr,eof)` 형식으로 사용하게 됩니다.
- `su`는 SuTrace 객체로 읽어들인 트레이스(헤더+데이터)를 저장합니다.
- `itr`은 SU 파일을 특정 트레이스로 바로 이동해서 읽고 싶을 때 트레이스 번호를 지정해주기 위해 사용합니다. 사용 가능한 트레이스 번호는 1부터 `ntr`까지입니다. 생략하면 첫번째 트레이스부터 `call` 할 때마다 하나씩 순차적으로 읽습니다.
- `eof`는 파일 끝에 도달했을 경우 `.true.`, 그렇지 않을 경우 `.false.`가 됩니다.

`fread` 함수는 위 `eof`의 반대값을 반환하는 함수입니다. 파일 끝에 도달하기 전까지는 `.true.`이다가 파일 끝에 도달하면 `.false.`가 됩니다. `do while` 루프에서 사용할 수 있습니다.

```
logical function fread(sf,trc,itr)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(out):: su
integer,intent(in),optional:: itr
end function
```

`trace` 함수는 `SuTrace` 자체를 반환합니다.

```
type(SuTrace) function trace(sf,itr)
type(SuFile),intent(inout):: sf
integer,intent(in),optional:: itr
end function
```

#### 예제

```
type(SuTrace):: trc

do itr = 1, ntr
	call sf%read(trc)
enddo

do itr = 1, ntr
	trc = sf%trace()
enddo

do while( sf%fread(trc) )
	!! work
enddo
```

## SU trace header만 읽기

헤더 값만 필요할 경우 트레이스 전체를 읽는 것보다는 헤더만 읽는 것이 효율적입니다. 헤더만 읽고 싶을 때는 앞의 경우와 유사하나, `SuTrace` 대신 `SuHeader` 타입(클래스)를 사용합니다.

```
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

트레이스를 읽을 때와 다른 점은 트레이스 번호 `itr`이 꼭 필요하다는 점입니다.


## SU trace 편집하기

SU 트레이스를 SuTrace 변수에 읽어들인 후에는 트레이스의 헤더, 헤더의 키워드, 트레이스의 데이터를 읽거나 수정할 수 있습니다. 헤더만 SuHeader 변수에 읽었다면 헤더만 수정 가능합니다. 이 때에는 SuTrace나 SuHeader의 type-bound procedures를 이용합니다.

### SU header keywords 읽기
헤더의 특정 키워드를 읽을 때에는 `get` 서브루틴과 `key` 함수, keyword 이름으로 된 함수를 이용할 수 있습니다. 본 포트란 라이브러리에서 SU keyword에 접근할 때에는 (실제 SU 구조와 상관 없이) 정수는 모두 4바이트 정수를 사용합니다. `d1, f1, d2, f2, ungpow, unscale`는 4바이트 실수이고, 나머지 모든 키워드는 4바이트 정수를 이용하여 접근합니다. 아래 예에서 `trc` 위치에는 SuTrace 변수와 SuHeader 변수 모두 사용 가능합니다.

```
type(SuTrace):: trc
! type(SuHeader):: trc
integer:: fldr, tracf, sx
real:: f1

!! single keyword
!!! get 함수
call trc%get('fldr',fldr)
call trc%get('f1',f1) !! 정수, 실수 모두 get

!!! key (정수), key_real (실수) 함수
fldr = trc%key('fldr') !! 정수: key
f1 = trc%key_real('f1') !! 실수: key_real

!!! keyword 이름으로 된 함수
fldr = trc%fldr()
f1 = trc%f1()

!! multiple keywords
call trc%get(fldr=fldr, tracf=tracf, sx=sx, f1=f1)
```


### SU header keywords 쓰기
헤더의 특정 키워드를 쓸 때에는 `set` 서브루틴을 이용합니다.

```
integer:: fldr=10, tracf=1
real:: f1=0.0
!! singel keyword
call trc%set('fldr',fldr)
call trc%set('f1',f1)

!! multiple keywords
call trc%set(fldr=fldr, f1=f1)
```

### SU trace header 읽기
SuTrace 변수에서 헤더 부분만 가져다가 SuHeader에 저장할 때에는 `get` 서브루틴이나 `header` 함수를 이용합니다.

```
type(SuTrace):: trc
type(SuHeader):: sh
call trc%get(sh)
sh = trc%header()
```

### SU trace header 쓰기
SuTrace 변수에서 헤더 부분을 바꿀 때에는 `set` 서브루틴을 이용합니다.

```
type(SuTrace):: trc
type(SuHeader):: sh
call trc%set(sh)
```

### SU trace data 읽기
SuTrace 변수에서 데이터 부분만 가져다가 4바이트 실수 배열에 저장할 때에는 `get` 서브루틴이나 `data` 함수를 이용합니다.

```
type(SuTrace):: trc
real(kind=4):: arr(ns) !! 파일을 열 때 ns를 읽은 후 allocate 합니다.
call trc%get(arr)
arr = trc%data()
```

### SU trace data 쓰기
SuTrace 변수에서 데이터 부분을 바꿀 때에는 `set` 서브루틴을 이용합니다.

```
type(SuTrace):: trc
real(kind=4):: arr(ns) !! 파일을 열 때 ns를 읽은 후 allocate 합니다.
call trc%set(arr)
```

### get, set 서브루틴
위에서 볼 수 있는 것처럼, `get`, `set` 서브루틴을 이용하면 헤더, 키워드, 데이터를 모두 다룰 수 있습니다. 한꺼번에 다루는 것도 가능합니다.

```
type(SuTrace):: trc
type(SuHeader):: sh
real(kind=4):: arr(ns)
integer:: fldr, tracf
real:: f1, d1

call trc%get(fldr=fldr, tracf=tracf, f1=f1, d1=d1, header=sh, data=arr)
call trc%set(fldr=fldr, tracf=tracf, f1=f1, d1=d1, header=sh, data=arr)
```

## SU trace 초기화
SuTrace 변수를 SU 파일에서 읽어들일 경우에는 초기화가 필요 없지만 직접 트레이스를 만들어서 쓸 경우에는 초기화가 필요합니다. 이 경우에는 `SuHeader`와 `SuTrace` 생성자 함수를 이용합니다.

```
type(SuTrace):: trc
type(SuHeader):: sh, head
integer:: ns=3000, dt=4000
sh = SuHeader(ns=ns, dt=dt)
trc = SuTrace(ns=ns, dt=dt, header=head)
```

위의 SuTrace에서 header 내에 keyword 들이 있는데 일부 keyword들(`ns, dt`)을 다시 할당했습니다. 이 경우에는 header에서 할당한 값을 새로 지정한 keyword들이 덮어씁니다.

## SU trace 쓰기
SuTrace를 가지고 작업을 한 후에는 쓰기 전용으로 연 SU 파일에 트레이스를 쓸 수 있습니다. 쓸 때에는 SuFile의 type-bound procedure인 `write` 서브루틴을 이용합니다. 인터페이스는 다음과 같습니다.

```
subroutine write(sf,trc)
type(SuFile),intent(inout):: sf
type(SuTrace),intent(in):: su
end subroutine
```

쓸 때에는 읽을 때와 달리 항상 처음부터 순차적으로 쓰고, 트레이스 단위로 쓰도록 만들었습니다. 예제를 보겠습니다. 더 실제적인 예제는 [사용예제](http://suio.readthedocs.org/examples/)에서 볼 수 있습니다.

```
type(SuFile):: sf, so, sd
type(SuTrace):: trc
! open

do while( sf%fread(trc) )
    call so%write(trc)
enddo

do itr=1,ntr
	trc = sd%trace(itr)
	call trc%set(header=sf%header(itr),ns=ns)
	call so%write(trc)
enddo
```

## SU 파일 닫기
SU 파일을 이용한 읽기 또는 쓰기 작업을 끝냈으면 파일을 닫아줍니다. 닫을 때에는 `su_close` 서브루틴을 이용합니다. 한번에 최대 10개의 파일을 닫을 수 있습니다.

```
type(SuFile):: sf, so, sd
! open

call su_close(sf)
call su_close(sd, so)
```

## 정리
정리하면, 읽거나 쓰기 위해 SU 파일을 열 때에는

- `sf = su_input('input.su',ns=ns,ntr=ntr)`
- `so = su_output(xdr=.true.)`
- `sf = SuFile('r')`

SU 파일을 닫을 때에는 

- `call su_close(sf,so)`

트레이스 또는 트레이스 헤더를 읽을 때에는

- `call sf%read(trc)`
- `sf%fread(trc)`
- `trc = sf%trace(itr)`

- `call sf%read(head,itr)`
- `sf%fread(head,itr)`
- `head = sf%header(itr)`

트레이스를 쓸 때에는

- `call sf%write(trc)`

트레이스에서 keyword를 읽고 쓸 때에는

- `int_value = trc%key('fldr')`
- `real_value = trc%key_real('f1')`
- `fldr = trc%fldr() !! 등등 모든 SU Keywords`
- `call trc%get('fldr',fldr)`
- `call trc%get(fldr=fldr, tracf=tracf)`

- `call trc%set('fldr',fldr)`
- `call trc%set(fldr=fldr, tracf=tracf)`

트레이스에서 헤더나 데이터를 읽고 쓸 때에는

- `arr = trc%data()`
- `call trc%get(arr)`
- `call trc%get(data=arr)`

- `head = trc%header()`
- `call trc%set(head)`
- `call trc%set(header=head)`

트레이스나 헤더를 초기화할 때에는

- `trc = SuTrace(header=head,fldr=fldr,ns=ns,dt=dt)`
- `head = SuHeader(fldr=fldr,ns=ns,dt=dt)`

