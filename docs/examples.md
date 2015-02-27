# 사용 예제

먼저, 연습 삼아 Seismic Un\*x에 있는 프로그램들과 동일한 기능(`suwind`, `supaste`)을 구현해본 후에 Seismic Un\*x에서 지원하지 않는 기능을 구현해보겠습니다.

## suwind

Seismic Un*x의 `suwind` 명령어와 같은 기능을 하는 프로그램입니다. 표준 입력 SU 파일에서 `fldr` 키 값이 10 이상 20 이하인 트레이스만 추출하여 표준 출력으로 내보냅니다.

```
 1 program suwind 2 use suio 3 type(SuFile):: sf, so 4 type(SuTrace):: trc 5 character(len=4):: keyword=“fldr” 6 integer:: keymin=10, keymax=20, keyval
 7 8 sf = su_input() 9 so = su_output()10 do while( sf%fread(trc) )       keyval = trc%key(keyword)       if(keymin <= keyval .and. keyval <= keymax) call so%write(trc)11 enddo12 call su_close(sf, so)13 end program
```

### 선언 부분

1행: 프로그램을 시작합니다.

2행: SU 파일 입출력 모듈을 불러옵니다.

3행: 입력 및 출력 SU 파일에 접근하기 위한 SuFile 객체들을 선언합니다.

4행: SU 트레이스를 저장하기 위한 SuTrace 객체를 선언합니다.

5-6행: window 적용에 사용할 키와 키의 최소 및 최대값을 선언합니다.

### 파일 열기

8-9행: 표준 입력과 표준 출력을 통해 SU 파일을 참조하는 SuFile 객체들을 생성합니다.

### 트레이스 읽고 쓰기

10-13행의 루프에서는 입력 파일(sf)의 끝에 도달할 때까지 반복적으로 SU 트레이스를 하나씩 읽어 들여 SuTrace 객체(trc)에 저장합니다.

11행: SuTrace 객체에서 원하는 키 값을 keyval에 저장합니다.

12행: 읽어들인 키 값이 원하는 범위에 속할 경우 출력 파일(so)에 SU 트레이스를 씁니다.

### 파일 닫고 끝내기

14행: 입출력 SuFile 객체 들을 제거하여 SU 파일에 대한 참조를 마칩니다.

15행: 프로그램을 끝냅니다.

위의 프로그램은 다음 명령과 동일한 기능을 수행합니다.

```
suwind key=fldr min=10 max=20 < input.su > output.su
```


## supaste

`input_header.su` 파일에서 트레이스 헤더를 읽고, `input_data.su` 파일에서 트레이스 자료를 읽어 `output.su` 파일에 적습니다.

```
program supasteuse suiotype(SuFile):: sh, sd, sotype(SuTrace):: trcinteger:: itr, ntr, nssh = su_input(“input_header.su”, ntr=ntr)sd = su_input(“input_data.su”, ns=ns)so = su_output(“output.su”)do itr = 1,ntr    trc = sd%trace(itr)    call trc%set(header=sh%header(itr), ns=ns)    call so%write(trc)enddocall su_close(sh, sd, so)end program
```

샘플 개수가 1000개일 때, 위의 프로그램은 다음 명령과 동일한 기능을 수행합니다.

```
sustrip head=header < input_header.su > /dev/null
sustrip < input_data.su > data.binsupaste ns=1000 head=header < data.bin > output.su
```

## sunewtoold

XDR 형식의 SU 파일을 표준 입력으로부터 읽어 native binary 형식으로 출력하는 프로그램으로, suoldtonew의 역변환 프로그램입니다.

```
program suNewToOlduse suiotype(SuFile):: sf, sotype(SuTrace):: trcsf = su_input(xdr=.true.)so = su_output(xdr=.false.)do while( sf%fread(trc) )    call so%write(trc)enddocall su_close(sf,so)end program
```

## Write a velocity file

메모리에 존재하는 속도모델을 SU 파일로 출력하는 프로그램입니다.

```
subroutine vel_to_su(fout, vel, n1, n2, d1, d2)use suiocharacter(len=*), intent(in):: foutinteger, intent(in):: n1, n2real, intent(in):: d1, d2, vel(n1,n2)integer:: itr, scalco=100type(SuFile):: sftype(SuTrace):: trctrc = SuTrace(ns=n1, dt=nint(d1), scalco=-scalco, ntr=n2, d1=d1, d2=d2) ! same for all tracessf = su_output(trim(fout))do itr = 1,n2    call trc%set(tracl=itr, gx=nint((itr-1)*d2*scalco), data=vel(:,itr))    call sf%write(trc)enddocall sf%close()end subroutine
```
