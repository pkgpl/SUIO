# 설치

우선 [Github](https://github.com/pkgpl/SUIO)에서 라이브러리를 받습니다.
`/path/to/SUIO/src` 디렉토리에서 `Makefile`의 필요한 옵션을 바꾼 후에 다음을 실행합니다.

	$ make install

SUIO 라이브러리를 이용하여 프로그램을 작성한 후 컴파일할 때에는

	$ gfortran -o myprogram mycode.f90 -I/path/to/SUIO/include -L/path/to/SUIO/lib -lsuio

와 같이 실행하시면 됩니다. 설치한 라이브러리를 지울 때에는

	$ make uninstall

을 실행합니다.

## XDR 형식

컴파일하기 전에 시스템에서 기본적을 사용할 SU 파일의 형식을 지정할 수 있습니다. `module_xdr_default.f90` 파일의 `XDR_DEFAULT`를 `.false.`로 선언하면 (기본값) native binary SU 파일을 기본값으로 하고, `.true.`로 선언하면 XDR SU 파일을 기본값으로 합니다. 기본값을 어떤 값으로 하든 실제 SU 파일을 열 때 형식은 함수의 `xdr` 옵션을 통해 바꿀 수 있습니다. 기본값은 `xdr` 옵션을 생략했을 때 쓰입니다.