# 설치

우선 [Github](https://github.com/pkgpl/SUIO)에서 [라이브러리](https://github.com/pkgpl/SUIO/archive/master.zip)를 받습니다. 압축을 푼 후 `/path/to/SUIO/src` 디렉토리에서 `Makefile`의 필요한 옵션을 바꾼 후에 다음을 실행합니다.

	$ make install

SUIO 라이브러리를 이용하여 프로그램을 작성한 후 컴파일할 때에는

	$ gfortran -o myprogram mycode.f90 -I/path/to/SUIO/include -L/path/to/SUIO/lib -lsuio

와 같이 실행하시면 됩니다. 설치한 라이브러리를 지울 때에는

	$ make uninstall

을 실행합니다.
