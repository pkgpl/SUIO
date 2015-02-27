CC = icc
FC = ifort

CFLAGS = -O2
FFLAGS = -O2 -assume byterecl -warn all

TARGET = libsuio.a
OBJECTS = func_suio.o xdrhdrsub.o module_types.o module_binding.o module_trace.o module_xdr_default.o module_file.o module_suio.o

all: $(TARGET)

$(TARGET): $(OBJECTS)
	ar rc $@ $^
	ranlib $@


.SUFFIXES: .o .c .f90
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm $(TARGET) $(OBJECTS) *.mod

install: $(TARGET)
	mkdir ../include ../lib
	cp *.mod ../include
	cp $(TARGET) ../lib

uninstall: clean
	rm -r ../include ../lib
