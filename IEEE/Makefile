# Build IEEE modules + test program

F90=gfortran
CC=gcc
OUT=IEEE_TEST

all:
	 $(F90) -c ./precision_m.f90
	 $(F90) -c ./messy_m.f90
	 $(CC) -c ./c_control.c
	 $(F90) -fno-range-check -c ./IEEE_FEATURES.f90
	 $(F90) -fno-range-check -c ./IEEE_EXCEPTIONS.f90
	 $(F90) -fno-range-check -c ./IEEE_ARITHMETIC.f90
	 $(F90) -o $(OUT) IEEE_tests_gfortran.f90 precision_m.f90 messy_m.f90 \
	   IEEE_FEATURES.f90 IEEE_EXCEPTIONS.f90 IEEE_ARITHMETIC.f90 c_control.c

clean:
	rm -f -r f_{files,modd}* *.o *.mod *.M *.d V*.inc *.vo \
	V*.f *.dbg album F.err

