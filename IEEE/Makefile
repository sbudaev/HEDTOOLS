# Build IEEE modules + test program

FC=gfortran
CC=gcc
OUT=IEEE_TEST

all:
	 $(FC) -c ./precision_m.f90
	 $(FC) -c ./messy_m.f90
	 $(CC) -c ./c_control.c
	 $(FC) -fno-range-check -c ./IEEE_FEATURES.f90
	 $(FC) -fno-range-check -c ./IEEE_EXCEPTIONS.f90
	 $(FC) -fno-range-check -c ./IEEE_ARITHMETIC.f90
	 $(FC) -o $(OUT) IEEE_tests_gfortran.f90 precision_m.f90 messy_m.f90 \
	   IEEE_FEATURES.f90 IEEE_EXCEPTIONS.f90 IEEE_ARITHMETIC.f90 c_control.c

clean:
	-rm -f -r f_{files,modd}* *.o *.mod *.M *.d V*.inc *.vo \
	V*.f *.dbg album F.err

run: $(OUT)
	./$(OUT)
