FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto -march=native
FOPT=-O3 -ftree-vectorize -ftree-loop-vectorize -fopt-info-vec -fopenmp -fopenmp-simd -fopenmp-allocators

all:
	$(FC) $(FFLAGS) -c src/environment.f08 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/array_io.f08 -J obj/ -o obj/array_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/matrix_process.f08 -J obj/ -o obj/matrix_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f08 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/array_io.o obj/matrix_process.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
