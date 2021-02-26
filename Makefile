# Build PETSc using
#   http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.9.2.tar.gz
#   ./configure --with-cc=gcc-mp-6 --with-cxx=g++-mp-6 --with-fc=gfortran-mp-6 --download-mpich --download-fblaslapack
#   make all test
#   export PETSC_DIR=$(pwd)
#   e.g. for me it's
#   export PETSC_DIR=/Users/spollard/Documents/uo/research/matrix-free/petsc-3.8.3
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules

out: out.o
	-${CLINKER} -o $@ $^ ${PETSC_KSP_LIB}
	${RM} out.o
clean::
	${RM} out