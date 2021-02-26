# Project: Matlab2PETSc
This project implements a tiny subset of Matlab and converts them to PETSc routines.

For more information on the concepts, see the project proposal inside `proposal`.

An example code is shown in out.c, which is the Matlab program `matlab/mm.m`

Currently the features supported are:
	- Matlab printing if a line does not end in ; (; suppressed output)
	- Allocate and set matrices and vectors using eye (identity), as well
		as vectors of all zeros and all ones

If you want to compile this yourself, you can see instructions in the Makefile. Once you have PETSc installed, a typical workflow would consist of the following:

```
export PETSC_DIR=/path/to/petsc
stack build
stack exec m2petsc/mm.m
make out
./out
```

This project successfully can multiply two matrices in PETSc! As for much else, I'm not certain. This project was more challenging than I originally intended because it took me a long time to grok the Parser package. Additionally, there are just many considerations when writing a parser, and PETSc has a relatively steep learning curve. Fortunately, I already have some experience with both PETSc and Matlab.

I also had to make many design decisions, such as storing the table of variables as a List and represeting a variable as simply a string (instead of something more that can keep track of metadata such as sparsity pattern, whether it was initialized, etc.). I think these will come back to bite me but in the interest of simplicity of implementation this worked.

Regardless, this project was very interesting and I believe this can turn into my future research project. Either that, or using this framework to generate the raw C instead of calling PETSc could be interesting to explore.

