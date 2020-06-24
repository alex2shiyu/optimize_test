##
##
## Introduction
## ============
##
## Makefile for iqist/src/common directory.
##
## Usage
## =====
##
## 'make' or 'make clean'
##
## Author
## ======
##
## This building system is designed, created, implemented, and maintained by
##
## Li Huang // email: huangli712@gmail.com
##
## History
## =======
##
## 09/18/2014 by li huang
##
##

.SUFFIXES: .f90

#include ../build/make.sys
include ./make.sys
mod = enorm.o dogleg.o qform.o qrfac.o r1mpyq.o r1updt.o hybrj.o hybrj1.o sf_root.o
mod2 = enorm.o dogleg.o qform.o qrfac.o r1mpyq.o r1updt.o hybrj.o hybrj1.o sf_root.o main-2-root.o
objects = $(mod)
objects2 = $(mod2)
WORKDIR = build
SRCDIR  = src
VPATH = $(SRCDIR):$(WORKDIR)

default: all

all: build-mod build-lib

build-mod: $(mod)
build-dylib: headers exemmp
build-exe: headers exe
headers:
	@if [ ! -d $(WORKDIR)  ];then \
	mkdir $(WORKDIR);\
	echo "create build @ RTGW_lIB"; \
	fi

exemmp: $(objects)
	(cd $(WORKDIR);$(F90) $(ARCHIVERdlib) -o $(dlibname) $(objects) $(LIBS); cd ..)

exe: $(objects2)
	(cd $(WORKDIR);$(F90) -o $(nameexe) $(objects2) $(LIBS); cd ..)

build-lib: headers libmmp


.f90.o:
	(cd $(WORKDIR);$(F90) $(FFLAGS)  ../$< -o $@;cd ..)
####(cd $(WORKDIR);$(F90) $(FFLAGS) ../$*.f90; cd ..)

clean:
	rm -rf $(WORKDIR)

clean-dat:
	rm -f *.dat
	rm -f *.bin.*
	rm -f *.out

clean-all: clean clean-dat

show:
	@echo "make build-mod"
	@echo "make build-sub"
	@echo "make build-dylib"
	@echo "make build-lib"
	@echo "make clean"
	@echo "make clean-dat"
	@echo "make clean-all"
