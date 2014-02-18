
OBJS = \
	cl++.o \
	builtin.o \
	harlan.o \

SRC = \
	harlan.hpp \
	harlan.cpp \
	builtin.cpp \
	cl++.h \
	cl++.cpp \
	gpu_common.h \
	gpu_only.h \

CXXFLAGS = -I/usr/local/cuda/include -I/opt/nvidia/cudatoolkit/default/include -g -fPIC -O2

.phony : clean all

all: libharlanrt.a libharlanrts.a

libharlanrt.a : $(OBJS) $(SRC) harlan_main.o harlan_main.cpp
	rm -f libharlanrt.a
	$(AR) rcs libharlanrt.a $(OBJS) harlan_main.o

libharlanrts.a : $(OBJS) $(SRC)
	rm -f libharlanrts.a
	$(AR) rcs libharlanrts.a $(OBJS)


clean :
	rm -rf *.o libharlantrt.a
