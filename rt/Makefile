
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

CXXFLAGS = -I/usr/local/cuda/include -g -fPIC -O2

libharlanrt.a : $(OBJS) $(SRC)
	rm -f libharlanrt.a
	$(AR) rcs libharlanrt.a $(OBJS)

.PHONY : clean

clean :
	rm -rf *.o libharlantrt.a
