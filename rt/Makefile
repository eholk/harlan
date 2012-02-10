
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

CXXFLAGS = -I../gc/include

libharlanrt.a : $(OBJS) $(SRC)
	rm -f libharlanrt.a
	$(AR) rcs libharlanrt.a $(OBJS)

.PHONY : clean

clean :
	rm -rf *.o libharlantrt.o
