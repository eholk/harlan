
OBJS = \
	cl++.o \
	builtin.o \
	harlan.o \

CXXFLAGS = -I../gc/include

libharlanrt.a : $(OBJS)
	rm -f libharlanrt.a
	$(AR) rcs libharlanrt.a $(OBJS)

.PHONY : clean

clean :
	rm -rf *.o libharlantrt.o
