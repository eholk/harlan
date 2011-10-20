
OBJS = \
	cl++.o \
	builtin.o \

CXXFLAGS = -I../gc/include

libharlanrt.a : $(OBJS)
	$(AR) rcs libharlanrt.a $(OBJS)

.PHONY : clean

clean :
	rm -rf *.o libharlantrt.o
