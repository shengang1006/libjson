#������ָ��������·��
CC      := g++

AR := ar rc

# args

#������ָ����Ҫ�Ŀ��ļ� -L
LIBS    := -lrt

#������ָ��������Ҫ��ͷ�ļ�
INCLUDE := -I./

#������lib·��
LIBPATH := 


#������Դ�ļ�
SRCS    := *.c

#������ָ��Ŀ���ļ� ���е�.cpp�ļ����.o�ļ�
OBJS    := $(SRCS:.c=.o)


#�����Ǳ���ѡ��
CFLAGS  := -g -Wall -c $(INCLUDE) $(LIBPATH) 
CFLAGS  += -DLINUX 

TARGETLIB := libjson.a

all:
	$(CC) $(CFLAGS) $(SRCS) $(LIBS)
	$(AR) $(TARGETLIB) $(OBJS) 
#make clean ɾ�����е�.o�ļ�
clean:
	rm -f ./*.o