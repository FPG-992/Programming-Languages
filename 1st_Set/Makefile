CC=g++
CFLAGS=-std=c++11 -Wall -Werror -O3
LIBS=-lm
TARGET=fairseq
SRC=fairseq.cpp

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(LIBS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET)

.PHONY: all clean
