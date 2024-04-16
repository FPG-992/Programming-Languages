# Makefile for exercise_1.cpp

# Compiler
CXX = g++

# Compiler flags
CXXFLAGS = -std=c++11 -Wall -Werror -O3 -lm

# Target executable
TARGET = fairseq

.PHONY: all clean

# Default target
all: $(TARGET)

# Linking
$(TARGET): exercise_1.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

# Clean target
clean:
	@rm -f $(TARGET)

