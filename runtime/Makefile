CXX=g++
CXXFLAGS=-std=c++11 -Wall -Wextra -Wpedantic -Iinclude

CXX_SOURCES=$(shell find src -type f -name '*.cpp')
CXX_OBJECTS=$(patsubst src/%.cpp,build/%.o,${CXX_SOURCES})

all: build/libniobiumrt.a

.PHONY: clean
clean:
	rm -rf build

build/libniobiumrt.a: ${CXX_OBJECTS}
	mkdir -p $(dir $@)
	ar rcs $@ $^

build/%.o: src/%.cpp
	mkdir -p $(dir $@)
	${CXX} ${CXXFLAGS} -c -o $@ $<
