cmake_minimum_required(VERSION 2.8.4)
project(oberon)

set(CMAKE_PREFIX_PATH /opt/llvm-3.5/)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs core jit native bitwriter)
message(STATUS "Using LLVM libs: ${llvm_libs}")

add_executable(test_parser ast.cpp lexer.cpp native.cpp parser.cpp printer.cpp test/test_parser.cpp)
add_executable(test_compiler ast.cpp lexer.cpp native.cpp parser.cpp generator.cpp test/test_compiler.cpp)

target_link_libraries(test_compiler ${llvm_libs})
