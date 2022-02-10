cmake_minimum_required(VERSION 3.18.0)
project(mill)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_compile_options(-Wall -Wextra -pedantic -std=c++20)
find_program(CXXBRIDGE cxxbridge)

set(CXX_BRIDGE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/../rust/rtl.rs)
set(CXX_BRIDGE_GEN ${CMAKE_CURRENT_BINARY_DIR}/generated.cpp)
set(CXX_BRIDGE_HDR ${CMAKE_CURRENT_BINARY_DIR}/generated.h)
set(CXX_BRIDGE_SHARED_HDR ${CMAKE_CURRENT_BINARY_DIR}/cxx.h)
add_custom_command(
  OUTPUT ${CXX_BRIDGE_GEN}
  COMMAND ${CXXBRIDGE} ${CXX_BRIDGE_SRC} -o ${CXX_BRIDGE_GEN}
  DEPENDS ${CXX_BRIDGE_SRC}
)
add_custom_command(
  OUTPUT ${CXX_BRIDGE_HDR}
  COMMAND ${CXXBRIDGE} ${CXX_BRIDGE_SRC} --header -o ${CXX_BRIDGE_HDR}
  DEPENDS ${CXX_BRIDGE_SRC}
)
add_custom_command(
  OUTPUT ${CXX_BRIDGE_SHARED_HDR}
  COMMAND ${CXXBRIDGE} --header -o ${CXX_BRIDGE_SHARED_HDR}
)

find_package(verilator
  HINTS $ENV{VERILATOR_ROOT}
)

add_library(
  meowv64rtl SHARED bridge.cpp bridge.h ${CXX_BRIDGE_GEN} ${CXX_BRIDGE_HDR} ${CXX_BRIDGE_SHARED_HDR}
)

verilate(
  meowv64rtl
  SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../Multicore.v
  PREFIX rtl
  TRACE_FST
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/verilated
  VERILATOR_ARGS --trace-params --trace-structs --top-module Multicore
)

install(TARGETS meowv64rtl)
