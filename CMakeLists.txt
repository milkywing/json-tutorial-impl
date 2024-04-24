cmake_minimum_required (VERSION 3.11)
project (leptjson C)

if (CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic -Wall")
endif()

add_library(leptjson src/leptjson.c)
add_executable(leptjson_test src/test.c)
target_link_libraries(leptjson_test leptjson)
