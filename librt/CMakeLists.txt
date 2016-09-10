cmake_minimum_required(VERSION 3.5)
project(cobraruntime)

include_directories(include)
add_library(cobraruntime STATIC src/cobrart.c)
install(TARGETS cobraruntime ARCHIVE DESTINATION lib)