cmake_minimum_required(VERSION 3.10.0)

project(jq VERSION 0.0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_FLAGS "-O1 -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")

########################################################################
#
# jq

set(JQ_VERSION 1.6)
set(JQ_DIR ${CMAKE_CURRENT_BINARY_DIR}/jq-${JQ_VERSION}/src/libjq)
set(JQ_INCLUDE_DIR ${JQ_DIR}/src)

include(ExternalProject)

ExternalProject_Add(libjq
  GIT_REPOSITORY https://github.com/stedolan/jq.git
  GIT_TAG jq-${JQ_VERSION}
  CONFIGURE_COMMAND autoreconf -fi
    && ./configure --with-oniguruma=builtin --with-pic
  BUILD_COMMAND make LDFLAGS=-all-static -j8
    && cd ${JQ_DIR}/modules/oniguruma && make
  BUILD_IN_SOURCE 1
  INSTALL_COMMAND cp .libs/libjq.a ${CMAKE_CURRENT_BINARY_DIR}
    && cp modules/oniguruma/src/.libs/libonig.a ${CMAKE_CURRENT_BINARY_DIR}
  PREFIX jq-${JQ_VERSION}
  )

link_directories(${CMAKE_BINARY_DIR})

########################################################################
#
# Define the jq-impl library.

add_library(jq_impl SHARED
  src/jq_impl.cc
  src/utils.cc
  )

target_include_directories(jq_impl PUBLIC
  ${JQ_INCLUDE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src)

target_link_libraries(jq_impl jq pthread onig)

set_target_properties(jq_impl PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION 1
  PREFIX ""
  OUTPUT_NAME "jq-impl"
  )
