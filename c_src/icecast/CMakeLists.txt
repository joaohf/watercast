cmake_minimum_required(VERSION 3.15...3.21)

project(icecast
  LANGUAGES C
  DESCRIPTION "Build and use icecast"
  HOMEPAGE_URL https://icecast.org/)

option(icecast_external "build icecast" true)

set_directory_properties(PROPERTIES EP_UPDATE_DISCONNECTED true)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

if(icecast_external)
  include(cmake/icecast.cmake)
endif()