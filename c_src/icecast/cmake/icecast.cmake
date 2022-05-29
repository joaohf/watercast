# this uses a technique to avoid autotools constantly rebuilding,
# by making the ExternalProject only visible once--until the binaries are built
# then, we use the Scotch_FOUND to switch in a dummy target.
# We do the same thing with HDF5 in other projects.
#
# https://www.scivision.dev/cmake-external-project-autotools/
# https://mirkokiefer.com/cmake-by-example-f95eb47d45b1
# https://github.com/scivision/scotch-cmake

set(icecast_external true CACHE BOOL "build icecast")

cmake_host_system_information(RESULT Ncpu QUERY NUMBER_OF_PHYSICAL_CORES)
message(STATUS "CMake ${CMAKE_VERSION} using ${Ncpu} threads")

find_program(MAKE_EXECUTABLE NAMES gmake make mingw32-make REQUIRED)

# --- help autotools based on CMake variables

include(ExternalProject)

# set(Scotch_LIBRARIES)
# foreach(_l esmumps scotch scotcherr scotcherrexit scotchmetis)
#   list(APPEND Scotch_LIBRARIES ${PROJECT_BINARY_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${_l}${CMAKE_STATIC_LIBRARY_SUFFIX})
# endforeach()

if(EXISTS ${PROJECT_BINARY_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}icecast${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(Icecast_FOUND true)
endif()

if(NOT Icecast_FOUND)

  set(_src ${PROJECT_BINARY_DIR}/Icecast-prefix/src/Icecast/src/)

  if(UNIX)
    if(CMAKE_C_COMPILER_ID STREQUAL GNU)
      set(icecast_cflags "")
      set(icecast_ldflags "")
      set(icecast_config "--disable-yp")
    endif()
  endif()

#   configure_file(${CMAKE_CURRENT_LIST_DIR}/Makefile.inc Makefile.inc @ONLY)

  set(_targ icecast)

  ExternalProject_Add(Icecast
  URL https://downloads.xiph.org/releases/icecast/icecast-2.4.4.tar.gz
  URL_HASH MD5=835c7b571643f6436726a6118defb366
  # PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_BINARY_DIR}/Makefile.inc ${_src}
  BUILD_IN_SOURCE true
  CONFIGURE_COMMAND ${PROJECT_BINARY_DIR}/Icecast-prefix/src/Icecast/configure ${icecast_config}
  BUILD_COMMAND ${MAKE_EXECUTABLE} -j${Ncpu}
  INSTALL_COMMAND ${MAKE_EXECUTABLE} -j${Ncpu} install prefix=${PROJECT_BINARY_DIR}/../../../priv/icecast
  # BUILD_BYPRODUCTS ${Scotch_LIBRARIES}
  )

#   file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/include)  # avoid race condition

#   ExternalProject_Add_Step(Scotch build_esmumps
#   COMMAND ${MAKE_EXECUTABLE} -j${Ncpu} -C ${_src}/esmumps install
#   DEPENDEES build
#   DEPENDERS install
#   )
endif()

# add_library(Scotch::Scotch INTERFACE IMPORTED GLOBAL)
# target_include_directories(Scotch::Scotch INTERFACE ${PROJECT_BINARY_DIR}/include)
# target_link_libraries(Scotch::Scotch INTERFACE ${Scotch_LIBRARIES})
# set_target_properties didn't work, but target_link_libraries did work

# if(NOT Scotch_FOUND)
#   add_dependencies(Scotch::Scotch Scotch)
# endif()