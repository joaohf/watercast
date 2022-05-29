watercast
=====

Just an example how one could use Cmake instead of Makefiles for building
C code.

Build
-----

    $ rebar3 compile

It's also possible to call cmake if necessary:

    $ cd c_src/icecast && cmake -B build
    $ cmake --build build 
