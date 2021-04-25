if (NOT TARGET gmock AND NOT TARGET gtest)
    add_subdirectory(external/googletest)
endif()

add_executable(unittests
    main.cpp
)

set(COMPILER_WARNINGS
    -Wall
    -Werror
    -Wextra # reasonable and standard
    -Wshadow # warn the user if a variable declaration shadows one from a parent context
    -Wnon-virtual-dtor # warn the user if a class with virtual functions has a non-virtual destructor. This helps
                       # catch hard to track down memory errors
    -Wold-style-cast # warn for c-style casts
    -Wcast-align # warn for potential performance problem casts
    -Wunused # warn on anything being unused
    -Woverloaded-virtual # warn if you overload (not override) a virtual function
    -Wpedantic # warn if non-standard C++ is used
    -Wconversion # warn on type conversions that may lose data
    -Wsign-conversion # warn on sign conversions
    -Wdouble-promotion # warn if float is implicit promoted to double
    -Wformat=2 # warn on security issues around functions that format output (ie printf)
    -Wlogical-op # warn about logical operations being used where bitwise were probably wanted
    -Wuseless-cast # warn if you perform a cast to the same type
)

set_property(TARGET unittests PROPERTY CXX_STANDARD 11)
set_property(TARGET unittests PROPERTY CXX_STANDARD_REQUIRED TRUE)
set_property(TARGET unittests PROPERTY CXX_EXTENSIONS OFF)

target_compile_options(unittests PRIVATE ${COMPILER_WARNINGS})

target_link_libraries(unittests
    PRIVATE lib-expected
    PRIVATE gmock
    PRIVATE gtest
)
