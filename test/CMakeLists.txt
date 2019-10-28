if (NOT TARGET gmock AND NOT TARGET gtest)
    add_subdirectory(external/googletest)
endif()

add_executable(unittests
    main.cpp
)

target_link_libraries(unittests
    PRIVATE lib-expected
    PRIVATE gmock
    PRIVATE gtest
)
