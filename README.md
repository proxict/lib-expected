![GitHub](https://img.shields.io/github/license/proxict/lib-expected)

lib-expected
------------

This library provides an `Expected` type - wrapper for representing an expected object which may either contain an initialized value or an error.
Also, this implementation allows storing references:

```c++
int k = 3;
Expected<int&> intRef(k);
k = 1;
assert(*intRef == 1);
```

The whole implementation is in `libExpected` namespace.
 
Integration with CMake
----------------------
```cmake
add_subdirectory(third-party/lib-expected)
target_link_libraries(your-target
    PRIVATE lib-expected
)
```

Tests can be allowed by setting `BUILD_TESTS` variable to `TRUE`:
```bash
mkdir -p build && cd build
cmake -DBUILD_TESTS=1 ..
```

How to use?
-----------
Hopefully, this short example might give you a rough idea about how this type could be used:
```c++
#include <lib-expected/expected.hpp>
using namespace libExpected;

Expected<std::string> getAttribute(const std::string& attributeName) {
    auto attribute = mAttributes.find(attributeName);
    if (attribute != mAttributes.end()) {
        return *attribute;
    }
    return makeUnexpected("The attribute " + attributeName + " doesn't exist");
}

int main() {
    auto id = getAttribute("id");
    if (id) {
        std::cout << "id=" << *id << '\n';
    } else {
        std::clog << "Error: " << id.error() << '\n';
        return 1;
    }
    return 0;
}
```
