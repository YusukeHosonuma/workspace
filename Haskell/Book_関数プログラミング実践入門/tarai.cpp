// tarai.cpp
// $ g++ -o tarai -O2 -std=C++0x tarai.cpp
#include <iostream>
#include <cstdlib>
#include <functional>

int tarai(int x, int y, int z) {
    return (x <= y)
        ? y
        : tarai(tarai(x - 1, y, z),
                tarai(y - 1, z, x),
                tarai(z - 1, x, y));
}

int lazy_tarai(int x, int y, int z) {
    std::function< int (const std::function< int() >&,
                        const std::function< int() >&,
                        const std::function< int() >&) > lt =
        [&lt] (const std::function< int() >& thunk_x,
               const std::function< int() >& thunk_y,
               const std::function< int() >& thunk_z) -> int {
        const int x = thunk_x(); // evolute x
        const int y = thunk_y(); // evolute y
        if (x <= y) {
            return y;
        } else {
            const int z = thunk_z(); // evolute z
            auto const_x = [&x] { return x; };
            auto const_y = [&y] { return y; };
            auto const_z = [&z] { return z; };
            return lt([&] { return lt([&x] { return x - 1; }, const_y, const_z); },
                      [&] { return lt([&y] { return y - 1; }, const_z, const_x); },
                      [&] { return lt([&z] { return z - 1; }, const_x, const_y); });
        }
    };
    return lt([=] { return x; }, [=] { return y; }, [=] { return z; });
}

int main(int argc, char *argv[]) {
    if (argc < 4) return 1;
    int x = std::atoi(argv[1]);
    int y = std::atoi(argv[2]);
    int z = std::atoi(argv[3]);
    std::cout << lazy_tarai(x, y, z) << std::endl;
    return 0;
}
