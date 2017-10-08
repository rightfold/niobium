#pragma once

#include <cstring>

namespace nb {
  namespace detail {
    template<typename T, typename U>
    T memcpy_cast(U from) noexcept {
      static_assert(sizeof(T) == sizeof(U),
                    "memcpy_cast requires that both types be of the same size.");
      static_assert(__has_trivial_copy(T) && __has_trivial_copy(U),
                    "memcpy_cast requires that both types be trivially copyable.");
      T to;
      std::memcpy(&to, &from, sizeof(U));
      return to;
    }
  }
}
