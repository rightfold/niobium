#pragma once

#include <niobium/context.hpp>
#include <niobium/value.hpp>

#include <cstddef>

namespace nb {
  namespace query {
    class result {
    };

    // FIXME(rightfold): These two functions should take as arguments the static
    //                   types of the USING and GIVING items, and use those to
    //                   determine how to marshal to/from PostgreSQL data.
    void execute(context&, result&, char const*, std::size_t, value const*, std::size_t);
    void next(result&, value* const*, std::size_t);
  }
}
