#include <niobium/context.hpp>
#include <niobium/query.hpp>
#include <niobium/value.hpp>

#include <cstddef>

void nb::query::execute(context&, result&, char const*, std::size_t, value const*, std::size_t) {
}

void nb::query::next(result&, value* const*, std::size_t) {
}
