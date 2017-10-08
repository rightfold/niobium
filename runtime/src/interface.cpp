#include <niobium/detail/utility.hpp>
#include <niobium/interface.hpp>
#include <niobium/value.hpp>

#include <array>
#include <cstdint>
#include <streambuf>

nb::interface::report::~report() = default;

nb::value nb::interface::read_int(std::streambuf& streambuf) {
  std::array<char, 4> buffer;
  auto n = streambuf.sgetn(buffer.data(), buffer.size());
  if (n != buffer.size()) {
    throw "could not read int";
  }
  auto integer_value = nb::detail::memcpy_cast<std::int32_t>(buffer);
  return nb::value::new_integer(integer_value);
}

void nb::interface::write_int(std::streambuf& streambuf, nb::value& value) {
  std::int32_t integer_value = value.integer_value();
  auto buffer = nb::detail::memcpy_cast<std::array<char, 4>>(integer_value);
  auto n = streambuf.sputn(buffer.data(), buffer.size());
  if (n != buffer.size()) {
    throw "could not write int";
  }
}
