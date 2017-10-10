#include <niobium/detail/utility.hpp>
#include <niobium/handler.hpp>
#include <niobium/value.hpp>

#include <boost/variant.hpp>

#include <cstdint>
#include <memory>
#include <string>
#include <utility>



nb::value nb::value::new_integer(std::int64_t integer_value) {
  value result;
  result.variant = integer_value;
  return result;
}

nb::value nb::value::new_array(std::size_t length) {
  value result;
  result.variant = std::make_shared<std::vector<value>>(length);
  return result;
}

nb::value nb::value::new_string(char const* bytes, std::size_t length) {
  value result;
  result.variant = std::make_shared<std::string>(bytes, length);
  return result;
}



std::int64_t nb::value::integer_value() const {
  auto pointer = boost::get<integer_type>(&variant);
  assert(pointer);
  return *pointer;
}



std::size_t nb::value::array_length() const {
  auto& pointer = boost::get<array_type>(variant);
  return pointer->size();
}

nb::value nb::value::array_element(std::size_t index) const {
  auto& pointer = boost::get<array_type>(variant);
  return pointer->at(index);
}

void nb::value::set_array_element(std::size_t index, value element) {
  auto& pointer = boost::get<array_type>(variant);
  pointer->at(index) = std::move(element);
}



nb::handler::report const& nb::value::report_handler_value() const {
  auto& pointer = boost::get<report_handler_type>(variant);
  return *pointer;
}



nb::value nb::operator+(value const& x, value const& y) {
  auto xu = nb::detail::memcpy_cast<std::uint64_t>(x.integer_value());
  auto yu = nb::detail::memcpy_cast<std::uint64_t>(y.integer_value());
  auto zu = xu + yu;
  auto z = value::new_integer(nb::detail::memcpy_cast<std::int64_t>(zu));
  return z;
}

nb::value nb::operator*(value const& x, value const& y) {
  auto xu = nb::detail::memcpy_cast<std::uint64_t>(x.integer_value());
  auto yu = nb::detail::memcpy_cast<std::uint64_t>(y.integer_value());
  auto zu = xu * yu;
  auto z = value::new_integer(nb::detail::memcpy_cast<std::int64_t>(zu));
  return z;
}



nb::value nb::iterator(nb::value array) {
  auto result = value::new_array(2);
  result.set_array_element(0, std::move(array));
  result.set_array_element(1, value::new_integer(0));
  return result;
}

bool nb::iterator_next(nb::value& element, nb::value iterator) {
  auto array = iterator.array_element(0);
  auto index = iterator.array_element(1);
  if ((std::size_t)index.integer_value() < array.array_length()) {
    element = array.array_element(index.integer_value());
    iterator.set_array_element(1, value::new_integer(index.integer_value() + 1));
    return true;
  } else {
    return false;
  }
}
