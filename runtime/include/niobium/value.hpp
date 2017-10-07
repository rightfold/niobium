#pragma once

#include <boost/variant.hpp>

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace nb {
  class value {
  public:
    static value new_integer(std::int64_t);
    static value new_array(std::size_t);
    static value new_string(char const*, std::size_t);

    std::int64_t integer_value() const;

    std::size_t array_length() const;
    value array_element(std::size_t) const;
    void set_array_element(std::size_t, value);

    std::size_t string_length() const;
    char *string_bytes() const;

  private:
    using integer_type = std::int64_t;
    using array_type = std::shared_ptr<std::vector<value>>;
    using string_type = std::shared_ptr<std::string>;

    boost::variant<integer_type, array_type, string_type> variant;
  };

  value iterator(value);
  bool iterator_next(value&, value);
}
