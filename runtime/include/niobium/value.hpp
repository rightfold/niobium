#pragma once

#include <boost/variant.hpp>

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace nb {
  namespace interface {
    class report;
  }

  class value {
  public:
    static value new_integer(std::int64_t);
    static value new_array(std::size_t);
    static value new_string(char const*, std::size_t);
    template<typename T, typename... Ts>
    static value new_report(Ts&&...);

    std::int64_t integer_value() const;

    std::size_t array_length() const;
    value array_element(std::size_t) const;
    void set_array_element(std::size_t, value);

    std::size_t string_length() const;
    char *string_bytes() const;

    interface::report const& report_value() const;

  private:
    using integer_type = std::int64_t;
    using array_type = std::shared_ptr<std::vector<value>>;
    using string_type = std::shared_ptr<std::string>;
    using report_type = std::shared_ptr<interface::report>;

    boost::variant<integer_type, array_type, string_type, report_type> variant;
  };

  template<typename T, typename... Ts>
  value value::new_report(Ts&&... arguments) {
    auto pointer = std::make_shared<T>(std::forward<Ts>(arguments)...);
    value result;
    result.variant = static_cast<value::report_type>(pointer);
    return result;
  }

  value operator+(value const&, value const&);
  value operator*(value const&, value const&);

  value iterator(value);
  bool iterator_next(value&, value);
}
