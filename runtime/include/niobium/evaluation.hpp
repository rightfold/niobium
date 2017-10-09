#pragma once

#include <niobium/interface.hpp>
#include <niobium/query.hpp>
#include <niobium/value.hpp>

#include <streambuf>
#include <utility>



#define NB_ADD(result, x, y)                    \
  result = (x) + (y)

#define NB_APPLY(result, applyee, ...)          \
  result = (applyee)(__VA_ARGS__)

#define NB_CALL(callee, ...)                    \
  (callee)(context, ##__VA_ARGS__)

#define NB_EXECUTE_QUERY(result, query_, query_length, ...)     \
  do {                                                          \
    nb::value arguments[] = { __VA_ARGS__ };                    \
    nb::query::execute(                                         \
      context,                                                  \
      result,                                                   \
      query_, query_length,                                     \
      arguments, sizeof(arguments) / sizeof(arguments[0])       \
    );                                                          \
  } while (0)

#define NB_EXECUTE_QUERY_NEXT(result, ...)              \
  do {                                                  \
    nb::value* columns[] = { __VA_ARGS__ };             \
    nb::query::next(                                    \
      result,                                           \
      columns, sizeof(columns) / sizeof(columns[0])     \
    );                                                  \
  } while (0)

#define NB_FOR_EACH(element, iterable, body)            \
  do {                                                  \
    nb::value element;                                  \
    auto iterator = nb::iterator(iterable);             \
    while (nb::iterator_next(element, iterator)) {      \
      body;                                             \
    }                                                   \
  } while (0)

#define NB_MULTIPLY(result, x, y)               \
  result = (x) * (y)

namespace nb {
  namespace evaluation {
    template<typename F>
    value report_interface(F body) {
      class implementation : public nb::interface::report {
      public:
        implementation(F body)
          : body(std::move(body)) {
        }

        void get(nb::context& context,
                 std::streambuf& using_,
                 std::streambuf& giving) const override {
          body(context, using_, giving);
        }

      private:
        F body;
      };
      return nb::value::new_report<implementation>(body);
    }
  }
}
