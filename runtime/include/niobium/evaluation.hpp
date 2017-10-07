#pragma once

#include <niobium/query.hpp>
#include <niobium/value.hpp>



#define NB_CALL(callee, ...)                    \
  (callee)(context, ##__VA_ARGS__)

#define NB_EXECUTE_QUERY(result, query_, query_length, ...)             \
  do {                                                                  \
    nb::value arguments[] = { __VA_ARGS__ };                            \
    nb::query::execute(                                                 \
      context,                                                          \
      result,                                                           \
      query_, query_length,                                             \
      arguments, sizeof(arguments) / sizeof(arguments[0])               \
    );                                                                  \
  } while (0)

#define NB_EXECUTE_QUERY_NEXT(result, ...)                      \
  do {                                                          \
    nb::value* columns[] = { __VA_ARGS__ };                     \
    nb::query::next(                                            \
      result,                                                   \
      columns, sizeof(columns) / sizeof(columns[0])             \
    );                                                          \
  } while (0)

#define NB_FOR_EACH(element, iterable, body)                    \
  do {                                                          \
    nb::value element;                                          \
    auto iterator = nb::iterator(iterable);                     \
    while (nb::iterator_next(element, iterator)) {              \
      body;                                                     \
    }                                                           \
  } while (0)



#define NB_APPLY(result, applyee, ...)          \
  result = (applyee)(__VA_ARGS__);
