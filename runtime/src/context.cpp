#include <niobium/context.hpp>

#include <chrono>
#include <string>

nb::context::call_info::call_info(std::chrono::steady_clock::time_point timestamp)
  : timestamp(timestamp) {
}
