#include <niobium/context.hpp>
#include <niobium/execution_log.hpp>

#include <chrono>
#include <cstddef>

namespace {
  void log_enter(nb::context& context, char const* procedure) {
    *context.execution_log
      << std::string(context.call_infos.size(), ' ')
      << "<call procedure=\"" << procedure << "\">\n";
  }

  void record_enter(nb::context& context) {
    context.call_infos.emplace_back(std::chrono::steady_clock::time_point());
    context.call_infos.back().timestamp = std::chrono::steady_clock::now();
  }

  std::chrono::steady_clock::duration record_leave(nb::context& context) {
    auto after = std::chrono::steady_clock::now();
    auto before = context.call_infos.back().timestamp;
    context.call_infos.pop_back();
    return after - before;
  }

  void log_leave(nb::context& context, std::chrono::steady_clock::duration duration) {
    auto duration_ns = std::chrono::nanoseconds(duration).count();
    *context.execution_log
      << std::string(context.call_infos.size() + 1, ' ')
      << "<duration>" << duration_ns << "</duration>\n"
      << std::string(context.call_infos.size(), ' ')
      << "</call>\n";
  }
}

void nb::execution_log::enter(context& context, char const* procedure) {
  log_enter(context, procedure);
  record_enter(context);
}

void nb::execution_log::leave(context& context) {
  auto duration = record_leave(context);
  log_leave(context, duration);
}
