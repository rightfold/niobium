#pragma once

#include <niobium/context.hpp>

namespace nb {
  namespace execution_log {
    void enter(context&, char const*);
    void leave(context&);
  }
}
