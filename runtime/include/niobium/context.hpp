#pragma once

#include <chrono>
#include <iostream>
#include <vector>

namespace nb {
  class context {
  public:
    struct call_info {
      call_info(std::chrono::steady_clock::time_point);

      std::chrono::steady_clock::time_point timestamp;
    };

    std::ostream* execution_log;
    std::vector<call_info> call_infos;
  };
}
