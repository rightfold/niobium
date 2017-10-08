#pragma once

#include <niobium/context.hpp>
#include <niobium/value.hpp>

#include <streambuf>

namespace nb {
  namespace interface {
    class report {
    public:
      virtual ~report() = 0;

      virtual void get(context&, std::streambuf&, std::streambuf&) const = 0;
    };

    value read_int(std::streambuf&);
    void write_int(std::streambuf&, value&);
  }
}
