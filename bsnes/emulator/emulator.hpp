#pragma once

#include <libco/libco.h>

#include <nall/platform.hpp>
#include <nall/adaptive-array.hpp>
#include <nall/any.hpp>
#include <nall/chrono.hpp>
#include <nall/dl.hpp>
#include <nall/endian.hpp>
#include <nall/image.hpp>
#include <nall/literals.hpp>
#include <nall/random.hpp>
#include <nall/serializer.hpp>
#include <nall/shared-pointer.hpp>
#include <nall/string.hpp>
#include <nall/traits.hpp>
#include <nall/unique-pointer.hpp>
#include <nall/vector.hpp>
#include <nall/vfs.hpp>
#include <nall/hash/crc32.hpp>
#include <nall/hash/sha256.hpp>
using namespace nall;

#include <emulator/types.hpp>
#include <emulator/memory/readable.hpp>
#include <emulator/memory/writable.hpp>
#include <emulator/audio/audio.hpp>

namespace Emulator {
  static const string Name      = "bsnes";
  static const string Version   = "115";
  static const string Copyright = "byuu et al";
  static const string License   = "GPLv3 or later";
  static const string Website   = "https://byuu.org/bsnes/";

  //incremented only when serialization format changes
  static const string SerializerVersion = "115";

  namespace Constants {
    namespace Colorburst {
      static constexpr double NTSC = 315.0 / 88.0 * 1'000'000.0;
      static constexpr double PAL  = 283.75 * 15'625.0 + 25.0;
    }
  }

  //nall/vfs shorthand constants for open(), load()
  namespace File {
    static const auto Read  = vfs::file::mode::read;
    static const auto Write = vfs::file::mode::write;
    static const auto Optional = false;
    static const auto Required = true;
  };
}

//debugging function hook:
template<typename T> struct hook;
template<typename R, typename... P> struct hook<auto (P...) -> R> {
  function<auto (P...) -> R> callback;

  auto operator()(P... p) const -> R {
    if(callback) return callback(forward<P>(p)...);
    return R();
  }

  hook() {}
  hook(const hook& hook) { callback = hook.callback; }
  hook(void* function) { callback = function; }
  hook(auto (*function)(P...) -> R) { callback = function; }
  template<typename C> hook(auto (C::*function)(P...) -> R, C* object) { callback = {function, object}; }
  template<typename C> hook(auto (C::*function)(P...) const -> R, C* object) { callback = {function, object}; }
  template<typename L> hook(const L& function) { callback = function; }

  auto operator=(const hook& source) -> hook& { callback = source.callback; return *this; }
};

#include "platform.hpp"
#include "interface.hpp"
#include "game.hpp"
