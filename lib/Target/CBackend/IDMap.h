#include "llvm/ADT/DenseMap.h"

namespace llvm_cbe {
using namespace llvm;

template <class KeyT> class IDMap {
  DenseMap<KeyT, unsigned> Map;
  unsigned NextValue = 0;

public:
  IDMap() {}

  unsigned has(KeyT key) { return Map.count(key) > 0; }

  unsigned getOrInsert(KeyT Key) {
    unsigned &i = Map[Key];
    if (i == 0) {
      i = ++NextValue;
    }

    return i;
  }

  void clear() {
    Map.clear();
    NextValue = 0;
  }

  typename DenseMap<KeyT, unsigned>::iterator begin() { return Map.begin(); }

  typename DenseMap<KeyT, unsigned>::iterator end() { return Map.end(); }

  typename DenseMap<KeyT, unsigned>::iterator find(KeyT Key) {
    return Map.find(Key);
  }
};

} // namespace llvm_cbe
