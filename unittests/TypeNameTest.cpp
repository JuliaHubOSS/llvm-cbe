#include "../lib/Target/CBackend/CBackend.h"
#include "gtest/gtest.h"

using namespace llvm;

namespace llvm_cbe {

class CWriterTestHelper {
public:
  static std::string getTypeName(Type *ty) {
    raw_null_ostream NullStm;
    CWriter Writer(NullStm);

    std::string Str;
    raw_string_ostream StrStm(Str);
    Writer.printTypeName(StrStm, ty);
    StrStm.flush();

    return Str;
  }
};

TEST(TypeNameTest, Void) {
  LLVMContext ctx;
  Type *ty = Type::getVoidTy(ctx);
  EXPECT_EQ(CWriterTestHelper::getTypeName(ty), "void");
}

} // namespace llvm_cbe
