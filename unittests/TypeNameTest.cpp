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

TEST(TypeNameTest, Bool) {
  LLVMContext ctx;
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt1Ty(ctx)), "bool");
}

TEST(TypeNameTest, SimpleNumeric) {
  LLVMContext ctx;
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt8Ty(ctx)), "uint8_t");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt16Ty(ctx)), "uint16_t");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt32Ty(ctx)), "uint32_t");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt64Ty(ctx)), "uint64_t");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getInt128Ty(ctx)),
            "uint128_t");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getFloatTy(ctx)), "float");
  EXPECT_EQ(CWriterTestHelper::getTypeName(Type::getDoubleTy(ctx)), "double");
}

TEST(TypeNameTest, BoolPtr) {
  LLVMContext ctx;
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt1Ty(ctx)->getPointerTo()),
      "bool*");
}

TEST(TypeNameTest, SimpleNumericPtr) {
  LLVMContext ctx;
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt8Ty(ctx)->getPointerTo()),
      "uint8_t*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt16Ty(ctx)->getPointerTo()),
      "uint16_t*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt32Ty(ctx)->getPointerTo()),
      "uint32_t*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt64Ty(ctx)->getPointerTo()),
      "uint64_t*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getInt128Ty(ctx)->getPointerTo()),
      "uint128_t*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getFloatTy(ctx)->getPointerTo()),
      "float*");
  EXPECT_EQ(
      CWriterTestHelper::getTypeName(Type::getDoubleTy(ctx)->getPointerTo()),
      "double*");
}

} // namespace llvm_cbe
