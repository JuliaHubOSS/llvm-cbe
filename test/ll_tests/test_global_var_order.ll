@test_array = constant { [1 x i8*] } { [1 x i8*] [i8* @defined_after_test_array] }
@defined_after_test_array = constant i8* null

define dso_local i32 @main() #0 {
  ret i32 6
}
