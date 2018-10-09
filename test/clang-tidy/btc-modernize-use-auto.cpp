// RUN: %check_clang_tidy %s btc-modernize-use-auto %t \
// RUN:   -- -std=c++11 -frtti

class MyType {};

class MyDerivedType : public MyType {};

// FIXME: the replacement sometimes results in two consecutive spaces after
// the word 'auto' (due to the presence of spaces at both sides of '*').
void auto_new() {
  MyType a;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a = MyType{};

  // Don't warn when 'auto' is already being used.
  auto aut = MyType{};
}

// FIXME: Verify the applied fix.
//   * Make the CHECK patterns specific enough and try to make verified lines
//     unique to avoid incorrect matches.
//   * Use {{}} for regular expressions.
// CHECK-FIXES: {{^}}void awesome_f();{{$}}

