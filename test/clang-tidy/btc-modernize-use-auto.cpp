// RUN: %check_clang_tidy %s btc-modernize-use-auto %t -- -- -std=c++11 -frtti

class MyType {
public:
  MyType() = default;
  explicit MyType(int) {}
};

class MyDerivedType : public MyType {};

void auto_default_initialized() {
  MyType ax = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto ax = MyType{};

  MyType ay = MyType();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto ay = MyType{};

  MyType a;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a = MyType{};

  MyType az = {};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto az = MyType{};

  // FIXME use auto* in this case
  MyType *aPtr;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto aPtr = MyType*{};

  // FIXME also apply to multiple variables declared in one statement?
  MyType a1, a2;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto a1 = MyType{}; auto a2 = MyType{};

  int b;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto b = int{};

  // Don't warn when 'auto' is already being used.
  auto aut = MyType{};
}

void auto_non_default_initialized() {
  // FIXME implement this case
  MyType a(42);
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto a = MyType{42};

  // FIXME implement this case
  MyType b{42};
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto b = MyType{42};

  // FIXME implement this case
  MyType c = MyType{42};
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto c = MyType{42};
}

// Don't warn for parameters, both for declarations and definitions.
void func(MyType myVar);
void func(MyType myVar) {}

// Don't warn for static/non-static data members.
class OtherType {
  static MyType staticDataMember;
  MyType nonStaticDataMember;
};

//   * Use {{}} for regular expressions.
// ex.: {{^}}void awesome_f();{{$}}
