// RUN: %check_clang_tidy %s btc-modernize-use-auto %t -- -- -std=c++11 -frtti

class MyType {
public:
  MyType() = default;
  explicit MyType(int) {}
};

using MyTypeAlias = MyType;

class MyDerivedType : public MyType {};

void auto_default_initialized() {
  const MyType ac1;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: const auto ac1 = MyType{};

  const MyType ac2 = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: const auto ac2 = MyType{};

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

  MyTypeAlias aAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto aAlias = MyTypeAlias{};

  // FIXME can this somehow use auto?
  MyType *aPtr;

  // FIXME also apply to multiple variables declared in one statement?
  MyType a1, a2;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto a1 = MyType{}; auto a2 = MyType{};

  int b;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto b = int{};

  // FIXME implement this case
  int bArray[1];
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto bArray = int[1]{};

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

