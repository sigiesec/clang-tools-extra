// RUN: %check_clang_tidy %s btc-modernize-use-auto %t -- -- -std=c++11 -frtti

class MyType {
public:
  MyType() = default;
  explicit MyType(int) {}
};

class MyTypeWithDefaultArguments {
public:
  MyTypeWithDefaultArguments(int x = 0);
};

template <typename T>
class MyTemplate {
  void member_function() {
    MyType ac1;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: use auto when declaring a default-initialized variable
    // CHECK-FIXES: auto ac1 = MyType{};

    T t;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: use auto when declaring a default-initialized variable
    // CHECK-FIXES: auto t = T{};
  }
};

template class MyTemplate<int>;

using MyTypeAlias = MyType;
using MyTypePtrAlias = MyType *;
using MyTypeConstAlias = const MyType;

class MyDerivedType : public MyType {};

void auto_default_initialized() {
  const MyType ac1;
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto ac1 = MyType{};

  const MyType ac2 = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto ac2 = MyType{};

  MyType ax = MyType{};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto ax = MyType{};

  MyType ay = MyType();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto ay = MyType{};

  MyType a;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto a = MyType{};

  MyType az = {};
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto az = MyType{};

  MyTypeAlias aAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto aAlias = MyTypeAlias{};

  MyTypePtrAlias aPtrAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto aPtrAlias = MyTypePtrAlias{};

  MyTypeConstAlias aConstAlias;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto aConstAlias = MyTypeConstAlias{};

  const MyTypeConstAlias aConstAliasExplicitlyConst;
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: const auto aConstAliasExplicitlyConst = MyTypeConstAlias{};

  // FIXME make a warning but no autofix in this case?
  // do not provide an autofix if the types do not match literally
  MyTypeAlias aAliasMismatch = MyType{};

  // FIXME can this somehow use auto?
  MyType *aPtr;

  // FIXME also apply to multiple variables declared in one statement?
  MyType a1, a2;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // noCHECK-FIXES: auto a1 = MyType{}; auto a2 = MyType{};

  MyTypeWithDefaultArguments xDefault;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto xDefault = MyTypeWithDefaultArguments{};

  int b;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // CHECK-FIXES: auto b = int{};

  // FIXME implement this case
  int bArray[1];
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto when declaring a default-initialized variable
  // noCHECK-FIXES: auto bArray = int[1]{};

  // auto uShort = unsigned short{}; would not be valid because unsigned short is not a "single-word type name"
  // FIXME create a warning but no autofix?
  unsigned short uShort;

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

MyType MakeMyType();
MyType MakeMyType(int);
MyTypeAlias MakeMyTypeAlias();

void auto_initialized_from_single_function_call() {
  MyType a = MakeMyType();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a = MakeMyType();

  MyType a1(MakeMyType());
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a1 = MakeMyType();

  MyType a2 = MakeMyType(42);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a2 = MakeMyType(42);

  MyTypeAlias a3 = MakeMyTypeAlias();
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // CHECK-FIXES: auto a3 = MakeMyTypeAlias();

  // FIXME create a warning but no autofix in this case? or create an autofix for auto b = MyTypeAlias{MakeMyType()};
  // do not fix if the types do not match
  MyType a4 = MakeMyTypeAlias();

  // FIXME create a warning but no autofix in this case? or create an autofix for auto b = MyTypeAlias{MakeMyType()};
  // do not fix if the types do not match
  MyTypeAlias b = MakeMyType();
}

void auto_initialized_from_other_expression() {
  // FIXME implement this case
  int a = 4 + 5;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto a = 4 + 5;

  // FIXME implement this case
  long b = 4 + 5;
  // noCHECK-MESSAGES: :[[@LINE-1]]:3: warning: use auto
  // noCHECK-FIXES: auto b = long{4 + 5};
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
