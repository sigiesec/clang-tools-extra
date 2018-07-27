//===--- CallingSfehlerWithStopOnErrorCheck.h - clang-tidy-------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_CALLINGSFEHLERWITHSTOPONERRORCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_CALLINGSFEHLERWITHSTOPONERRORCHECK_H

#include "../ClangTidy.h"

namespace clang {
namespace tidy {
namespace btc {

/// Finds function SFehler calls with STOPONERROR as parameter.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/btc-calling-SFehler-with-stop-on-error.html
class CallingSfehlerWithStopOnErrorCheck : public ClangTidyCheck {
 public:
  CallingSfehlerWithStopOnErrorCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

}  // namespace btc
}  // namespace tidy
}  // namespace clang

#endif  // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BTC_CALLINGSFEHLERWITHSTOPONERRORCHECK_H
