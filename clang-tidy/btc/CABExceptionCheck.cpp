//===--- CABExceptionCheck.cpp - clang-tidy----------------===//
//    This Check is intended to detect the places in code
//    where BTC::Commons::Core::Exception  are
//    wrongly used.
//===-----------------------------------------------------===//

#include "CABExceptionCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace btc {

void CABExceptionCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      cxxCatchStmt(
          has(varDecl(hasType(pointsTo(cxxRecordDecl(isSameOrDerivedFrom(
              hasName("BTC::Commons::Core::Exception"))))))))
          .bind(CABExceptionMatch),
      this);
  Finder->addMatcher(cxxThrowExpr().bind(CxxThrowExprMatch), this);
}

void CABExceptionCheck::check(const MatchFinder::MatchResult &Result) {
  // catchStmt(has(varDecl(hasType(references(isConstQualified()))))) müsste
  // auch funktionieren
  const auto *MatchedStmt =
      Result.Nodes.getNodeAs<CXXCatchStmt>(CABExceptionMatch);
  if (MatchedStmt) {
    const auto *ExceptionDecl = MatchedStmt->getExceptionDecl();
    if (ExceptionDecl) { // no catch all
      const auto CaughtType = MatchedStmt->getCaughtType();
      const auto *BaseType = CaughtType.getTypePtr();
      if (BaseType->isPointerType()) {
        const auto *Decl = BaseType->getPointeeCXXRecordDecl();
        if (Decl) {
          if (BaseType->getPointeeType().isConstQualified()) {
            if (!mExceptionsAsPointers) {
              diag(MatchedStmt->getLocStart(),
                   "cab exception " + CaughtType.getAsString() +
                       " is caught as a pointer. It must be caught as const "
                       "reference.",
                   clang::DiagnosticIDs::Warning);
            }
          } else {
            if (mExceptionsAsPointers) {
              diag(MatchedStmt->getLocStart(),
                   "cab exception " + CaughtType.getAsString() +
                       " must be caught const qualified.",
                   clang::DiagnosticIDs::Warning);

            } else {
              diag(MatchedStmt->getLocStart(),
                   "cab exception " + CaughtType.getAsString() +
                       " must be caught as const reference.",
                   clang::DiagnosticIDs::Warning);
            }
          }
        }
      }
    }
  } else {
    const auto *CxxThrowExpr =
        Result.Nodes.getNodeAs<CXXThrowExpr>(CxxThrowExprMatch);
    if (CxxThrowExpr) {
      auto *subExpr = CxxThrowExpr->getSubExpr();
      if (!subExpr)
        return;
      auto qualType = subExpr->getType();
      if (qualType->isPointerType()) {
        // The code is throwing a pointer.
        if (!mExceptionsAsPointers) {
          diag(CxxThrowExpr->getLocStart(),
               "cab exception must be thrown as value",
               clang::DiagnosticIDs::Warning);
        }
      }
    } 
  }
}

void CABExceptionCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, CABExceptionAsPointerParamName, mExceptionsAsPointers);
}

} // namespace btc
} // namespace tidy
} // namespace clang
