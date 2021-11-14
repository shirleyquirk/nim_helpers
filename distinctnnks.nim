import macros,std/genasts
import fusion/astdsl #tiny bit prettier

macro distinctNimNode(names:static varargs[NimNodeKind]) =
  result = nnkStmtList.newNimNode
  for name in names:
    let tmp = genast(name = ident(($name)[3..^1])):
      type name = distinct NimNode
      #converter `to name`(x:NimNode):name =
      #  x.expectKind(`nnk name`)
      #  x.name
      #converter toNimNode(x:name):NimNode = x.NimNode
      discard
    for x in tmp:
      result.add x
  #echo result.treeRepr
#[type
  Infix = distinct NimNode
converter toInfix(x:NimNode):Infix = 
  x.expectKind(nnkInfix)
  x.Infix
converter toNimNode(x:Infix):NimNode = x.NimNode
]#
#expandMacros:
  
#[
nnkNone, nnkEmpty, nnkIdent, nnkSym, nnkType, nnkCharLit, nnkIntLit,
  nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit, nnkUIntLit, nnkUInt8Lit,
  nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit, nnkFloatLit, nnkFloat32Lit,
  nnkFloat64Lit, nnkFloat128Lit, nnkStrLit, nnkRStrLit, nnkTripleStrLit,
  nnkNilLit, nnkComesFrom, nnkDotCall, nnkCommand, nnkCall, nnkCallStrLit,
  nnkInfix, nnkPrefix, nnkPostfix, nnkHiddenCallConv, nnkExprEqExpr,
  nnkExprColonExpr, nnkIdentDefs, nnkVarTuple, nnkPar, nnkObjConstr, nnkCurly,
  nnkCurlyExpr, nnkBracket, nnkBracketExpr, nnkPragmaExpr, nnkRange, nnkDotExpr,
  nnkCheckedFieldExpr, nnkDerefExpr, nnkIfExpr, nnkElifExpr, nnkElseExpr,
  nnkLambda, nnkDo, nnkAccQuoted, nnkTableConstr, nnkBind, nnkClosedSymChoice,
  nnkOpenSymChoice, nnkHiddenStdConv, nnkHiddenSubConv, nnkConv, nnkCast,
  nnkStaticExpr, nnkAddr, nnkHiddenAddr, nnkHiddenDeref, nnkObjDownConv,
  nnkObjUpConv, nnkChckRangeF, nnkChckRange64, nnkChckRange, nnkStringToCString,
  nnkCStringToString, nnkAsgn, nnkFastAsgn, nnkGenericParams, nnkFormalParams,
  nnkOfInherit, nnkImportAs, nnkProcDef, nnkMethodDef, nnkConverterDef,
  nnkMacroDef, nnkTemplateDef, nnkIteratorDef, nnkOfBranch, nnkElifBranch,
  nnkExceptBranch, nnkElse, nnkAsmStmt, nnkPragma, nnkPragmaBlock, nnkIfStmt,
  nnkWhenStmt, nnkForStmt, nnkParForStmt, nnkWhileStmt, nnkCaseStmt,
  nnkTypeSection, nnkVarSection, nnkLetSection, nnkConstSection, nnkConstDef,
  nnkTypeDef, nnkYieldStmt, nnkDefer, nnkTryStmt, nnkFinally, nnkRaiseStmt,
  nnkReturnStmt, nnkBreakStmt, nnkContinueStmt, nnkBlockStmt, nnkStaticStmt,
  nnkDiscardStmt, nnkStmtList, nnkImportStmt, nnkImportExceptStmt,
  nnkExportStmt, nnkExportExceptStmt, nnkFromStmt, nnkIncludeStmt, nnkBindStmt,
  nnkMixinStmt, nnkUsingStmt, nnkCommentStmt, nnkStmtListExpr, nnkBlockExpr,
  nnkStmtListType, nnkBlockType, nnkWith, nnkWithout, nnkTypeOfExpr,
  nnkObjectTy, nnkTupleTy, nnkTupleClassTy, nnkTypeClassTy, nnkStaticTy,
  nnkRecList, nnkRecCase, nnkRecWhen, nnkRefTy, nnkPtrTy, nnkVarTy, nnkConstTy,
  nnkMutableTy, nnkDistinctTy, nnkProcTy, nnkIteratorTy, nnkSharedTy, nnkEnumTy,
  nnkEnumFieldDef, nnkArgList, nnkPattern, nnkHiddenTryStmt, nnkClosure,
  nnkGotoState, nnkState, nnkBreakState, nnkFuncDef, nnkTupleConstr, nnkError
]#

import std/enumutils,sequtils
distinctNimNode(toSeq(NimNodeKind))

macro dispatchImpl(n:NimNode,b:untyped):untyped =
    buildAst(caseStmt):
        genAst(n):
            n.kind
        for k in NimNodeKind:
            ofBranch ident($k),
              genast(b, n,it = ident(n.strval), name = ident ($k)[3..^1]) do:
                var it{.inject.} = name(n)
                b
template dispatch(n:NimNode,body:untyped):untyped =
    dispatchImpl(n,body)


when isMainModule:
  proc test[T](x:T) = echo "unimplemented ", T
  proc test(x:Call)
  proc test(x:StmtList) =
      echo "test StmtList"
      for j in x.NimNode:
          dispatch(j):
            test(j)
  proc test(x:Call) =
      echo "test Call"
      for i in x.NimNode:
          i.dispatch:
              test(i)
  macro foo(x:untyped) = 
      #echo x.kind
      x.dispatch: #TODO: dispatch(x,y...)
         test(x)
      #echo fooImpl(x).treeRepr
  proc bar(y:int):int = y + 1
  foo:
    bar(7)
