block:
  import macros
  proc toDistinct(x:static NimNode):auto =
    when x.kind == nnkStmtList: x.StmtList
    elif x.kind == nnkCall: x.Call
    else: x
  discard toDistinct(nnkStmtList.newNimNode())
  
  
block:
  import macros
  type
  SafeNimNodeObj = object
    case kind: NimNodeKind
    of nnkNone, nnkEmpty, nnkNilLit:
      discard                        # node contains no additional fields
    of nnkCharLit..nnkUInt64Lit:
      intVal: BiggestInt             # the int literal
    of nnkFloatLit..nnkFloat64Lit:
      floatVal: BiggestFloat         # the float literal
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
      strVal: string                 # the string literal
    else:
      sons: seq[SafeNimNode]             # the node's sons (or children)
  SafeNimNode = ref SafeNimNodeObj

  converter toSafeNimNode(x:NimNode):SafeNimNode =
    result = SafeNimNode(kind: x.kind)
    case x.kind:
      of nnkNone, nnkEmpty, nnkNilLit:
        discard
      of nnkCharLit..nnkUInt64Lit:
        result.intVal = x.intVal
      of nnkFloatLit..nnkFloat64Lit:
        result.floatVal = x.floatVal
      of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
        result.strVal = x.strVal
      else:
        for s in x:
          result.sons.add toSafeNimNode(s)
  proc toDistinct(x:static SafeNimNode):auto =
  when x.kind == nnkStmtList: 1#x.StmtList
  elif x.kind == nnkCall: 2#x.Call
  else: 3
  echo toDistinct(nnkStmtList.newNimNode()) # toDistinct(nnkStmtList.newNimNode().toSafeNimNode) doesn't crash
