import macros,strutils,tables

macro `~`(op:typed,body:untyped):untyped =
  echo op.treeRepr
  #can't use colons inside, so for a : b in tables, named tuples, object constructors instead use a = b
  var mappings = {nnkAsgn:nnkExprColonExpr}.toTable
  case op.kind
  of nnkCurly:#Table or set
    if body[0].kind == nnkAsgn:
      result = nnkTableConstr.newNimNode
    else:#set
      result = op.copyNimTree()
  of nnkCall:#assumes varargs
      result = op.copyNimTree
      result.del(result.len - 1)#remove empty varargs.
  of nnkSym:#function call
    result = newCall(ident(op.strval))
  else:
    result = op.copyNimTree()
  
  var dest = result
  
  #special case seq constructor
  if op.kind == nnkPrefix and op[0].strval == "@":
    result = nnkPrefix.newTree(ident"@",nnkBracket.newNimNode)
    dest = result[1]
  
  for stmt in body:
    dest.add if stmt.kind in mappings:
      var res = mappings[stmt.kind].newNimNode
      stmt.copyChildrenTo(res)
      res
    else:
      stmt


when isMainModule:

type Foo = object
  a: int
  b: set[char]
proc `$`(f:Foo):string = "Foo" & system.`$`(f)

proc weighted_avg(weight:float,data:varargs[float]):float =
  for d in data:
    result += d
  result = result * weight / data.len.float

proc foo(x,y,z:int):int = x + y * z

let nested = toTable: ~{}:  #table
    "thing" = ~():  #anonymous tuple
      "foo"
      ~[]:  #array
        ~Foo(): #object constructor
          a = int: ~weighted_avg(0.5): #call to varargs proc that is then passed to int()
            11.0
            13.5
            19.3
          b = {'x'}
      ~ @[]: #seq constructor
        3.0
        3.1
        3.2
    "wrong" = ~(): #named tuple
      alpha = "bar"
      beta = ~[]:
        ~Foo():
          a = ~foo: #call to normal proc
            3
            4
            2
          b = ~{}: #set constructor
            'y'
            'z'
      gamma = @[]

echo nested
