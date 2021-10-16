import macros,sugar
template helper(body:untyped):untyped =
  body
macro dataclass*(x:untyped):untyped =
  ## dataclass macro replaces a typedef with itself, plus some
  ## helper procs. for now, just an initFoo() proc
  ## input is, e.g.
  ## type
  ##   Foo {. pragmas.. .} = object
  ##     x*:int
  ##     y,z*,w: float
  ## TODO: variants
  ##
  ## output is:
  ## template anonymous:type =
  ##   type Foo {. pragmas..,inject .} = object
  ##     x*:int
  ##     y,z*,w: float
  ##   proc initFoo(x:int,y,z,w:float):auto = Foo(x:x,y:y,z:z,w:w)
  ##   Foo
  ## type
  ##   FooDataClass {. pragmas.. .} = anonymous()
  
  result = x.copyNimTree()
  
  x.expectKind(nnkTypeDef)

  let basename = x[0]
  
  let outname = basename.copyNimTree()
  outname[0] = ident(outname[0].strval & "Dataclass")
  #type BaseNameDataclass{. pragmalist.. .} = helper(templatebody)

  basename[1].add(ident"inject")
  # type BaseName{.inject.} = object

  x[2].expectKind(nnkObjectTy)
  let typedef = x[2]
  let identdefs = typedef[2].copyNimTree()
  var ids: seq[NimNode]
  
  #get identdef, ident seqs, stripped of `*` postfix
  for i in 0..<identdefs.len:
    for j in 0..<identdefs[i].len - 2: #last two are type and i think pragma?
      if identdefs[i][j].kind == nnkPostfix:
        identdefs[i][j] = identdefs[i][j][1]
      ids.add identdefs[i][j]
   
  let params = @[ident"auto"] & collect(newSeq,for c in identdefs.children: c)
  let assignments = @[basename[0]] & collect(newSeq, for i in ids: nnkExprColonExpr.newTree(i,i))

  let procdef = newProc(ident("init" & basename[0].strval),
                        params,
                        nnkObjConstr.newTree( assignments )
                       )

  let templatebody = nnkStmtList.newTree(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        basename,
        newEmptyNode(),
        typedef
      )
    ),
    procdef,
    basename[0]
  )
  result[0] = outname
  result[2] = newCall(ident"helper",[templatebody])

type
  Foo = int
  Bar{.dataclass.} = object
    x*: int
    y,z*,w: float
  Baz = float



var x:Bar
let y = initBar(3,1.0,2.0,3.0)
echo y
echo x.type,',',x.x
