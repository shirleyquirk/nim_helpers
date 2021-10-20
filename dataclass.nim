import macros,sugar,std/genasts,fusion/matching
{.experimental:"caseStmtMacros".}
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
  ## TODO: variants, 
  ## generics work in principle. more testing obvs
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
  
  #result = x.copyNimTree() #get rid of this too please
  
  x.assertMatch:
    TypeDef:
      @basename is PragmaExpr([ @name, .._])
      @genericParams
      @typedef is ObjectTy([
                             _,
                             _,
                             @identdefs# is RecList([ all IdentDefs([Postfix([_,@ids]) | @ids,.._]) ])
                           ])
  
  let outname = ident(name.strval & "Dataclass")
  #outname[0] = ident(outname[0].strval & "Dataclass")
  #outname[1] = newEmptyNode()
  basename[1].add(ident"inject")
  #[ type BaseNameDataClass = stmtlist:
      type BaseName[T]{.packed,inject.} = object
        x:T
      BaseName
  
  ]#
  var ids:seq[NimNode]
  for i in 0..<identdefs.len:
    for j in 0..<identdefs[i].len - 2: #last two are type and i think pragma?
      if identdefs[i][j].kind == nnkPostfix:
        identdefs[i][j] = identdefs[i][j][1] #?can we do this with matching?
      ids.add identdefs[i][j]
  let params = @[ident"auto"] & collect(newSeq,for c in identdefs.children: c)
  let assignments = (if genericParams.kind == nnkEmpty: @[name] else: @[nnkBracketExpr.newTree(@[outname] & collect(for p in genericParams:p[0]))]) & collect(for i in ids: nnkExprColonExpr.newTree(i,i))

  #[let procdef = newProc(ident("init" & basename[0].strval),
                        params,
                        nnkObjConstr.newTree( assignments )
                       )
]#let procdef = nnkProcDef.newTree(
    ident("init" & name.strval),
    newEmptyNode(),#term rewriting
    genericParams,
    nnkFormalParams.newTree(params),
    newEmptyNode(),#pragmas
    newEmptyNode(),#reserved
    nnkObjConstr.newTree(assignments)
  )
  let templatebody = nnkStmtList.newTree(
    nnkTypeSection.newTree(nnkTypeDef.newTree(basename,genericParams,typedef)),#genAst(basename,typedef,procdef,name):#=basename[0]):
    procdef,
    quote do:
      Bar
  )
  #[  type
      basename[genericParams] = typedef
    procdef
    name
  ]#
  #echo templatebody.treeRepr
  result = nnkTypeDef.newTree(
    outname,
    newEmptyNode(),#genericParams,
    newCall(ident"helper",[templatebody])
  )
  echo result.repr
template ugh():type =
  type Baz[T]{.inject.} = object
    x:T
  proc initBaz[T](x:T):auto = Baz[T](x:x)
  Baz
template plugh():type =
  type Baf{.inject.} = object
   x:int
  Baf
type
  Foo = int
  Bar[T]{.dataclass.} = object
    x*: T
    y,z*,w: float
  BazDataclass = ugh()
  Boz = plugh()

let y = initBaz(3.5)
echo y,typeof(y)
var z:Baz[float]
echo z,typeof(z)
var x:Bar[float]
let w = initBar(3,1.0,2.0,3.0)
echo w
echo x.type,',',x
