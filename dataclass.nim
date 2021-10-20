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
  ## TODO: variants, generics
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
  
  x.assertMatch:
    TypeDef:
      @basename is PragmaExpr([ @name, .._])
      @genericParams
      @typedef is ObjectTy([
                             _,
                             _,
                             @identdefs# is RecList([ all IdentDefs([Postfix([_,@ids]) | @ids,.._]) ]) #this is NOT how to match ids, you only get the first
                           ])
  
  let outname = gensym(nskType)#ident(name.strval & "Dataclass")
  basename[1].add(ident"inject")

  var ids:seq[NimNode]
  for i in 0..<identdefs.len:
    for j in 0..<identdefs[i].len - 2: #last two are type and i think pragma?
      if identdefs[i][j].kind == nnkPostfix:
        identdefs[i][j] = identdefs[i][j][1] #?can we do this with matching?
      ids.add identdefs[i][j]

  let params = collect(`@`([ident"auto"]),for c in identdefs.children: c)
  let assignments = collect(`@`(
      if genericParams.kind == nnkEmpty:
        [name]
      else:
        [ nnkBracketExpr.newTree( @[name] & genericParams[0][0..^3]  ) ]
      ),
      for i in ids: nnkExprColonExpr.newTree(i,i)
  )
  
  #proc initFoo[T,U](x:T,y:U,z,w:int):auto = Foo[T,U](x:x,y:y,z:z,w:w)
  let procdef = nnkProcDef.newTree(
    ident("init" & name.strval),
    newEmptyNode(),#term rewriting
    genericParams,
    nnkFormalParams.newTree(params),
    newEmptyNode(),#pragmas
    newEmptyNode(),#reserved
    nnkObjConstr.newTree(assignments)
  )
  
  #type Foo[T,U]{.packed,inject.} = object
  #  x:T
  #  y:U
  #  z,w:int
  #proc initFoo...
  #Foo
  let templatebody = nnkStmtList.newTree(
    nnkTypeSection.newTree(nnkTypeDef.newTree(basename,genericParams,typedef)),
    procdef,
    name
  )
  
  #:tmp_1234 = helper(templatebody) 
  result = nnkTypeDef.newTree(
    outname,
    newEmptyNode(),#genericParams,
    newCall(ident"helper",[templatebody])
  )
  
  echo result.repr
  
######tests#######  
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
  Bar[T,U]{.dataclass.} = object
    x*: T
    y,z*: float
    v: U
  BazDataclass = ugh()
  Qux{.dataclass.} = object
    x,y,z:int


var x:Bar[float,string]
let y = initBar(3,1.0,2.0,3.0)
echo y,typeof(y)
echo x.type,',',x

let z = initQux(5,5,5)
echo z,typeof(z)
let w = Qux(x:5,y:5,z:5)
var v:Qux
echo w,v
