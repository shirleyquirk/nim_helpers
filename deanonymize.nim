import macros

macro named(name:static string,f:proc):untyped =
  f.expectKind(nnkLambda)
  result = nnkProcDef.newNimNode()
  f.copyChildrenTo(result)
  let id = ident(name)
  result[0] = id
  result = quote do:
    block:
      `result`
      `id`

var callbacks: seq[proc()]

callbacks.add proc() =
  discard

callbacks.add proc (){.named:"foo".} =
  raise

for callback in callbacks:
  callback()
