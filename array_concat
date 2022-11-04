import sequtils,macros
const
  x = [1]
  y = [2,3]
  z = [4,5,6]

template arrayConcat[T](arrs: varargs[seq[T],`@`]): auto =
  const tmp = unpackVarargs(concat,arrs)
  var res: array[tmp.len,T]
  for i, x in tmp:
     res[i] = x
  res

const foo = arrayConcat(x,y,z)
assert foo == [1,2,3,4,5,6]
assert foo is array
