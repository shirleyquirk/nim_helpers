import tables,sugar
export tables

type
    DefaultTableBehaviour = enum
      UsesDefault,UsesConst,UsesLambda
    DefaultTable*[A,B;Behaviour:static DefaultTableBehaviour] = object
      super:Table[A,B]
      when Behaviour==UsesConst:
        missing: B
      elif Behaviour==UsesLambda:
        missing: proc(x:A):B
      else:
        discard
    NestedTable*[A,B] = DefaultTable[A,B,UsesDefault]
#all these overloads /could/ be done with one big typeclass
#but then sugar cant infer its argument types so we have this
proc toDefaultTable*[A,B](pairs: openArray[(A,B)],missing: B):DefaultTable[A,B,UsesConst] =
  result.super = pairs.toTable()
  result.missing = missing

proc toDefaultTable*[A,B](pairs: openArray[(A,B)]):DefaultTable[A,B,UsesDefault] =
    result.super = pairs.toTable()
    #result.missing = proc(x:A):B = default(B)

proc toDefaultTable*[A,B](pairs: openArray[(A,B)],missing: proc(x:A):B):DefaultTable[A,B,UsesLambda] =
    result.super = pairs.toTable()
    result.missing = missing

proc toDefaultTable*[A,B](pairs: openArray[(A,B)],missing:(void)->B):DefaultTable[A,B,UsesLambda] =
    result.super = pairs.toTable()
    result.missing = proc(x:A):B = missing()

proc initDefaultTable*[A,B](missing:proc():B):DefaultTable[A,B,UsesLambda] =
    result.super = initTable[A,B]()
    result.missing = proc(x:A):B = missing()
proc initDefaultTable*[A,B](missing:proc(x:A):B):DefaultTable[A,B,UsesLambda] =
    result.super = initTable[A,B]()
    result.missing = missing
proc initDefaultTable*[A,B](missing:B):DefaultTable[A,B,UsesConst] =
    result.super = initTable[A,B]()
    result.missing = missing
proc initDefaultTable*[A,B]():DefaultTable[A,B,UsesDefault] =
    result.super = initTable[A,B]()


#[template `[]`*[A,B;Behaviour:static DefaultTableBehaviour](t: var DefaultTable[A,B,Behaviour]; key: A):var B =
    when Behaviour==UsesDefault:
      t.super.mgetOrPut(key,default(B))
    elif Behaviour==UsesConst:
      t.super.mgetOrPut(key,t.missing)
    elif Behaviour==UsesLambda:
      if t.missing.isNil:
        t.missing = proc(x:A):B = default(B)
      t.super.mgetOrPut(key,t.missing(key))
]#
proc `[]`*[A,B](t:var DefaultTable[A,B,UsesDefault]; key:A): var B =
  t.super.mgetOrPut(key,default(B))
proc `[]`*[A,B](t:var DefaultTable[A,B,UsesConst]; key:A): var B =
  t.super.mgetOrPut(key,t.missing)
proc `[]`*[A,B](t:var DefaultTable[A,B,UsesLambda]; key:A): var B =
  t.super.mgetOrPut(key,t.missing(key))

#no immutable [] access

###rest inherit

#proc `[]=`*[A,B](t: var DefaultTable[A,B,UsesDefault]; key: A; val: sink B) = t.super.`[]=`(key,val)
#proc `[]=`*[A,B](t: var DefaultTable[A,B,UsesConst]; key: A; val: sink B) = t.super.`[]=`(key,val)
#proc `[]=`*[A,B](t: var DefaultTable[A,B,UsesLambda]; key: A; val: sink B) = t.super.`[]=`(key,val)
#AUGH
template `[]=`*[A,B;C:static DefaultTableBehaviour](t: var DefaultTable[A,B,C];key:A;val:sink B) = t.super.`[]=`(key,val)
#what's the point of generics

type DTB = static[DefaultTableBehaviour]
template hasKey*[A,B;C:DTB](t: DefaultTable[A,B,C]; key: A):bool = t.super.hasKey(key)
template contains*[A,B;C:DTB](t: DefaultTable[A,B,C]; key: A):bool = t.super.contains(key)
template hasKeyOrPut*[A,B;C:DTB](t: var DefaultTable[A,B,C],key:A,val:B):bool = t.super.hasKeyOrPut(key,val)
template getOrDefault*[A,B;C:DTB](t: DefaultTable[A,B,C],key:A):B = t.super.getOrDefault(key)
template getOrDefault*[A,B;C:DTB](t: DefaultTable[A,B,C],key:A; default: B):B = t.super.getOrDefault(key,default)
template mgetOrPut*[A,B;C:DTB](t: DefaultTable[A,B,C],key:A; val: B):var B = t.super.mgetOrPut(key,val)
template len*[A,B;C:DTB](t: DefaultTable[A,B,C]):int = t.super.len
#proc add*[A,B](t: var DefaultTable[A,B]; key: A) = t.super.add(key)
template del*[A,B,C](t: var DefaultTable[A,B,C]; key: A) = t.super.del(key)
template pop*[A,B,C](t: var DefaultTable[A,B,C]; key: A, val: var B): bool = t.super.pop(key,val)
template take*[A,B,C](t: var DefaultTable[A,B,C]; key: A, val: var B): bool = t.super.take(key,val)
template clear*[A,B,C](t: var DefaultTable[A,B,C]) = t.super.clear()
template `$`*[A,B,C](t: DefaultTable[A,B,C]):string = $(t.super)
template withValue*[A,B,C](t: var DefaultTable[A,B,C]; key: A; value,body: untyped) = t.super.withValue(key,value,body)
template withValue*[A,B,C](t: var DefaultTable[A,B,C]; key: A; value, body1, body2: untyped) = t.super.withValue(key,value,body1,body2)
iterator pairs*[A,B,C](t: DefaultTable[A,B,C]):(A,B) =
    for i in t.super.pairs:
        yield i
iterator mpairs*[A,B,C](t: var DefaultTable[A,B,C]):(A,var B) =
    for i in t.super.mpairs:
        yield i
iterator keys*[A,B,C](t: DefaultTable[A,B,C]):lent A =
    for i in t.super.keys:
        yield i
iterator values*[A,B,C](t: DefaultTable[A,B,C]): lent B =
    for i in t.super.values:
        yield i
iterator mvalues*[A,B,C](t:var DefaultTable[A,B,C]): var B =
    for i in t.super.mvalues:
        yield i
iterator allValues*[A,B,C](t: DefaultTable[A,B,C]; key: A): B =
    for i in t.super.allValues(key):
        yield i

when isMainModule:
  var x = {"a":1}.toDefaultTable(9)
  echo x["b"]
  var y = {"a":1}.toDefaultTable()
  echo y["b"]
  var z = {"a":1}.toDefaultTable((x)=>x.len)
  echo z["alphabet"]
  z["spoons"] = 9
