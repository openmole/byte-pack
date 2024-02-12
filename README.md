

# Byte Pack

Byte Pack is a library to pack and unpack scala case classes into `IArray[Byte]`.

This is intented for:
 - memory hungry application that want to store many case classes into by `Array[Array[Byte]]`
 - application that want to serialize case classes on disk in efficient manner

The basic operation work as follow:
```scala3
import bytepack.*

enum En derives EnumMirror:
  case E1, E2

case class Test(i: Int, e: En) derives Pack

// packed length should be 5
val packed: IArray[Byte] = Pack.pack(Test(1, En.T2)

val test: Test = Pack.unpack[Test](packed)
```

SBT dependency is:

```
libraryDependencies += "org.openmole" %% "byte-pack" % "0.1"
```
