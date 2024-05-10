

# Byte Pack

Byte Pack is a library to pack and unpack scala case classes into `IArray[Byte]`.

This is intented for:
 - memory hungry application that want to store many case classes into by `Array[Array[Byte]]`
 - application that want to serialize case classes on disk in efficient manner

Packing contains no type information, only the data. The classes are unpacked using the static type information derived from the type parameter of the unpack method. 

The basic operation work as follow:
```scala3
import bytepack.*

enum En derives EnumMirror:
  case E1, E2

case class Test(i: Int, e: En) derives Pack

// packed length should be 5
val packed: IArray[Byte] = Pack.pack(Test(1, En.T2)

// Unpack the entire case class
val test: Test = Pack.unpack[Test](packed)

// Unpack single field for efficiency
Pack.unpack[Test](_.e)(packed)

// Work with nested case classes
case class Nested(test: Test, i: Double) derives Pack
val netesd = Nested(test, 8.0)

val nestedPacked: IArray[Byte] = Pack.pack(nested)
Pack.unpack[Nested](_.test._i)(nestedPacked)

```

SBT dependency is:

```
libraryDependencies += "org.openmole" %% "byte-pack" % "0.5"
```
