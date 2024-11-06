

# Byte Pack

Byte Pack is a library to pack and unpack scala case classes into `IArray[Byte]`.

This is intented for:
 - memory hungry application that want to store many case classes into by `Array[Array[Byte]]`
 - application that want to serialize case classes on disk in efficient manner
 - application that want to manipulate datastructure minimaly stored in Array[Byte] but with a high level syntax

Packing contains no type information, only the data. The classes are unpacked using the static type information derived from the type parameter of the unpack method. 

The basic operation work as follow:
```scala3
import bytepack.*

enum En derives EnumMirror:
  case E1, E2

case class Test(i: Int, e: En) derives Pack

// packed length should be 5
val packed: IArray[Byte] = Pack.pack(Test(1, En.E2)

// Unpack the entire case class
val test: Test = Pack.unpack[Test](packed)

// Unpack single field for efficiency
Pack.access[Test](_.e).get(packed)

// Work with nested case classes
case class Nested(test: Test, i: Double) derives Pack
val netesd = Nested(test, 8.0)

val nestedPacked: IArray[Byte] = Pack.pack(nested)
Pack.access[Nested](_.test.i).get(nestedPacked)

// Efficiently modify some fields without unpacking
val modifyE = Pack.access[Nested](_.test.e)
val modifyI = Pack.access[Nested](_.i)

val newNestedPacked: IArray[Byte] = Pack.modify(nested, modifyE.set(En.E1), modifyI.modify(_ + 10.0))
```


Use array operations as follow:
```scala3
import bytepack.*

val a = FixedArray[4](IArray(9, 10, 11, 12))
val packed = Pack.pack(a)

// If you don't know the size a compile time you can do
// make sure the size is the same between pack and unpack otherwise you will get wrong data

object MyArray:
  given Conversion[IArray[Int], MyArray] = identity
  
opaque type MyArray = IArray[Int]

case class MyClass(v: Int, a: MyArray)

def load(d: IArray[Byte], arraySize: Int): MyClass =
  given Pack[MyArray] = Pack.immutableArray(arraSize)
  Pack.unpack(d)

def pack(d: MyClass, arraySize: Int): IArray[Byte] =
  given Pack[MyArray] = Pack.immutableArray(arraySize)
  Pack.pack(d)
```

Use [Monocle](https://github.com/optics-dev/Monocle) lenses as follow:

```scala3
import bytepack.*

enum En derives EnumMirror:
    case V1, V2

case class TestClass(i: Int, x: Float, e: En, e2: Option[En], e3: Option[En])
case class UpperClass(testClass: TestClass, j: Byte)

val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)

val packed = bytepack.Pack.pack(p)
val lens = Pack.lens[UpperClass](_.testClass.x)
lens.get(packed) 
```

SBT dependencies are:

```
libraryDependencies += "org.openmole" %% "byte-pack" % "0.10"
libraryDependencies += "org.openmole" %% "byte-pack-mon" % "0.10"
```
