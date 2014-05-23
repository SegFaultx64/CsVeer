Extremely simple example

```scala
import shapeless._
import poly._
import com.traversalsoftware.csv.CsVeer._
val test = """
5,4
5,4.0
2,7
Max,5
5,6,7
6,7
"""
object TestRules extends Rules {
  type Row = CsvInt :: CsvLong :: HNil
  type RowRaw = Int :: Long :: HNil
  val fake = CsvInt(0) :: CsvLong(0) :: HNil
}
val temp = test.split('\n').toStream.map(TestRules.run(_)).flatten
println(temp.toList)
```