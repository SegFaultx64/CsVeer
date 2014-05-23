Extremely simple example

```scala
import shapeless._
import poly._
import com.traversalsoftware.CsVeer._
type Row = CsvInt :: CsvLong :: HNil
type RowRaw = Int :: Long :: HNil
implicit val fake = CsvInt(0) :: CsvLong(0) :: HNil
val test = """
5,4
5,4.0
2,7
Max,5
5,6,7
6,7
"""
test.split('\n').toStream.map(doAll[Row, RowRaw](_)).flatten
```