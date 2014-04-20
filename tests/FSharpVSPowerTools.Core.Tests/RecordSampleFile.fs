module RecordSampleFile

type Record = {
    Field: int
}

type Record2 = {
    Field1: int
    Field2: float
}

let x: Record = { Field = 0 }
let y: Record2 =
    { Field1 = failwith "Field1 was not given a value" 
      Field2 = failwith "Field2 was not given a value" }