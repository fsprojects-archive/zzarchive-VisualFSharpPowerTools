
type T() =
    member this.Test(x: int * int, y: int): int = 3
    
    let x = new T()