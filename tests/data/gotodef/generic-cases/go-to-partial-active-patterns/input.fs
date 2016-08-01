
let (|DivisibleBy|_|) by n = 
    if n % by = 0 then Some DivisibleBy else None
    
let fizzBuzz = function 
    | DivisibleBy 3 & DivisibleBy 5 -> "FizzBuzz" 
    | DivisibleBy 3 -> "Fizz" 
    | DivisibleBy 5 -> "Buzz" 
    | _ -> "" 