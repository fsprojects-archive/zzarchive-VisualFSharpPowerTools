namespace FSharpVSPowerTools.Outlining

[<Struct; NoComparison; NoEquality>]
type Lens<'Struct ,'Prop> =
    val Get : 'Struct -> 'Prop
    val Set : 'Prop   -> 'Struct -> 'Struct

    member lens.Update func prop =
        let value  = lens.Get prop
        let result = func value
        lens.Set result prop

    new ( get, set ) =
        {   Get = get
            Set = set   }




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =



    let inline lens   get set = Lens( get, set )
    let inline create get set = lens get set

    let inline get a    (l:Lens<_,_>) = l.Get a
    let inline set v a  (l:Lens<_,_>) = l.Set v a
    let inline update f (l:Lens<_,_>) = l.Update f

    /// Sequentially compose two lenses
    let inline compose  (l1:Lens<_,_>)  (l2:Lens<_,_>) = 
        lens ( l2.Get >> l1.Get     )
             ( l1.Set >> l2.Update  )   


    /// Composes two lenses through a sum in the source
//    let inline choice (l1: Lens<_,_>) (l2: Lens<_,_>) = 
//        lens ( Choice.choice l1.Get l2.Get )
//             ( fun b -> Choice.bimap (l1.Set b) (l2.Set b) )

    /// Pair two lenses
    let inline pair (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        lens ( fun (a,b)        -> ( l1.Get a    , l2.Get b   ))
             ( fun (a,c) (b,d)  -> ( l1.Set a b  , l2.Set c d ))


    /// Gets/sets the fst element in a pair
    let fst lp =
        lens ( Operators.fst )
             ( fun v a -> v, Operators.snd a )
        |> compose lp

    /// Gets/sets the snd element in a pair
    let snd lp =
        lens ( Operators.snd )
             ( fun v a -> Operators.fst a, v )
        |> compose lp

    /// Identity lens
    let id l = 
        lens ( Operators.id )
             ( fun a _ -> a ) |> compose l


    let cond pred lensTrue lensFalse =
        let inline choose a = if pred a then lensTrue else lensFalse
        lens ( fun a    -> choose a |> get a    )
             ( fun b a  -> choose a |> set b a  )

    /// Applies a lens in the 'get' direction within a state monad      
    let getState l = 
        fun a -> get a l, a

    /// Applies a lens in the 'set' direction within a state monad
    let setState l v = 
        fun a -> (), set v a l

    /// Update through a lens within a state monad
    let updateState l f =
        fun a -> (), update f l a

    [<AutoOpen>]
    module Operators = 

        let inline ( >>|  ) l1 l2    = compose l2 l1
        let inline ( |<<  ) l1 l2    = compose l1 l2
        let inline ( |+|  ) l1 l2    = pair l2 l1
        let inline ( +=   ) l v      = update ( (+)   v ) l
        let inline ( -=   ) l v      = update ( (-)   v ) l
        let inline ( /=   ) l v      = update ( (/)   v ) l
        let inline ( *=   ) l v      = update ( ( *)  v ) l
        let inline ( |||= ) l v      = update ( (|||) v ) l
        let inline ( ||=  ) l v      = update ( (||)  v ) l
        let inline ( &&&= ) l v      = update ( (&&&) v ) l
        let inline ( &&=  ) l v      = update ( (&&)  v ) l
        let inline ( <=!  ) l v      = fun a -> set  v a l


    [<Struct>]
    type Car = 
        val Make    : string 
        val Model   : string 
        val Mileage : int 

        new ( make, model, mileage ) =
            {   Make    = make 
                Model   = model
                Mileage = mileage   }

        static member Mileage_ =
            lens  ( fun   (c:Car) -> c.Mileage )            
                  ( fun v (c:Car) -> Car(c.Make,c.Model,v)) 
        
        static member Make_ =
            lens  ( fun   (c:Car) -> c.Make )               
                  ( fun v (c:Car) -> Car(v,c.Model,c.Mileage))

        static member Model_ =
            lens  ( fun   (c:Car) -> c.Model )              
                  ( fun v (c:Car) -> Car(c.Make,v,c.Mileage))
