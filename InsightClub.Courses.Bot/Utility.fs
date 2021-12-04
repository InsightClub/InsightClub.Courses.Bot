namespace InsightClub.Courses.Bot


[<AutoOpen>]
module General =
  let always x _ = x

  // Wraps tuple result into an option
  let tryParseWith (tryParseFunc: string -> bool * _) =
    tryParseFunc >> function
    | true, v  -> Some v
    | false, _ -> None

module Async =
  let singleton n = async { return n }

  let map f comp =
    async
      { let! r = comp
        return f r }
