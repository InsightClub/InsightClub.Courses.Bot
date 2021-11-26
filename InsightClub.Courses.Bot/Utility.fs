namespace InsightClub.Courses.Bot


module Defer =
  let func f a _ = f a

module Async =
  let singleton n = async { return n }
