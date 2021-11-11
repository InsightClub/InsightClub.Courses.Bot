namespace InsightClub.Courses.Bot

open System


module Defer =
  let func f a _ = f a

module Uri =
  let createHttp onOk onError uri =
    let ok, uri = Uri.TryCreate(uri, UriKind.Absolute)

    if ok && (uri.Scheme = Uri.UriSchemeHttp || uri.Scheme = Uri.UriSchemeHttps)
    then onOk uri
    else onError ()
