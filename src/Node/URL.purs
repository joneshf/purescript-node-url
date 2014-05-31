module Node.URL
  ( parse
  , format
  , resolve
  , URL()
  ) where

  import Data.Function
  import Data.Maybe

  foreign import url
    "var url = require('url')" ::
      { parse :: Fn1 String URLObjQ
      , format :: Fn1 URLObjQ String
      , resolve :: Fn2 String String String
      }
  -- These are the js versions of things because they can also be null.
  foreign import data StringQ :: *
  foreign import data BooleanQ :: *

  type URLObjQ =
    { auth :: StringQ
    , hash :: StringQ
    , host :: StringQ
    , hostname :: StringQ
    , href :: StringQ
    , path :: StringQ
    , pathname :: StringQ
    , port :: StringQ
    , protocol :: StringQ
    , query :: StringQ
    , search :: StringQ
    , slashes :: BooleanQ
    }
  type URLObj =
    { auth :: Maybe String
    , hash :: Maybe String
    , host :: Maybe String
    , hostname :: Maybe String
    , href :: Maybe String
    , path :: Maybe String
    , pathname :: Maybe String
    , port :: Maybe String
    , protocol :: Maybe String
    , query :: Maybe String
    , search :: Maybe String
    , slashes :: Maybe Boolean
    }
  data URL = URL URLObj

  -- Wrappers so we don't have to expose the actual node module.
  parse = runFn1 url.parse >>> maybeify >>> URL
  format (URL obj) = unMaybeify obj # runFn1 url.format
  resolve = runFn2 url.resolve

  -- Isomorphic helpers.
  -- We wouldn't need to dip into the ffi if ps had instance support for records.
  foreign import maybeify
    "function maybeify(obj) {\
    \  for (var k in obj) {\
    \    if (obj.hasOwnProperty(k)) {\
    \      obj[k] = obj[k] == null ? makeNothing() : makeJust(obj[k]);\
    \    }\
    \  }\
    \  return obj;\
    \}" :: URLObjQ -> URLObj

  foreign import unMaybeify
    "function unMaybeify(obj) {\
    \  for (var k in obj) {\
    \    if (obj.hasOwnProperty(k)) {\
    \      obj[k] = fromMaybeQ(null, obj[k]);\
    \    }\
    \  }\
    \  return obj;\
    \}" :: URLObj -> URLObjQ

  -- Some local definitions so we don't have to rely on how psc mangles names.
  makeNothing = const Nothing
  makeJust = Just
  fromMaybeQ = mkFn2 fromMaybe
