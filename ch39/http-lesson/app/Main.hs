module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

myToken :: BC.ByteString
myToken = "xiHpQldSwfIfCoeiDFvOwEmcDxWZdiFo"

noaaHost :: BC.ByteString
noaaHost = "www.ncei.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "token" [myToken]
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest

-- Q39-1
buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
                  -> BC.ByteString -> Request
buildRequestNOSSL token host method path = setRequestMethod method
                                         $ setRequestHost host
                                         $ setRequestHeader "token" [myToken]
                                         $ setRequestPath path
                                         $ setRequestSecure False
                                         $ setRequestPort 443
                                         $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

main :: IO ()
main = do
    response <- httpLBS request
    -- Q39-2
    let status = getResponseStatus response
    if statusCode status == 200
    then do
        print "saving request to file"
        let jsonBody = getResponseBody response
        L.writeFile "data.json" jsonBody
    else print $ statusMessage status
