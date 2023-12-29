import Control.Concurrent.STM.TStore
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Timeout

main = do
    store0 <- atomically (newTStore "first string")
    let store = store0

    forkIO $ void $ timeout 100000 $ forever do
        threadDelay 12010
        atomically (writeTStore store "111")

    forkIO $ forever do
        atomically (writeTStore store "lots lots lots") {- will be called a lot -}

    store1 <-  atomically (lookupTStore 0)
    let store = store1

    unless (store0 == store1) (error "fuen")

    threadDelay 1000000
    print "using a store gathered with lookup"

    forkIO $ forever do
        threadDelay 523
        atomically do
            writeTStore store "------- im rarely called ----------" {- this one will never get called rarely -}
            writeTStore store "30000000000000000000000"

    forkIO $ void $ timeout 100 (forever do atomically (writeTStore store ":)"))

    forkIO $ forever do
        threadDelay 501
        atomically (writeTStore store ":)")

    forkIO $ forever do
        threadDelay 14300
        putStrLn . (">>>"<>)  =<< atomically (readTStore store)

    forkIO $ forever do
        threadDelay 125000
        putStrLn . ("+++"<>)  =<< atomically (readTStore store)

    store2 <- atomically (newTStore "im new")

    unless (store0 /= store2) (error "fuen")

    threadDelay 2040800
