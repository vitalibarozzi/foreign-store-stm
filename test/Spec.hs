import Control.Concurrent.STM.TStore
import Control.Concurrent.STM (atomically)
import Control.Monad
import Control.Concurrent
import System.Timeout

main :: IO ()
main = do
    let dataX = "x" :: String
    let dataY = "y" :: String
    do ------------------------------------------------------------------------
        do
            s0 <- atomically do newTStore dataX
            s1 <- atomically do unsafeLookupTStore (tStoreIndex s0)
            when (Just s0 /= s1) (error "reference not equal")
            r0 <- atomically $ readTStore s0
            when (r0 /= dataX) (error "data not equal")
        do 
            (s0,s1) <- atomically do
                s0 <- newTStore dataX
                s1 <- unsafeLookupTStore (tStoreIndex s0)
                pure (s0,s1)
            when (Just s0 /= s1) (error "not equal")
            r0 <- atomically $ readTStore s0
            when (r0 /= dataX) (error "data not equal")
    do ------------------------------------------------------------------------
        do
            s0 <- atomically do newTStore dataX
            s1 <- atomically do newTStore dataY
            when (s0 == s1) (error $ "not different: "<>show s0)
            r0 <- atomically $ readTStore s0
            r1 <- atomically $ readTStore s1
            when (r0 /= dataX) (error "data not equal (0)")
            when (r1 /= dataY) (error "data not equal (1)")
        do 
            (s0,s1) <- atomically do
                s0 <- newTStore dataX
                s1 <- newTStore dataY
                pure (s0,s1)
            when (s0 == s1) (error "not different")
            r0 <- atomically $ readTStore s0
            r1 <- atomically $ readTStore s1
            when (r0 /= dataX) (error "data not equal (0)")
            when (r1 /= dataY) (error "data not equal (1)")
    
    store0 <- atomically (newTStore ("first string" :: String))
    store0 <- atomically (newTStore ("first string" :: String))

    forkIO $ void $ timeout 100000 $ forever do
        threadDelay 12010
        atomically (writeTStore store0 "111")

    forkIO $ forever do
        atomically (writeTStore store0 "lots lots lots") {- will be called a lot -}

    Just store1 <- atomically (unsafeLookupTStore (tStoreIndex store0))

    unless (store0 == store1) (error $ "store0 == store1: "<>show store0<>" /= "<>show store1)

    threadDelay 1000000
    print "using a store gathered with lookup"

    forkIO $ forever do
        threadDelay 523
        atomically do
            writeTStore store1 "------- im rarely called ----------" {- this one will never get called rarely -}
            writeTStore store1 "30000000000000000000000"

    forkIO $ void $ timeout 100 (forever do atomically (writeTStore store1 ":)"))

    forkIO $ forever do
        threadDelay 501
        atomically (writeTStore store1 ":)")

    forkIO $ forever do
        threadDelay 14300
        putStrLn . (">>>"<>)  =<< atomically (readTStore store1)

    forkIO $ forever do
        threadDelay 125000
        putStrLn . ("+++"<>)  =<< atomically (readTStore store1)

    store2 <- atomically (newTStore "im new")

    unless (store0 /= store2) (error $ "store0 /= store2: "<>show store0<>" == "<>show store2)

    threadDelay 5000000
