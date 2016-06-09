{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module System.IO.Tun
    ( openTun
    ) where

import Foreign.C.String (CString, withCString)
import Language.C.Inline (CInt)
import System.Posix.Types (Fd (..))

import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "<string.h>"
C.include "<unistd.h>"
C.include "<fcntl.h>"
C.include "<sys/ioctl.h>"
C.include "<net/if.h>"
C.include "<linux/if_tun.h>"

openTun :: String -> IO (Maybe Fd)
openTun path = do
    fd <- withCString path openTunC
    if fd < 0 then return Nothing
              else return $ Just (Fd fd)

openTunC :: CString -> IO CInt
openTunC path =
    [C.block| int {
        struct ifreq ifr;
        int fd = -1;

        printf("Got %s\n", $(char* path));
        return fd;
    }|]
