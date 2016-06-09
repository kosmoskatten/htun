{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
-- |
-- Module: System.IO.Tun
-- Copyright: (c) 2016 Patrik Sandahl
--
-- Licence: MIT
-- Maintainer: patrik.sandahl@gmail.com
-- Stability: experimental
-- Portability: non-portable (requires Linux)
--
-- Utility function to access Linux Tun devices.
--
-- The function 'openTun' open a Linux Tun device and provides
-- a @Handle@ to it for reading and writing.
--
-- Before calling 'openTun' it is expected that a Tun device, e.g. tun0,
-- is created. E.g.:
--
-- > sudo ip tuntap add mode tun tun0
--
-- Bring the new Tun device up, and then route some traffic to it.
-- E.g. route all traffic to the 213.0.0.0/24 network to it:
--
-- > sudo ip link set tun0 up
-- > sudo ip route add 213.0.0.0/24 dev tun0
--
-- Now the Tun device can be opened. E.g.:
--
-- > import System.IO (hGetContents)
-- > Just h <- openTun "tun0"
-- > hGetContents h
--
-- If some network data is sent to the network it will be printed to stdout.
--
-- > ping 213.0.0.13
module System.IO.Tun
    ( openTun
    ) where

import Foreign.C.String (CString, withCString)
import GHC.IO.Handle (Handle)
import Language.C.Inline (CInt)
import System.Posix (Fd (..), fdToHandle)

import qualified Language.C.Inline as C

C.include "<string.h>"
C.include "<unistd.h>"
C.include "<fcntl.h>"
C.include "<sys/ioctl.h>"
C.include "<net/if.h>"
C.include "<linux/if_tun.h>"

-- | Open the named Tun device. If successfull a 'Handle' usable for
-- reading and writing is returned.
openTun :: String -> IO (Maybe Handle)
openTun device = do
    fd <- withCString device openTunC
    if fd < 0 then return Nothing
              else Just <$> fdToHandle (Fd fd)

-- | "inline-c" function to setup a Tun device.
openTunC :: CString -> IO CInt
openTunC device =
    [C.block| int {
        struct ifreq ifr;
        int fd = 1;

        if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
            return -1;
        }

        memset(&ifr, 0, sizeof(ifr));
        ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
        strncpy((char *)&ifr.ifr_name,
                $(const char* device), 
                sizeof(ifr.ifr_name) - 1);

        if (ioctl(fd, TUNSETIFF, (void*)&ifr) < 0) {
            close(fd);
            return -1;
        }

        return fd;
    }|]
