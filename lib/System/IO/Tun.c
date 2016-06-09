
#include <string.h>

#include <unistd.h>

#include <fcntl.h>

#include <sys/ioctl.h>

#include <net/if.h>

#include <linux/if_tun.h>

int inline_c_System_IO_Tun_0_da36479f0ed2e2ec91e4b4ea08c8d4115a36d98d(const char * device_inline_c_0) {

        struct ifreq ifr;
        int fd = 1;

        if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
            return -1;
        }

        memset(&ifr, 0, sizeof(ifr));
        ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
        strncpy((char *)&ifr.ifr_name,
                device_inline_c_0, 
                sizeof(ifr.ifr_name) - 1);

        if (ioctl(fd, TUNSETIFF, (void*)&ifr) < 0) {
            close(fd);
            return -1;
        }

        return fd;
    
}

