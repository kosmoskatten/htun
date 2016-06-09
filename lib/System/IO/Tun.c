
#include <stdio.h>

#include <string.h>

#include <unistd.h>

#include <fcntl.h>

#include <sys/ioctl.h>

#include <net/if.h>

#include <linux/if_tun.h>

int inline_c_System_IO_Tun_0_bd2a7fb9efe496bd2ce0895e72f4562981ffe9d7(char * path_inline_c_0) {

        struct ifreq ifr;
        int fd = -1;

        printf("Got %s\n", path_inline_c_0);
        return fd;
    
}

