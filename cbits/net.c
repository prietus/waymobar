/* The first IPv4 address of a network interface. */
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/socket.h>

int waymobar_ipv4(const char *ifname, char *out, int outlen)
{
    struct ifaddrs *list, *a;
    int found = 0;
    if (getifaddrs(&list) < 0)
        return 0;
    for (a = list; a && !found; a = a->ifa_next) {
        if (!a->ifa_addr || a->ifa_addr->sa_family != AF_INET || strcmp(a->ifa_name, ifname) != 0)
            continue;
        struct sockaddr_in *sin = (struct sockaddr_in *)a->ifa_addr;
        found = inet_ntop(AF_INET, &sin->sin_addr, out, outlen) != NULL;
    }
    freeifaddrs(list);
    return found;
}
