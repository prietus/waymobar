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

/* The first IPv6 address of a network interface that is not link-local. */
int waymobar_ipv6(const char *ifname, char *out, int outlen)
{
    struct ifaddrs *list, *a;
    int found = 0;
    if (getifaddrs(&list) < 0)
        return 0;
    for (a = list; a && !found; a = a->ifa_next) {
        if (!a->ifa_addr || a->ifa_addr->sa_family != AF_INET6 || strcmp(a->ifa_name, ifname) != 0)
            continue;
        struct sockaddr_in6 *sin = (struct sockaddr_in6 *)a->ifa_addr;
        if (IN6_IS_ADDR_LINKLOCAL(&sin->sin6_addr) || IN6_IS_ADDR_LOOPBACK(&sin->sin6_addr))
            continue;
        found = inet_ntop(AF_INET6, &sin->sin6_addr, out, outlen) != NULL;
    }
    freeifaddrs(list);
    return found;
}
