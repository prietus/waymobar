/* The little C waymobar needs: sendmsg with SCM_RIGHTS (to hand shared
 * memory to the compositor) and anonymous shared memory files. */
#define _GNU_SOURCE
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>

#define MAX_FDS 28

ssize_t waymobar_send_fds(int sock, const void *buf, size_t len, const int *fds, int nfds)
{
    char control[CMSG_SPACE(sizeof(int) * MAX_FDS)];
    struct iovec iov = { .iov_base = (void *)buf, .iov_len = len };
    struct msghdr msg;
    memset(&msg, 0, sizeof msg);
    memset(control, 0, sizeof control);
    if (nfds > MAX_FDS) {
        errno = EINVAL;
        return -1;
    }
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    if (nfds > 0) {
        msg.msg_control = control;
        msg.msg_controllen = CMSG_SPACE(sizeof(int) * nfds);
        struct cmsghdr *c = CMSG_FIRSTHDR(&msg);
        c->cmsg_level = SOL_SOCKET;
        c->cmsg_type = SCM_RIGHTS;
        c->cmsg_len = CMSG_LEN(sizeof(int) * nfds);
        memcpy(CMSG_DATA(c), fds, sizeof(int) * nfds);
    }
    return sendmsg(sock, &msg, MSG_NOSIGNAL);
}

/* An anonymous file of the given size, or -1. */
int waymobar_shm_file(size_t size)
{
    int fd = memfd_create("waymobar-shm", MFD_CLOEXEC);
    if (fd < 0)
        return -1;
    while (ftruncate(fd, size) < 0) {
        if (errno != EINTR) {
            close(fd);
            return -1;
        }
    }
    return fd;
}

void *waymobar_mmap(int fd, size_t size)
{
    void *p = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    return p == MAP_FAILED ? NULL : p;
}

void waymobar_munmap(void *p, size_t size)
{
    munmap(p, size);
}
