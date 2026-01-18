#ifndef RFP_COMMON_H
#define RFP_COMMON_H

#include <errno.h>
#include <spawn.h>

static inline int add_close_action(posix_spawn_file_actions_t *actions, int fd,
                                   const char **failed_doing) {
  int r = posix_spawn_file_actions_addclose(actions, fd);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: close";
    return -1;
  }
  return 0;
}

static inline int add_dup2_action(posix_spawn_file_actions_t *actions, int src,
                                  int dst, const char **failed_doing) {
  if (src == dst) {
    return 0;
  }
  int r = posix_spawn_file_actions_adddup2(actions, src, dst);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: dup2";
    return -1;
  }
  return 0;
}

#endif
