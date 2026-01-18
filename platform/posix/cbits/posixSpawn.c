#define _GNU_SOURCE
/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   POSIX spawn-based process support (non-Linux)

   (c) XT <https://xtendo.org/> 2016, 2025
   ------------------------------------------------------------------------- */

#include "HsBase.h"
#include "Rts.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "processFlags.h"
#include "rfpCommon.h"

extern char **environ;

static int set_fd_cloexec(int fd) {
  int flags = fcntl(fd, F_GETFD);
  if (flags == -1)
    return -1;
  return fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
}

pid_t runInteractiveProcess(char *const args[], char *workingDirectory,
                            char **environment, int fdStdIn, int fdStdOut,
                            int fdStdErr, int *pfdStdInput, int *pfdStdOutput,
                            int *pfdStdError, gid_t *childGroup,
                            uid_t *childUser, int reset_int_quit_handlers,
                            int flags, char **failed_doing) {
  int fdStdInput[2], fdStdOutput[2], fdStdError[2];
  int r;
  pid_t pid = -1;
  int haveStdInPipe = 0;
  int haveStdOutPipe = 0;
  int haveStdErrPipe = 0;

  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attr;
  short attr_flags = 0;

  if (fdStdIn == -1) {
    if (pipe(fdStdInput) == -1) {
      *failed_doing = "posix_spawn: pipe";
      return -1;
    }
    set_fd_cloexec(fdStdInput[0]);
    set_fd_cloexec(fdStdInput[1]);
    haveStdInPipe = 1;
  }
  if (fdStdOut == -1) {
    if (pipe(fdStdOutput) == -1) {
      *failed_doing = "posix_spawn: pipe";
      goto fail;
    }
    set_fd_cloexec(fdStdOutput[0]);
    set_fd_cloexec(fdStdOutput[1]);
    haveStdOutPipe = 1;
  }
  if (fdStdErr == -1) {
    if (pipe(fdStdError) == -1) {
      *failed_doing = "posix_spawn: pipe";
      goto fail;
    }
    set_fd_cloexec(fdStdError[0]);
    set_fd_cloexec(fdStdError[1]);
    haveStdErrPipe = 1;
  }

  r = posix_spawn_file_actions_init(&actions);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: file_actions_init";
    goto fail;
  }

  r = posix_spawnattr_init(&attr);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: attr_init";
    goto fail_actions;
  }

  if (reset_int_quit_handlers) {
    sigset_t sigdefault;
    sigemptyset(&sigdefault);
    sigaddset(&sigdefault, SIGINT);
    sigaddset(&sigdefault, SIGQUIT);
    r = posix_spawnattr_setsigdefault(&attr, &sigdefault);
    if (r != 0) {
      errno = r;
      *failed_doing = "posix_spawn: setsigdefault";
      goto fail_attr;
    }
    attr_flags |= POSIX_SPAWN_SETSIGDEF;
  }

  if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
    r = posix_spawnattr_setpgroup(&attr, 0);
    if (r != 0) {
      errno = r;
      *failed_doing = "posix_spawn: setpgroup";
      goto fail_attr;
    }
    attr_flags |= POSIX_SPAWN_SETPGROUP;
  }

  if ((flags & RUN_PROCESS_NEW_SESSION) != 0) {
#ifdef POSIX_SPAWN_SETSID
    attr_flags |= POSIX_SPAWN_SETSID;
#else
    errno = ENOTSUP;
    *failed_doing = "posix_spawn: setsid";
    goto fail_attr;
#endif
  }

#if defined(__APPLE__)
  if (childGroup) {
    r = posix_spawnattr_setgid_np(&attr, *childGroup);
    if (r != 0) {
      errno = r;
      *failed_doing = "posix_spawn: setgid";
      goto fail_attr;
    }
  }
  if (childUser) {
    r = posix_spawnattr_setuid_np(&attr, *childUser);
    if (r != 0) {
      errno = r;
      *failed_doing = "posix_spawn: setuid";
      goto fail_attr;
    }
  }
#else
  if (childGroup || childUser) {
    errno = ENOTSUP;
    *failed_doing = "posix_spawn: setuid/setgid";
    goto fail_attr;
  }
#endif

  r = posix_spawnattr_setflags(&attr, attr_flags);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: setflags";
    goto fail_attr;
  }

  if (workingDirectory) {
    r = posix_spawn_file_actions_addchdir_np(&actions, workingDirectory);
    if (r != 0) {
      errno = r;
      *failed_doing = "posix_spawn: chdir";
      goto fail_attr;
    }
  }

  if (fdStdIn == -1) {
    if (add_dup2_action(&actions, fdStdInput[0], STDIN_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  } else if (fdStdIn == -2) {
    if (add_close_action(&actions, STDIN_FILENO,
                         (const char **)failed_doing) < 0)
      goto fail_attr;
  } else {
    if (add_dup2_action(&actions, fdStdIn, STDIN_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  }

  if (fdStdOut == -1) {
    if (add_dup2_action(&actions, fdStdOutput[1], STDOUT_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  } else if (fdStdOut == -2) {
    if (add_close_action(&actions, STDOUT_FILENO,
                         (const char **)failed_doing) < 0)
      goto fail_attr;
  } else {
    if (add_dup2_action(&actions, fdStdOut, STDOUT_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  }

  if (fdStdErr == -1) {
    if (add_dup2_action(&actions, fdStdError[1], STDERR_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  } else if (fdStdErr == -2) {
    if (add_close_action(&actions, STDERR_FILENO,
                         (const char **)failed_doing) < 0)
      goto fail_attr;
  } else {
    if (add_dup2_action(&actions, fdStdErr, STDERR_FILENO,
                        (const char **)failed_doing) < 0)
      goto fail_attr;
  }

  r = posix_spawn_file_actions_addclosefrom_np(&actions, 3);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawn: closefrom";
    goto fail_attr;
  }

  char *const *envp = environment ? (char *const *)environment : environ;
  r = posix_spawnp(&pid, args[0], &actions, &attr, args, envp);
  if (r != 0) {
    errno = r;
    *failed_doing = "posix_spawnp";
    pid = -1;
    goto fail_attr;
  }

  if (fdStdIn == -1) {
    close(fdStdInput[0]);
    *pfdStdInput = fdStdInput[1];
  }
  if (fdStdOut == -1) {
    close(fdStdOutput[1]);
    *pfdStdOutput = fdStdOutput[0];
  }
  if (fdStdErr == -1) {
    close(fdStdError[1]);
    *pfdStdError = fdStdError[0];
  }

  posix_spawnattr_destroy(&attr);
  posix_spawn_file_actions_destroy(&actions);

  return pid;

fail_attr:
  posix_spawnattr_destroy(&attr);
fail_actions:
  posix_spawn_file_actions_destroy(&actions);
fail:
  if (haveStdInPipe) {
    close(fdStdInput[0]);
    close(fdStdInput[1]);
  }
  if (haveStdOutPipe) {
    close(fdStdOutput[0]);
    close(fdStdOutput[1]);
  }
  if (haveStdErrPipe) {
    close(fdStdError[0]);
    close(fdStdError[1]);
  }
  return -1;
}
