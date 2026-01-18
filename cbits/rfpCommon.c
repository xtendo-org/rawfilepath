/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Common process helpers shared across platforms

   (c) XT <https://xtendo.org/> 2016, 2025
   ------------------------------------------------------------------------- */

#include "HsBase.h"
#include "Rts.h"

#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// If a process was terminated by a signal, the exit status we return
// via the System.Process API is (-signum). This encoding avoids collision with
// normal process termination status codes. See also #7229.
#define TERMSIG_EXITSTATUS(s) (-(WTERMSIG(s)))

int terminateProcess(pid_t handle) { return (kill(handle, SIGTERM) == 0); }

int getProcessExitCode(pid_t handle, int *pExitCode) {
  int wstat, res;

  *pExitCode = 0;

  if ((res = waitpid(handle, &wstat, WNOHANG)) > 0) {
    if (WIFEXITED(wstat)) {
      *pExitCode = WEXITSTATUS(wstat);
      return 1;
    } else if (WIFSIGNALED(wstat)) {
      *pExitCode = TERMSIG_EXITSTATUS(wstat);
      return 1;
    } else {
      /* This should never happen */
    }
  }

  if (res == 0)
    return 0;

  if (errno == ECHILD) {
    *pExitCode = 0;
    return 1;
  }

  return -1;
}

int waitForProcess(pid_t handle, int *pret) {
  int wstat;

  if (waitpid(handle, &wstat, 0) < 0) {
    return -1;
  }

  if (WIFEXITED(wstat)) {
    *pret = WEXITSTATUS(wstat);
    return 0;
  } else {
    if (WIFSIGNALED(wstat)) {
      *pret = TERMSIG_EXITSTATUS(wstat);
      return 0;
    } else {
      /* This should never happen */
    }
  }

  return -1;
}
