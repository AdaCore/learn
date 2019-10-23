#include <unistd.h>
#include <stdio.h>
#include <unistd.h>

pid_t fork(void) {
  printf("fork not allowed\n");
   _exit(1);
}

pid_t vfork(void) {
   printf("vfork not allowed\n");
   _exit(1);
}

int system(char* text) {
  printf("system not allowed\n");
   _exit(1);
}

int execve(const char *filename, char *const argv[],
	   char *const envp[]) {
  printf("execve not allowed\n");
  _exit(1);
}
