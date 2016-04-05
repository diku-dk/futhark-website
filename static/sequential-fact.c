/* Compile the factorial function using a simple sequential loop. */

#include <stdlib.h>
#include <stdio.h>

#include <sys/time.h>

int main(int argc, char** argv) {
  static struct timeval t_start, t_end;
  int n = atoi(argv[1]);
  int res = 1;

  gettimeofday(&t_start, NULL);
  for (int i = 1; i <= n; i++) {
    res *= i;
  }
  gettimeofday(&t_end, NULL);

  printf("Runtime: %dus\n",
	 (t_end.tv_sec*1000000+t_end.tv_usec) - (t_start.tv_sec*1000000+t_start.tv_usec));

  printf("Result: %d\n", res);
}
