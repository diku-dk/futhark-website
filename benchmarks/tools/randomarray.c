#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SEED 1337

void random_flat_array(int min, int max, size_t num) {
  size_t i;
  printf("[");
  for (i = 0; i < num; i++) {
    if (i != 0) {
      printf(", ");
    }
    printf("%d", (rand()%(max-min))+min);
    if ((i+1) % 30 == 0) {
      putchar('\n');
    }
  }
  printf("]");
}

void random_array(int min, int max, int num_dims, size_t *dims) {
  if (num_dims == 1) {
    random_flat_array(min, max, dims[0]);
  } else {
    size_t i;
    size_t num = dims[0];
    printf("[");
    for (i = 0; i < num; i++) {
      if (i != 0) {
        printf(", ");
      }
      random_array(min, max, num_dims-1, dims+1);
    }
    printf("]");
  }
}

int main(int argc, char** argv) {
  srand(SEED);

  if (argc < 4) {
    fprintf(stderr, "Usage: %s <min> <max> <n>...\n", argv[0]);
    exit(1);
  }

  int num_dims = 0;
  size_t *dims = malloc(num_dims * sizeof(size_t));
  int i;

  for (i = 3; argv[i] != NULL; i++) {
    dims[num_dims++] = atoi(argv[i]);
    dims = realloc(dims, num_dims * sizeof(*dims));
  }

  if (num_dims != 0) {
    random_array(atoi(argv[1]), atoi(argv[2]), num_dims, dims);
  }
  return 0;
}
