#include <stdio.h>

int simpleloop (int x, int y) {
  while (x < y) {
    if (x < 3)
      x++;
    else
      x+=2;
  }
  return x;
}

int main () {
  printf("%i", simpleloop(0, 10), stdout);
}
