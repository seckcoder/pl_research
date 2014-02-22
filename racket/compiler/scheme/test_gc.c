#include <stdio.h>
#include <assert.h>
#include "runtime.h"

char* next_stack_pos(char* stack) {
  return stack - wordsize;
}

char* next_heap_pos(char* heap) {
  return heap + wordsize;
}

char* add_vectag(char* p) {
  return (p + vec_tag);
}

void set_word_value(void* p, int v) {
  set_word(p, &v);
}

char* vector_alloc(memory* mem, char* stack, unsigned int len, int *v) {
  char* heap = heap_alloc(mem, stack, align_heap((len+1)*wordsize));
  char* pv = heap;
  set_word_value(heap, to_fixnum_rep(len));
  heap = next_heap_pos(heap);
  int i = 0;
  for(i = 0; i < len; i++) {
    /*set_word(heap, &v[i]);*/
    set_word_value(heap, to_fixnum_rep(v[i]));
    heap = next_heap_pos(heap);
  }
  return add_vectag(pv);
}

void test_gc1() {
  memory mem;
  int stack_size=16 * 4096,
      heap_size=64,
      global_size=16 * 4096;
  allocate_memory(&mem, stack_size, heap_size, global_size);
  char* stack = mem.stack_top; // stack on top
  int v1[2] = {1, 2};
  char* pv1 = vector_alloc(&mem, stack, 2, v1);
  int v2[1] = {3};
  vector_alloc(&mem, stack, 1, v2);
  stack = next_stack_pos(stack);
  set_word(stack, &pv1); // stack point to pv1;
  int v3[1] = {4};
  char* pv3 = vector_alloc(&mem, stack, 1, v3); // allocate 8 bytes space, gc started.
  pv1 = (char*)get_word(stack);
  print_ptr((ptr)pv1);
  /*print_ptr((ptr)pv2);*/
  print_ptr((ptr)pv3);
  printf("%u %u\n", (ptr)mem.heap, (ptr)mem.heap_base);
  assert(mem.heap - mem.heap_base == 24);
  delete_memory(&mem);
}

int main(int argc, const char *argv[])
{
  test_gc1();
  return 0;
}
