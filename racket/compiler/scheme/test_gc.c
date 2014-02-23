#include <stdio.h>
#include <assert.h>
#include "runtime.h"

char* next_stack_pos(char* stack) {
  return stack - wordsize;
}

char* next_heap_pos(char* heap) {
  return heap + wordsize;
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
      global_size=16 * 4096,
      temp_size=stack_size;
  allocate_memory(&mem, stack_size, heap_size, global_size, temp_size);
  char* stack = mem.stack_top; // stack on top
  int v1[2] = {1, 2};
  char* pv1 = vector_alloc(&mem, stack, 2, v1); // 16 bytes
  stack = next_stack_pos(stack);
  set_word(stack, &pv1); // stack point to pv1;
  int v2[2] = {3, 4};
  vector_alloc(&mem, stack, 2, v2); // 16 bytes garbage
  vector_alloc(&mem, stack, 1, v2); // 8 bytes, trigger gc
  assert(mem.heap - mem.heap_base == 24);
  delete_memory(&mem);
}

void test_gc2() {
  memory mem;
  int stack_size = 16,
      heap_size = 112,
      global_size = 16,
      temp_size = stack_size;
  allocate_memory(&mem, stack_size, heap_size, global_size,
      temp_size);
  char* stack = mem.stack_top;
  int v[1] = {0};
  char* pva = vector_alloc(&mem, stack, 1, v);
  /*vector_rep_set((ptr)pva, 0, to_fixnum_rep(2));
  printf("%d\n", to_fixnum(vector_rep_ref((ptr)pva, 0)));*/
  stack = next_stack_pos(stack);
  set_word(stack, &pva);
  char* pvb = vector_alloc(&mem, stack, 1, v);
  char* pvc = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pva, 0, (int)pvc);
  char* pvd = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pvb, 0, (int)pvd);
  char* pve = vector_alloc(&mem, stack, 1, v);
  char* pvf = vector_alloc(&mem, stack, 1, v);
  vector_rep_set((ptr)pvc, 0, (int)pvf);
  vector_rep_set((ptr)pvf, 0, (int)pva);
  /*print_ptr((ptr)pva);*/

  // pva->pvc->pvf->pva; recursive. pva can't be printed
  vector_alloc(&mem, stack, 1, v); // trigger gc
  assert((mem.heap - mem.heap_base) == 56);
  pva = (char*)get_word(stack);
  delete_memory(&mem);
}

void test_gc3() {
  int stack_size = 16,
      heap_size = 64,
      global_size = 16,
      temp_size = stack_size;
  memory mem;
  allocate_memory(&mem, stack_size, heap_size, global_size,
      temp_size);
  char* stack = mem.stack_top;
  int v1[1] = {1};
  int v2[1] = {2};
  char* pva = vector_alloc(&mem, stack, 1, v1); // 8 bytes
  stack = next_stack_pos(stack);
  set_word(stack, &pva);
  char* pvb = vector_alloc(&mem, stack, 1, v2); // 8 bytes
  vector_rep_set((ptr)pva, 0, (int)pvb);
  vector_rep_set((ptr)pvb, 0, (int)pva);
  int v3[3] = {1,2,3};
  vector_alloc(&mem, stack, 3, v3); // 16bytes garbage
  vector_alloc(&mem, stack, 1, v3); // 8bytes, trigger gc
  assert((mem.heap - mem.heap_base) == 24);
  delete_memory(&mem);
}

int main(int argc, const char *argv[])
{
  test_gc1();
  test_gc2();
  test_gc3();
  return 0;
}
