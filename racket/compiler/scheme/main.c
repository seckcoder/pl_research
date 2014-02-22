#include "runtime.h"

int main(int argc, char** argv) {
  memory mem;
  context ctx;
  int stack_size=16 * 4096,
      heap_size=(4 * 16 * 4096),
      global_size=16 * 4096;
  allocate_memory(&mem, stack_size, heap_size, global_size);
  print_ptr(scheme_entry(&ctx, mem.stack_top,
        &mem));
  delete_memory(&mem);
  return 0;
}