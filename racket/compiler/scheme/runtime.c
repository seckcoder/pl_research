#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "runtime.h"

int is_fixnum(ptr x) {
  return (x & fxmask) == fx_tag;
}

int is_char(ptr x) {
  return (x & charmask) == char_tag;
}

int to_fixnum(ptr x) {
  /*printf("%u\n", x);*/
  return ((int)x) >> fxshift;
}

char to_char(ptr x) {
  return (char)((int)x >> charshift);
}

char beautify_temp[10];
char* beautify(char c) {
  if (c == '\t') {
    return "#\\tab";
  } else if (c == '\n') {
    return "#\\newline";
  } else if (c == '\r'){
    return "#\\return";
  } else if (c == ' ') {
    return "#\\space";
  } else {
    sprintf(beautify_temp, "#\\%c", c);
    return beautify_temp;
  }
}

int is_null(ptr x) {
  return x == null_v;
}

int is_pair(ptr x) {
  return (x & pairmask) == pair_tag;
}

int is_list(ptr x) {
  return (is_pair(x) | is_null(x));
}

pair* to_pair(ptr x) {
  return (pair*)(x - pair_tag);
}

int is_closure(ptr x) {
  return (x & cljmask) == clj_tag;
}

int is_vector(ptr x) {
  return (x & vecmask) == vec_tag;
}

int is_string(ptr x) {
  return (x & strmask) == str_tag;
}

int is_heap_ptr(ptr x) {
  return is_pair(x) | is_closure(x) | is_vector(x) | is_string(x);
}

void set_heap_ptr_tag(ptr *p, int tag) {
  (*p) += tag;
}

void print_null() {
  printf("()");
}

void print_ptr_rec(ptr x);
void print_pair(ptr x) {
  pair* p = to_pair(x);
  print_ptr_rec(p->car);
  if (is_pair(p->cdr)) {
    printf(" ");
    print_pair(p->cdr);
  } else if (is_null(p->cdr)) {
    /*pass*/
  } else {
    printf(" . ");
    print_ptr_rec(p->cdr);
  }
}

void print_vector(vector x) {
  int len = vector_length(x);
  printf("%d: [", len);
  int i = 0;
  for(i = 0; i < len; i++) {
    print_ptr_rec((ptr)(vector_ref(x, i)));
    assert(get_word(vector_pi(x, i)) == vector_ref(x,i));
    if (i < len-1) {
      printf(", ");
    }
  }
  printf("]");
}

void print_ptr_rec(ptr x) {
  /*printf("%u\n", x);*/
  if (is_fixnum(x)) {
    printf("%d", to_fixnum(x));
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (is_null(x)) {
    print_null();
  } else if (is_char(x)) {
    printf("%s", beautify(to_char(x)));
  } else if (is_pair(x)) {
    printf("(");
    print_pair(x);
    printf(")");
  } else if (is_vector(x)) {
    print_vector(to_vector((ptr)x));
  }  else {
    printf("#<unknown 0x%08x>", x);
  }
}

void print_ptr(ptr x) {
  print_ptr_rec(x);
  printf("\n");
}


int get_word(char *p) {
  int v;
  memcpy(&v, p, wordsize);
  return v;
}

void set_word(char *p, int v) {
  memcpy(p, (char*)v, wordsize);
}

// we use double word to store a forward_ptr
// since the heap alignment is double word
int is_forward_ptr(ptr x) {
  char* p = (char*)x;
  int tag = get_word(p);
  return (tag & gc_forward_mask) == gc_forward_tag;
}

char* get_forward_ptr(ptr x) {
  char* p = (char*)x;
  return p + wordsize;
}

void mark_forward(char *p, char *new_p) {
  unsigned int tag = gc_forward_tag;
  set_word(p, tag);
  set_word(p + wordsize, (int)new_p);
}

vector to_vector(ptr p) {
  return (vector)(p - vec_tag);
}

// get length of the vector stored in p
int vector_length(vector v) {
  return to_fixnum(v[0]);
}

// get ith pointer of vector
char* vector_pi(vector v, int i) {
  return (char*)&v[i+1];
}
// get ith value of vector
int vector_ref(vector v, int i) {
  return v[i+1];
}

// recursive version of gc_forward
// DFS search
void gc_forward_rec(char* p, char* pp, memory* mem) {
  if (is_forward_ptr((ptr)p)) {
    set_word(pp, (int)(get_forward_ptr((ptr)p)));
  } else if (is_heap_ptr((ptr)p)) {
    if (is_vector((ptr)p)) {
      vector vec = to_vector((ptr)p);
      int vec_size = vector_length(vec) * wordsize + wordsize;
      char* new_p = mem->heap;
      mem->heap += vec_size;
      memcpy(p, new_p, vec_size);  // copy content
      mark_forward(p, new_p);
      int i = 0;
      for(i = 0; i < vec_size; i++) {
        char* ppi = new_p + (i+1)*wordsize;
        char* pi = (char*)get_word(ppi);
        gc_forward_rec(pi, ppi, mem);
      }
    } // ...
  } else {
    // ignored
  }
}


// copy/forward p to the newspace
char* gc_forward(char* p, memory* mem) {
  if (!is_heap_ptr((ptr)p)) {
    // not a pointer to heap
    // should be immediate value
    return p;
  } else if (is_forward_ptr((ptr)p)) {
    // forward p to new space
    return get_forward_ptr((ptr)p); // get ptr
  } else if (is_vector((ptr)p)) {
    vector vec = to_vector((ptr)p);
    int vec_size = (vector_length(vec) + 1) * wordsize;
    char* new_p = mem->heap;
    mem->heap += vec_size;
    memcpy(p, new_p, vec_size); // copy content of p to new space
    set_heap_ptr_tag((ptr*)&new_p, vec_tag); // add tag for new_p
    mark_forward(p, new_p); // p->new_p
    return new_p;
  } else {
    print_ptr((ptr)p);
    exit(20);
  }
}

void swap_mem_heap(memory* mem) {
  char* temp = mem->heap_base1;
  mem->heap_base1 = mem->heap_base;
  mem->heap_base = temp;
  temp = mem->heap_top1;
  mem->heap_top1 = mem->heap_top;
  mem->heap_top = temp;
}

void reset_memory(char* start, char* end) {
  memset(start, 0, end-start);
}

// stop and copy garbage collection.
// BFS
void gc(memory *mem, char* stack) {
  printf("gc\n");
  reset_memory(mem->heap_base1, mem->heap_top1);
  mem->heap = mem->heap_base1;

  // stack roots
  while (stack < mem->stack_top)  {
    char* pp = stack;
    char* p = (char*)get_word(pp);
    char* new_p = gc_forward(p, mem);
    assert(new_p != NULL);
    if (p != new_p) {
      set_word(pp, (int)new_p);
    }
    stack += wordsize;
  }

  // global roots

  char* scan = mem->heap_base1;
  char* alloc = mem->heap;
  while (scan < alloc) {
    char* p = (char*)get_word(scan);
    if (is_vector((ptr)p)) {
      vector vec = to_vector((ptr)p);
      int vec_len = vector_length(vec);
      int i = 0;
      for(i = 0; i < vec_len; i+=1) {
        char* pvi = vector_pi(vec, i); // pointer to ith element of vector
        char* vi = (char*)get_word(pvi); // get ith element
        char* new_vi = gc_forward(vi, mem);
        assert(new_vi!= NULL);
        if (new_vi != vi) {
          set_word(pvi, (int)new_vi);
        }
      }
      scan += (vec_len+1) * wordsize; // to next element
    } // other data types...

    alloc = mem->heap; // update alloc since mem->heap is changin after gc_forward
  }
  swap_mem_heap(mem); // swap old space and new space
  // mem->heap now points to the address of the first free position in heap
}

char* heap_alloc(memory *mem, char* stack, int size) {
  /*printf("heap_alloc\n");*/
  // [heap_base, heap_top)
  if (mem->heap + size >= mem->heap_top) {
    gc(mem, stack);
  }
  assert((mem->heap + size) < mem->heap_top);
  mem->heap += size;
  return mem->heap;
  // allocate at mem->heap
}


char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
      PROT_READ | PROT_WRITE,
      MAP_ANONYMOUS | MAP_PRIVATE,
      0, 0);
  if (p == MAP_FAILED) { perror("map"); exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  return (p + page);
}

void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { perror("munmap"); exit(status); }
}


int scheme_entry();
int main(int argc, char** argv) {
  int stack_size = 16 * 4096; //  16K byte
  int heap_size = (4 * 16 * 4096);
  int global_size = 16 * 4096;
  char* stack_base = allocate_protected_space(stack_size);
  char* heap_base = allocate_protected_space(heap_size);
  char* global_base = allocate_protected_space(global_size);

  memory mem;
  mem.heap = heap_base;
  mem.global = global_base;
  mem.heap_base = heap_base;
  mem.heap_top = mem.heap_base + heap_size/2;
  mem.heap_base1 = mem.heap_top;
  mem.heap_top1 = mem.heap_base + heap_size;
  mem.stack_base = stack_base;
  mem.stack_top = mem.stack_base + stack_size;
  mem.global_base = global_base;
  mem.global_top = mem.global_base + global_size;
  reset_memory(mem.heap_base1, mem.heap_top1);
  context ctx;
  print_ptr(scheme_entry(&ctx, mem.stack_top,
        &mem));
  deallocate_protected_space(stack_base, stack_size);
  deallocate_protected_space(heap_base, heap_size);
  return 0;
}
