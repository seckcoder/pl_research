
#define fxshift 2
#define fxmask 0x03
#define bool_f 0x2F
#define bool_t 0x6F
#define null_v 0x3F
#define wordsize 4
#define fx_tag 0x00
#define objshift 3
#define objmask 0x07
#define char_tag 0x0F
#define charmask 0xFF
#define charshift 8
#define pair_tag 0x01
#define pairmask objmask
#define symmask objmask
#define symtag 0x03
#define symshift 3
#define cljmask objmask
#define clj_tag 0x02
#define cljshift 3
#define vecmask objmask
#define vec_tag 0x05
#define vecshift 3
#define strmask objmask
#define str_tag 0x06
#define gc_forward_tag 0x47
#define gc_forward_mask 0xFF
#define gc_forward_shift 8
#define obj_shift 3  // object shift
#define heap_align (1 << obj_shift) // double word


typedef unsigned int ptr;

// used to store temporary register value
typedef struct {
  void* eax; /* 0 scratch */
  void* ebx; /* 4 preserve */
  void* ecx; /* 8 scratch */
  void* edx; /* 12 scratch */
  void* esi; /* 16 preserve */
  void* edi; /*20 preserve*/
  void* ebp; /*24 preserve*/
  void* esp; /*28 preserve*/
} context;

typedef struct {
  char* heap;
  char* global;
  char* heap_base;
  char* heap_top;
  char* heap_base1;
  char* heap_top1;
  char* stack_top;
  char* stack_base;
  char* global_base;
  char* global_top;
} memory;

typedef struct {
  ptr car;
  ptr cdr;
} pair;

typedef int* vector;

// ptr points to heap
int is_heap_ptr(ptr p);
int is_vector(ptr p);
void set_heap_ptr_tag(ptr *p, int tag);

void print_ptr(ptr x);
void print_ptr_rec(ptr x) ;
int vector_length(vector v);
vector to_vector(ptr p) ;
char* vector_pi(vector v, int i);
int vector_ref(vector v, int i);
int get_word(char *p);
