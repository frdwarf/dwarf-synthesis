#include "dwarf_write.h"

#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

/* dump functions */

void dump_pre_dwarf_entry(struct pre_dwarf_entry e) {
  printf("    %lx  %d + %lld\n", e.location, e.cfa_offset_reg, e.cfa_offset);
}

void dump_pre_dwarf_fde(struct pre_dwarf_fde f) {
  printf("%ld\n", f.num); 
  printf("%lx  %lx\n", f.initial_location, f.end_location);
  for (int i=0; i < f.num; i++)
    dump_pre_dwarf_entry(f.entries[i]);
}
  
void dump_pre_dwarf (struct pre_dwarf p) {
  printf("num_fde: %ld \n", p.num_fde);
  for (int i=0; i < p.num_fde; i++) {
    dump_pre_dwarf_fde(p.fdes[i]);
  }
}

void margin (int n)
  { while (n-- > 0) printf(".");  return; }

void print_block (value v,int m) 
{
  int size, i;
  margin(m);
  if (Is_long(v)) 
    { printf("immediate value (%ld)\n", Long_val(v));  return; };
  printf ("memory block: size=%d  -  ", size=Wosize_val(v));
  switch (Tag_val(v))
   {
    case Closure_tag : 
        printf("closure with %d free variables\n", size-1);
        margin(m+4); printf("code pointer: %p\n",Code_val(v)) ;
        for (i=1;i<size;i++)  print_block(Field(v,i), m+4);
        break;
    case String_tag :
        printf("string: %s (%s)\n", String_val(v),(char *) v);  
        break;
    case Double_tag:  
        printf("float: %g\n", Double_val(v));
        break;
    case Double_array_tag : 
        printf ("float array: "); 
        for (i=0;i<size/Double_wosize;i++)  printf("  %g", Double_field(v,i));
        printf("\n");
        break;
    case Abstract_tag : printf("abstract type\n"); break;
      //    case Final_tag : printf("abstract finalized type\n"); break;
    default:  
        if (Tag_val(v)>=No_scan_tag) { printf("unknown tag"); break; }; 
        printf("structured block (tag=%d):\n",Tag_val(v));
        for (i=0;i<size;i++)  print_block(Field(v,i),m+4);
   }
  return ;
}

value inspect_block (value v)  
  { print_block(v,4); fflush(stdout); return v; }


/* conversion functions */

long int64_of_value(value v) {
  union { int i[2]; long j; } buffer;
  buffer.i[0] = ((int *) Data_custom_val(v))[0];
  buffer.i[1] = ((int *) Data_custom_val(v))[1];
  return buffer.j;
}

addr_t convert_addr_t(value addr) {
  CAMLparam1(addr);
  return (addr_t) int64_of_value(addr);
} 


reg_t convert_reg_t(value reg) {
  CAMLparam1(reg);
  return (reg_t) Int_val(reg);
}

offset_t convert_offset_t(value offset) {
  CAMLparam1(offset);
  return (offset_t) int64_of_value(offset);
}

int convert_bool(value boolval) {
  CAMLparam1(boolval);
  return Bool_val(boolval);
}

struct pre_dwarf_entry * convert_pre_dwarf_entry(value oc_pde) {

  struct pre_dwarf_entry *pde = malloc(sizeof(struct pre_dwarf_entry));

  CAMLparam1(oc_pde);

  pde->location = convert_addr_t(Field(oc_pde, 0));
  pde->cfa_offset = convert_offset_t(Field(oc_pde, 1));
  pde->cfa_offset_reg = convert_reg_t(Field(oc_pde, 2));
  pde->rbp_defined = convert_bool(Field(oc_pde, 3));
  pde->rbp_offset = convert_offset_t(Field(oc_pde, 4));

  return pde;
}

struct pre_dwarf_fde convert_pre_dwarf_fde(value oc_pre_dwarf_fde) {
  
  struct pre_dwarf_fde * pre_dwarf_fde = malloc(sizeof(struct pre_dwarf_fde));

  CAMLparam1(oc_pre_dwarf_fde);

  pre_dwarf_fde->num = Int_val(Field(oc_pre_dwarf_fde,0));
  pre_dwarf_fde->initial_location = int64_of_value (Field(oc_pre_dwarf_fde,1));
  pre_dwarf_fde->end_location = int64_of_value(Field(oc_pre_dwarf_fde,2));
  
  // FZ: is num the correct size?  we can also read the size from the array.
  
  pre_dwarf_fde->entries = malloc(sizeof(struct pre_dwarf_entry) * pre_dwarf_fde->num);

  for (unsigned int i=0; i < pre_dwarf_fde->num; i++)
    pre_dwarf_fde->entries[i] = *convert_pre_dwarf_entry(Field(Field(oc_pre_dwarf_fde,4),i));
  
  return *pre_dwarf_fde;
}

struct pre_dwarf * convert_pre_dwarf(value oc_pre_dwarf) {
  struct pre_dwarf * pre_dwarf = malloc(sizeof(struct pre_dwarf));

  pre_dwarf->num_fde = (size_t) Int_val(Field(oc_pre_dwarf,0));

  //array
  
  pre_dwarf->fdes = malloc(sizeof(struct pre_dwarf_fde) * pre_dwarf->num_fde);
  for (unsigned int i=0; i < pre_dwarf->num_fde; i++) {
    pre_dwarf->fdes[i] = convert_pre_dwarf_fde(Field(Field(oc_pre_dwarf,1),i));
  }
  return pre_dwarf;
}

// OCaml type: string -> pre_c_dwarf -> int
value caml_write_dwarf (value oc_obj_path, value oc_eh_path, value oc_pre_dwarf) {

  char *obj_path, *eh_path;
  struct pre_dwarf *pre_dwarf;

  CAMLparam2(oc_obj_path, oc_pre_dwarf);

  //  inspect_block(oc_pre_dwarf);

  obj_path = String_val(oc_obj_path);

  eh_path = String_val(oc_eh_path);

  pre_dwarf = convert_pre_dwarf(oc_pre_dwarf);

  dump_pre_dwarf(*pre_dwarf);

  CAMLreturn(write_dwarf(obj_path, eh_path, pre_dwarf));
}
