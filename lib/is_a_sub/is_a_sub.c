#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <stdint.h>

CAMLprim value
caml_is_a_sub(value va, value valen, value vb, value vblen) {
  CAMLparam4(va, valen, vb, vblen);

  uint8_t* a = Caml_ba_data_val(va);
  long unsigned alen = Long_val(valen);
  uint8_t* b = Caml_ba_data_val(vb);
  long unsigned blen = Long_val(vblen);

  CAMLreturn(Val_bool(a >= b && (a + alen) <= (b + blen)));
}
