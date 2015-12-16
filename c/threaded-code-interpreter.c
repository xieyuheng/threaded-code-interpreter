#include <stdio.h>
#include <stdint.h>
#include <setjmp.h>
#include <string.h>

typedef enum { false, true } Bool;

typedef char* String;

Bool string_equal(String string1, String string2) {
  if (strcmp(string1, string2) == 0) {
    return true;
  }
  else {
    return false;
  }
}

typedef uint8_t Byte;
typedef void Void;

#define CELL 4
typedef uint32_t Value;
typedef Value Address;
Value cell = CELL;

#define MEMORY_SIZE 1024 * 1024
Value memory_size = MEMORY_SIZE;

Byte memory_array_buffer[MEMORY_SIZE];
Address memory_current_free_address;

Byte memory_get_byte(Value index) {
  return (Byte) memory_array_buffer[index];
}
Void memory_set_byte(Value index, Byte value) {
  memory_array_buffer[index] = value;
}

Value memory_get(Value index) {
    Value sum = 0;
    Value i;
    Value base = 1;
    for (i=0; i < cell; i=i+1) {
      sum = sum + (memory_get_byte(index + i) * base);
      base = base * 256;
    }
    return sum;
}
Void memory_set(Value index, Value value) {
  Value i;
  for (i=0; i < cell; i=i+1) {
    memory_set_byte(index + i, value % 256);
    value = value >> 8;
  }
}

Value memory_allocate(Value size) {
  Value return_address = memory_current_free_address;
  memory_current_free_address = return_address + size;
  return return_address;
}

#define ARGUMENT_STACK_ADDRESS_UNDERFLOW \
  CELL * 64
#define ARGUMENT_STACK_ADDRESS \
  ARGUMENT_STACK_ADDRESS_UNDERFLOW
#define ARGUMENT_STACK_ADDRESS_SIZE \
  CELL * 1024

#define RETURN_STACK_ADDRESS_UNDERFLOW \
  CELL * 64
#define RETURN_STACK_ADDRESS \
  ARGUMENT_STACK_ADDRESS + \
  ARGUMENT_STACK_ADDRESS_SIZE + \
  RETURN_STACK_ADDRESS_UNDERFLOW
#define RETURN_STACK_ADDRESS_SIZE \
  CELL * 1024

#define MEMORY_CURRENT_FREE_ADDRESS \
  RETURN_STACK_ADDRESS + \
  RETURN_STACK_ADDRESS_SIZE

Value argument_stack_address =
  ARGUMENT_STACK_ADDRESS;
Value argument_stack_current_free_address =
  ARGUMENT_STACK_ADDRESS;

Void argument_stack_push(Value value) {
  memory_set(argument_stack_current_free_address, value);
  argument_stack_current_free_address =
    argument_stack_current_free_address + cell;
}
Value argument_stack_pop() {
  argument_stack_current_free_address =
    argument_stack_current_free_address - cell;
  return memory_get(argument_stack_current_free_address);
}

Value return_stack_address =
  RETURN_STACK_ADDRESS;
Value return_stack_current_free_address =
  RETURN_STACK_ADDRESS;

Void return_stack_push(Value value) {
  memory_set(return_stack_current_free_address, value);
  return_stack_current_free_address =
    return_stack_current_free_address + cell;
}
Value return_stack_pop() {
  return_stack_current_free_address =
    return_stack_current_free_address - cell;
  return memory_get(return_stack_current_free_address);
}

typedef Void (*PrimitiveFunction)();

typedef PrimitiveFunction PrimitiveFunctionRecord[1024];

PrimitiveFunctionRecord primitive_function_record;
Value primitive_function_record_counter = 0;

PrimitiveFunction primitive_function_record_get(Value index) {
  return primitive_function_record[index];
}

Void primitive_function_record_set
(Value index, PrimitiveFunction fun) {
  primitive_function_record[index] = fun;
}

Value create_primitive_function(PrimitiveFunction fun) {
  Value return_address = primitive_function_record_counter;
  primitive_function_record_set
    (primitive_function_record_counter, fun);
  primitive_function_record_counter =
    primitive_function_record_counter + 1;
  return return_address;
}

Value address_after_explainer = 0;

jmp_buf jmp_buffer;

Bool exit_interpreter() {
  longjmp(jmp_buffer, 666);
}

Void interpreter() {
  if (666 == setjmp(jmp_buffer)) {
    return;
  }
  else {
    while (true) {
      Address function_body = return_stack_pop();
      Address explainer = memory_get(memory_get(function_body));
      return_stack_push(function_body + cell);
      address_after_explainer =
        memory_get(function_body) + cell;
      PrimitiveFunction explainer_function =
        primitive_function_record_get(explainer);
      explainer_function();
    }
  }
}

typedef struct InHostTagRecordEntry {
  String string;
  Address address;
} InHostTagRecordEntry;

typedef InHostTagRecordEntry InHostTagRecord[1024];

InHostTagRecord in_host_tag_record;
Value in_host_tag_record_counter = 0;

Address in_host_tag_record_get(String string) {
  Value i;
  Bool match_p;
  for (i=0; i < in_host_tag_record_counter; i=i+1) {
    match_p = (string_equal
               (string,
                (in_host_tag_record[i].string)));
    if (match_p) {
      return (in_host_tag_record[i].address);
    }
  }
}

Byte in_host_tag_record_string_buffer[1024 * 1024];
Value in_host_tag_record_string_buffer_counter = 0;

Void in_host_tag_record_set(String string, Address address) {
  strcpy((in_host_tag_record_string_buffer +
          in_host_tag_record_string_buffer_counter),
         string);
  in_host_tag_record
    [in_host_tag_record_counter]
    .string = (in_host_tag_record_string_buffer +
               in_host_tag_record_string_buffer_counter);
  in_host_tag_record_string_buffer_counter =
    in_host_tag_record_string_buffer_counter +
    strlen(string) + 1;
  in_host_tag_record
    [in_host_tag_record_counter]
    .address = address;
  in_host_tag_record_counter =
    in_host_tag_record_counter + 1;
}

Void data(Value value) {
  memory_set(memory_current_free_address, value);
  memory_current_free_address =
    memory_current_free_address + cell;
}

Void mark(String tag_string) {
  in_host_tag_record_set
    (tag_string,
     memory_current_free_address);
}

Value link = 0;

Void PRIM_primitive_function_explainer() {
  PrimitiveFunction primitive_function =
    (primitive_function_record_get
     (memory_get
      (address_after_explainer)));
  primitive_function();
}

Value primitive_function_explainer = 0;

Void define_primitive_function
(String tag_string, PrimitiveFunction fun) {
  Value function_index = create_primitive_function(fun);
  data(link);
  link = memory_current_free_address - cell;
  mark(tag_string);
  data(primitive_function_explainer);
  data(function_index);
}

Void PRIM_function_explainer() {
  return_stack_push(address_after_explainer);
}

Value function_explainer = 1;

Void define_function
(String tag_string, Value length,
 String *function_tag_string_array) {
  data(link);
  link = memory_current_free_address - cell;
  mark(tag_string);
  data(function_explainer);
  Value i;
  for (i=0; i < length; i=i+1) {
    data(in_host_tag_record_get
         (function_tag_string_array[i]));
  }
}

Void PRIM_variable_explainer() {
  argument_stack_push
    (memory_get(address_after_explainer));
}

Value variable_explainer = 2;

Void define_variable(String tag_string, Value value) {
  data(link);
  link = memory_current_free_address - cell;
  mark(tag_string);
  data(variable_explainer);
  data(value);
}

Void PRIM_end() {
  return_stack_pop();
}

Void PRIM_bye() {
  printf("bye bye ^-^/\n");
  exit_interpreter();
}

Void PRIM_dup() {
  Value a = argument_stack_pop();
  argument_stack_push(a);
  argument_stack_push(a);
}

Void PRIM_mul() {
  Value a = argument_stack_pop();
  Value b = argument_stack_pop();
  argument_stack_push(a * b);
}

Void PRIM_simple_wirte() {
  printf("%d\n", argument_stack_pop());
}

int main (int argc, String* argv) {

  memory_current_free_address = MEMORY_CURRENT_FREE_ADDRESS;

  create_primitive_function(PRIM_primitive_function_explainer);
  create_primitive_function(PRIM_function_explainer);
  create_primitive_function(PRIM_variable_explainer);

  define_primitive_function("end", PRIM_end);
  define_primitive_function("bye", PRIM_bye);
  define_primitive_function("dup", PRIM_dup);
  define_primitive_function("mul", PRIM_mul);
  define_primitive_function("simple-wirte", PRIM_simple_wirte);

  define_variable("little-test-number", 4);

  String PRIM_square[] = {
    "dup",
    "mul",
    "end"
  };
  define_function("square", 3, PRIM_square);

  String PRIM_little_test[] = {
    "little-test-number",
    "square",
    "simple-wirte",
    "bye"
  };
  define_function("little-test", 4, PRIM_little_test);

  String PRIM_first_function[] = {
    "little-test",
    "end"
  };
  define_function("first-function", 2, PRIM_first_function);

  return_stack_push
    (in_host_tag_record_get("first-function") + cell);
  interpreter();
  return 0;

}
