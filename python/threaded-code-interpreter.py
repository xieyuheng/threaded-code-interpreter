cell = 4
# unit byte
# this global value should match the following interface

memory_size = 1024 * 1024
memory_array_buffer = bytearray(memory_size)
memory_current_free_address = 0

def memory_get_byte(index):
    return memory_array_buffer[index]
def memory_set_byte(index, value):
    memory_array_buffer[index] = value

# little endian
def memory_get(index):
    sum = 0
    for i in range(cell):
        sum = sum + (memory_array_buffer[index + i] * 256**i)
    return sum
def memory_set(index, value):
    for i in range(cell):
        memory_array_buffer[index + i] = value % 256
        value = value // 256

def memory_allocate(size):
    global memory_current_free_address
    return_address = memory_current_free_address
    memory_current_free_address = return_address + size
    return return_address

# memory_set(1, 233)
# memory_get(1)
# memory_set(1, 0)
# memory_get(1)
# memory_allocate(16)
# memory_current_free_address

memory_allocate(cell * 64)
# underflow

argument_stack_address = memory_allocate(cell * 1024)
argument_stack_current_free_address = argument_stack_address

def argument_stack_push(value):
    global argument_stack_current_free_address
    memory_set(argument_stack_current_free_address, value)
    argument_stack_current_free_address = \
      argument_stack_current_free_address + cell

def argument_stack_pop():
    global argument_stack_current_free_address
    argument_stack_current_free_address = \
      argument_stack_current_free_address - cell
    return memory_get(argument_stack_current_free_address)

memory_allocate(cell * 64)
# underflow

return_stack_address = memory_allocate(cell * 1024)
return_stack_current_free_address = return_stack_address

def return_stack_push(value):
    global return_stack_current_free_address
    memory_set(return_stack_current_free_address, value)
    return_stack_current_free_address = \
      return_stack_current_free_address + cell

def return_stack_pop():
    global return_stack_current_free_address
    return_stack_current_free_address = \
      return_stack_current_free_address - cell
    return memory_get(return_stack_current_free_address)

primitive_function_record_counter = 0
primitive_function_record = {}

def primitive_function_record_get(index):
    return primitive_function_record[index]

def primitive_function_record_set(index, fun):
    primitive_function_record.update({index : fun})

def create_primitive_function(fun):
    global primitive_function_record_counter
    return_address = primitive_function_record_counter
    primitive_function_record_set(
        primitive_function_record_counter, fun)
    primitive_function_record_counter = \
      primitive_function_record_counter + 1
    return return_address

address_after_explainer = 0

class InterpreterException(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

def interpreter():
    try:
        while True:
            function_body = return_stack_pop()
            explainer = memory_get(memory_get(function_body))
            return_stack_push(function_body + cell)
            global address_after_explainer
            address_after_explainer = memory_get(function_body) + cell
            explainer_function = \
              primitive_function_record_get(explainer)
            explainer_function()
    except InterpreterException as exception:
        if exception.value == "bye":
            return

in_host_tag_hash_table = {}

def in_host_tag_hash_table_get(string):
    return in_host_tag_hash_table[string]

def in_host_tag_hash_table_set(string, address):
    in_host_tag_hash_table.update({string : address})

def data(value):
    global memory_current_free_address
    memory_set(memory_current_free_address, value)
    memory_current_free_address = \
      memory_current_free_address + cell

def mark (tag_string):
    in_host_tag_hash_table_set(
        tag_string,
        memory_current_free_address)

link = 0

def _primitive_function_explainer():
    primitive_function = \
      primitive_function_record_get(
          memory_get(address_after_explainer))
    primitive_function()

primitive_function_explainer = \
  create_primitive_function(
      _primitive_function_explainer)

def define_primitive_function(tag_string, fun):
    global link
    function_index = create_primitive_function(fun)
    data(link)
    link = memory_current_free_address - cell
    mark(tag_string)
    data(primitive_function_explainer)
    data(function_index)

def _function_explainer():
    return_stack_push(address_after_explainer)

function_explainer = \
  create_primitive_function(
      _function_explainer)

def define_function(tag_string, function_tag_string_array):
    global link
    data(link)
    link = memory_current_free_address - cell
    mark(tag_string)
    data(function_explainer)
    for function_tag_string in function_tag_string_array:
        data(in_host_tag_hash_table_get(function_tag_string))

def _variable_explainer():
    argument_stack_push(
        memory_get(address_after_explainer))

variable_explainer = \
  create_primitive_function(
      _variable_explainer)

def define_variable(tag_string, value):
    global link
    data(link)
    link = memory_current_free_address - cell
    mark(tag_string)
    data(variable_explainer)
    data(value)

def _end():
    return_stack_pop()

define_primitive_function("end", _end)

def _bye():
    print("bye bye ^-^/")
    raise InterpreterException("bye")

define_primitive_function("bye", _bye)

def _dup():
    a = argument_stack_pop()
    argument_stack_push(a)
    argument_stack_push(a)

define_primitive_function("dup", _dup)

def _mul():
    a = argument_stack_pop()
    b = argument_stack_pop()
    argument_stack_push(a * b)

define_primitive_function("mul", _mul)

def _simple_wirte():
    print(argument_stack_pop())

define_primitive_function("simple-wirte", _simple_wirte)

define_variable("little-test-number", 4)

define_function(
    "square",
    [ "dup",
      "mul",
      "end"
    ]
)

define_function(
    "little-test",
    [ "little-test-number",
      "square",
      "simple-wirte",
      "bye"
    ]
)

define_function(
    "first-function",
    [ "little-test",
      "end"
    ]
)

function_body_for_little_test = \
  in_host_tag_hash_table_get("first-function") + cell

def begin_to_interpret_threaded_code():
    return_stack_push(function_body_for_little_test)
    interpreter()

begin_to_interpret_threaded_code()