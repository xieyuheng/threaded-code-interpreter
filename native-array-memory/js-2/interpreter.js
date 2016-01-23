"use strict";

let cell_area = new Buffer(1024 * 1024, 0);

let argument_stack = new Stack(1024, 0);
let return_stack = new Stack(1024, 0);

let primitive_function_record = new Buffer(1024, 0);

let create_primitive_function = (fun) => {
  let return_address = primitive_function_record.cursor;
  primitive_function_record
    .set(return_address, fun);
  primitive_function_record.add_cursor(1);
  return return_address;
};

var address_after_explainer = 0;

let interpreter = () => {
  try {
    while (true) {
      let jojo = return_stack.pop();
      let jo = cell_area.get(jojo);
      let explainer = cell_area.get(jo);
      return_stack.push(jojo + 1);
      address_after_explainer = jo + 1;
      primitive_function_record.get(explainer).call();
      continue;
    }

  } catch (string) {
    switch (string) {
    case "bye":
      break;
    }
  }
};

let in_host_tag_record = new Map();

let data = (value) => {
  cell_area.set(cell_area.cursor, value);
  cell_area.add_cursor(1);
};

let mark = (tag_string) => {
    in_host_tag_record.set(tag_string, cell_area.cursor);
};

let link = 0;

let define_header = (tag_string, explainer) => {
  data(link);
  link = cell_area.cursor - 1;
  mark(tag_string);
  data(explainer);
};

let primitive_function_explainer = create_primitive_function(
  () => {
    primitive_function_record.get(
      cell_area.get(address_after_explainer)
    ).call();
  });

let define_primitive_function = (tag_string, fun) => {
  let function_index = create_primitive_function(fun);
  define_header(tag_string, primitive_function_explainer);
  data(function_index);
};

let function_explainer = create_primitive_function(
  () => {
    return_stack.push(address_after_explainer);
  });

let define_function = (tag_string, function_tag_string_array) => {
  define_header(tag_string, function_explainer);
  function_tag_string_array.forEach(
    function_tag_string => {
      data(in_host_tag_record
           .get(function_tag_string));
    }
  );
};

let variable_explainer = create_primitive_function(
  () => {
    argument_stack.push(
      cell_area.get(address_after_explainer));
  });

let define_variable = (tag_string, value) => {
  define_header(tag_string, variable_explainer);
  data(value);
};

define_primitive_function(
  "end",
  () => {
    return_stack.pop();
  }
);

define_primitive_function(
  "bye",
  () => {
    console.log("bye bye ^-^/");
    throw "bye";
  }
);

define_primitive_function(
  "dup",
  () => {
    let a = argument_stack.pop();
    argument_stack.push(a);
    argument_stack.push(a);
  }
);

define_primitive_function(
  "mul",
  () => {
    let a = argument_stack.pop();
    let b = argument_stack.pop();
    argument_stack.push(a * b);
  }
);

define_primitive_function(
  "simple-wirte",
  () => {
    console.log(argument_stack.pop());
  }
);

define_variable("little-test-number", 4);

define_function(
  "square",
  [ "dup",
    "mul",
    "end"
  ]
);

define_function(
  "little-test",
  [ "little-test-number",
    "square",
    "simple-wirte",
    "bye"
  ]
);

define_function(
  "first-function",
  [ "little-test",
    "end"
  ]
);

let begin_to_interpret_threaded_code = () => {
  let function_body_for_little_test =
        in_host_tag_record.get("first-function") + 1;
  return_stack.push(function_body_for_little_test);
  interpreter();
};

begin_to_interpret_threaded_code();
