"use strict";

let cell = 4;
// unit byte
// this global value should match the following interface

let memory = {};

memory.size = 64 * 1024 * 1024;
memory.array_buffer = new ArrayBuffer(memory.size);
memory.dataview = new DataView(memory.array_buffer);
memory.current_free_address = 0;

memory.get =
    function (index) {
        return memory.dataview.getUint32(index);
    };
memory.set =
    function (index, value) {
        memory.dataview.setUint32(index, value);
    };
memory.get_byte =
    function (index) {
        return memory.dataview.getUint8(index);
    };
memory.set_byte =
    function (index, value) {
        memory.dataview.setUint8(index, value);
    };
memory.allocate =
    function (size) {
        let return_address = memory.current_free_address;
        memory.current_free_address = return_address + size;
        return return_address;
    };

// memory.set(1, 231);
// memory.get(1);
// memory.allocate(16);
// memory.current_free_address;

memory.allocate(1024);
// 1k safe underflow

let argument_stack = {};
argument_stack.address = memory.allocate(1 * 1024 * 1024);
argument_stack.current_free_address = argument_stack.address;

argument_stack.push =
    function (value) {
        memory.set(argument_stack.current_free_address, value);
        argument_stack.current_free_address =
            argument_stack.current_free_address + cell;
    };

argument_stack.pop =
    function () {
        argument_stack.current_free_address =
            argument_stack.current_free_address - cell;
        return memory.get(argument_stack.current_free_address);
    };

argument_stack.tos =
    function () {
        return memory.get(argument_stack.current_free_address - cell);
    };

memory.allocate(1024);
// 1k safe underflow

let return_stack = {};
return_stack.address = memory.allocate(1 * 1024 * 1024);
return_stack.current_free_address = return_stack.address;

return_stack.push =
    function (value) {
        memory.set(return_stack.current_free_address, value);
        return_stack.current_free_address =
            return_stack.current_free_address + cell;
    };

return_stack.pop =
    function () {
        return_stack.current_free_address =
            return_stack.current_free_address - cell;
        return memory.get(return_stack.current_free_address);
    };

return_stack.tos =
    function () {
        return memory.get(return_stack.current_free_address - cell);
    };

let primitive_function_record = {};

// primitive_function_record.size = 4 * 1024;
// primitive_function_record.map = new Array(primitive_function_record.size);

primitive_function_record.counter = 0;
primitive_function_record.map = new Map();

primitive_function_record.get =
    function (index) {
        return primitive_function_record.map.get(index);
    };

primitive_function_record.set =
    function (index, fun) {
        return primitive_function_record.map.set(index, fun);
    };

let create_primitive_function =
    function (fun) {
        let return_address = primitive_function_record.counter;
        primitive_function_record
            .set(primitive_function_record.counter, fun);
        primitive_function_record.counter =
            primitive_function_record.counter + 1;
        return return_address;
    };

var next_explainer_argument = 0;

let next =
    function () {
        let function_body = return_stack.pop();
        let next_function_body = function_body + cell;
        let explainer = memory.get(memory.get(function_body));
        return_stack.push(next_function_body);
        next_explainer_argument = memory.get(function_body) + cell;
        primitive_function_record.get(explainer).call();
    };

let in_host_tag_hash_table = new Map();

let data =
    function (value) {
        memory.set(memory.current_free_address, value);
        memory.current_free_address =
            memory.current_free_address + cell;
    };

let mark =
    function (tag_string) {
        in_host_tag_hash_table
            .set(tag_string, memory.current_free_address);
    };

let link = 0;

let primitive_function_explainer =
    create_primitive_function(
        function () {
            primitive_function_record.get(
                memory.get(next_explainer_argument)
            ).call();
        });

let define_primitive_function =
    function (tag_string, fun) {
        let function_index = create_primitive_function(fun);
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(primitive_function_explainer);
        data(function_index);
    };

let function_explainer =
    create_primitive_function(
        function () {
            return_stack.push(next_explainer_argument);
            next();
        });

let define_function =
    function (tag_string, function_tag_string_array) {
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(function_explainer);
        function_tag_string_array.forEach(
            function (function_tag_string) {
                data(in_host_tag_hash_table.get(function_tag_string));
            }
        );
    };

let variable_explainer =
    create_primitive_function(
        function () {
            argument_stack.push(
                (memory.get(next_explainer_argument)));
            next();
        });

let define_variable =
    function (tag_string, value) {
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(variable_explainer);
        data(value);
    };

define_primitive_function(
    "end",
    function () {
        return_stack.pop();
        next();
    }
);

define_primitive_function(
    "dup",
    function () {
        let a = argument_stack.pop();
        argument_stack.push(a);
        argument_stack.push(a);
        next();
    }
);

define_primitive_function(
    "mul",
    function () {
        let a = argument_stack.pop();
        let b = argument_stack.pop();
        argument_stack.push(a * b);
        next();
    }
);

define_function(
    "square",
    [ "dup",
      "mul",
      "end"
    ]
);

define_primitive_function(
    "simple-wirte",
    function () {
        console.log(argument_stack.pop());
        next();
    }
);

define_variable("little-test-number", 4);

define_primitive_function(
    "bye",
    function () {
        console.log("bye bye ^-^/");
    }
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

let function_body_for_little_test =
    in_host_tag_hash_table.get("first-function")
    + cell;

let begin_to_interpret_threaded_code =
    function () {
        return_stack.push(function_body_for_little_test);
        next();
    };

begin_to_interpret_threaded_code();
