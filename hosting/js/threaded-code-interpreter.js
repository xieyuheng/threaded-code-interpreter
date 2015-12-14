"use strict";

let cell = 4;
// unit byte
// this global value should match the following interface

let memory = {};

memory.size = 1024 * 1024;
memory.array_buffer = new ArrayBuffer(memory.size);
memory.dataview = new DataView(memory.array_buffer);
memory.current_free_address = 0;

memory.get_byte =
    (index) => {
        return memory.dataview.getUint8(index);
    };
memory.set_byte =
    (index, value) => {
        memory.dataview.setUint8(index, value);
    };

memory.get =
    (index) => {
        return memory.dataview.getUint32(index);
    };
memory.set =
    (index, value) => {
        memory.dataview.setUint32(index, value);
    };

memory.allocate =
    (size) => {
        let return_address = memory.current_free_address;
        memory.current_free_address = return_address + size;
        return return_address;
    };

// memory.set(1, 231);
// memory.get(1);
// memory.set(1, 0);
// memory.get(1);
// memory.allocate(16);
// memory.current_free_address;

memory.allocate(64);
// underflow

let argument_stack = {};
argument_stack.address = memory.allocate(cell * 1024);
argument_stack.current_free_address = argument_stack.address;

argument_stack.push =
    (value) => {
        memory.set(argument_stack.current_free_address, value);
                 argument_stack.current_free_address =
                 argument_stack.current_free_address + cell;
               };

argument_stack.pop =
    () => {
        argument_stack.current_free_address =
            argument_stack.current_free_address - cell;
        return memory.get(argument_stack.current_free_address);
    };

memory.allocate(64);
// underflow

let return_stack = {};
return_stack.address = memory.allocate(cell * 1024);
return_stack.current_free_address = return_stack.address;

return_stack.push =
    (value) => {
        memory.set(return_stack.current_free_address, value);
        return_stack.current_free_address =
            return_stack.current_free_address + cell;
    };

return_stack.pop =
    () => {
        return_stack.current_free_address =
            return_stack.current_free_address - cell;
        return memory.get(return_stack.current_free_address);
    };

let primitive_function_record = {};

// primitive_function_record.size = 4 * 1024;
// primitive_function_record.map = new Array(primitive_function_record.size);

primitive_function_record.counter = 0;
primitive_function_record.map = new Map();

primitive_function_record.get =
    (index) => {
        return primitive_function_record.map.get(index);
    };

primitive_function_record.set =
    (index, fun) => {
        primitive_function_record.map.set(index, fun);
    };

let create_primitive_function =
    (fun) => {
        let return_address = primitive_function_record.counter;
        primitive_function_record
            .set(primitive_function_record.counter, fun);
        primitive_function_record.counter =
            primitive_function_record.counter + 1;
        return return_address;
    };

var address_after_explainer = 0;

let interpreter =
    () => {
        try {
            while (true) {
                let function_body = return_stack.pop();
                let explainer = memory.get(memory.get(function_body));
                return_stack.push(function_body + cell);
                address_after_explainer = memory.get(function_body) + cell;
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

let in_host_tag_hash_table = new Map();

let data =
    (value) => {
        memory.set(memory.current_free_address, value);
        memory.current_free_address =
            memory.current_free_address + cell;
    };

let mark =
    (tag_string) => {
        in_host_tag_hash_table
            .set(tag_string, memory.current_free_address);
    };

let link = 0;

let primitive_function_explainer =
    create_primitive_function(
        () => {
            primitive_function_record.get(
                memory.get(address_after_explainer)
            ).call();
        });

let define_primitive_function =
    (tag_string, fun) => {
        let function_index = create_primitive_function(fun);
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(primitive_function_explainer);
        data(function_index);
    };

let function_explainer =
    create_primitive_function(() => {
        return_stack.push(address_after_explainer);
    });

let define_function =
    (tag_string, function_tag_string_array) => {
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(function_explainer);
        function_tag_string_array.forEach(
            function_tag_string => {
                data(in_host_tag_hash_table
                     .get(function_tag_string));
            }
        );
    };

let variable_explainer =
    create_primitive_function(() => {
        argument_stack.push(
        memory.get(address_after_explainer));
    });

let define_variable =
    (tag_string, value) => {
        data(link);
        link = memory.current_free_address - cell;
        mark(tag_string);
        data(variable_explainer);
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

let function_body_for_little_test =
    in_host_tag_hash_table.get("first-function")
    + cell;

let begin_to_interpret_threaded_code =
    () => {
        return_stack.push(function_body_for_little_test);
        interpreter();
    };

begin_to_interpret_threaded_code();
