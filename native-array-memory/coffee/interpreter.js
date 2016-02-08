// Generated by CoffeeScript 1.10.0
(function() {
  var BUFFER, STACK, address_after_explainer, argument_stack, begin_to_interpret_threaded_code, cell_area, create_primitive_function, data, define_function, define_header, define_primitive_function, define_variable, function_explainer, in_host_tag_record, interpreter, link, mark, primitive_function_explainer, primitive_function_record, return_stack, variable_explainer,
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  BUFFER = (function() {
    function BUFFER(size1, init_value) {
      this.size = size1;
      this.init_value = init_value;
      this.array = new Array(this.size);
      this.array.fill(this.init_value);
      this.cursor = 0;
    }

    BUFFER.prototype.set = function(index, value) {
      return this.array[index] = value;
    };

    BUFFER.prototype.get = function(index) {
      return this.array[index];
    };

    BUFFER.prototype.add_cursor = function(value) {
      return this.cursor = this.cursor + value;
    };

    BUFFER.prototype.allocate = function(size) {
      var return_address;
      return_address = this.cursor;
      this.add_cursor(size);
      return return_address;
    };

    return BUFFER;

  })();

  STACK = (function(superClass) {
    extend(STACK, superClass);

    function STACK() {
      return STACK.__super__.constructor.apply(this, arguments);
    }

    STACK.prototype.push = function(value) {
      this.set(this.cursor, value);
      return this.add_cursor(1);
    };

    STACK.prototype.pop = function() {
      this.add_cursor(-1);
      return this.get(this.cursor);
    };

    return STACK;

  })(BUFFER);

  cell_area = new BUFFER(1024 * 1024, 0);

  argument_stack = new STACK(1024, 0);

  return_stack = new STACK(1024, 0);

  primitive_function_record = new BUFFER(1024, 0);

  create_primitive_function = function(fun) {
    var return_address;
    return_address = primitive_function_record.cursor;
    primitive_function_record.set(return_address, fun);
    primitive_function_record.add_cursor(1);
    return return_address;
  };

  address_after_explainer = 0;

  interpreter = function() {
    var error, explainer, jo, jojo, results, string;
    try {
      results = [];
      while (true) {
        jojo = return_stack.pop();
        jo = cell_area.get(jojo);
        explainer = cell_area.get(jo);
        return_stack.push(jojo + 1);
        address_after_explainer = jo + 1;
        results.push(primitive_function_record.get(explainer).call());
      }
      return results;
    } catch (error) {
      string = error;
      if (string === "bye") {
        return console.log("bye");
      } else {
        return console.log("unknow exception");
      }
    }
  };

  in_host_tag_record = new Map();

  data = function(value) {
    cell_area.set(cell_area.cursor, value);
    return cell_area.add_cursor(1);
  };

  mark = function(tag_string) {
    return in_host_tag_record.set(tag_string, cell_area.cursor);
  };

  link = 0;

  define_header = function(tag_string, explainer) {
    data(link);
    link = cell_area.cursor - 1;
    mark(tag_string);
    return data(explainer);
  };

  primitive_function_explainer = create_primitive_function(function() {
    return (primitive_function_record.get(cell_area.get(address_after_explainer))).call();
  });

  define_primitive_function = function(tag_string, fun) {
    var function_index;
    function_index = create_primitive_function(fun);
    define_header(tag_string, primitive_function_explainer);
    return data(function_index);
  };

  function_explainer = create_primitive_function(function() {
    return return_stack.push(address_after_explainer);
  });

  define_function = function(tag_string, function_tag_string_array) {
    define_header(tag_string, function_explainer);
    return function_tag_string_array.forEach(function(function_tag_string) {
      return data(in_host_tag_record.get(function_tag_string));
    });
  };

  variable_explainer = create_primitive_function(function() {
    return argument_stack.push(cell_area.get(address_after_explainer));
  });

  define_variable = function(tag_string, value) {
    define_header(tag_string, variable_explainer);
    return data(value);
  };

  define_primitive_function("end", function() {
    return return_stack.pop();
  });

  define_primitive_function("bye", function() {
    console.log("bye bye ^-^/");
    throw "bye";
  });

  define_primitive_function("dup", function() {
    var a;
    a = argument_stack.pop();
    argument_stack.push(a);
    return argument_stack.push(a);
  });

  define_primitive_function("mul", function() {
    var a, b;
    a = argument_stack.pop();
    b = argument_stack.pop();
    return argument_stack.push(a * b);
  });

  define_primitive_function("simple-wirte", function() {
    return console.log(argument_stack.pop());
  });

  define_variable("little-test-number", 4);

  define_function("square", ["dup", "mul", "end"]);

  define_function("little-test", ["little-test-number", "square", "simple-wirte", "bye"]);

  define_function("first-function", ["little-test", "end"]);

  begin_to_interpret_threaded_code = function() {
    var function_body_for_little_test;
    function_body_for_little_test = in_host_tag_record.get("first-function") + 1;
    return_stack.push(function_body_for_little_test);
    return interpreter();
  };

  begin_to_interpret_threaded_code();

}).call(this);
