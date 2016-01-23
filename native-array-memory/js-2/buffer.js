"use strict";

function Buffer (size, init_value) {
  this.size = size;
  this.array = new Array(size);
  this.array.fill(init_value);
  this.cursor = 0;
};

Buffer.prototype = {
  set: function (index, value) {
    this.array[index] = value;
  },

  get: function (index) {
    return this.array[index];
  },

  add_cursor: function (value) {
    this.cursor = this.cursor + value;
  },

  allocate: function (size) {
    let return_address = this.cursor;
    this.add_cursor(size);
    return return_address;
  }
};
