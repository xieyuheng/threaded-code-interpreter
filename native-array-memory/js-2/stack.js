"use strict";

function Stack (size, init_value) {
  Buffer.call(this, size, init_value);
  this.cursor = 0;
};

Stack.prototype = Object.create(Buffer.prototype);

Stack.prototype.push = function (value) {
  this.set(this.cursor, value);
  this.add_cursor(1);
};

Stack.prototype.pop = function () {
  this.add_cursor(-1);
  return this.get(this.cursor);
};
