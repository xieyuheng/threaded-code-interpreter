var gulp = require('gulp');
var nymph = require('./threaded-code-interpreter.js');

gulp.task('default', function() {
    console.log('hello world');
});

gulp.task('begin', function() {
    nymph.begin_to_interpret_threaded_code();
});


gulp.task('test', function() {

});
