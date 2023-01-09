main :: () {
   write_string(".. inside main!\n");
   some_proc(); // call 'some_proc' again at runtime
}

#import "file.jisp"; // Compile and import our jisp file
