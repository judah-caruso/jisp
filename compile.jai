#run {
   set_build_options_dc(.{ do_output = false });

   ws := compiler_create_workspace();

   opts := get_build_options(ws);
   opts.output_executable_name = "jisp";
   set_build_options(opts, ws);

   // Make sure any .jisp imports fail so we can intercept them
   remap_import(ws, "*", "*.jisp", "");

   compiler_begin_intercept(ws);
   add_build_file("main.jai", ws);

   while true {
      message := compiler_wait_for_message();
      if message.kind == {
         case .COMPLETE;
            break;

         case .FAILED_IMPORT;
            m := cast(*Message_Failed_Import)message;
            print(".. importing jisp file '%'\n", m.target_module_name);

            source, ok := read_entire_file(m.target_module_name);
            assert(ok, "Unable to open %", m.target_module_name);

            compiled := compile(source, m.workspace);
            provide_import(ws, m, .FULL_TEXT, compiled);
      }
   }

   compiler_end_intercept(ws);
}

#add_context jisp_workspace: Workspace;

compile :: (str: string, workspace: Workspace) -> string {
   context.jisp_workspace = workspace;

   // Generate a Code_Type_Instantiation table so we don't
   // have to build them manually.

   // We make declarations that have an instantiation so
   // we can simply get the nodes and map between the
   // identifier and the instantiation.
   #insert -> string {
      // Pointer types we can make on-the-fly
      builtins :: Type.[
         bool,
         u8,
         u16,
         u32,
         u64,
         s8,
         s16,
         s32,
         s64,
         float32,
         float64,
         string,
         void,
      ];

      b: String_Builder;

      append(*b, "decls := Code.[\n");
      for builtins {
         print_to_builder(*b, "\t#code _: %,\n", it);
      }

      append(*b, "];");

      return builder_to_string(*b);
   }

   Type_Table :: Table(string, *Code_Type_Instantiation);
   type_insts: Type_Table;

   // Map the type name to an instantiation
   for decls {
      root  := compiler_get_nodes(it);
      decl  := cast(*Code_Declaration)root;
      type  := decl.type_inst;
      ident := cast(*Code_Ident)type.type_valued_expression;
      table_set(*type_insts, ident.name, type);
   }

   // The Jisp parser starts here
   Value :: struct {
      kind: enum {
         None;
         Number;
         Identifier;
         Operator;
         List;
         Directive_Run;
         Directive_Import;
         Raw_Jai_String;
      };
      union {
         str:    string;
         number: float64;
         list:   [..]Value;
      };
   }

   parse :: (str: *string) -> []Value, bool {
      values: [..]Value;
      while str.count {
         v, ok := parse_value(str);
         if !ok return .[], false;
         array_add(*values, v);
      }

      return values, true;
   }

   parse_raw_jai :: (str: *string) -> Value, bool {
      advance(str); // consume @

      ident, ok := parse_ident(str);
      if !ok return .{}, false;

      raw_string: string;
      raw_string.data  = str.data;
      raw_string.count = 0;

      while str.count {
         if str.data[0] == #char "@" {
            advance(str);

            raw_string.count = str.data - raw_string.data - 1;

            possible, ok := parse_ident(str);
            if !ok return .{}, false;

            if possible.str == ident.str {
               break;
            }

            continue;
         }

         advance(str);
      }

      return .{
         kind = .Raw_Jai_String,
         str  = raw_string,
      }, true;
   }

   parse_value :: (str: *string) -> Value, bool {
      while str.count && is_space(str.data[0]) {
         advance(str);
      }

      if !str.count return .{}, true;

      while str.count {
         c := str.data[0];

         if c == {
            case #char "(";
               lst, ok := parse_list(str);
               return lst, ok;

            case #char "#";
               directive, ok := parse_directive(str);
               return directive, ok;

            case #char "@";
               raw_jai, ok := parse_raw_jai(str);
               return raw_jai, ok;

            case #char "+"; #through;
            case #char "-"; #through;
            case #char "*"; #through;
            case #char "/";
               s: string = ---;
               s.data  = *c;
               s.count = 1;
               advance(str);
               return .{ kind = .Operator, str = copy_string(s) }, true;

            case #char ";";
               while str.count && str.data[0] != #char "\n" {
                  advance(str);
               }

               return .{}, true;

            case;
               if is_alpha(c) {
                  ident, ok := parse_ident(str);
                  return ident, ok;
               }
               else if is_digit(c) {
                  number, ok := parse_number(str);
                  return number, ok;
               }
               else {
                  s: string = ---;
                  s.data  = *c;
                  s.count = 1;
                  assert(false, "Invalid token '%'", s);
               }
         }
      }

      return .{}, false;
   }

   parse_directive :: (str: *string) -> Value, bool {
      advance(str); // consume #

      ident, i_ok := parse_ident(str);
      if !i_ok return .{}, false;

      directive, d_ok := parse_value(str);
      if !d_ok return .{}, false;

      assert(directive.kind == .Identifier);

      if ident.str == {
         case "run";
            return .{
               kind = .Directive_Run,
               str  = directive.str,
            }, true;
         case "import";
            return .{
               kind = .Directive_Import,
               str  = directive.str,
            }, true;
         case;
            assert(false, "Invalid directive '#%'", ident.str);
      }

      return .{}, false;
   }

   parse_list :: (str: *string) -> Value, bool {
      advance(str); // consume (
      if !str.count return .{}, false;

      list: [..]Value;
      while str.count && str.data[0] != #char ")" {
         value, ok := parse_value(str);
         assert(ok, "Unable to parse value in list!");
         array_add(*list, value);
      }

      assert(str.count || str.data[0] == #char ")", "Unclosed list");
      advance(str); // consume )

      return .{
         kind = .List,
         list = list,
      }, true;
   }

   parse_ident :: (str: *string) -> Value, bool {
      ident := <<str;
      ident.count = 0;

      while str.count && is_alnum(str.data[0]) {
         ident.count += 1;
         advance(str);
      }

      return .{
         kind = .Identifier,
         str  = ident,
      }, true;
   }

   parse_number :: (str: *string) -> Value, bool {
      num := <<str;
      num.count = 0;

      while str.count && is_alnum(str.data[0]) {
         num.count += 1;
         advance(str);
      }

      raw := num;
      f, ok := parse_float(*raw);
      assert(ok, "Unable to parse float: %", num);

      return .{
         kind   = .Number,
         number = f,
      }, true;
   }

   // Converts our AST nodes to Jai Code_Nodes

   emit_value :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      assert(v != null);

      if v.kind == {
         case .List;
            n, ok := emit_list(v, types);
            return n, ok;
         case .Identifier;
            n, ok := emit_ident(v, types);
            return n, ok;
         case .Number;
            n, ok := emit_number(v, types);
            return n, ok;

         case .Directive_Run;
            // @Todo(Judah): This should emit a *Code_Node so we can pass arguments
            add_build_string(sprint("#run %();", v.str), context.jisp_workspace);
            return null, true;

         case .Directive_Import;
            assert(false, "Because of how Program_Print handles import printing, this doesn't work. Use a @JAI_CODE block instead");
            n, ok := emit_import(v, types);
            return n, ok;

         case .Raw_Jai_String;
            add_build_string(v.str, context.jisp_workspace);
            return null, true;

         case;
            assert(false, "Unhandled kind %", v.kind);
      }

      return null, false;
   }

   emit_op :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      car := v.list[0];
      cdr := v.list;
      cdr.count -= 1;
      cdr.data  += 1;

      if car.str == {
         case "/"; #through;
         case "*"; #through;
         case "-"; #through;
         case "+";
            assert(cdr.count == 2, "Operator '%' requires 2 arguments!", car.str);

            node := New(Code_Binary_Operator);
            node.kind          = .BINARY_OPERATOR;
            node.operator_type = car.str[0];

            lhs, l_ok := emit_value(*cdr[0], types);
            if !l_ok return null, false;

            rhs, r_ok := emit_value(*cdr[1], types);
            if !r_ok return null, false;

            node.left  = lhs;
            node.right = rhs;
            return node, true;
         case;
            assert(false, "Unknown operator '%'", car.str);
      }

      return null, false;
   }

   emit_list :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      if !v.list.count return null, true;

      car := v.list[0];
      if car.kind == {
         case .Operator;
            n, ok := emit_op(v, types);
            return n, ok;

         case .Identifier; if car.str == {
         case "mut"; #through;
         case "let";
            cdr := v.list;
            cdr.count -= 1;
            cdr.data  += 1;
            assert(cdr.count > 0, "Let requires arguments");

            lst: Value;
            lst.kind = .List;
            lst.list = cdr;

            n, ok := emit_decl(*lst, types, car.str == "let");
            return n, ok;
         }
      }

      return null, false;
   }

   emit_ident :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      ident := New(Code_Ident);
      ident.kind = .IDENT;
      ident.name = v.str;
      return ident, true;
   }

   // Everything is just a float64 in Jisp
   emit_number :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      literal := New(Code_Literal);

      literal.kind        = .LITERAL;
      literal.value_type  = .NUMBER;
      literal.value_flags = .FLOAT;
      literal._float64    = v.number;

      return literal, true;
   }

   // Because all numbers in Jisp are float64, we just grab the
   // "float64" type instantiation from our table.
   emit_decl :: (v: *Value, types: *Type_Table, const := false) -> *Code_Node, bool {
      car := v.list[0];
      assert(car.kind == .Identifier, "Expected ident for decl");

      decl := New(Code_Declaration);
      decl.name           = car.str;
      decl.type_inst      = <<table_find_pointer(types, "float64");
      if const decl.flags = .IS_CONSTANT;

      if v.list.count > 1 {
         expr, ok := emit_value(*v.list[1], types);
         if !ok return null, false;
         decl.expression = expr;
      }

      return decl, true;
   }

   emit_import :: (v: *Value, types: *Type_Table) -> *Code_Node, bool {
      import := New(Code_Directive_Import);
      import.kind        = .DIRECTIVE_IMPORT;
      import.name        = v.str;
      import.import_type = .SHORT_NAME;
      return import, true;
   }

   // The code for compilation starts here 

   source := str;

   values, p_ok := parse(*source);
   assert(p_ok, "Unable to parse!");

   // Emit Jai Code_Nodes and call print_expression on them.

   b: String_Builder;

   for * values if it.kind != .None {
      node, e_ok := emit_value(it, *type_insts);
      assert(e_ok, "Unable to emit!");
      if !node continue;
      print_expression(*b, node);
      append(*b, ";\n");
   }

   // Give the "module" we created back to the compiler
   return builder_to_string(*b);
}

#import "File";
#import "Basic";
#import "String";
#import "Compiler";
#import "Hash_Table";
#import "Program_Print";
