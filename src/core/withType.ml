open Type

type with_type_source_information = {
	si_name : string;
	si_doc : string option;
}

type with_type_source =
	| FunctionArgument of with_type_source_information
	| StructureField of with_type_source_information
	| ImplicitReturn
	| LocalVariable of string

type t =
	| NoValue
	| Value of with_type_source option
	| WithType of Type.t * with_type_source option

let make_with_type_source_information name doc = {
	si_name = name;
	si_doc = doc;
}

let with_type t = WithType(t,None)
let of_implicit_return t = WithType(t,Some ImplicitReturn)
let with_argument t name = WithType(t,Some(FunctionArgument (make_with_type_source_information name None)))
let with_argument_and_doc t name doc = WithType(t,Some(FunctionArgument (make_with_type_source_information name (Some doc))))
let with_structure_field t name = WithType(t,Some(StructureField (make_with_type_source_information name None)))
let with_local_variable t name = WithType(t,Some(LocalVariable name))
let value = Value None
let named_argument name = Value (Some(FunctionArgument (make_with_type_source_information name None)))
let named_structure_field name = Value (Some(StructureField (make_with_type_source_information name None)))
let no_value = NoValue

let to_string = function
	| NoValue -> "NoValue"
	| Value (None | Some ImplicitReturn) -> "Value"
	| Value (Some(FunctionArgument si | StructureField si)) -> "Value " ^ si.si_name
	| Value (Some(LocalVariable name)) -> "Value " ^ name
	| WithType(t,s) ->
		let name = match s with
			| Some(FunctionArgument si | StructureField si) -> si.si_name
			| Some(LocalVariable name) -> name
			| _ -> "None"
		in
		Printf.sprintf "WithType(%s, %s)" (s_type (print_context()) t) name

let get_expected_name with_type = match with_type with
	| Value (Some src) | WithType(_,Some src) ->
		(match src with
		| FunctionArgument si -> Some si.si_name
		| StructureField si -> Some si.si_name
		| LocalVariable name -> Some name
		| ImplicitReturn -> None
		)
	| _ ->
		None