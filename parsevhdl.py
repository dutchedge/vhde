#!/usr/bin/env python3

from pyparsing import *

# 
#     Hyperlinked VHDL-93 BNF Syntax
# 
# ------------------------------------------------------------------------
# HTML version by Gerhard Petrowitsch <mailto:gerhard.petrowitsch@sp3d.de>
# - SP3D Chip Design GmbH <http://www.sp3dtech.com/>.
# Please contact me for comments and bug reports.
# 
# ------------------------------------------------------------------------
# Color Legend:
# 
#   * blue:
#     	Link, that has not been followed yet
#   * pink:
#     	Link, that has already been followed
#   * brown:
#     	VHDL-Keyword or required character(s)
#   * white:
#     	other text
# 
# ------------------------------------------------------------------------
#

basic_identifier = Word(alphas, alphanums + '_')
extended_identifier = Word(printables)
identifier = basic_identifier | extended_identifier
string_literal = dblQuotedString
sign = oneOf('+ -')

identifier_list = delimitedList(identifier)


integer = Word(nums, nums + '_')

exponent = (Literal('E') + Optional(Literal('+')) + integer) | (Literal('E') + Literal('-') + integer)

decimal_literal = integer + Optional(Literal('.') + integer) + Optional( exponent )

based_integer = Word(alphanums, alphanums + '_')

base = integer

based_literal =	base + Literal('#') + based_integer + Optional(Literal('.') + based_integer) + Literal('#') + Optional(exponent)

abstract_literal = decimal_literal | based_literal

name = Forward()

physical_literal = Optional(abstract_literal) + name

numeric_literal = abstract_literal | physical_literal

#basic_graphic_character = upper_case_letter | digit | special_character| space_character
#graphic_character = basic_graphic_character | lower_case_letter | other_special_character

character_literal = Combine(Literal("'") + Word(printables, max = 1) + Literal("'"))

enumeration_literal = identifier | character_literal

bit_value = Word(alphanums, alphanums + '_')

base_specifier = oneOf('B O X')

bit_string_literal = base_specifier + Literal('"') + bit_value + Literal('"')

literal = (
	numeric_literal
	| enumeration_literal
	| string_literal
	| bit_string_literal
	| Literal('NULL') )

adding_operator = oneOf('+ - &')
multiplying_operator = oneOf('* / MOD REM')

direction = Keyword('TO') | Keyword('DOWNTO')

shift_operator = Keyword('SLL') | Keyword('SRL') | Keyword('SLA') | Keyword('SRA') | Keyword('ROL') | Keyword('ROR')

mode = Keyword('IN') | Keyword('OUT') | Keyword('INOUT') | Keyword('BUFFER') | Keyword('LINKAGE')


primary = Forward()






expression = Forward()






factor = ( (primary + ZeroOrMore(Literal('**') + primary)) |
           (Literal('ABS') + primary) |
	       (Literal('NOT') + primary) )

term = factor + ZeroOrMore(multiplying_operator + factor)

simple_expression = Optional(sign) + term + ZeroOrMore(adding_operator + term)

prefix = Forward()

type_mark = name
signature = Optional( Optional(delimitedList(type_mark)) + Optional( Literal('return') + type_mark ) )

simple_name = identifier
attribute_designator = simple_name

attribute_name = prefix + Optional(signature) + Literal("'") + attribute_designator + Optional( Literal('(') + expression + Literal(')') )

range_ = attribute_name | (simple_expression + direction + simple_expression)

subtype_indication = Forward()
discrete_range = subtype_indication | range_

choice = (
	simple_expression
	| discrete_range
	| simple_name
	| Literal('OTHERS') )

choices = delimitedList(choice, delim='|')

element_association = Optional(choices + Literal('=>')) + expression

aggregate = Literal('(') + delimitedList(element_association) + Literal(')')

shift_expression = simple_expression + Optional( shift_operator + simple_expression )

relational_operator = oneOf('= /= < <= > >=')

relation = shift_expression + Optional( relational_operator + shift_expression )

expression << ( (relation + ZeroOrMore(Keyword('AND') + relation)) |
               (relation + ZeroOrMore(Keyword('OR') + relation)) |
               (relation + ZeroOrMore(Keyword('XOR') + relation)) |
               (relation + ZeroOrMore(Keyword('NAND') + relation)) |
               (relation + ZeroOrMore(Keyword('NOR') + relation)) |
               (relation + ZeroOrMore(Keyword('XNOR') + relation)) )

formal_designator = name

formal_part = (formal_designator |
               (name + Literal('(') + formal_designator + Literal(')')) |
               (type_mark + Literal('(') + formal_designator + Literal(')')))

actual_designator = (expression |
                     name |
                     Keyword('OPEN'))

actual_part = (actual_designator |
               (name + Literal('(') + actual_designator + Literal(')')) |
               (type_mark + Literal('(') + actual_designator + Literal(')')))

association_element = Optional( formal_part + Literal('=>') ) + actual_part

association_list = delimitedList(association_element)

actual_parameter_part = association_list

function_call = name + Optional( Literal('(') + actual_parameter_part + Literal(')') )

prefix << name | function_call

qualified_expression = ( (type_mark + Literal("'") + Literal('(') + expression + Literal(')')) |
	                     (type_mark + Literal("'") + aggregate) )

type_conversion = type_mark + Literal('(') + expression + Literal(')')

allocator = ( (Keyword('NEW') + subtype_indication) |
	          (Keyword('NEW') + qualified_expression) )

primary << (
	name
	| literal
	| aggregate
	| function_call
	| qualified_expression
	| type_conversion
	| allocator
	| (Literal('(') + expression + Literal(')'))
    )

operator_symbol = string_literal

suffix = ( simple_name
	| character_literal
	| operator_symbol
	| Keyword('ALL') )


selected_name = prefix + Literal('.') + suffix
indexed_name = prefix + Literal('(') + delimitedList(expression) + Literal(')')

slice_name = prefix + Literal('(') + discrete_range + Literal(')')





name << ( simple_name |
          operator_symbol |
          selected_name |
          indexed_name |
          slice_name |
          attribute_name )

range_constraint = range_ + range_

index_constraint = Literal('(') + delimitedList(discrete_range) + Literal(')')

constraint = range_constraint | index_constraint

subtype_indication << Optional(name) + type_mark + Optional( constraint )


'''
access_type_definition ::= *ACCESS* subtype_indication <#subtype_indication>

alias_declaration ::=
	*ALIAS* alias_designator <#alias_designator> [ *:* subtype_indication <#subtype_indication> ] *IS* name <#name> [ signature <#signature> ] 	*;*

alias_designator ::= identifier <#identifier> | character_literal <#character_literal> | operator_symbol <#operator_symbol>

architecture_body ::=
	*ARCHITECTURE* identifier <#identifier> *OF* entity_name <#name> *IS*
		architecture_declarative_part <#architecture_declarative_part>
	*BEGIN*
		architecture_statement_part <#architecture_statement_part>
	*END* [ *ARCHITECTURE* ] [ architecture_simple_name <#simple_name> ] *;*

architecture_declarative_part ::=
	{ block_declarative_item <#block_declarative_item> }

architecture_statement_part ::=
	{ concurrent_statement <#concurrent_statement> }

array_type_definition ::=
	unconstrained_array_definition <#unconstrained_array_definition>	|   constrained_array_definition <#constrained_array_definition>

assertion ::=
	*ASSERT* condition <#condition>
		[ *REPORT* expression <#expression> ]
		[ *SEVERITY* expression <#expression> ]

assertion_statement ::=	 [ label <#label> *:* ] assertion <#assertion> *;*

attribute_declaration ::=
	*ATTRIBUTE* identifier <#identifier> *:* type_mark <#type_mark> *;*

attribute_specification ::=
	*ATTRIBUTE* attribute_designator <#attribute_designator> *OF* entity_specification <#entity_specification> *IS* expression <#expression> *;*

base_unit_declaration ::= identifier <#identifier> *;*

basic_character ::=
	basic_graphic_character <#basic_graphic_character> | format_effector

'''


'''
binding_indication ::=
	[ *USE* entity_aspect <#entity_aspect> ]
	[ generic_map_aspect <#generic_map_aspect> ]
	[ port_map_aspect <#port_map_aspect> ]

block_configuration ::=
	*FOR* block_specification <#block_specification>
		{ use_clause <#use_clause> }
		{ configuration_item <#configuration_item> }
	*END FOR ;*

block_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| subprogram_body <#subprogram_body>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| signal_declaration <#signal_declaration>
	| shared_variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| component_declaration <#component_declaration>
	| attribute_declaration <#attribute_declaration>
	| attribute_specification <#attribute_specification>
	| configuration_specification <#configuration_specification>
	| disconnection_specification <#disconnection_specification>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

block_declarative_part ::=
	{ block_declarative_item <#block_declarative_item> }

block_header ::=
	[ generic_clause <#generic_clause>
	[ generic_map_aspect <#generic_map_aspect> *;* ] ]
	[ port_clause <#port_clause>
	[ port_map_aspect <#port_map_aspect> *;* ] ]

block_specification ::=
	architecture_name <#name>
	| block_statement_label <#label>
	| generate_statement_label <#label> [ *(* index_specification <#index_specification> *)* ]

block_statement ::=
	block_label <#label> *:*
		*BLOCK* [ *(* guard_expression <#expression> *)* ] [ *IS* ]
			block_header <#block_header>
			block_declarative_part <#block_declarative_part>
		*BEGIN*
			block_statement_part <#block_statement_part>
		*END BLOCK* [ block_label <#label> ] *;*

block_statement_part ::=
	{ concurrent_statement <#concurrent_statement> }

case_statement ::=
	[ case_label <#label> *:* ]
		*CASE* expression <#expression> *IS*
			case_statement_alternative <#case_statement_alternative>
			{ case_statement_alternative <#case_statement_alternative> }
		*END* *CASE* [ case_label <#label> ] *;*

case_statement_alternative ::=
	*WHEN* choices <#choices> *=>*
		sequence_of_statements <#sequence_of_statements>

component_configuration ::=
	*FOR* component_specification <#component_specification>
		[ binding_indication <#binding_indication> *;* ]
		[ block_configuration <#block_configuration> ]
	*END* *FOR* *;*

component_declaration ::=
	*COMPONENT* identifier <#identifier> [ *IS* ]
		[ local_generic_clause ]
		[ local_port_clause <#port_clause> ]
	*END* *COMPONENT* [ component_simple_name <#simple_name> ] *;*

component_instantiation_statement ::=
	instantiation_label <#label> *:*
		instantiated_unit <#instantiated_unit>
			[ generic_map_aspect <#generic_map_aspect> ]
			[ port_map_aspect <#port_map_aspect> ] *;*

component_specification ::=
	instantiation_list <#instantiation_list> *:* component_name <#name>

composite_type_definition ::=
	array_type_definition <#array_type_definition>
	| record_type_definition <#record_type_definition>

concurrent_assertion_statement ::=
	[ label <#label> *:* ] [ *POSTPONED* ] assertion <#assertion> *;*

concurrent_procedure_call_statement ::=
	[ label <#label> *:* ] [ *POSTPONED* ] procedure_call <#procedure_call> *;*

concurrent_signal_assignment_statement ::=
	  [ label <#label> *:* ] [ *POSTPONED* ] conditional_signal_assignment <#conditional_signal_assignment>
	| [ label <#label> *:* ] [ *POSTPONED* ] selected_signal_assignment <#selected_signal_assignment>

concurrent_statement ::=
	block_statement <#block_statement>
	| process_statement <#process_statement>
	| concurrent_procedure_call_statement <#concurrent_procedure_call_statement>
	| concurrent_assertion_statement <#concurrent_assertion_statement>
	| concurrent_signal_assignment_statement
<#concurrent_signal_assignment_statement>
	| component_instantiation_statement <#component_instantiation_statement>
	| generate_statement <#generate_statement>

condition ::= boolean_expression <#expression>

condition_clause ::= *UNTIL* condition <#condition>

conditional_signal_assignment ::=
	target <#target>	*<=* options <#options> conditional_waveforms <#conditional_waveforms> *;*

conditional_waveforms ::=
	{ waveform <#waveform> *WHEN* condition <#condition> *ELSE* }
	waveform <#waveform> [ *WHEN* condition <#condition> ]

configuration_declaration ::=
	*CONFIGURATION* identifier <#identifier> *OF* entity_name <#name> *IS*
		configuration_declarative_part <#configuration_declarative_part>
		block_configuration <#block_configuration>
	*END* [ *CONFIGURATION* ] [ configuration_simple_name <#simple_name> ] *;*

configuration_declarative_item ::=
	use_clause <#use_clause>
	| attribute_specification <#attribute_specification>
	| group_declaration <#group_declaration>

configuration_declarative_part ::=
	{ configuration_declarative_item <#configuration_declarative_item> }

configuration_item ::=
	block_configuration <#block_configuration>
	| component_configuration <#component_configuration>

configuration_specification ::=
	*FOR* component_specification <#component_specification> binding_indication <#binding_indication> *;*

constant_declaration ::=
	*CONSTANT* identifier_list <#identifier_list> *:* subtype_indication <#subtype_indication> [ *:=* expression <#expression> ] *;*

constrained_array_definition ::=
	*ARRAY* index_constraint <#index_constraint> *OF* element_subtype_indication <#subtype_indication>

context_clause ::= { context_item <#context_item> }

context_item ::=
	library_clause <#library_clause>
	| use_clause <#use_clause>

declaration ::=
	type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| object_declaration <#object_declaration>
	| interface_declaration <#interface_declaration>
	| alias_declaration <#alias_declaration>
	| attribute_declaration <#attribute_declaration>
	| component_declaration <#component_declaration>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>
	| entity_declaration <#entity_declaration>
	| configuration_declaration <#configuration_declaration>
	| subprogram_declaration <#subprogram_declaration>
	| package_declaration <#package_declaration>

delay_mechanism ::=
	*TRANSPORT*
	| [ *REJECT* time_expression <#expression> ] *INERTIAL*

design_file ::= design_unit <#design_unit> { design_unit <#design_unit> }

design_unit ::= context_clause <#context_clause> library_unit <#library_unit>

designator ::= identifier <#identifier>  |  operator_symbol <#operator_symbol>

disconnection_specification ::=
	*DISCONNECT* guarded_signal_specification <#guarded_signal_specification> *AFTER* time_expression <#expression> *;*

element_declaration ::=
	identifier_list <#identifier_list> *:* element_subtype_definition <#element_subtype_definition> *;*

element_subtype_definition ::= subtype_indication <#subtype_indication>

entity_aspect ::=
	  *ENTITY* entity_name <#name> [ *(* architecture_identifier*)* ]
	| *CONFIGURATION* configuration_name <#name>
	| *OPEN*

entity_class ::=
	*ENTITY*	     | *ARCHITECTURE*  | *CONFIGURATION*
	| *PROCEDURE*  | *FUNCTION*	     | *PACKAGE*
	| *TYPE*	     | *SUBTYPE*	     | *CONSTANT*
	| *SIGNAL*     | *VARIABLE*	     | *COMPONENT*
	| *LABEL*	     | *LITERAL*	     | *UNITS*
	| *GROUP*	     | *FILE*

entity_class_entry ::=	entity_class <#entity_class> [ *<>* ]

entity_class_entry_list ::=
	entity_class_entry <#entity_class_entry> { *,* entity_class_entry <#entity_class_entry> }
'''
interface_constant_declaration = Optional(Keyword('CONSTANT')) + identifier_list + Literal(':') + Optional(Keyword('IN')) + subtype_indication + Optional( Literal(':=') + expression )

interface_signal_declaration = Optional(Keyword('SIGNAL')) + identifier_list + Literal(':') + Optional(mode) + subtype_indication + Optional(Keyword('BUS')) + Optional(Literal(':=') + expression)

interface_variable_declaration = Optional(Keyword('VARIABLE')) + identifier_list + Literal(':') + Optional(mode) + subtype_indication + Optional(Literal(':=') + expression)

interface_file_declaration = Keyword('FILE') + identifier_list + Literal(':') + subtype_indication


interface_declaration = ( interface_constant_declaration |
                          interface_signal_declaration |
                          interface_variable_declaration |
                          interface_file_declaration )

interface_element = interface_declaration

interface_list = delimitedList(interface_element, delim=';')

port_list = interface_list
port_clause = Keyword('PORT') + Literal('(') + port_list + Literal(')') + Literal(';')

generic_list = interface_list

generic_clause = Keyword('GENERIC') + Literal('(') + generic_list + Literal(')') + Literal(';')

entity_header = (
	Optional(generic_clause) +
	Optional(port_clause)
)

entity_declaration = (
    Keyword('ENTITY') + identifier + Keyword('IS') +
		entity_header +
        entity_declarative_part +
        Optional( Keyword('BEGIN') + entity_statement_part ) +
	Keyword('END') + Optional( Keyword('ENTITY') ) + Optional( entity_simple_name ) + Literal(';')
)

'''
entity_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| subprogram_body <#subprogram_body>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| signal_declaration <#signal_declaration>
	| shared_variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| attribute_declaration <#attribute_declaration>
	| attribute_specification <#attribute_specification>
	| disconnection_specification <#disconnection_specification>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

entity_declarative_part ::=
	{ entity_declarative_item <#entity_declarative_item> }

entity_designator ::= entity_tag <#entity_tag> [ signature <#signature> ]
'''

'''
entity_name_list ::=
	entity_designator <#entity_designator> { *,* entity_designator <#entity_designator> }
	| *OTHERS*
	| *ALL*

entity_specification ::=
	entity_name_list <#entity_name_list> *:* entity_class <#entity_class>

entity_statement ::=
	concurrent_assertion_statement <#concurrent_assertion_statement>
	| passive_concurrent_procedure_call_statement <#concurrent_procedure_call_statement>
	| passive_process_statement <#process_statement>

entity_statement_part ::=
	{ entity_statement <#entity_statement> }

entity_tag ::=	simple_name <#simple_name> | character_literal <#character_literal> | operator_symbol <#operator_symbol>

enumeration_type_definition ::=
	*(* enumeration_literal <#enumeration_literal> { *,* enumeration_literal <#enumeration_literal> } *)*

exit_statement ::=
	[ label <#label> *:* ] *EXIT* [ loop_label <#label> ] [ *WHEN* condition <#condition> ] *;*


'''


'''
file_declaration ::=
	*FILE* identifier_list <#identifier_list> *:* subtype_indication <#subtype_indication> file_open_information <#file_open_information> ] *;*

file_logical_name ::= string_expression <#expression>

file_open_information ::=
	[ *OPEN* file_open_kind_expression <#expression> ] *IS* file_logical_name <#file_logical_name>

file_type_definition ::=
	*FILE*  *OF* type_mark <#type_mark>

floating_type_definition ::=  range_constraint <#range_constraint>

formal_parameter_list ::= parameter_interface_list <#interface_list>

full_type_declaration ::=
	*TYPE* identifier <#identifier> *IS* type_definition <#type_definition> *;*

generate_statement ::=
	generate_label <#label> *:*
		generation_scheme <#generation_scheme> *GENERATE*
			[ { block_declarative_item <#block_declarative_item> }
		*BEGIN* ]
			{ concurrent_statement <#concurrent_statement> }
		*END* *GENERATE* [ generate_label <#label> ] *;*

generation_scheme ::=
	*FOR* generate_parameter_specification <#parameter_specification>
	| *IF* condition <#condition>

generic_map_aspect ::=
	*GENERIC* *MAP* *(* generic_association_list <#association_list> *)*

group_constituent ::= name <#name> | character_literal <#character_literal>

group_constituent_list ::= group_constituent <#group_constituent> { *,* group_constituent <#group_constituent> }

group_template_declaration ::=
	*GROUP* identifier <#identifier> *IS* *(* entity_class_entry_list <#entity_class_entry_list> *)* *;*

group_declaration ::=
	*GROUP* identifier <#identifier> *:* group_template_name <#name> *(* group_constituent_list <#group_constituent_list> *)* *;*

guarded_signal_specification ::=
	guarded_signal_list <#signal_list> *:* type_mark <#type_mark>
'''

'''
if_statement ::=
	[ if_label <#label> *:* ]
		*IF* condition <#condition> *THEN*
			sequence_of_statements <#sequence_of_statements>
		{ *ELSIF* condition <#condition> *THEN*
			sequence_of_statements <#sequence_of_statements> }
		[ *ELSE*
			sequence_of_statements <#sequence_of_statements> ]
		*END* *IF* [ if_label <#label> ] *;*

incomplete_type_declaration ::=	 *TYPE* identifier <#identifier> *;*

index_specification ::=
	discrete_range <#discrete_range>
	| static_expression <#expression>

index_subtype_definition ::= type_mark <#type_mark> range <#range> <>

instantiated_unit ::=
	[ *COMPONENT* ] component_name <#name>
	| *ENTITY* entity_name <#name> [ *(* architecture_identifier *)* ]
	| *CONFIGURATION* configuration_name <#name>

instantiation_list ::=
	instantiation_label <#label> { *,* instantiation_label <#label> }
	| *OTHERS*
	| *ALL*

integer_type_definition ::= range_constraint <#range_constraint>

iteration_scheme ::=
	*WHILE* condition <#condition>
	| *FOR* loop_parameter_specification <#parameter_specification>

label ::= identifier <#identifier>

letter ::= upper_case_letter | lower_case_letter

letter_or_digit ::= letter <#letter> | digit

library_clause ::= *LIBRARY* logical_name_list <#logical_name_list> *;*

library_unit ::=
	primary_unit <#primary_unit>
	| secondary_unit <#secondary_unit>

logical_name ::= identifier <#identifier>

logical_name_list ::= logical_name <#logical_name> { *,* logical_name <#logical_name> }

logical_operator ::= *AND* | *OR* | *NAND* | *NOR* | *XOR* | *XNOR*

loop_statement ::=
	[ loop_label <#label> *:* ]
		[ iteration_scheme <#iteration_scheme> ] *LOOP*
			sequence_of_statements <#sequence_of_statements>
		*END* *LOOP* [ loop_label <#label> ] *;*

miscellaneous_operator ::= **** | *ABS* | *NOT*

next_statement ::=
	[ label <#label> *:* ] *NEXT* [ loop_label <#label> ] [ *WHEN* condition <#condition> ] *;*

null_statement ::= [ label <#label> *:* ] *NULL* *;*

object_declaration ::=
	constant_declaration <#constant_declaration>
	| signal_declaration <#signal_declaration>
	| variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>

options ::= [ *GUARDED* ] [ delay_mechanism <#delay_mechanism> ]

package_body ::=
	*PACKAGE* body package_simple_name <#simple_name> *IS*
		package_body_declarative_part <#package_body_declarative_part>
	*END* [ *PACKAGE* *BODY* ] [ package_simple_name <#simple_name> ] *;*

package_body_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| subprogram_body <#subprogram_body>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| shared_variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

package_body_declarative_part ::=
	{ package_body_declarative_item <#package_body_declarative_item> }

package_declaration ::=
	*PACKAGE* identifier <#identifier> *IS*
		package_declarative_part <#package_declarative_part>
	*END* [ *PACKAGE* ] [ package_simple_name <#simple_name> ] *;*

package_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| signal_declaration <#signal_declaration>
	| shared_variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| component_declaration <#component_declaration>
	| attribute_declaration <#attribute_declaration>
	| attribute_specification <#attribute_specification>
	| disconnection_specification <#disconnection_specification>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

package_declarative_part ::=
	{ package_declarative_item <#package_declarative_item> }

parameter_specification ::=
	identifier <#identifier> *IN* discrete_range <#discrete_range>

physical_type_definition ::=
	range_constraint <#range_constraint>
		*UNITS*
			base_unit_declaration <#base_unit_declaration>
			{ secondary_unit_declaration <#secondary_unit_declaration> }
		*END* *UNITS* [ physical_type_simple_name <#simple_name> ]

port_map_aspect ::=
	*PORT* *MAP* *(* port_association_list <#association_list> *)*

primary_unit ::=
	entity_declaration <#entity_declaration>
	| configuration_declaration <#configuration_declaration>
	| package_declaration <#package_declaration>

procedure_call ::= procedure_name <#name> [ *(* actual_parameter_part <#actual_parameter_part> *)* ]

procedure_call_statement ::=
	[ label <#label> *:* ] procedure_call <#procedure_call> *;*

process_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| subprogram_body <#subprogram_body>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| attribute_declaration <#attribute_declaration>
	| attribute_specification <#attribute_specification>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

process_declarative_part ::=
	{ process_declarative_item <#process_declarative_item> }

process_statement ::=
	[ process_label <#label> *:* ]
		[ *POSTPONED* ] *PROCESS* [ *(* sensitivity_list <#sensitivity_list> *)* ] [ *IS* ]
			process_declarative_part <#process_declarative_part>
		*BEGIN*
			process_statement_part <#process_statement_part>
		*END* [ *POSTPONED* ] *PROCESS* [ process_label <#label> ] *;*

process_statement_part ::=
	{ sequential_statement <#sequential_statement> }

record_type_definition ::=
	*RECORD*
		element_declaration <#element_declaration>
		{ element_declaration <#element_declaration> }
	*END* *RECORD* [ record_type_simple_name <#simple_name> ]

report_statement ::=
	[ label <#label> *:* ]
		*REPORT* expression <#expression>
			[ *SEVERITY* expression <#expression> ] *;*

return_statement ::=
	[ label <#label> *:* ] *RETURN* [ expression <#expression> ] *;*

scalar_type_definition ::=
	enumeration_type_definition <#enumeration_type_definition>   | integer_type_definition <#integer_type_definition>
	| floating_type_definition <#floating_type_definition>	  | physical_type_definition <#physical_type_definition>

secondary_unit ::=
	architecture_body <#architecture_body>
	| package_body <#package_body>

secondary_unit_declaration ::=	identifier <#identifier> = physical_literal <#physical_literal> *;*

selected_signal_assignment ::=
	*WITH* expression <#expression> *SELECT*
		target <#target>	*<=* options <#options> selected_waveforms <#selected_waveforms> *;*

selected_waveforms ::=
	{ waveform <#waveform> *WHEN* choices <#choices> *,* }
	waveform <#waveform> *WHEN* choices <#choices>

sensitivity_clause ::=	*ON* sensitivity_list <#sensitivity_list>

sensitivity_list ::= signal_name <#name> { *,* signal_name <#name> }

sequence_of_statements ::=
	{ sequential_statement <#sequential_statement> }

sequential_statement ::=
	wait_statement <#wait_statement>
	| assertion_statement <#assertion_statement>
	| report_statement <#report_statement>
	| signal_assignment_statement <#signal_assignment_statement>
	| variable_assignment_statement <#variable_assignment_statement>
	| procedure_call_statement <#procedure_call_statement>
	| if_statement <#if_statement>
	| case_statement <#case_statement>
	| loop_statement <#loop_statement>
	| next_statement <#next_statement>
	| exit_statement <#exit_statement>
	| return_statement <#return_statement>
	| null_statement <#null_statement>

signal_assignment_statement ::=
	[ label <#label> *:* ] target <#target> *<=* [ delay_mechanism <#delay_mechanism> ] waveform <#waveform> *;*

signal_declaration ::=
	signal identifier_list <#identifier_list> *:* subtype_indication <#subtype_indication> [ signal_kind <#signal_kind> ] [ *:=* expression <#expression> ] *;*

signal_kind ::=	 *REGISTER*  |  *BUS*

signal_list ::=
	signal_name <#name> { *,* signal_name <#name> }
	| *OTHERS*
	| *ALL*

subprogram_body ::=
	subprogram_specification <#subprogram_specification> *IS*
		subprogram_declarative_part <#subprogram_declarative_part>
	*BEGIN*
		subprogram_statement_part <#subprogram_statement_part>
	*END* [ subprogram_kind <#subprogram_kind> ] [ designator <#designator> ] *;*

subprogram_declaration ::=
	subprogram_specification <#subprogram_specification> *;*

subprogram_declarative_item ::=
	subprogram_declaration <#subprogram_declaration>
	| subprogram_body <#subprogram_body>
	| type_declaration <#type_declaration>
	| subtype_declaration <#subtype_declaration>
	| constant_declaration <#constant_declaration>
	| variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>
	| alias_declaration <#alias_declaration>
	| attribute_declaration <#attribute_declaration>
	| attribute_specification <#attribute_specification>
	| use_clause <#use_clause>
	| group_template_declaration <#group_template_declaration>
	| group_declaration <#group_declaration>

subprogram_declarative_part ::=
	{ subprogram_declarative_item <#subprogram_declarative_item> }

subprogram_kind ::= *PROCEDURE* | *FUNCTION*

subprogram_specification ::=
	*PROCEDURE* designator <#designator> [ *(* formal_parameter_list <#formal_parameter_list> *)* ]
	| [ *PURE* | *IMPURE* ]  *FUNCTION* designator <#designator> [ *(* formal_parameter_list <#formal_parameter_list> *)* ]
		*RETURN* type_mark <#type_mark>

subprogram_statement_part ::=
	{ sequential_statement <#sequential_statement> }

subtype_declaration ::=
	*SUBTYPE* identifier <#identifier> *IS* subtype_indication <#subtype_indication> *;*

target ::=
	name <#name>
	| aggregate <#aggregate>

timeout_clause ::= *FOR* time_expression <#expression>

type_declaration ::=
	full_type_declaration <#full_type_declaration>
	| incomplete_type_declaration <#incomplete_type_declaration>

type_definition ::=
	scalar_type_definition <#scalar_type_definition>
	| composite_type_definition <#composite_type_definition>
	| access_type_definition <#access_type_definition>
	| file_type_definition <#file_type_definition>

unconstrained_array_definition ::=
	*ARRAY* *(* index_subtype_definition <#index_subtype_definition> { *,* index_subtype_definition <#index_subtype_definition> } *)*
		*OF* element_subtype_indication <#subtype_indication>

use_clause ::=
	*USE* selected_name <#selected_name> { *,* selected_name <#selected_name> } *;*

variable_assignment_statement ::=
	[ label <#label> *:* ] target <#target>  *:=* expression <#expression> *;*

variable_declaration ::=
	[ *SHARED* ] *VARIABLE* identifier_list <#identifier_list> *:* subtype_indication <#subtype_indication> [ *:=* expression <#expression> ] *;*

wait_statement ::=
	[ label <#label> *:* ] *WAIT* [ sensitivity_clause <#sensitivity_clause> ] [ condition_clause <#condition_clause> ] [ timeout_clause <#timeout_clause> ] *;*

waveform ::=
	waveform_element <#waveform_element> { *,* waveform_element <#waveform_element> }
	| *UNAFFECTED*

waveform_element ::=
	value_expression <#expression> [ *AFTER* time_expression <#expression> ]
	| *NULL* [ *AFTER* time_expression <#expression> ]
'''

if __name__ == '__main__':
    entity_declaration.parseFile('dinges.vhd')




        


