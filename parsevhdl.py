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
label = identifier


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

designator = identifier | operator_symbol

formal_parameter_list = interface_list

subprogram_specification = (
	( Keyword('PROCEDURE') + designator + Optional(Literal('(') + formal_parameter_list + Literal(')')) ) |
	( Optional(Keyword('PURE') | Keyword('IMPURE')) + Keyword('FUNCTION') + designator + Optional( Literal('(') + formal_parameter_list + Literal(')') ) + Keyword('RETURN') + type_mark )
    )

subprogram_declaration = subprogram_specification + Literal(';')

subprogram_declarative_item = Forward()

subprogram_declarative_part = ZeroOrMore(subprogram_declarative_item)

sensitivity_list = delimitedList(name)

sensitivity_clause = Keyword('ON') + sensitivity_list

condition = expression

condition_clause = Keyword('UNTIL') + condition

timeout_clause = Keyword('FOR') + expression

wait_statement = Optional(label + Literal(':')) + Keyword('WAIT') + Optional(sensitivity_clause) + Optional(condition_clause) + Optional(timeout_clause) + Literal(';')

assertion = (
	Keyword('ASSERT') + condition +
		Optional(Keyword('REPORT') + expression) +
		Optional(Keyword('SEVERITY') + expression)
        )

assertion_statement = Optional(label + Literal(':')) + assertion + Literal(';')

report_statement = (
	Optional(label + Literal(':')) +
		Keyword('REPORT') + expression +
			Optional(Keyword('SEVERITY') + expression) + Literal(';')
            )

target = name | aggregate

delay_mechanism = Keyword('TRANSPORT') | (Optional(Keyword('REJECT') + expression) + Keyword('INERTIAL'))

waveform_element = (expression + Optional(Keyword('AFTER') + expression)) | (Keyword('NULL') + Optional(Keyword('AFTER') + expression))

waveform = delimitedList(waveform_element) | Keyword('UNAFFECTED')

signal_assignment_statement = Optional(label + Literal(':')) + target + Keyword('<=') + Optional(delay_mechanism) + waveform + Literal(';')

variable_assignment_statement = target + Keyword(':=') + expression + Literal(';')

procedure_call = name + Optional( Literal('(') + actual_parameter_part + Literal(')') )

procedure_call_statement = Optional(label + Literal(':')) + procedure_call + Literal(';')

sequential_statement = Forward()

sequence_of_statements = ZeroOrMore(sequential_statement)

if_statement = (
	Optional(label + Literal(':')) +
		Keyword('IF') + condition + Keyword('THEN') +
			sequence_of_statements +
		ZeroOrMore( Keyword('ELSIF') + condition + Keyword('THEN') +
			sequence_of_statements ) +
		Optional(Keyword('ELSE') +
			sequence_of_statements) +
		Keyword('END') + Keyword('IF') + Optional(label) + Literal(';')
        )

case_statement_alternative = (
	Keyword('WHEN') + choices + Keyword('=>') +
		sequence_of_statements )

case_statement = (
	Optional(label + Literal(':') ) +
		Keyword('CASE') + expression + Keyword('IS') +
			OneOrMore(case_statement_alternative) +
		Keyword('END') + Keyword('CASE') + Optional( label ) + Literal(';')
        )

parameter_specification = identifier + Keyword('IN') + discrete_range

iteration_scheme = (Keyword('WHILE') + condition) | (Keyword('FOR') + parameter_specification)

loop_statement = (
	Optional(label + Literal(':') ) +
		Optional( iteration_scheme ) + Keyword('LOOP') +
			sequence_of_statements +
		Keyword('END') + Keyword('LOOP') + Optional(label) + Literal(';')
        )

next_statement = Optional(label + Literal(':')) + Keyword('NEXT') + Optional(label) + Optional(Keyword('WHEN') + condition) + Literal(';')

exit_statement = Optional(label + Literal(':')) + Keyword('EXIT') + Optional(label) + Optional(Keyword('WHEN') + condition) + Literal(';')

return_statement = Optional(label + Literal(':')) + Keyword('RETURN') + Optional(expression) + Literal(';')

null_statement = Optional(label + Literal(':')) + Keyword('NULL') + Literal(';')

sequential_statement << (
	wait_statement
	| assertion_statement
	| report_statement
	| signal_assignment_statement
	| variable_assignment_statement
	| procedure_call_statement
	| if_statement
	| case_statement
	| loop_statement
	| next_statement
	| exit_statement
	| return_statement
	| null_statement
    )

subprogram_statement_part = ZeroOrMore(sequential_statement)

subprogram_kind = Keyword('PROCEDURE') | Keyword('FUNCTION')

subprogram_body = (
	subprogram_specification + Keyword('IS') +
		subprogram_declarative_part +
	Keyword('BEGIN') +
		subprogram_statement_part +
	Keyword('END') + Optional(subprogram_kind) + Optional( designator ) + Literal(';')
    )

enumeration_type_definition = Literal('(') + delimitedList(enumeration_literal) + Literal(')')

integer_type_definition = range_constraint

floating_type_definition = range_constraint

base_unit_declaration = identifier + Literal(';')

secondary_unit_declaration = identifier + Literal('=') + physical_literal + Literal(';')

physical_type_definition = (
	range_constraint +
		Keyword('UNITS') +
			base_unit_declaration +
			ZeroOrMore(secondary_unit_declaration) +
		Keyword('END') + Keyword('UNITS') + Optional(simple_name)
        )

scalar_type_definition = enumeration_type_definition | integer_type_definition | floating_type_definition | physical_type_definition

index_subtype_definition = type_mark + range_ + Literal('<>')

unconstrained_array_definition = (
	Keyword('ARRAY') + Literal('(') + delimitedList(index_subtype_definition) + Literal(')') +
		Keyword('OF') + subtype_indication
        )

constrained_array_definition = Keyword('ARRAY') + index_constraint + Keyword('OF') + subtype_indication

array_type_definition = unconstrained_array_definition | constrained_array_definition

element_subtype_definition = subtype_indication

element_declaration = identifier_list + Literal(':') + element_subtype_definition + Literal(';')

record_type_definition = (
	Keyword('RECORD') +
		OneOrMore(element_declaration) +
	Keyword('END') + Keyword('RECORD') + Optional(simple_name)
    )

composite_type_definition = array_type_definition | record_type_definition

access_type_definition = Keyword('ACCESS') + subtype_indication

file_type_definition = Keyword('FILE') + Keyword('OF') + type_mark

type_definition = (
	scalar_type_definition
	| composite_type_definition
	| access_type_definition
	| file_type_definition
    )

full_type_declaration = Keyword('TYPE') + identifier + Keyword('IS') + type_definition + Literal(';')

incomplete_type_declaration = Keyword('TYPE') + identifier + Literal(';')

type_declaration = full_type_declaration | incomplete_type_declaration

subtype_declaration = Keyword('SUBTYPE') + identifier + Keyword('IS') + subtype_indication + Literal(';')

constant_declaration = Keyword('CONSTANT') + identifier_list + Literal(':') + subtype_indication + Optional(Keyword(':=') + expression) + Literal(';')

variable_declaration = Optional(Keyword('SHARED')) + Keyword('VARIABLE') + identifier_list + Literal(':') + subtype_indication + Optional(Keyword(':=') + expression) + Literal(';')

file_logical_name = expression

file_open_information = Optional(Keyword('OPEN') + expression) + Keyword('IS') + file_logical_name

file_declaration = Keyword('FILE') + identifier_list + Literal(':') + subtype_indication + Optional(file_open_information) + Literal(';')

alias_designator = identifier | character_literal | operator_symbol

alias_declaration = Keyword('ALIAS') + alias_designator + Optional(Literal(':') + subtype_indication) + Keyword('IS') + name + Optional(signature) + Literal(';')

attribute_declaration = Keyword('ATTRIBUTE') + identifier + Literal(':') + type_mark + Literal(';')

entity_tag = simple_name | character_literal | operator_symbol

entity_designator = entity_tag + Optional(signature)

entity_name_list = delimitedList(entity_designator) | Keyword('OTHERS') | Keyword('ALL')

entity_class = (
	Keyword('ENTITY')	     | Keyword('ARCHITECTURE')  | Keyword('CONFIGURATION')
	| Keyword('PROCEDURE')  | Keyword('FUNCTION')	     | Keyword('PACKAGE')
	| Keyword('TYPE')	     | Keyword('SUBTYPE')	     | Keyword('CONSTANT')
	| Keyword('SIGNAL')     | Keyword('VARIABLE')	     | Keyword('COMPONENT')
	| Keyword('LABEL')	     | Keyword('LITERAL')	     | Keyword('UNITS')
	| Keyword('GROUP')	     | Keyword('FILE')
    )

entity_specification = entity_name_list + Literal(':') + entity_class

attribute_specification = Keyword('ATTRIBUTE') + attribute_designator + Keyword('OF') + entity_specification + Keyword('IS') + expression + Literal(';')

use_clause = Keyword('USE') + delimitedList(selected_name) + Literal(';')

entity_class_entry = entity_class + Optional(Literal('<>'))

entity_class_entry_list = delimitedList(entity_class_entry)

group_template_declaration = Keyword('GROUP') + identifier + Keyword('IS') + Literal('(') + entity_class_entry_list + Literal(')') + Literal(';')

group_constituent = name | character_literal

group_constituent_list = delimitedList(group_constituent)

group_declaration = Keyword('GROUP') + identifier + Literal(':') + name + Literal('(') + group_constituent_list + Literal(')') + Literal(';')

subprogram_declarative_item << (
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration
    )

signal_kind = Keyword('REGISTER') | Keyword('BUS')

signal_declaration = Keyword('signal') + identifier_list + Literal(':') + subtype_indication + Optional(signal_kind) + Optional(Keyword(':=') + expression) + Literal(';')

signal_list = delimitedList(name) | Keyword('OTHERS') | Keyword('ALL')

guarded_signal_specification = signal_list + Literal(':') + type_mark

disconnection_specification = Keyword('DISCONNECT') + guarded_signal_specification + Keyword('AFTER') + expression + Literal(';')

entity_declarative_item = (
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
    )

entity_declarative_part = ZeroOrMore(entity_declarative_item)

concurrent_assertion_statement = Optional(label + Literal(':')) + Optional(Keyword('POSTPONED')) + assertion + Literal(';')

concurrent_procedure_call_statement = Optional(label + Literal(':')) + Optional(Keyword('POSTPONED')) + procedure_call + Literal(';')

process_declarative_item = (
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration
    )

process_declarative_part = ZeroOrMore(process_declarative_item)

process_statement_part = ZeroOrMore(sequential_statement)

process_statement = (
    Optional( label + Literal(':') ) +
		Optional(Keyword('POSTPONED')) + Keyword('PROCESS') + Optional( Literal('(') + sensitivity_list + Literal(')')) + Optional(Keyword('IS')) +
			process_declarative_part +
		Keyword('BEGIN') +
			process_statement_part +
		Keyword('END') + Optional(Keyword('POSTPONED')) + Keyword('PROCESS') + Optional(label) + Literal(';')
        )

entity_statement = concurrent_assertion_statement | concurrent_procedure_call_statement | process_statement

entity_statement_part = ZeroOrMore(entity_statement)

entity_declaration = (
    Keyword('ENTITY') + identifier + Keyword('IS') +
		entity_header +
        entity_declarative_part +
        Optional( Keyword('BEGIN') + entity_statement_part ) +
	Keyword('END') + Optional( Keyword('ENTITY') ) + Optional( simple_name ) + Literal(';')
)

configuration_declarative_item = use_clause | attribute_specification | group_declaration

configuration_declarative_part = ZeroOrMore(configuration_declarative_item)

index_specification = discrete_range | expression

block_specification = name | label | (label + Optional(Literal('(') + index_specification + Literal(')')))

block_configuration = Forward()

instantiation_list = delimitedList(label) | Keyword('OTHERS') | Keyword('ALL')

component_specification = instantiation_list + Literal(':') + name

entity_aspect = (
	  (Keyword('ENTITY') + name + Optional( Literal('(') + identifier + Literal(')') ))
	| (Keyword('CONFIGURATION') + name)
	| Keyword('OPEN')
    )

generic_map_aspect = Keyword('GENERIC') + Keyword('MAP') + Literal('(') + association_list + Literal(')')

port_map_aspect = Keyword('PORT') + Keyword('MAP') + Literal('(') + association_list + Literal(')')

binding_indication = (
	Optional( Keyword('USE') + entity_aspect ) +
	Optional( generic_map_aspect ) +
	Optional( port_map_aspect )
        )

component_configuration = (
	Keyword('FOR') + component_specification +
		Optional(binding_indication + Literal(';')) +
		Optional(block_configuration) +
	Keyword('END') + Keyword('FOR') + Literal(';')
    )

configuration_item = block_configuration | component_configuration

block_configuration << (
	Keyword('FOR') + block_specification +
		ZeroOrMore(use_clause) +
		ZeroOrMore(configuration_item) +
	Keyword('END') + Keyword('FOR') + Literal(';')
    )

configuration_declaration = (
	Keyword('CONFIGURATION') + identifier + Keyword('OF') + name + Keyword('IS') +
		configuration_declarative_part +
		block_configuration +
	Keyword('END') + Optional('CONFIGURATION') + Optional(simple_name) + Literal(';')
    )

component_declaration = (
	Keyword('COMPONENT') + identifier + Optional(Keyword('IS')) +
		Optional(generic_clause) +
		Optional(port_clause) +
	Keyword('END') + Keyword('COMPONENT') + Optional(simple_name) + Literal(';')
    )

package_declarative_item = (
	subprogram_declaration
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
    )

package_declarative_part = ZeroOrMore(package_declarative_item)

package_declaration = (
	Keyword('PACKAGE') + identifier + Keyword('IS') +
		package_declarative_part +
	Keyword('END') + Optional(Keyword('PACKAGE')) + Optional(simple_name) + Literal(';')
    )

primary_unit = (
	entity_declaration
	| configuration_declaration
	| package_declaration
    )

configuration_specification = Keyword('FOR') + component_specification + binding_indication + Literal(';')

block_declarative_item = (
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| configuration_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
    )

architecture_declarative_part = ZeroOrMore(block_declarative_item)

block_header = (
	Optional(generic_clause +
	         Optional(generic_map_aspect + Literal(';'))) +
	Optional(port_clause +
             Optional(port_map_aspect + Literal(';')))
    )

block_declarative_part = ZeroOrMore(block_declarative_item)

concurrent_statement = Forward()

block_statement_part = ZeroOrMore(concurrent_statement)

block_statement = (
    label + Literal(':') +
		Keyword('BLOCK') + Optional( Literal('(') + expression + Literal(')') ) + Optional(Keyword('IS')) +
			block_header +
			block_declarative_part +
		Keyword('BEGIN') +
			block_statement_part +
		Keyword('END') + Keyword('BLOCK') + Optional(label) + Literal(';')
        )

options = Optional(Keyword('GUARDED')) + Optional(delay_mechanism)

conditional_waveforms = (
	ZeroOrMore( waveform + Keyword('WHEN') + condition + Keyword('ELSE') ) +
	waveform + Optional(Keyword('WHEN') + condition)
    )

conditional_signal_assignment = target + Keyword('<=') + options + conditional_waveforms + Literal(';')

selected_waveforms = (
	ZeroOrMore(waveform + Keyword('WHEN') + choices + Literal(',')) +
	waveform + Keyword('WHEN') + choices
    )

selected_signal_assignment = (
	Keyword('WITH') + expression + Keyword('SELECT') +
		target + Keyword('<=') + options + selected_waveforms + Literal(';')
        )

concurrent_signal_assignment_statement = (
	  (Optional(label + Literal(':')) + Optional(Keyword('POSTPONED')) + conditional_signal_assignment)
	| (Optional(label + Literal(':')) + Optional(Keyword('POSTPONED')) + selected_signal_assignment)
    )

instantiated_unit = (
	(Optional(Keyword('COMPONENT')) + name)
	| (Keyword('ENTITY') + name + Optional( Literal('(') + identifier + Literal(')') ))
	| (Keyword('CONFIGURATION') + name)
    )

component_instantiation_statement = (
	label + Literal(':') +
		instantiated_unit +
			Optional(generic_map_aspect) +
			Optional(port_map_aspect) + Literal(';')
            )

generation_scheme = (
	(Keyword('FOR') + parameter_specification)
	| (Keyword('IF') + condition)
    )

generate_statement = (
	label + Literal(':') +
		generation_scheme + Keyword('GENERATE') +
			Optional( ZeroOrMore(block_declarative_item) +
		Keyword('BEGIN')) +
			ZeroOrMore(concurrent_statement) +
		Keyword('END') + Keyword('GENERATE') + Optional(label) + Literal(';')
        )

concurrent_statement << (
	block_statement
	| process_statement
	| concurrent_procedure_call_statement
	| concurrent_assertion_statement
	| concurrent_signal_assignment_statement
	| component_instantiation_statement
	| generate_statement
    )

architecture_statement_part = ZeroOrMore(concurrent_statement)

architecture_body = (
	Keyword('ARCHITECTURE') + identifier + Keyword('OF') + name + Keyword('IS') +
		architecture_declarative_part +
	Keyword('BEGIN') +
		architecture_statement_part +
	Keyword('END') + Optional(Keyword('ARCHITECTURE')) + Optional(simple_name) + Literal(';')
    )

package_body_declarative_item = (
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| use_clause
	| group_template_declaration
	| group_declaration
    )

package_body_declarative_part = ZeroOrMore(package_body_declarative_item)

package_body = (
	Keyword('PACKAGE') + Keyword('BODY') + simple_name + Keyword('IS') +
		package_body_declarative_part +
	Keyword('END') + Optional(Keyword('PACKAGE') + Keyword('BODY')) + Optional(simple_name) + Literal(';')
    )

secondary_unit = architecture_body | package_body

library_unit = primary_unit | secondary_unit

logical_name = identifier

logical_name_list = delimitedList(logical_name)

library_clause = Keyword('LIBRARY') + logical_name_list + Literal(';')

context_item = library_clause | use_clause

context_clause = ZeroOrMore(context_item)

design_unit = context_clause + library_unit

design_file = OneOrMore(design_unit)

'''
basic_graphic_character = upper_case_letter | digit | special_character| space_character
graphic_character = basic_graphic_character | lower_case_letter | other_special_character

basic_character ::=
	basic_graphic_character <#basic_graphic_character> | format_effector

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

letter ::= upper_case_letter | lower_case_letter

letter_or_digit ::= letter <#letter> | digit

logical_operator ::= *AND* | *OR* | *NAND* | *NOR* | *XOR* | *XNOR*

miscellaneous_operator ::= **** | *ABS* | *NOT*

object_declaration ::=
	constant_declaration <#constant_declaration>
	| signal_declaration <#signal_declaration>
	| variable_declaration <#variable_declaration>
	| file_declaration <#file_declaration>

'''

if __name__ == '__main__':
    design_file.parseFile('dinges.vhd')




        


