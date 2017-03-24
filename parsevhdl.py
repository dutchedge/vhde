#!/usr/bin/env python3

from pprint import pprint
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
extended_identifier = QuotedString(quoteChar = '\\')
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

direction = CaselessKeyword('TO') | CaselessKeyword('DOWNTO')

shift_operator = CaselessKeyword('SLL') | CaselessKeyword('SRL') | CaselessKeyword('SLA') | CaselessKeyword('SRA') | CaselessKeyword('ROL') | CaselessKeyword('ROR')

mode = CaselessKeyword('IN') | CaselessKeyword('OUT') | CaselessKeyword('INOUT') | CaselessKeyword('BUFFER') | CaselessKeyword('LINKAGE')


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

expression << ( (relation + ZeroOrMore(CaselessKeyword('AND') + relation)) |
               (relation + ZeroOrMore(CaselessKeyword('OR') + relation)) |
               (relation + ZeroOrMore(CaselessKeyword('XOR') + relation)) |
               (relation + ZeroOrMore(CaselessKeyword('NAND') + relation)) |
               (relation + ZeroOrMore(CaselessKeyword('NOR') + relation)) |
               (relation + ZeroOrMore(CaselessKeyword('XNOR') + relation)) )

formal_designator = name

formal_part = (formal_designator |
               (name + Literal('(') + formal_designator + Literal(')')) |
               (type_mark + Literal('(') + formal_designator + Literal(')')))

actual_designator = (expression |
                     name |
                     CaselessKeyword('OPEN'))

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

allocator = ( (CaselessKeyword('NEW') + subtype_indication) |
	          (CaselessKeyword('NEW') + qualified_expression) )

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
	| CaselessKeyword('ALL') )


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


interface_constant_declaration = Optional(CaselessKeyword('CONSTANT')) + identifier_list + Literal(':') + Optional(CaselessKeyword('IN')) + subtype_indication + Optional( Literal(':=') + expression )
interface_signal_declaration =   Optional(CaselessKeyword('SIGNAL'))   + identifier_list + Literal(':') + Optional(mode) + subtype_indication + Optional(CaselessKeyword('BUS')) + Optional(Literal(':=') + expression)
interface_variable_declaration = Optional(CaselessKeyword('VARIABLE')) + identifier_list + Literal(':') + Optional(mode) + subtype_indication + Optional(Literal(':=') + expression)

interface_file_declaration = CaselessKeyword('FILE') + identifier_list + Literal(':') + subtype_indication


interface_declaration = ( interface_constant_declaration |
                          interface_signal_declaration |
                          interface_variable_declaration |
                          interface_file_declaration )

interface_element = interface_declaration

interface_list = delimitedList(interface_element, delim=';')

port_list = interface_list
port_clause = CaselessKeyword('PORT') - Literal('(') + port_list + Literal(')') + Literal(';')

generic_list = interface_list

generic_clause = CaselessKeyword('GENERIC') - Literal('(') + generic_list + Literal(')') + Literal(';')

entity_header = (
	Optional(generic_clause) +
	Optional(port_clause)
)

designator = identifier | operator_symbol

formal_parameter_list = interface_list

subprogram_specification = (
	( CaselessKeyword('PROCEDURE') + designator + Optional(Literal('(') + formal_parameter_list + Literal(')')) ) |
	( Optional(CaselessKeyword('PURE') | CaselessKeyword('IMPURE')) + CaselessKeyword('FUNCTION') + designator + Optional( Literal('(') + formal_parameter_list + Literal(')') ) + CaselessKeyword('RETURN') + type_mark )
    )

subprogram_declaration = subprogram_specification + Literal(';')

subprogram_declarative_item = Forward()

subprogram_declarative_part = ZeroOrMore(subprogram_declarative_item)

sensitivity_list = delimitedList(name)

sensitivity_clause = CaselessKeyword('ON') + sensitivity_list

condition = expression

condition_clause = CaselessKeyword('UNTIL') + condition

timeout_clause = CaselessKeyword('FOR') + expression

wait_statement = Optional(label + Literal(':')) + CaselessKeyword('WAIT') + Optional(sensitivity_clause) + Optional(condition_clause) + Optional(timeout_clause) + Literal(';')

assertion = (
	CaselessKeyword('ASSERT') + condition +
		Optional(CaselessKeyword('REPORT') + expression) +
		Optional(CaselessKeyword('SEVERITY') + expression)
        )

assertion_statement = Optional(label + Literal(':')) + assertion + Literal(';')

report_statement = (
	Optional(label + Literal(':')) +
		CaselessKeyword('REPORT') + expression +
			Optional(CaselessKeyword('SEVERITY') + expression) + Literal(';')
            )

target = name | aggregate

delay_mechanism = CaselessKeyword('TRANSPORT') | (Optional(CaselessKeyword('REJECT') + expression) + CaselessKeyword('INERTIAL'))

waveform_element = (expression + Optional(CaselessKeyword('AFTER') + expression)) | (CaselessKeyword('NULL') + Optional(CaselessKeyword('AFTER') + expression))

waveform = delimitedList(waveform_element) | CaselessKeyword('UNAFFECTED')

signal_assignment_statement = Optional(label + Literal(':')) + target + CaselessKeyword('<=') + Optional(delay_mechanism) + waveform + Literal(';')

variable_assignment_statement = target + CaselessKeyword(':=') + expression + Literal(';')

procedure_call = name + Optional( Literal('(') + actual_parameter_part + Literal(')') )

procedure_call_statement = Optional(label + Literal(':')) + procedure_call + Literal(';')

sequential_statement = Forward()

sequence_of_statements = ZeroOrMore(sequential_statement)

if_statement = (
	Optional(label + Literal(':')) +
		CaselessKeyword('IF') + condition + CaselessKeyword('THEN') +
			sequence_of_statements +
		ZeroOrMore( CaselessKeyword('ELSIF') + condition + CaselessKeyword('THEN') +
			sequence_of_statements ) +
		Optional(CaselessKeyword('ELSE') +
			sequence_of_statements) +
		CaselessKeyword('END') + CaselessKeyword('IF') + Optional(label) + Literal(';')
        )

case_statement_alternative = (
	CaselessKeyword('WHEN') + choices + CaselessKeyword('=>') +
		sequence_of_statements )

case_statement = (
	Optional(label + Literal(':') ) +
		CaselessKeyword('CASE') - expression + CaselessKeyword('IS') -
			OneOrMore(case_statement_alternative) +
		CaselessKeyword('END') + CaselessKeyword('CASE') + Optional( label ) + Literal(';')
        )

parameter_specification = identifier + CaselessKeyword('IN') + discrete_range

iteration_scheme = (CaselessKeyword('WHILE') + condition) | (CaselessKeyword('FOR') + parameter_specification)

loop_statement = (
	Optional(label + Literal(':') ) +
		Optional( iteration_scheme ) + CaselessKeyword('LOOP') +
			sequence_of_statements +
		CaselessKeyword('END') + CaselessKeyword('LOOP') + Optional(label) + Literal(';')
        )

next_statement = Optional(label + Literal(':')) + CaselessKeyword('NEXT') + Optional(label) + Optional(CaselessKeyword('WHEN') + condition) + Literal(';')

exit_statement = Optional(label + Literal(':')) + CaselessKeyword('EXIT') + Optional(label) + Optional(CaselessKeyword('WHEN') + condition) + Literal(';')

return_statement = Optional(label + Literal(':')) + CaselessKeyword('RETURN') + Optional(expression) + Literal(';')

null_statement = Optional(label + Literal(':')) + CaselessKeyword('NULL') + Literal(';')

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

subprogram_kind = CaselessKeyword('PROCEDURE') | CaselessKeyword('FUNCTION')

subprogram_body = (
	subprogram_specification + CaselessKeyword('IS') -
		subprogram_declarative_part +
	CaselessKeyword('BEGIN') +
		subprogram_statement_part +
	CaselessKeyword('END') + Optional(subprogram_kind) + Optional( designator ) + Literal(';')
    )

enumeration_type_definition = Literal('(') + delimitedList(enumeration_literal) + Literal(')')

integer_type_definition = range_constraint

floating_type_definition = range_constraint

base_unit_declaration = identifier + Literal(';')

secondary_unit_declaration = identifier + Literal('=') + physical_literal + Literal(';')

physical_type_definition = (
	range_constraint +
		CaselessKeyword('UNITS') +
			base_unit_declaration +
			ZeroOrMore(secondary_unit_declaration) +
		CaselessKeyword('END') + CaselessKeyword('UNITS') + Optional(simple_name)
        )

scalar_type_definition = enumeration_type_definition | integer_type_definition | floating_type_definition | physical_type_definition

index_subtype_definition = type_mark + range_ + Literal('<>')

unconstrained_array_definition = (
	CaselessKeyword('ARRAY') + Literal('(') + delimitedList(index_subtype_definition) + Literal(')') +
		CaselessKeyword('OF') + subtype_indication
        )

constrained_array_definition = CaselessKeyword('ARRAY') + index_constraint + CaselessKeyword('OF') + subtype_indication

array_type_definition = unconstrained_array_definition | constrained_array_definition

element_subtype_definition = subtype_indication

element_declaration = identifier_list + Literal(':') + element_subtype_definition + Literal(';')

record_type_definition = (
	CaselessKeyword('RECORD') +
		OneOrMore(element_declaration) +
	CaselessKeyword('END') + CaselessKeyword('RECORD') + Optional(simple_name)
    )

composite_type_definition = array_type_definition | record_type_definition

access_type_definition = CaselessKeyword('ACCESS') + subtype_indication

file_type_definition = CaselessKeyword('FILE') + CaselessKeyword('OF') + type_mark

type_definition = (
	scalar_type_definition
	| composite_type_definition
	| access_type_definition
	| file_type_definition
    )

full_type_declaration = CaselessKeyword('TYPE') + identifier + CaselessKeyword('IS') - type_definition + Literal(';')

incomplete_type_declaration = CaselessKeyword('TYPE') + identifier + Literal(';')

type_declaration = full_type_declaration | incomplete_type_declaration

subtype_declaration = CaselessKeyword('SUBTYPE') - identifier + CaselessKeyword('IS') - subtype_indication + Literal(';')

constant_declaration = CaselessKeyword('CONSTANT') + identifier_list + Literal(':') + subtype_indication + Optional(CaselessKeyword(':=') + expression) + Literal(';')

variable_declaration = Optional(CaselessKeyword('SHARED')) + CaselessKeyword('VARIABLE') + identifier_list + Literal(':') + subtype_indication + Optional(CaselessKeyword(':=') + expression) + Literal(';')

file_logical_name = expression

file_open_information = Optional(CaselessKeyword('OPEN') + expression) + CaselessKeyword('IS') - file_logical_name

file_declaration = CaselessKeyword('FILE') + identifier_list + Literal(':') + subtype_indication + Optional(file_open_information) + Literal(';')

alias_designator = identifier | character_literal | operator_symbol

alias_declaration = CaselessKeyword('ALIAS') - alias_designator + Optional(Literal(':') + subtype_indication) + CaselessKeyword('IS') - name + Optional(signature) + Literal(';')

attribute_declaration = CaselessKeyword('ATTRIBUTE') + identifier + Literal(':') + type_mark + Literal(';')

entity_tag = simple_name | character_literal | operator_symbol

entity_designator = entity_tag + Optional(signature)

entity_name_list = delimitedList(entity_designator) | CaselessKeyword('OTHERS') | CaselessKeyword('ALL')

entity_class = (
	CaselessKeyword('ENTITY')	     | CaselessKeyword('ARCHITECTURE')  | CaselessKeyword('CONFIGURATION')
	| CaselessKeyword('PROCEDURE')  | CaselessKeyword('FUNCTION')	     | CaselessKeyword('PACKAGE')
	| CaselessKeyword('TYPE')	     | CaselessKeyword('SUBTYPE')	     | CaselessKeyword('CONSTANT')
	| CaselessKeyword('SIGNAL')     | CaselessKeyword('VARIABLE')	     | CaselessKeyword('COMPONENT')
	| CaselessKeyword('LABEL')	     | CaselessKeyword('LITERAL')	     | CaselessKeyword('UNITS')
	| CaselessKeyword('GROUP')	     | CaselessKeyword('FILE')
    )

entity_specification = entity_name_list + Literal(':') + entity_class

attribute_specification = CaselessKeyword('ATTRIBUTE') + attribute_designator + CaselessKeyword('OF') + entity_specification + CaselessKeyword('IS') - expression + Literal(';')

use_clause = CaselessKeyword('USE') + delimitedList(selected_name) + Literal(';')

entity_class_entry = entity_class + Optional(Literal('<>'))

entity_class_entry_list = delimitedList(entity_class_entry)

group_template_declaration = CaselessKeyword('GROUP') + identifier + CaselessKeyword('IS') - Literal('(') + entity_class_entry_list + Literal(')') + Literal(';')

group_constituent = name | character_literal

group_constituent_list = delimitedList(group_constituent)

group_declaration = CaselessKeyword('GROUP') + identifier + Literal(':') + name + Literal('(') + group_constituent_list + Literal(')') + Literal(';')

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

signal_kind = CaselessKeyword('REGISTER') | CaselessKeyword('BUS')

signal_declaration = CaselessKeyword('SIGNAL') + identifier_list + Literal(':') + subtype_indication + Optional(signal_kind) + Optional(CaselessKeyword(':=') + expression) + Literal(';')

signal_list = delimitedList(name) | CaselessKeyword('OTHERS') | CaselessKeyword('ALL')

guarded_signal_specification = signal_list + Literal(':') + type_mark

disconnection_specification = CaselessKeyword('DISCONNECT') + guarded_signal_specification + CaselessKeyword('AFTER') + expression + Literal(';')

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

concurrent_assertion_statement = Optional(label + Literal(':')) + Optional(CaselessKeyword('POSTPONED')) + assertion + Literal(';')

concurrent_procedure_call_statement = Optional(label + Literal(':')) + Optional(CaselessKeyword('POSTPONED')) + procedure_call + Literal(';')

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
		Optional(CaselessKeyword('POSTPONED')) + CaselessKeyword('PROCESS') + Optional( Literal('(') + sensitivity_list + Literal(')')) + Optional(CaselessKeyword('IS')) +
			process_declarative_part +
		CaselessKeyword('BEGIN') +
			process_statement_part +
		CaselessKeyword('END') + Optional(CaselessKeyword('POSTPONED')) + CaselessKeyword('PROCESS') + Optional(label) + Literal(';')
        )

entity_statement = concurrent_assertion_statement | concurrent_procedure_call_statement | process_statement

entity_statement_part = ZeroOrMore(entity_statement)

entity_declaration = (
    CaselessKeyword('ENTITY') - identifier + CaselessKeyword('IS') +
		entity_header +
        entity_declarative_part +
        Optional( CaselessKeyword('BEGIN') + entity_statement_part ) +
	CaselessKeyword('END') + Optional( CaselessKeyword('ENTITY') ) + Optional( simple_name ) + Literal(';')
)

configuration_declarative_item = use_clause | attribute_specification | group_declaration

configuration_declarative_part = ZeroOrMore(configuration_declarative_item)

index_specification = discrete_range | expression

block_specification = name | label | (label + Optional(Literal('(') + index_specification + Literal(')')))

block_configuration = Forward()

instantiation_list = delimitedList(label) | CaselessKeyword('OTHERS') | CaselessKeyword('ALL')

component_specification = instantiation_list + Literal(':') + name

entity_aspect = (
	  (CaselessKeyword('ENTITY') + name + Optional( Literal('(') + identifier + Literal(')') ))
	| (CaselessKeyword('CONFIGURATION') + name)
	| CaselessKeyword('OPEN')
    )

generic_map_aspect = CaselessKeyword('GENERIC') + CaselessKeyword('MAP') + Literal('(') + association_list + Literal(')')

port_map_aspect = CaselessKeyword('PORT') + CaselessKeyword('MAP') + Literal('(') + association_list + Literal(')')

binding_indication = (
	Optional( CaselessKeyword('USE') + entity_aspect ) +
	Optional( generic_map_aspect ) +
	Optional( port_map_aspect )
        )

component_configuration = (
	CaselessKeyword('FOR') + component_specification +
		Optional(binding_indication + Literal(';')) +
		Optional(block_configuration) +
	CaselessKeyword('END') + CaselessKeyword('FOR') + Literal(';')
    )

configuration_item = block_configuration | component_configuration

block_configuration << (
	CaselessKeyword('FOR') + block_specification +
		ZeroOrMore(use_clause) +
		ZeroOrMore(configuration_item) +
	CaselessKeyword('END') + CaselessKeyword('FOR') + Literal(';')
    )

configuration_declaration = (
	CaselessKeyword('CONFIGURATION') - identifier + CaselessKeyword('OF') - name + CaselessKeyword('IS') -
		configuration_declarative_part +
		block_configuration +
	CaselessKeyword('END') + Optional('CONFIGURATION') + Optional(simple_name) + Literal(';')
    )

component_declaration = (
	CaselessKeyword('COMPONENT') + identifier + Optional(CaselessKeyword('IS')) -
		Optional(generic_clause) +
		Optional(port_clause) +
	CaselessKeyword('END') + CaselessKeyword('COMPONENT') - Optional(simple_name) + Literal(';')
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
	CaselessKeyword('PACKAGE') - identifier + CaselessKeyword('IS') -
		package_declarative_part +
	CaselessKeyword('END') + Optional(CaselessKeyword('PACKAGE')) - Optional(simple_name) + Literal(';')
    )

primary_unit = (
	entity_declaration
	| configuration_declaration
	| package_declaration
    )

configuration_specification = CaselessKeyword('FOR') + component_specification + binding_indication + Literal(';')

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
		CaselessKeyword('BLOCK') + Optional( Literal('(') + expression + Literal(')') ) + Optional(CaselessKeyword('IS')) +
			block_header +
			block_declarative_part +
		CaselessKeyword('BEGIN') +
			block_statement_part +
		CaselessKeyword('END') + CaselessKeyword('BLOCK') + Optional(label) + Literal(';')
        )

options = Optional(CaselessKeyword('GUARDED')) + Optional(delay_mechanism)

conditional_waveforms = (
	ZeroOrMore( waveform + CaselessKeyword('WHEN') + condition + CaselessKeyword('ELSE') ) +
	waveform + Optional(CaselessKeyword('WHEN') + condition)
    )

conditional_signal_assignment = target + CaselessKeyword('<=') + options + conditional_waveforms + Literal(';')

selected_waveforms = (
	ZeroOrMore(waveform + CaselessKeyword('WHEN') + choices + Literal(',')) +
	waveform + CaselessKeyword('WHEN') + choices
    )

selected_signal_assignment = (
	CaselessKeyword('WITH') + expression + CaselessKeyword('SELECT') +
		target + CaselessKeyword('<=') + options + selected_waveforms + Literal(';')
        )

concurrent_signal_assignment_statement = (
	  (Optional(label + Literal(':')) + Optional(CaselessKeyword('POSTPONED')) + conditional_signal_assignment)
	| (Optional(label + Literal(':')) + Optional(CaselessKeyword('POSTPONED')) + selected_signal_assignment)
    )

instantiated_unit = (
	(Optional(CaselessKeyword('COMPONENT')) + name)
	| (CaselessKeyword('ENTITY') + name + Optional( Literal('(') + identifier + Literal(')') ))
	| (CaselessKeyword('CONFIGURATION') + name)
    )

component_instantiation_statement = (
	label + Literal(':') +
		instantiated_unit +
			Optional(generic_map_aspect) +
			Optional(port_map_aspect) + Literal(';')
            )

generation_scheme = (
	(CaselessKeyword('FOR') + parameter_specification)
	| (CaselessKeyword('IF') + condition)
    )

generate_statement = (
	label + Literal(':') +
		generation_scheme + CaselessKeyword('GENERATE') +
			Optional( ZeroOrMore(block_declarative_item) +
		CaselessKeyword('BEGIN')) +
			ZeroOrMore(concurrent_statement) +
		CaselessKeyword('END') + CaselessKeyword('GENERATE') + Optional(label) + Literal(';')
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
	CaselessKeyword('ARCHITECTURE') - identifier + CaselessKeyword('OF') + name + CaselessKeyword('IS') +
		architecture_declarative_part +
	CaselessKeyword('BEGIN') +
		architecture_statement_part +
	CaselessKeyword('END') + Optional(CaselessKeyword('ARCHITECTURE')) + Optional(simple_name) + Literal(';')
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
	CaselessKeyword('PACKAGE') + CaselessKeyword('BODY') - simple_name + CaselessKeyword('IS') +
		package_body_declarative_part +
	CaselessKeyword('END') + Optional(CaselessKeyword('PACKAGE') + CaselessKeyword('BODY')) + Optional(simple_name) + Literal(';')
    )

secondary_unit = architecture_body | package_body

library_unit = primary_unit | secondary_unit

logical_name = identifier

logical_name_list = delimitedList(logical_name)

library_clause = CaselessKeyword('LIBRARY') + logical_name_list + Literal(';')

context_item = library_clause | use_clause

context_clause = ZeroOrMore(context_item)

design_unit = context_clause + library_unit

design_file = OneOrMore(design_unit) + StringEnd()

if __name__ == '__main__':
    results = design_file.parseFile('dinges.vhd')
    pprint(results.asList())

