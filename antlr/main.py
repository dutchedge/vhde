#!/usr/bin/env python3

import antlr4
import sys
import pprint
from vhdlLexer import vhdlLexer
from vhdlListener import vhdlListener
from vhdlParser import vhdlParser
from vhdlVisitor import vhdlVisitor

class vhdlPrinter(vhdlListener):

    def __init__(self):
        self.in_arch = False

    def enterArchitecture_body(self, ctx:vhdlParser.Architecture_bodyContext):
        self.in_arch = True
        print('object is "%s"' % ctx.start.getInputStream().getText(ctx.start.start, ctx.stop.stop))

    def exitArchitecture_body(self, ctx:vhdlParser.Architecture_bodyContext):
        self.in_arch = False

    def enterSignal_declaration(self, ctx:vhdlParser.Signal_declarationContext):
        print('in arch is %s' % self.in_arch)

class myVhdlVisitor(vhdlVisitor):

    def visitDesign_file(self, ctx:vhdlParser.Design_fileContext):
        return [self.visit(c) for c in ctx.design_unit()]

    def visitDesign_unit(self, ctx:vhdlParser.Design_unitContext):
        return self.visit(ctx.library_unit())

    def visitLibrary_unit(self, ctx:vhdlParser.Library_unitContext):
        result = {}
        if ctx.primary_unit():
            result.update(self.visit(ctx.primary_unit()))
        if ctx.secondary_unit():
            result.update(self.visit(ctx.secondary_unit()))
        return result

    def visitPrimary_unit(self, ctx:vhdlParser.Primary_unitContext):
        if ctx.entity_declaration():
            return self.visit(ctx.entity_declaration())
        else:
            return {}

    def visitSecondary_unit(self, ctx:vhdlParser.Secondary_unitContext):
        if ctx.architecture_body():
            return self.visit(ctx.architecture_body())
        else:
            return {}

    def visitEntity_declaration(self, ctx:vhdlParser.Entity_declarationContext):
        result = { 'name' : self.visit(ctx.identifier(0)) }
        result.update(self.visit(ctx.entity_header()))
        return result

    def visitEntity_header(self, ctx:vhdlParser.Entity_headerContext):
        if ctx.port_clause():
            return self.visit(ctx.port_clause())
        else:
            return {}

    def visitArchitecture_body(self, ctx:vhdlParser.Architecture_bodyContext):
        result = { 'name' : self.visit(ctx.identifier(0)),
                   'entity' : self.visit(ctx.identifier(1)) }

        result.update(self.visit(ctx.architecture_declarative_part()))
        result.update(self.visit(ctx.architecture_statement_part()))

        return result

    def visitArchitecture_declarative_part(self, ctx:vhdlParser.Architecture_declarative_partContext):
        result = {
            'components' : [],
            'signals' : [],
        }

        for itemctx in ctx.block_declarative_item():
            item = self.visit(itemctx)
            print(item)
            if item['type'] == 'component_declaration':
                result['components'].append(item['value'])
            elif item['type'] == 'signal_declaration':
                result['signals'].append(item['value'])
            else:
                raise RuntimeError('Oops')

        return result

    def visitArchitecture_statement_part(self, ctx:vhdlParser.Architecture_statement_partContext):
        return [self.visit(c) for c in ctx.architecture_statement()]

    def visitArchitecture_statement(self, ctx:vhdlParser.Architecture_statementContext):
        if ctx.component_instantiation_statement():
            return self.visit(ctx.component_instantiation_statement())
        else:
            return {}

    def visitBlock_declarative_item(self, ctx:vhdlParser.Block_declarative_itemContext):
        if ctx.component_declaration():
            return { 'type' : 'component_declaration',
                     'value' : self.visit(ctx.component_declaration()) }
        elif ctx.signal_declaration():
            return { 'type' : 'signal_declaration',
                     'value' : self.visit(ctx.signal_declaration()) }
        else:
            raise RuntimeError('Oops')

    def visitComponent_declaration(self, ctx:vhdlParser.Component_declarationContext):
        result = { 'name' : self.visit(ctx.identifier(0))}
        if ctx.port_clause():
            result.update(self.visit(ctx.port_clause()))
        return result

    def visitPort_clause(self, ctx:vhdlParser.Port_clauseContext):
        return { 'ports' : self.visit(ctx.port_list()) }

    def visitPort_list(self, ctx:vhdlParser.Port_listContext):
        return [self.visit(portctx) for portctx in ctx.interface_port_list().interface_port_declaration()]

    def visitInterface_port_declaration(self, ctx:vhdlParser.Interface_port_declarationContext):
        return { 'name' : self.visit(ctx.identifier_list()),
                 'signal_mode' : self.visit(ctx.signal_mode()),
                 'subtype_indication' : self.visit(ctx.subtype_indication()) }

    def visitIdentifier(self, ctx:vhdlParser.IdentifierContext):
        return ctx.getText()

def main(argv):
    print('Parsing file %s' % argv[1])
    input_ = antlr4.FileStream(argv[1])
    lexer = vhdlLexer(input_)
    stream = antlr4.CommonTokenStream(lexer)
    parser = vhdlParser(stream)
    tree = parser.design_file()

    visitor = myVhdlVisitor()
    pprint.pprint(visitor.visit(tree))

    #printer = vhdlPrinter()
    #walker = antlr4.ParseTreeWalker()
    #walker.walk(printer, tree)
    print(tree.toStringTree(None, parser))
                          
if __name__ == '__main__':
    main(sys.argv)
