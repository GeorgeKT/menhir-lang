use ast::*;
use tokenqueue::TokenQueue;
use compileerror::*;
use tokens::*;
use parser::*;

fn parse_import(tq: &mut TokenQueue) -> Result<Statement, CompileError>
{
    let (file, pos) = try!(tq.expect_string());
    Ok(Statement::Import(Import::new(file, pos)))
}


fn parse_type(tq: &mut TokenQueue) -> Result<Type, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    Ok(Type::Primitive(name))
}


fn parse_optional_type(tq: &mut TokenQueue) -> Result<Option<Type>, CompileError>
{
    if tq.is_next(TokenKind::Colon)
    {
        // variable with type declaration
        try!(tq.pop());
        Ok(Some(try!(parse_type(tq))))
    }
    else
    {
        Ok(None)
    }
}


fn parse_vars(tq: &mut TokenQueue, indent_level: usize, constants: bool) -> Result<Statement, CompileError>
{
    let mut vars = Vec::new();
    loop
    {
        let tok = try!(tq.pop());
        match tok.kind
        {
            TokenKind::Identifier(id) => {
                let type_of_var = try!(parse_optional_type(tq));
                try!(tq.expect(TokenKind::Operator(Operator::Assign)));
                let expr = try!(parse_expression(tq, indent_level));
                vars.push(Variable::new(id, type_of_var, constants, expr));
            },
            TokenKind::Indent(level) => if level > indent_level {continue;} else {break;},
            TokenKind::Comma => continue,
            TokenKind::EOF => break,
            _ => {
                return err(tok.pos, ErrorType::UnexpectedToken(tok));
            }
        }
    }

    Ok(Statement::Variable(vars))
}

pub fn parse_block(tq: &mut TokenQueue, indent_level: usize) -> Result<Block, CompileError>
{
    let mut statements = Vec::new();
    if tq.next_indent().is_none() {
        statements.push(try!(parse_statement(tq, indent_level)));
    }

    loop {
        match tq.next_indent()
        {
            Some(lvl) if lvl > indent_level => {
                try!(tq.pop());

                if tq.is_next(TokenKind::EOF) {
                    break;
                }

                statements.push(try!(parse_statement(tq, lvl)));
            },
            _ => break,
        }
    }

    Ok(Block::new(statements))
}

fn parse_func(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut args = Vec::new();

    try!(tq.expect(TokenKind::OpenParen));
    while !tq.is_next(TokenKind::CloseParen)
    {
        let const_arg = if tq.is_next(TokenKind::Const) {
            try!(tq.pop());
            true
        } else {
            false
        };

        let (arg_name, _) = try!(tq.expect_identifier());
        try!(tq.expect(TokenKind::Colon));
        args.push(Argument::new(arg_name, try!(parse_type(tq)), const_arg));

        if !tq.is_next(TokenKind::Comma) {
            break;
        }

        try!(tq.expect(TokenKind::Comma));
    }

    try!(tq.expect(TokenKind::CloseParen));

    let ret_type = if tq.is_next(TokenKind::Operator(Operator::Arrow)) {
        try!(tq.pop());
        try!(parse_type(tq))
    } else {
        Type::Void
    };

    try!(tq.expect(TokenKind::Colon));

    Ok(Statement::Function(
        Function::new(
            name,
            ret_type,
            args,
            try!(parse_block(tq, indent_level))
        )))
}

fn parse_while(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level));
    Ok(Statement::While(While::new(cond, block)))
}

fn parse_if(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let if_block = try!(parse_block(tq, indent_level));
    let mut else_block = None;

    if let Some(lvl) = tq.next_indent() {
        if lvl == indent_level && tq.is_next_at(1, TokenKind::Else) {
            try!(tq.pop()); // indent
            try!(tq.pop()); // else
            try!(tq.expect(TokenKind::Colon));
            else_block = Some(try!(parse_block(tq, indent_level)));
        }
    }

    Ok(Statement::If(If::new(cond, if_block, else_block)))
}


fn parse_return(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let e = try!(parse_expression(tq, indent_level));
    Ok(Statement::Return(Return::new(e)))
}

pub fn parse_statement(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Import => parse_import(tq),
        TokenKind::Var => parse_vars(tq, indent_level, false),
        TokenKind::Const => parse_vars(tq, indent_level, true),
        TokenKind::Func => parse_func(tq, indent_level),
        TokenKind::While => parse_while(tq, indent_level),
        TokenKind::If => parse_if(tq, indent_level),
        TokenKind::Return => parse_return(tq, indent_level),
        TokenKind::Identifier(id) => parse_function_call(tq, indent_level, id).map(|c| Statement::Call(c)),
        _ => err(tok.pos, ErrorType::UnexpectedToken(tok)),
    }
}
