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


fn parse_vars(tq: &mut TokenQueue, indent_level: usize, constants: bool, public: bool) -> Result<Vec<Variable>, CompileError>
{
    let mut vars = Vec::new();
    loop
    {
        if let Some(level) = tq.next_indent() {
            if level <= indent_level {break}
        }

        let tok = try!(tq.pop());
        match tok.kind
        {
            TokenKind::Identifier(id) => {
                let type_of_var = try!(parse_optional_type(tq));
                try!(tq.expect(TokenKind::Operator(Operator::Assign)));
                let expr = try!(parse_expression(tq, indent_level));
                vars.push(Variable::new(id, type_of_var, constants, public, expr));
            },
            TokenKind::Comma | TokenKind::Indent(_) => continue,
            TokenKind::EOF => break,
            _ => {
                return err(tok.pos, ErrorType::UnexpectedToken(tok));
            }
        }
    }

    Ok(vars)
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

fn parse_func(tq: &mut TokenQueue, indent_level: usize, public: bool, self_type: Type) -> Result<Function, CompileError>
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

        let (arg_name, arg_pos) = try!(tq.expect_identifier());

        if arg_name == "self" {
            if args.is_empty() {
                args.push(Argument::new(arg_name, self_type.clone(), const_arg));
            } else {
                return err(arg_pos, ErrorType::SelfNotAllowed);
            }
        } else {
            try!(tq.expect(TokenKind::Colon));
            args.push(Argument::new(arg_name, try!(parse_type(tq)), const_arg));
        }

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

    Ok(Function::new(
        name,
        ret_type,
        args,
        public,
        try!(parse_block(tq, indent_level))
    ))
}

fn parse_while(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level));
    Ok(Statement::While(While::new(cond, block)))
}

fn parse_else(tq: &mut TokenQueue, indent_level: usize) -> Result<ElsePart, CompileError>
{
    if tq.is_next(TokenKind::If)
    {
        try!(tq.pop());
        Ok(ElsePart::If(Box::new(try!(parse_if(tq, indent_level)))))
    }
    else
    {
        try!(tq.expect(TokenKind::Colon));
        Ok(ElsePart::Block(try!(parse_block(tq, indent_level))))
    }
}

fn parse_if(tq: &mut TokenQueue, indent_level: usize) -> Result<If, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let if_block = try!(parse_block(tq, indent_level));
    let mut else_part = ElsePart::Empty;

    if let Some(lvl) = tq.next_indent() {
        if lvl == indent_level && tq.is_next_at(1, TokenKind::Else) {
            try!(tq.pop()); // indent
            try!(tq.pop()); // else
            else_part = try!(parse_else(tq, indent_level));
        }
    }

    Ok(If::new(cond, if_block, else_part))
}


fn parse_return(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let e = try!(parse_expression(tq, indent_level));
    Ok(Statement::Return(Return::new(e)))
}

fn parse_struct_member(s: &mut Struct, tq: &mut TokenQueue, indent_level: usize, public: bool) -> Result<(), CompileError>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Pub => {
            return parse_struct_member(s, tq, indent_level, true);
        },
        TokenKind::Func => {
            let st = Type::Struct(s.name.clone());
            s.functions.push(try!(parse_func(tq, indent_level, public, st)));
        },
        TokenKind::Var => {
            let vars = try!(parse_vars(tq, indent_level, false, public));
            s.variables.extend(vars.into_iter());
        },
        TokenKind::Const => {
            let vars = try!(parse_vars(tq, indent_level, true, public));
            s.variables.extend(vars.into_iter());
        },
        TokenKind::EOF => {},
        _ => {
            return err(tok.pos, ErrorType::UnexpectedToken(tok));
        },
    }

    Ok(())
}

fn parse_struct(tq: &mut TokenQueue, indent_level: usize, public: bool) -> Result<Struct, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    try!(tq.expect(TokenKind::Colon));

    let mut s = Struct::new(name, public);
    while let Some(level) = tq.next_indent()
    {
        if level <= indent_level {break;}
        try!(tq.pop()); // indent
        try!(parse_struct_member(&mut s, tq, level, false))
    }

    Ok(s)
}

fn eat_comma(tq: &mut TokenQueue) -> Result<(), CompileError>
{
    tq.pop_if(|tok| tok.kind == TokenKind::Comma).map(|_| ())
}

fn parse_union_case(tq: &mut TokenQueue) -> Result<UnionCase, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut uc = UnionCase::new(name);
    if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::CloseParen)
        {
            let (name, _) = try!(tq.expect_identifier());
            try!(tq.expect(TokenKind::Colon));
            let typ = try!(parse_type(tq));
            uc.vars.push(Argument::new(name, typ, false));
            try!(eat_comma(tq));
        }

        try!(tq.expect(TokenKind::CloseParen));
    }

    try!(eat_comma(tq)); // Eat trailing comma
    Ok(uc)
}

fn parse_union_member(tq: &mut TokenQueue, indent_level: usize, public: bool, ut: Type) -> Result<Function, CompileError>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Pub => parse_union_member(tq, indent_level, true, ut),
        TokenKind::Func => parse_func(tq, indent_level, public, ut),
        _ => err(tok.pos, ErrorType::UnexpectedToken(tok)),
    }
}

fn parse_union(tq: &mut TokenQueue, indent_level: usize, public: bool) -> Result<Union, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut u = Union::new(name, public);
    let mut indent = indent_level;
    try!(tq.expect(TokenKind::Colon));
    loop
    {
        if let Some(level) = tq.next_indent() {
            if level <= indent_level {break;}
            indent = level;
            try!(tq.pop()); // indent
        } else if tq.is_next_identifier() {
            u.cases.push(try!(parse_union_case(tq)));
        } else if tq.is_next(TokenKind::EOF) {
            break;
        } else {
            u.functions.push(try!(parse_union_member(tq, indent, false, Type::Union(u.name.clone()))));
        }
    }

    Ok(u)
}

fn parse_match_case(tq: &mut TokenQueue, indent_level: usize) -> Result<MatchCase, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut bindings = Vec::new();
    if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::CloseParen)
        {
            let (name, _) = try!(tq.expect_identifier());
            bindings.push(name);
            try!(eat_comma(tq));
        }

        try!(tq.expect(TokenKind::CloseParen));
    }

    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level));
    Ok(MatchCase::new(name, bindings, block))
}

fn parse_match(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let expr = try!(parse_expression(tq, indent_level));
    let mut m = Match::new(expr);
    try!(tq.expect(TokenKind::Colon));
    while let Some(level) = tq.next_indent()
    {
        if level <= indent_level {break}
        try!(tq.pop()); // indent

        if tq.is_next(TokenKind::EOF) {break;}
        
        m.cases.push(try!(parse_match_case(tq, level)));
    }

    Ok(Statement::Match(m))
}

pub fn parse_statement(tq: &mut TokenQueue, indent_level: usize) -> Result<Statement, CompileError>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Import => parse_import(tq),
        TokenKind::Var => parse_vars(tq, indent_level, false, false).map(|v| Statement::Variable(v)),
        TokenKind::Const => parse_vars(tq, indent_level, true, false).map(|v| Statement::Variable(v)),
        TokenKind::Func => parse_func(tq, indent_level, false, Type::Void).map(|f| Statement::Function(f)),
        TokenKind::Struct => parse_struct(tq, indent_level, false).map(|s| Statement::Struct(s)),
        TokenKind::Union => parse_union(tq, indent_level, false).map(|u| Statement::Union(u)),
        TokenKind::While => parse_while(tq, indent_level),
        TokenKind::If => parse_if(tq, indent_level).map(|i| Statement::If(i)),
        TokenKind::Return => parse_return(tq, indent_level),
        TokenKind::Match => parse_match(tq, indent_level),
        TokenKind::Identifier(id) => parse_function_call(tq, indent_level, id).map(|c| Statement::Call(c)),
        TokenKind::Pub => {
            let next = try!(tq.pop());
            match next.kind
            {
                TokenKind::Var => parse_vars(tq, indent_level, false, true).map(|v| Statement::Variable(v)),
                TokenKind::Const => parse_vars(tq, indent_level, true, true).map(|v| Statement::Variable(v)),
                TokenKind::Func => parse_func(tq, indent_level, true, Type::Void).map(|f| Statement::Function(f)),
                TokenKind::Struct => parse_struct(tq, indent_level, true).map(|s| Statement::Struct(s)),
                TokenKind::Union => parse_union(tq, indent_level, true).map(|u| Statement::Union(u)),
                _ => err(tok.pos, ErrorType::UnexpectedToken(next)),
            }
        },
        _ => err(tok.pos, ErrorType::UnexpectedToken(tok)),
    }
}
