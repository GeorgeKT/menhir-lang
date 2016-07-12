use ast::*;
use compileerror::*;
use parser::*;

fn parse_module_name(tq: &mut TokenQueue) -> Result<ModuleName, CompileError>
{
    let mut start_pos = Pos::zero();
    let mut parts = Vec::new();
    while tq.is_next_identifier()
    {
        let (m, pos) = try!(tq.expect_identifier());
        if start_pos == Pos::zero() {
            start_pos = pos;
        }

        parts.push(m);
        if tq.is_next(TokenKind::DoubleColon) {
            try!(tq.pop());
        } else {
            break;
        }
    }

    Ok(ModuleName::new(parts, Span::new(start_pos, tq.pos())))
}

fn parse_import(tq: &mut TokenQueue, pos: Pos) -> Result<Statement, CompileError>
{
    let mut modules = Vec::new();
    modules.push(try!(parse_module_name(tq)));
    while tq.is_next(TokenKind::Comma) {
        try!(tq.pop());
        if !tq.is_next_identifier() {break}

        modules.push(try!(parse_module_name(tq)));
    }

    Ok(Statement::Import(Import::new(modules, Span::new(pos, tq.pos()))))
}

fn is_primitive_type(name: &str) -> bool
{
    match name
    {
        "uint8" | "int8" | "char" | "byte" |
        "uint16" | "int16" | "uint32" | "int32" |
        "int" | "uint" | "uint64"| "bool" |
        "float" | "double" => true,
        _ => false,
    }
}

fn parse_type(tq: &mut TokenQueue) -> Result<Type, CompileError>
{
    if tq.is_next(TokenKind::Operator(Operator::Mul)) {
        try!(tq.pop());
        let st = try!(parse_type(tq));
        Ok(Type::Pointer(Box::new(st)))
    } else {
        let (name, _pos) = try!(tq.expect_identifier());
        if is_primitive_type(&name) {
            Ok(Type::Primitive(name))
        } else {
            Ok(Type::Complex(name))
        }
    }

}

fn parse_optional_type(tq: &mut TokenQueue) -> Result<Type, CompileError>
{
    if tq.is_next(TokenKind::Colon)
    {
        // variable with type declaration
        try!(tq.pop());
        Ok(try!(parse_type(tq)))
    }
    else
    {
        Ok(Type::Unknown)
    }
}

fn parse_vars(tq: &mut TokenQueue, indent_level: usize, constants: bool, public: bool) -> Result<Vec<Variable>, CompileError>
{
    let mut vars = Vec::new();
    loop
    {
        if let Some(level) = tq.next_indent() {
            if level <= indent_level {
                break;
            }
        }

        let tok = try!(tq.pop());
        match tok.kind
        {
            TokenKind::Identifier(id) => {
                let type_of_var = try!(parse_optional_type(tq));
                try!(tq.expect(TokenKind::Operator(Operator::Assign)));
                let expr = try!(parse_expression(tq, indent_level));
                vars.push(
                    Variable::new(
                        id,
                        type_of_var,
                        constants,
                        public,
                        expr,
                        Span::new(tok.span.start, tq.pos()),
                ));
            },
            TokenKind::Comma | TokenKind::Indent(_) => continue,
            TokenKind::EOF => break,
            _ => {
                return err(tok.span.start, ErrorType::UnexpectedToken(tok));
            }
        }
    }

    Ok(vars)
}

pub fn parse_block(tq: &mut TokenQueue, indent_level: usize, mode: ParseMode) -> Result<Block, CompileError>
{
    let mut statements = Vec::new();
    if tq.next_indent().is_none() {
        statements.push(try!(parse_statement(tq, indent_level, mode)));
    }

    loop {
        match tq.next_indent()
        {
            Some(lvl) if lvl > indent_level => {
                try!(tq.expect_indent());

                if tq.is_next(TokenKind::EOF) {
                    break;
                }

                statements.push(try!(parse_statement(tq, lvl, mode)));
            },
            _ => break,
        }
    }

    Ok(Block::new(statements))
}

fn parse_func_signature(tq: &mut TokenQueue, self_type: Type) -> Result<FunctionSignature, CompileError>
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
                args.push(Argument::new(arg_name, Type::ptr(self_type.clone()), const_arg, Span::new(arg_pos, arg_pos)));
            } else {
                return err(arg_pos, ErrorType::SelfNotAllowed);
            }
        } else {
            try!(tq.expect(TokenKind::Colon));
            let typ = try!(parse_type(tq));
            args.push(Argument::new(arg_name, typ, const_arg, Span::new(arg_pos, tq.pos())));
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

    Ok(FunctionSignature{
        name: if let Type::Complex(ref type_name) = self_type {format!("{}::{}", type_name, name)} else {name},
        return_type: ret_type,
        args: args,
    })
}

fn parse_func(tq: &mut TokenQueue, start_pos: Pos, indent_level: usize, public: bool, self_type: Type) -> Result<Function, CompileError>
{
    let sig = try!(parse_func_signature(tq, self_type));
    try!(tq.expect(TokenKind::Colon));

    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(Function::new(
        sig,
        public,
        block,
        Span::new(start_pos, tq.pos())
    ))
}

fn parse_external_func(tq: &mut TokenQueue,  start_pos: Pos) -> Result<ExternalFunction, CompileError>
{
    try!(tq.expect(TokenKind::Func));
    let sig = try!(parse_func_signature(tq, Type::Void));
    Ok(ExternalFunction::new(sig, Span::new(start_pos, tq.pos())))
}

fn parse_while(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> Result<Statement, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(Statement::While(While::new(cond, block, Span::new(pos, tq.pos()))))
}

fn parse_else(tq: &mut TokenQueue, indent_level: usize) -> Result<ElsePart, CompileError>
{
    if tq.is_next(TokenKind::If)
    {
        let tok = try!(tq.pop());
        Ok(ElsePart::If(Box::new(try!(parse_if(tq, indent_level, tok.span.start)))))
    }
    else
    {
        try!(tq.expect(TokenKind::Colon));
        Ok(ElsePart::Block(try!(parse_block(tq, indent_level, ParseMode::Block))))
    }
}

fn parse_if(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> Result<If, CompileError>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let if_block = try!(parse_block(tq, indent_level, ParseMode::Block));
    let mut else_part = ElsePart::Empty;

    if let Some(lvl) = tq.next_indent() {
        if lvl == indent_level && tq.is_next_at(1, TokenKind::Else) {
            try!(tq.pop()); // indent
            try!(tq.pop()); // else
            else_part = try!(parse_else(tq, indent_level));
        }
    }

    Ok(If::new(cond, if_block, else_part, Span::new(pos, tq.pos())))
}


fn parse_return(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> Result<Statement, CompileError>
{
    let e = try!(parse_expression(tq, indent_level));
    let span = Span::new(pos, tq.pos());
    Ok(Statement::Return(Return::new(e, span)))
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
            let st = Type::Complex(s.name.clone());
            s.functions.push(try!(parse_func(tq, tok.span.start, indent_level, public, st)));
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
            return err(tok.span.start, ErrorType::UnexpectedToken(tok));
        },
    }

    Ok(())
}

fn parse_struct(tq: &mut TokenQueue, indent_level: usize, public: bool, pos: Pos) -> Result<Struct, CompileError>
{
    let (name, _) = try!(tq.expect_identifier());
    try!(tq.expect(TokenKind::Colon));

    let mut s = Struct::new(name, public, Span::zero());
    while let Some(level) = tq.next_indent()
    {
        if level <= indent_level {break;}
        try!(tq.pop()); // indent
        try!(parse_struct_member(&mut s, tq, level, false))
    }

    s.span = Span::new(pos, tq.pos());
    Ok(s)
}

fn eat_comma(tq: &mut TokenQueue) -> Result<(), CompileError>
{
    tq.pop_if(|tok| tok.kind == TokenKind::Comma).map(|_| ())
}

fn parse_union_case(tq: &mut TokenQueue) -> Result<UnionCase, CompileError>
{
    let (name, pos) = try!(tq.expect_identifier());
    let mut uc = UnionCase::new(name, Span::zero());
    if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::CloseParen)
        {
            let (name, arg_pos) = try!(tq.expect_identifier());
            try!(tq.expect(TokenKind::Colon));
            let typ = try!(parse_type(tq));
            uc.vars.push(Argument::new(name, typ, false, Span::new(arg_pos, tq.pos())));
            try!(eat_comma(tq));
        }

        try!(tq.expect(TokenKind::CloseParen));
    }

    try!(eat_comma(tq)); // Eat trailing comma
    uc.span = Span::new(pos, tq.pos());
    Ok(uc)
}

fn parse_union_member(tq: &mut TokenQueue, indent_level: usize, public: bool, ut: Type) -> Result<Function, CompileError>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Pub => parse_union_member(tq, indent_level, true, ut),
        TokenKind::Func => parse_func(tq, tok.span.start, indent_level, public, ut),
        _ => err(tok.span.start, ErrorType::UnexpectedToken(tok)),
    }
}

fn parse_union(tq: &mut TokenQueue, indent_level: usize, public: bool) -> Result<Union, CompileError>
{
    let (name, name_pos) = try!(tq.expect_identifier());
    let mut u = Union::new(name, public, Span::zero());
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
            u.functions.push(try!(parse_union_member(tq, indent, false, Type::Complex(u.name.clone()))));
        }
    }

    u.span = Span::new(name_pos, tq.pos());
    Ok(u)
}

fn parse_match_case(tq: &mut TokenQueue, indent_level: usize) -> Result<MatchCase, CompileError>
{
    let (name, pos) = try!(tq.expect_identifier());
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
    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(MatchCase::new(name, bindings, block, Span::new(pos, tq.pos())))
}

fn parse_match(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> Result<Statement, CompileError>
{
    let expr = try!(parse_expression(tq, indent_level));
    let mut m = Match::new(expr, Span::zero());
    try!(tq.expect(TokenKind::Colon));
    while let Some(level) = tq.next_indent()
    {
        if level <= indent_level {break}
        try!(tq.pop()); // indent

        if tq.is_next(TokenKind::EOF) {break;}

        m.cases.push(try!(parse_match_case(tq, level)));
    }

    m.span = Span::new(pos, tq.pos());
    Ok(Statement::Match(m))
}

pub fn parse_statement(tq: &mut TokenQueue, indent_level: usize, mode: ParseMode) -> Result<Statement, CompileError>
{
    let tok = try!(tq.pop());
    if mode == ParseMode::Module {
        return parse_module_statement(tq, indent_level, tok);
    }

    match tok.kind
    {
        TokenKind::While => parse_while(tq, indent_level, tok.span.start),
        TokenKind::If => parse_if(tq, indent_level, tok.span.start).map(|i| Statement::If(i)),
        TokenKind::Return => parse_return(tq, indent_level, tok.span.start),
        TokenKind::Match => parse_match(tq, indent_level, tok.span.start),
        TokenKind::Identifier(id) => {
            tq.push_front(Token::new(TokenKind::Identifier(id), tok.span));
            parse_expression(tq, indent_level).map(|e| Statement::Expression(e))
        },
        _ => parse_module_statement(tq, indent_level, tok),
    }
}

pub fn parse_module_statement(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> Result<Statement, CompileError>
{
    match tok.kind
    {
        TokenKind::Import => parse_import(tq, tok.span.start),
        TokenKind::Var => parse_vars(tq, indent_level, false, false).map(|v| Statement::Variable(v)),
        TokenKind::Const => parse_vars(tq, indent_level, true, false).map(|v| Statement::Variable(v)),
        TokenKind::Func => parse_func(tq, tok.span.start, indent_level, false, Type::Void).map(|f| Statement::Function(f)),
        TokenKind::Struct => parse_struct(tq, indent_level, false, tok.span.start).map(|s| Statement::Struct(s)),
        TokenKind::Union => parse_union(tq, indent_level, false).map(|u| Statement::Union(u)),
        TokenKind::Extern => parse_external_func(tq, tok.span.start).map(|f| Statement::ExternalFunction(f)),
        TokenKind::Pub => {
            let next = try!(tq.pop());
            match next.kind
            {
                TokenKind::Var => parse_vars(tq, indent_level, false, true).map(|v| Statement::Variable(v)),
                TokenKind::Const => parse_vars(tq, indent_level, true, true).map(|v| Statement::Variable(v)),
                TokenKind::Func => parse_func(tq, next.span.start, indent_level, true, Type::Void).map(|f| Statement::Function(f)),
                TokenKind::Struct => parse_struct(tq, indent_level, true, next.span.start).map(|s| Statement::Struct(s)),
                TokenKind::Union => parse_union(tq, indent_level, true).map(|u| Statement::Union(u)),
                _ => err(tok.span.start, ErrorType::UnexpectedToken(next)),
            }
        },
        _ => err(tok.span.start, ErrorType::UnexpectedToken(tok)),
    }
}
