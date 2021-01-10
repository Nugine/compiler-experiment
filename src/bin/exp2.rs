use compiler_experiment::exp1::lexer::Lexer;
use compiler_experiment::exp1::tokens::Token;
use compiler_experiment::exp2::grammar::{Grammar, LL1Table};
use compiler_experiment::exp2::symbol::Symbol;
use compiler_experiment::exp2::DynResult;
use compiler_experiment::source_file::SourceFile;
use compiler_experiment::utils::str_join;

use std::fmt::Write as _;
use std::io::{stdin, stdout, BufRead, Write};
use std::{env, fmt, fs, process};

#[derive(Debug)]
struct Args {
    src_path: String,
}

fn parse_args() -> Result<Args, String> {
    let mut args_iter = env::args();
    args_iter.next();

    let src_path = args_iter.next().ok_or("missing argument: source path")?;

    if args_iter.next().is_some() {
        return Err("too many arguments".into());
    }

    Ok(Args { src_path })
}

fn exit_on_error<T, E: fmt::Display>(result: Result<T, E>) -> T {
    match result {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: {}", e);
            process::exit(1)
        }
    }
}

fn print_grammar(grammar: &Grammar) {
    fn display(nt: &Symbol, g: &Grammar) {
        print!("{} -> ", nt);
        let rules = str_join(
            g.productions()[nt].iter().map(|r| Grammar::fmt_rule(r)),
            " | ",
        );
        println!("{} ;", rules);
    }
    display(grammar.start_symbol(), grammar);
    for nt in grammar.non_terminals() {
        if nt != grammar.start_symbol() {
            display(nt, grammar);
        }
    }
}

fn print_first(grammar: &Grammar) {
    let first = grammar.first().unwrap();
    {
        let nt = grammar.start_symbol();
        println!("FIRST({}) = {:?}", nt, first[nt]);
    }
    for nt in grammar.non_terminals() {
        if nt != grammar.start_symbol() {
            println!("FIRST({}) = {:?}", nt, first[nt]);
        }
    }
}

fn print_follow(grammar: &Grammar) {
    let follow = grammar.follow().unwrap();
    {
        let nt = grammar.start_symbol();
        println!("FOLLLOW({}) = {:?}", nt, follow[nt]);
    }
    for nt in grammar.non_terminals() {
        if nt != grammar.start_symbol() {
            println!("FOLLLOW({}) = {:?}", nt, follow[nt]);
        }
    }
}

fn print_ll1_table(grammar: &Grammar) {
    fn display(nt: &Symbol, ll1_table: &LL1Table) {
        let mut s = String::new();
        write!(&mut s, "{:^8}|", nt).unwrap();
        for rule in ll1_table[nt].values() {
            let m;
            let w = match rule {
                Some(rule) => {
                    let rule = rule.iter().map(|sym| sym.to_string()).collect::<String>();
                    m = format!("{}->{}", nt, rule);
                    &m
                }
                None => "error",
            };
            write!(&mut s, "{:^8}|", w).unwrap();
        }
        println!("{}", s);
    }

    let ll1_table = grammar.ll1_table().unwrap();

    let head = ll1_table
        .values()
        .next()
        .unwrap()
        .keys()
        .map(|sym| format!("{:^8}|", sym))
        .collect::<String>();
    println!("{:^8}|{}", "", head);

    display(grammar.start_symbol(), ll1_table);
    for nt in grammar.non_terminals() {
        if nt != grammar.start_symbol() {
            display(nt, ll1_table);
        }
    }
}

fn show_grammar(grammar: &mut Grammar) {
    println!("规格显示：");
    print_grammar(&grammar);
    println!();

    grammar.calc_first();
    println!("FIRST集：");
    print_first(&grammar);
    println!();

    grammar.calc_follow();
    println!("FOLLOW集：");
    print_follow(&grammar);
    println!();

    if let Err(err) = grammar.calc_ll1_table() {
        println!("发现 LL(1) 分析冲突：");
        println!("{}", err);
        println!();
    } else {
        println!("LL(1) 预测分析表：");
        print_ll1_table(&grammar);
        println!();
    }
}

fn input_line() -> DynResult<String> {
    stdout().lock().flush()?;
    let stdin = stdin();
    let stdin = stdin.lock();
    let line = stdin.lines().next().unwrap()?;
    Ok(line.trim().to_owned())
}

fn test_analyze(g: &Grammar, tokens: &[Token]) -> DynResult<()> {
    let input_symbols = {
        let mut v = Vec::new();
        for tk in tokens.iter() {
            v.push(Grammar::convert_token(tk)?)
        }
        v
    };

    match g.analyze_predict(&input_symbols) {
        Ok(()) => println!("识别成功"),
        Err(err) => println!("识别失败：{:?}", err),
    }

    Ok(())
}

fn run(args: Args) -> DynResult<()> {
    let content = exit_on_error(fs::read_to_string(&args.src_path));

    let mut grammar = Grammar::parse(&content)?;

    println!("/-------------------------没有消除左递归-----------------------------/");
    show_grammar(&mut grammar);
    println!("/------------------------------------------------------------------/\n\n");

    grammar = grammar.remove_left_recursion()?;

    println!("/-------------------------已经消除左递归-----------------------------/");
    show_grammar(&mut grammar);
    println!("/------------------------------------------------------------------/\n\n");

    println!("/----------------------------词法分析-------------------------------/");
    print!("输入串：");
    let src = input_line()?;
    let lexer = Lexer::from_src(SourceFile::in_memory(&src));
    let (tokens, errors) = lexer.resolve();
    if !errors.is_empty() {
        panic!("词法错误：{:?}", errors);
    }
    println!("词法单元：{:#?}", tokens);
    println!("/------------------------------------------------------------------/\n\n");

    println!("/----------------------------语法分析-------------------------------/");
    test_analyze(&grammar, &tokens)?;
    println!("/------------------------------------------------------------------/\n\n");

    Ok(())
}

fn main() {
    let args = exit_on_error(parse_args());
    exit_on_error(run(args))
}
