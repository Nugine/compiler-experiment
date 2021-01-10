use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::exp1::tokens::Token;
use crate::utils::str_join;

use super::symbol::Symbol;
use super::DynResult;

#[derive(Debug)]
pub struct Grammar {
    start_symbol: Symbol,
    non_terminals: BTreeSet<Symbol>,
    terminals: BTreeSet<Symbol>,
    productions: BTreeMap<Symbol, RuleSet>,

    first: Option<FirstSet>,
    follow: Option<FollowSet>,
    ll1_table: Option<LL1Table>,
}

pub type Rule = Vec<Symbol>;
pub type RuleSet = BTreeSet<Rule>;

pub type FirstSet = BTreeMap<Symbol, BTreeSet<Symbol>>;
pub type FollowSet = BTreeMap<Symbol, BTreeSet<Symbol>>;
pub type LL1Table = BTreeMap<Symbol, BTreeMap<Symbol, Option<Rule>>>;

fn tracked_extend<T: Ord>(set: &mut BTreeSet<T>, iter: impl IntoIterator<Item = T>) -> bool {
    let prev_len = set.len();
    set.extend(iter);
    prev_len != set.len()
}

impl Grammar {
    pub fn parse(src: &str) -> DynResult<Self> {
        let mut production_str = String::new();

        let mut non_terminals = BTreeSet::new();
        let mut terminals = BTreeSet::new();
        let mut productions = BTreeMap::new();
        let mut start_symbol = None;

        for ch in src.chars() {
            if !ch.is_ascii() {
                bail!("unexpected non-ascii char: {}", ch);
            }
            if ch == '\n' || ch.is_ascii_whitespace() {
                continue;
            }
            if !ch.is_ascii_graphic() {
                bail!("unexpected non-graphic char: {}", ch);
            }
            if ch == '$' {
                bail!("'$' is reserved")
            }
            if ch != ';' {
                production_str.push(ch);
                continue;
            }
            if production_str.is_empty() {
                continue;
            }

            let (nt, rules) = Self::parse_production(&production_str)?;
            start_symbol.get_or_insert(nt);
            non_terminals.insert(nt);
            terminals.extend(
                rules
                    .iter()
                    .flat_map(|r| r.iter().filter(|sym| sym.is_terminal())),
            );
            productions.insert(nt, rules);

            production_str.clear();
        }

        if start_symbol.is_none() {
            bail!("empty grammar");
        }

        if !production_str.is_empty() {
            bail!("expected ';' at the end");
        }

        Ok(Self {
            non_terminals,
            terminals,
            productions,
            start_symbol: start_symbol.unwrap(),
            first: None,
            follow: None,
            ll1_table: None,
        })
    }

    pub fn parse_production(src: &str) -> DynResult<(Symbol, RuleSet)> {
        let mut stream = src.chars().peekable();

        let non_terminal = match stream.next() {
            Some(ch) => {
                if ch.is_ascii_uppercase() {
                    Symbol::new(ch)?
                } else {
                    bail!("unexpected char for non-terminal: {}", ch)
                }
            }
            None => bail!("empty production"),
        };

        {
            let ch1 = stream.next().ok_or("invalid production")?;
            let ch2 = stream.next().ok_or("invalid production")?;
            match (ch1, ch2) {
                ('-', '>') => {}
                _ => bail!("invalid production"),
            }
        }

        let mut rules: BTreeSet<Vec<Symbol>> = BTreeSet::new();
        for s in src[3..].split('|') {
            let mut rule = Vec::new();
            for ch in s.chars() {
                rule.push(Symbol::new(ch)?);
            }
            if rule.is_empty() {
                bail!("empty rule");
            }
            if rule.len() > 1 && rule.iter().all(|sym| sym.is_empty()) {
                rule = vec![Symbol::EMPTY];
            }
            rules.insert(rule);
        }

        Ok((non_terminal, rules))
    }

    pub fn fmt_rule(rule: &[Symbol]) -> String {
        str_join(rule.iter().map(|sym| sym.to_string()), " ")
    }
}

impl Grammar {
    pub fn start_symbol(&self) -> &Symbol {
        &self.start_symbol
    }

    pub fn non_terminals(&self) -> &BTreeSet<Symbol> {
        &self.non_terminals
    }

    pub fn productions(&self) -> &BTreeMap<Symbol, RuleSet> {
        &self.productions
    }

    pub fn first(&self) -> Option<&FirstSet> {
        self.first.as_ref()
    }

    pub fn follow(&self) -> Option<&FollowSet> {
        self.follow.as_ref()
    }

    pub fn ll1_table(&self) -> Option<&LL1Table> {
        self.ll1_table.as_ref()
    }
}

impl Grammar {
    pub fn calc_first(&mut self) -> &FirstSet {
        let Self {
            non_terminals,
            terminals,
            productions,
            ..
        } = &*self;

        let mut map: BTreeMap<Symbol, BTreeSet<Symbol>> = BTreeMap::new();
        map.insert(Symbol::EMPTY, BTreeSet::new());

        for &t in terminals {
            let mut set = BTreeSet::new();
            set.insert(t);
            map.insert(t, set);
        }
        for &nt in non_terminals {
            map.insert(nt, BTreeSet::new());
        }

        loop {
            let mut is_changed = false;
            for (&nt, rules) in productions {
                for rule in rules {
                    for i in 0..rule.len() {
                        let to_merge = rule[i] != nt
                            && rule[..i].iter().all(|sym| {
                                sym.is_non_terminal() && map[sym].contains(&Symbol::EMPTY)
                            });

                        if to_merge {
                            let first_yi = map[&rule[i]].clone();
                            let first_x = map.get_mut(&nt).unwrap();
                            is_changed |= tracked_extend(
                                first_x,
                                first_yi.into_iter().filter(|sym| !sym.is_empty()),
                            );
                        }
                    }
                    {
                        if rule.iter().all(|sym| map[sym].contains(&Symbol::EMPTY))
                            || rule == &[Symbol::EMPTY]
                        {
                            let first_x = map.get_mut(&nt).unwrap();
                            is_changed |= tracked_extend(first_x, Some(Symbol::EMPTY));
                        }
                    }
                }
            }
            if !is_changed {
                break;
            }
        }

        self.first = Some(map);
        self.first.as_ref().unwrap()
    }

    pub fn calc_follow(&mut self) -> &FollowSet {
        if self.first.is_none() {
            self.calc_first();
        }
        let Self {
            non_terminals,
            productions,
            start_symbol,
            ..
        } = &*self;
        let first = &self.first.as_ref().unwrap();

        let mut map: BTreeMap<Symbol, BTreeSet<Symbol>> = BTreeMap::new();
        for &nt in &*non_terminals {
            map.insert(nt, BTreeSet::new());
        }
        {
            let follow_s = map.get_mut(start_symbol).unwrap();
            follow_s.insert(Symbol::EOF);
        }
        loop {
            let mut is_changed = false;
            for (&nt, rules) in productions {
                for rule in rules {
                    for i in 0..rule.len() - 1 {
                        if rule[i].is_non_terminal() {
                            let first_beta = {
                                match rule[i + 1..].iter().find(|sym| !sym.is_empty()) {
                                    Some(sym) => &first[sym],
                                    None => continue,
                                }
                            };

                            {
                                let follow_b = map.get_mut(&rule[i]).unwrap();
                                is_changed |= tracked_extend(
                                    follow_b,
                                    first_beta.iter().copied().filter(|sym| !sym.is_empty()),
                                );
                            }

                            if first_beta.contains(&Symbol::EMPTY) {
                                let follow_a = map[&nt].clone();
                                let follow_b = map.get_mut(&rule[i]).unwrap();
                                is_changed |= tracked_extend(follow_b, follow_a);
                            }
                        }
                    }
                    {
                        let b = rule.last().unwrap();
                        if b.is_non_terminal() {
                            let follow_a = map[&nt].clone();
                            let follow_b = map.get_mut(b).unwrap();
                            is_changed |= tracked_extend(follow_b, follow_a);
                        }
                    }
                }
            }
            if !is_changed {
                break;
            }
        }
        self.follow = Some(map);
        self.follow.as_ref().unwrap()
    }

    pub fn calc_ll1_table(&mut self) -> DynResult<&LL1Table> {
        if self.first.is_none() {
            self.calc_first();
        }
        if self.follow.is_none() {
            self.calc_follow();
        }
        let Self {
            non_terminals,
            productions,
            terminals,
            ..
        } = &*self;
        let first = self.first.as_ref().unwrap();
        let follow = self.follow.as_ref().unwrap();

        let mut table: LL1Table = BTreeMap::new();
        for &nt in non_terminals {
            let row = table.entry(nt).or_default();
            for &t in terminals {
                row.insert(t, None);
            }
            row.insert(Symbol::EOF, None);
        }

        for (&nt, rules) in productions {
            for rule in rules {
                // nt -> rule
                let select: Cow<BTreeSet<Symbol>> = match rule
                    .iter()
                    .find(|sym| !sym.is_empty())
                    .map(|sym| &first[sym])
                {
                    Some(first_alpha) => {
                        if first_alpha.contains(&Symbol::EMPTY) {
                            let mut s = first_alpha.clone();
                            s.remove(&Symbol::EMPTY);
                            s.extend(follow[&nt].iter().copied());
                            Cow::Owned(s)
                        } else {
                            Cow::Borrowed(first_alpha)
                        }
                    }
                    None => Cow::Borrowed(&follow[&nt]),
                };
                for t in select.as_ref() {
                    let table_cell = table.get_mut(&nt).unwrap().get_mut(&t).unwrap();
                    if let Some(conflict_rule) = table_cell {
                        let fmt_conflict_rule = Self::fmt_rule(conflict_rule);
                        bail!("LL1 conflict:  terminal: {:?}, select: {:?}, production: {} -> {}, production: {} -> {}",
                            t,
                            select,
                            nt,
                            fmt_conflict_rule,
                            nt,
                            Self::fmt_rule(rule)
                        );
                    }
                    *table_cell = Some(rule.clone());
                }
            }
        }
        self.ll1_table = Some(table);
        Ok(self.ll1_table.as_ref().unwrap())
    }

    pub fn remove_left_recursion(&self) -> DynResult<Self> {
        let mut new_non_terminals: BTreeSet<Symbol> = self.non_terminals.clone();
        let mut new_productions: BTreeMap<Symbol, RuleSet> = BTreeMap::new();

        let vn: Vec<Symbol> = self.non_terminals.iter().copied().collect();

        for i in 0..vn.len() {
            let mut new_rules: BTreeSet<Vec<Symbol>> = BTreeSet::new();
            for vn_i_rule in &self.productions[&vn[i]] {
                let mut is_replaced = false;
                for &vn_j in vn.iter().take(i) {
                    if vn_i_rule.starts_with(&[vn_j]) {
                        for vn_j_rule in &self.productions[&vn_j] {
                            let new_rule = vn_j_rule
                                .iter()
                                .copied()
                                .chain(vn_i_rule.iter().skip(1).copied())
                                .collect();
                            new_rules.insert(new_rule);
                        }
                        is_replaced = true;
                        break;
                    }
                }
                if !is_replaced {
                    new_rules.insert(vn_i_rule.clone());
                }
            }

            let need_new_nt = new_rules.iter().any(|rule| rule.starts_with(&[vn[i]]));
            if need_new_nt {
                let new_nt = match ('A'..='Z')
                    .map(|c| Symbol::new(c).unwrap())
                    .find(|sym| !new_non_terminals.contains(sym))
                {
                    Some(sym) => sym,
                    None => bail!("No enough uppercase letters"),
                };

                let mut alpha_rules: BTreeSet<Vec<Symbol>> = BTreeSet::new();
                let mut beta_rules: BTreeSet<Vec<Symbol>> = BTreeSet::new();
                for mut rule in new_rules {
                    if rule.starts_with(&[vn[i]]) {
                        rule.remove(0);
                        rule.push(new_nt);
                        alpha_rules.insert(rule);
                    } else {
                        rule.push(new_nt);
                        beta_rules.insert(rule);
                    }
                }
                alpha_rules.insert(vec![Symbol::EMPTY]);

                new_productions.insert(vn[i], beta_rules);
                new_productions.insert(new_nt, alpha_rules);
                new_non_terminals.insert(new_nt);
            } else {
                new_productions.insert(vn[i], new_rules);
            }
        }

        Ok(Self {
            start_symbol: self.start_symbol,
            non_terminals: new_non_terminals,
            terminals: self.terminals.clone(),
            productions: new_productions,
            first: None,
            follow: None,
            ll1_table: None,
        })
    }

    pub fn analyze_predict(&self, input: &[Symbol]) -> DynResult<()> {
        let ll1_table = self.ll1_table.as_ref().unwrap();

        let mut sym_stack: Vec<Symbol> = Vec::new();
        sym_stack.push(Symbol::EOF);
        sym_stack.push(self.start_symbol);

        let mut ip = 0;

        let mut accepted: Vec<Symbol> = Vec::new();

        while let Some(&top_sym) = sym_stack.last() {
            if top_sym == Symbol::EOF {
                break;
            }

            sym_stack.reverse();
            println!("本轮分析：");
            println!("已匹配  {:?}", accepted);
            println!("符号栈  {:?}", sym_stack);
            println!("输入串  {:?}", &input[ip..]);
            print!("动作：");
            sym_stack.reverse();

            let input_sym: Symbol = input.get(ip).copied().unwrap_or(Symbol::EOF);

            if top_sym == input_sym {
                println!("匹配终结符 {:?}", input_sym);
                sym_stack.pop();
                ip += 1;
                accepted.push(input_sym);
            } else if top_sym.is_terminal() {
                println!("报错");
                bail!("top symbol is terminal: sym_stack = {:?}", sym_stack);
            } else {
                match ll1_table[&top_sym][&input_sym] {
                    None => {
                        println!("报错");
                        bail!("LL(1) table has not item for this situation: sym_stack = {:?}, input_sym = {:?}",sym_stack, input_sym);
                    }
                    Some(ref rule) => {
                        println!("执行产生式 {} -> {}", top_sym, Self::fmt_rule(rule));
                        sym_stack.pop();
                        if rule != &[Symbol::EMPTY] {
                            sym_stack.extend(rule.iter().rev().copied());
                        }
                    }
                }
            }

            println!();
        }

        Ok(())
    }

    pub fn convert_token(token: &Token) -> DynResult<Symbol> {
        let sym = match token {
            Token::Identifier(_) | Token::Constant(_) => Symbol::new('i').unwrap(),
            Token::Punctuator(p) => {
                if p.literal.len() == 1 {
                    Symbol::new(p.literal.chars().next().unwrap()).unwrap()
                } else {
                    bail!("unsupported token: {:?}", token);
                }
            }
            Token::Operator(op) => {
                if op.literal.len() == 1 {
                    Symbol::new(op.literal.chars().next().unwrap()).unwrap()
                } else {
                    bail!("unsupported token: {:?}", token);
                }
            }
            _ => bail!("unsupported token: {:?}", token),
        };
        Ok(sym)
    }
}
