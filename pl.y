%skeleton "lalr1.cc"

%define parser_class_name{Parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose

%locations

%code requires {
#include<map>
#include<set>
#include<list>
#include<memory>
#include<string>
#include<vector>
#include<variant>
#include<iostream>
#include<algorithm>

struct Expression;
struct Atom { std::string name; };
struct Variable { std::string name; };
struct Predicate { std::string name; std::vector<Expression> arguments; };

using ExprT = std::variant<int, Atom, std::list<Expression>, Predicate, Variable>;
struct Expression: ExprT { using ExprT::ExprT; };

struct Condition;
struct Or  { std::shared_ptr<Condition> lhs, rhs; };
struct And { std::shared_ptr<Condition> lhs, rhs; };
struct Not { std::shared_ptr<Condition> cond; };
struct None{};

using CondT = std::variant<None, Or, And, Not, Predicate>;
struct Condition: CondT { using CondT::CondT; };

struct Rule: Predicate { Condition condition; };

struct LexerContext;

template<typename T>
void printall(std::ostream &os, T const &container, char const *delim) {
  if(container.empty()) return;
  os << container.front();
  std::for_each(std::next(container.begin()), container.end(), [&os,delim](auto v){ os << delim << v; });
}

#include <ostream>
std::ostream &operator<<(std::ostream &os, Predicate const &pred) {
  os << pred.name << '(';
  printall(os, pred.arguments, ", ");
  os << ')';
  return os;
}

std::ostream &operator<<(std::ostream &os, ExprT const &expr_) {
  std::visit([&os](auto expr) {
         if constexpr(std::is_same_v<decltype(expr), int> ) os << 'i' << expr;
    else if constexpr(std::is_same_v<decltype(expr), Atom>) os << '<' << expr.name << '>';
    else if constexpr(std::is_same_v<decltype(expr), Variable>) os << '%' << expr.name << '%';
    else if constexpr(std::is_same_v<decltype(expr), std::list<Expression>>) {
      os << '[';
      printall(os, expr, ", ");
      os << ']';
    }
    else if constexpr(std::is_same_v<decltype(expr), Predicate>) os << expr;
  }, expr_);
  return os;
}

std::ostream &operator<<(std::ostream &os, CondT const &cond_) {
  std::visit([&os](auto cond) {
         if constexpr(std::is_same_v<decltype(cond), Or> ) os << *cond.lhs << ';' << *cond.rhs;
    else if constexpr(std::is_same_v<decltype(cond), And>) os << *cond.lhs << ',' << *cond.rhs;
    else if constexpr(std::is_same_v<decltype(cond), Not>) os << "\\+" << *cond.cond;
    else if constexpr(std::is_same_v<decltype(cond), Expression>) os << cond;
  }, cond_);
  return os;
}

std::ostream &operator<<(std::ostream &os, Rule const &rule) {
  os << rule.name << '(';
  printall(os, rule.arguments, ", ");
  os << ')';
  os << ":-" << rule.condition << ".\n";
  return os;
}

}

%param { LexerContext &context }

%code {
using Rules = std::multimap<std::string, std::pair<std::vector<Expression>, Condition>>;

struct LexerContext {
  char const *cursor;
  char const *marker;
  yy::location loc;
  Rules rules;
};

namespace yy { Parser::symbol_type yylex(LexerContext &ctx); }

#define COND(C) std::make_shared<Condition>(std::move(C))
}

%token END 0 "end of file"
%token ATOM "atom"
%token INT "integer"
%token NAME "name"
%left OBR '[' CBR ']'
%left OPR '(' CPR ')'
%token PNT '.'
%left OR  ';'
%left AND ','
%token BEGR ":-"
%right NOT "\\+"

%type<Rule> predicateDef
%type<std::vector<Rule>> preddefs
%type<Predicate> predicate
%type<std::string> ATOM NAME
%type<int> INT
%type<Expression> expression;
%type<std::vector<Expression>> expressions;
%type<std::list<Expression>> list
%type<Condition> condition

%%
file: preddefs {
  for(auto &def: $1)
    context.rules.emplace(def.name,
      std::make_pair(std::move(def.arguments)
                   , std::move(def.condition)));
}

preddefs: preddefs predicateDef { $$ = std::move($1); $$.emplace_back(std::move($2)); }
        | predicateDef          { $$ = {};            $$.emplace_back(std::move($1)); }
        | %empty                { $$ = {};                                            }

predicate: NAME '(' expressions ')' { $$ = {std::move($1), std::move($3)}; }
list: '[' expressions ']'           { std::move($2.begin(), $2.end(), std::back_inserter($$)); }

predicateDef: predicate ":-" condition '.' { $$ = {$1}; $$.condition = std::move($3); }
            | predicate '.'                { $$ = {$1}; $$.condition = None{}; }

expression: INT                         { $$ = std::move($1); }
          | ATOM                        { $$ = Atom{std::move($1)}; }
          | list                        { $$ = std::move($1); }
          | predicate                   { $$ = std::move($1); }
          | NAME                        { $$ = Variable{std::move($1)}; }

condition: condition ',' condition      { $$ = And{COND($1), COND($3)}; }
         | condition ';' condition      { $$ = Or {COND($1), COND($3)}; }
         | "\\+" condition              { $$ = Not{COND($2)}; }
         | '(' condition ')'            { $$ = std::move($2); }
         | predicate                    { $$ = std::move($1); }

expressions: expressions ',' expression { $$ = std::move($1); $$.emplace_back($3); }
           | expression                 { $$ = {};            $$.emplace_back($1); }
           | %empty                     { $$ = {};                                 }

%%
#undef COND
yy::Parser::symbol_type yy::yylex(LexerContext &context) {
  char const *anchor = context.cursor;
  context.loc.step();
  char const *YYMARKER;
  auto s = [&](auto func, auto&&... params) {
    context.loc.columns(context.cursor - anchor);
    return func(params..., context.loc);
  };

  %{
    re2c:yyfill:enable   = 0;
    re2c:define:YYCTYPE  = "char";
    re2c:define:YYCURSOR = "context.cursor";

    "\000"       { return s(Parser::make_END); }
    [\t\v\b\f ]  { context.loc.columns(); return yylex(context); }
    [\r\n]       { context.loc.lines();   return yylex(context); }
    "%" [^\r\n]* { /* comment */          return yylex(context); }

    ":-"         { return s(Parser::make_BEGR); }
    "\\+"        { return s(Parser::make_NOT); }

    [a-z][a-zA-Z0-9_]* { return s(Parser::make_ATOM, std::string{anchor, context.cursor}); }
    [A-Z][a-zA-Z0-9_]* { return s(Parser::make_NAME, std::string{anchor, context.cursor}); }

    [0-9]+          { return s(Parser::make_INT, std::stoi(std::string{anchor, context.cursor})); }
    
    .               { return s([](auto...s){return Parser::symbol_type(s...);}, Parser::token_type(context.cursor[-1]&0xFF)); } // Single character tokens
  %}
}

void yy::Parser::error(location_type const &l, std::string const &m) {
  std::cerr << (l.begin.filename && l.begin.filename->size() ? l.begin.filename->c_str() : "(undefined)");
  std::cerr << ':' << l.begin.line << ":" << l.begin.column << '-' << l.end.column << ": " << m << "\n";
}

#include <fstream>
#include <cstdio>

int main(int argc, char **argv) {
  Rules rules;

  for(int i = 1; i < argc; ++i) {
    LexerContext context;
    std::string filename{argv[i]};
    std::ifstream file{filename};
    if(!file.good()) {
      std::cerr << "Could not open file: " << filename << "\n";
      exit(-1);
    }
    std::string buffer{std::istreambuf_iterator<char>{file}, {}};
    context.cursor = buffer.c_str();
    context.loc.begin.filename = &filename;
    context.loc.end.filename = &filename;

    yy::Parser parser{context};
    parser.parse();

    for(auto &rule: context.rules) rules.emplace(std::move(rule));
  }
  for(auto &[name, rule]: rules) {
    auto &[args, cond] = rule;
    std::cout << name << '(';
    printall(std::cout, args, ", ");
    std::cout << "):-" << cond << ".\n";
  }
  std::string filename{"STDIN"};
  std::string line;
  while(std::getline(std::cin, line)) {
    line += '\x00';
    LexerContext context;
    context.cursor = line.c_str();
    context.loc.begin.filename = &filename;
    context.loc.end.filename = &filename;
    yy::Parser parser{context};
    parser.parse();
  }
}
