#include "AST.h"
#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "Sema.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::desc("<input expression>"),
                                        llvm::cl::init(""));

int main(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "calc - the expression compiler\n");
  Lexer lex(Input);
  Parser parser(lex);
  AST *tree = parser.parse();
  if (!tree || parser.hasError()) {
    llvm::errs() << "Syntax errors occured\n";
    return 1;
  }

  Sema semantic;
  if (semantic.semantic(tree)) {
    llvm::errs() << "Semantic errors occured\n";
    return 1;
  }

  CodeGen codeGenerator;
  codeGenerator.compile(tree);
  return 0;
}
