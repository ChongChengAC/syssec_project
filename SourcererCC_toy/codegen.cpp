#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h" //?
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include "node.h"
#include "y.tab.h"
#include "logerror.h"
#include <map>
#include <string>
#include <memory>
#include <utility>

//#define COMPILE_TO_X64
#define COMPILE_TO_RISCV
//#define COMPILE_TO_LL

using namespace llvm;
using namespace llvm::sys;

extern DeclList decls;

static LLVMContext TheContext;

static IRBuilder<> Builder(TheContext);

static std::unique_ptr<Module> TheModule = std::make_unique<Module>("minigcc", TheContext);

static std::map<std::string, AllocaInst *> NamedValues;
static std::map<std::string, GlobalVariable *> GlobalNamedValues;

static const unsigned int_width = 32;
static const unsigned max_array_size = 9;
// 
Value *array_size_value = ConstantInt::get(TheContext, APInt(INT32_WIDTH, 9, false));

// Checked!
Value *Number::codegen() {
	return ConstantInt::get(TheContext, APInt(int_width, value, false));
	// 
}

// Checked!
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, ID *id) {
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
			TheFunction->getEntryBlock().begin());
	AllocaInst *return_value{nullptr};
	if (id->type == "void")
		LogError("allocate for void"); // 
	else
		if (id->is_arr)
			;//return_value = TmpB.CreateAlloca(ArrayType::get(int, max_array_size), ); //
		else
			return_value = TmpB.CreateAlloca(Type::getInt32Ty(TheContext), 0, id->id_name.c_str());
	return return_value;
}

// Checked!
Value *Var::codegen() {
	Value *return_value = nullptr;
	if (id->is_arr) {
		Value *V = NamedValues[id->id_name];
		/*
		if (!exp)
			return LogErrorV("use array without index!");
		*/
		Type *array_type = ArrayType::get(Type::getInt32Ty(TheContext), max_array_size);
		if (!V) {
			GlobalValue *GV = GlobalNamedValues[id->id_name];
			if (!GV)
				return_value = LogErrorV("Unknown variable name");
			else {
				if (exp) {
					Value *index = exp->codegen();
					return_value = Builder.CreateLoad(Type::getInt32Ty(TheContext), Builder.CreateInBoundsGEP(array_type, GV, index), id->id_name.c_str());
				} else
					return LogErrorV("use array without index!");
			}

		} else {
			if (exp) {
				Value *index = exp->codegen();
				return_value = Builder.CreateLoad(Type::getInt32Ty(TheContext), Builder.CreateInBoundsGEP(array_type, V, index), id->id_name.c_str());
			} else
				return_value = V;
		}
	} else {
		Value *V = NamedValues[id->id_name];
		if (!V) {
			GlobalValue *GV = GlobalNamedValues[id->id_name];
			if (!GV)
				return_value = LogErrorV("Unknown variable name");
			else
				return_value = Builder.CreateLoad(Type::getInt32Ty(TheContext), GV, id->id_name.c_str());
		} else
			return_value = Builder.CreateLoad(Type::getInt32Ty(TheContext), V, id->id_name.c_str());
	}
	return return_value;
}

// Checked!
Value *BinaryOp::codegen() {
	Value *L = lhs->codegen();
	Value *R = rhs->codegen();
	if (!L || !R)
		return nullptr;
	if (lhs->type == "void" || rhs->type == "void")
		return LogErrorV("unmatched types!");

	if (op.empty())
		return LogErrorV("Invalid binary operator");
	else if (op == "+")
		return Builder.CreateAdd(L, R, "addtmp");
	else if (op == "-")
		return Builder.CreateSub(L, R, "subtmp");
	else if (op == "*")
		return Builder.CreateMul(L, R, "multmp");
	else if (op == "/")
		return Builder.CreateSDiv(L, R, "divtmp");
	else if (op == "==")
		return Builder.CreateICmpEQ(L, R, "eqtmp");
	else if (op == "!=")
		return Builder.CreateICmpNE(L, R, "netmp");
	else if (op == "<")
		return Builder.CreateICmpSLT(L, R, "lttmp");
	else if (op == "<=")
		return Builder.CreateICmpSLE(L, R, "letmp");
	else if (op == ">")
		return Builder.CreateICmpSGT(L, R, "gttmp");
	else if (op == ">=")
		return Builder.CreateICmpSGE(L, R, "getmp");
	else
		return LogErrorV("Invalid binary operator");
}

// Checked!
Value *Assign::codegen() {
	if (rhs->type == "void")
		return LogErrorV("unmatched types!");
	Value *Val = rhs->codegen();
	if (!Val)
		return nullptr;

	Value *Variable = NamedValues[lhs->id->id_name]; 
	if (!Variable) {
		Variable = GlobalNamedValues[lhs->id->id_name];
		if (!Variable)
			return LogErrorV("Unknown variable name");
	}
	if (lhs->id->is_arr) {
		Type *array_type = ArrayType::get(Type::getInt32Ty(TheContext), max_array_size);
		Value *index = lhs->exp->codegen();
		Builder.CreateStore(Val, Builder.CreateInBoundsGEP(array_type, Variable, index));
	} else
		Builder.CreateStore(Val, Variable);
	return Val;
}

// Checked!
Value *Call::codegen() {
	Function *CalleeF = TheModule->getFunction(id->id_name);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");
	if (CalleeF->arg_size() != exp_list.size())
		// 
		return LogErrorV("Incorrect # arguments passed");

	//
	/*
	for (size_t i = 0; i < exp_list.size(); ++i) {
		// int
		if (CalleeF->getParamByValType(i) ==
			Type::getInt32Ty(TheContext) &&
			exp_list[i]->type == "int" && !exp_list[i]->is_arr)
			;
		else
			return LogErrorV("Arguments type unmatched");
	}
	*/

	std::vector<Value *> ArgsV;
	for (size_t i = 0; i < exp_list.size(); ++i) {
		ArgsV.push_back(exp_list[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}

	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

// Checked!
AllocaInst *FuncDecl::codegen(bool last) {
	// 
	Function *TheFunction = TheModule->getFunction(ident->id_name);
	if (TheFunction)
		return (AllocaInst *)LogErrorF("Function cannot be redefined.");

	if (last && ident->id_name != "main")
		return (AllocaInst *)LogErrorF("Last function is not `main'");

	// 
	std::vector<Type *> args_type;
	for (auto para : paras)
		if (para->is_arr)
			args_type.push_back(ArrayType::get(Type::getInt32Ty(TheContext), max_array_size));
		else
			args_type.push_back(Type::getInt32Ty(TheContext));

	// 
	Type *ret_type;
	if (ident->type == "int")
	       ret_type = Type::getInt32Ty(TheContext);
	else if (ident->type == "void")
		ret_type = Type::getVoidTy(TheContext);
	else
		return (AllocaInst *)LogErrorF("Invalid function return type");

	FunctionType *FT = FunctionType::get(ret_type, args_type, false);
	TheFunction = Function::Create(FT, Function::ExternalLinkage, ident->id_name, TheModule.get());

	// 
	size_t i = 0;
	for (auto &arg : TheFunction->args())
		arg.setName(paras[i++]->id_name);

	// 

	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	std::vector<AllocaInst *> OldBindings;
	for (auto &arg : TheFunction->args()) {
		Value *InitVal = &arg;
		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, new ID("int", (std::string)(arg.getName()), false));
		Builder.CreateStore(InitVal, Alloca);
		OldBindings.push_back(NamedValues[(std::string)(arg.getName())]);
		NamedValues[(std::string)(arg.getName())] = Alloca;
	}

	cpd->codegen();
	verifyFunction(*TheFunction);

	for (size_t i = 0; i < OldBindings.size(); ++i)
		NamedValues[paras[i]->id_name] = OldBindings[i];

	return (AllocaInst *)TheFunction;
}

// Checked!
void IfStmt::codegen() {
	Value *CondV = condition->codegen();
	if (!CondV)
		return LogError("none condition!");

	CondV = Builder.CreateICmpNE(
		CondV, ConstantInt::get(CondV->getType(), 0),
		"ifcond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	if (!true_stmt)
		return LogError("none then block");

	BasicBlock *ThenBB = BasicBlock::Create(
		TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	if (false_stmt)
		Builder.CreateCondBr(CondV, ThenBB, ElseBB);
	else
		Builder.CreateCondBr(CondV, ThenBB, MergeBB);

	Builder.SetInsertPoint(ThenBB);
	true_stmt->codegen();
	Builder.CreateBr(MergeBB);

	if (false_stmt) {
		TheFunction->getBasicBlockList().push_back(ElseBB);
		Builder.SetInsertPoint(ElseBB);
		false_stmt->codegen();
		Builder.CreateBr(MergeBB);
	}

	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
}

// Checked!
void WhileStmt::codegen() {
	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	BasicBlock *LoopStartBB =
		BasicBlock::Create(TheContext, "loopstart", TheFunction);
	Builder.CreateBr(LoopStartBB);
	Builder.SetInsertPoint(LoopStartBB);

	Value *EndCond = condition->codegen();
	if (!EndCond)
        	return;

	EndCond = Builder.CreateICmpNE(
	        EndCond, ConstantInt::get(EndCond->getType(), 0),
        	"loopcond");

	BasicBlock *LoopBodyBB =
        	BasicBlock::Create(TheContext, "loopbody", TheFunction);
	BasicBlock *LoopEndBB =
		BasicBlock::Create(TheContext, "loopend", TheFunction);
	Builder.CreateCondBr(EndCond, LoopBodyBB, LoopEndBB);

	Builder.SetInsertPoint(LoopBodyBB);
	stmt->codegen();
	Builder.CreateBr(LoopStartBB);

	Builder.SetInsertPoint(LoopEndBB);
}

// Checked!
void Compound::codegen() {
	std::vector<AllocaInst *> OldBindings;

	for (auto var_decl : var_list) {
		AllocaInst *Alloca = var_decl->codegen();
		OldBindings.push_back(NamedValues[var_decl->ident->id_name]);
		NamedValues[var_decl->ident->id_name] = Alloca;
	}

	for (auto stmt : stmt_list)
		stmt->codegen();

	for (size_t i = 0; i < OldBindings.size(); ++i)
		NamedValues[var_list[i]->ident->id_name] = OldBindings[i];
}

// Checked!
AllocaInst *VarDecl::codegen(bool is_local) {
	/*
	Function *TheFunction = Builder.GetInsertBlock()->getParent();//局部变量
	AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, ident);
	*/
	if (is_local) {
		AllocaInst *Alloca = nullptr;
		if (ident->is_arr)
			Alloca = Builder.CreateAlloca(ArrayType::get(Type::getInt32Ty(TheContext), max_array_size), 0, ident->id_name.c_str());
		else
			Alloca = Builder.CreateAlloca(Type::getInt32Ty(TheContext), 0, ident->id_name.c_str());
		return Alloca;
	} else {
		if (ident->is_arr) {
			ArrayType *array_type = ArrayType::get(Type::getInt32Ty(TheContext), max_array_size);
			GlobalVariable *v = new GlobalVariable(*TheModule, array_type, false, GlobalValue::CommonLinkage, 0, ident->id_name);
			ConstantInt *const_0 = ConstantInt::get(Type::getInt32Ty(TheContext), 0);
			std::vector<Constant *> const_array(max_array_size, const_0);
			v->setInitializer(ConstantArray::get(array_type, const_array));
			GlobalNamedValues[ident->id_name] = v;
		} else {
			GlobalVariable *v = new GlobalVariable(*TheModule, Type::getInt32Ty(TheContext), false, GlobalValue::CommonLinkage, 0, ident->id_name);
			v->setInitializer(ConstantInt::get(Type::getInt32Ty(TheContext), 0));
			GlobalNamedValues[ident->id_name] = v;
		}
	}
	return nullptr;
}

// Checked!
void ExpStmt::codegen() {
	exp->codegen();
}

// Checked!
void RtStmt::codegen() {
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	if (exp) {
		Value *ret_val = exp->codegen();
		if (TheFunction->getReturnType() ==
			Type::getVoidTy(TheContext))
			LogError("Return value unmatched!");
		else
			Builder.CreateRet(ret_val);
	} else {
		if (TheFunction->getReturnType() !=
			Type::getVoidTy(TheContext))
			LogError("Return value unmatched!");
		else
			Builder.CreateRetVoid();
	}
}

Value *Exp::codegen() {
	return nullptr;
}

void Statement::codegen() {
}

Value *ID::codegen() {
	return nullptr;
}

Value *Para::codegen() {
	return nullptr;
}

Function *MainFunction;

int main()
{
	std::cout << "Colors represent Base Classes: " << STDOUT_GREEN << 
		"Declaration " << STDOUT_BLUE << "Statement " << STDOUT_CYAN <<
		"Expression " << STDOUT_YELLOW << "ID" << STDOUT_RESET <<
		std::endl;
	yyparse();
	for (auto decl : decls)
		if (decl == decls.back())
			MainFunction = (Function *)(decl->codegen(true));
		else
			decl->codegen(false);
	TheModule->print(errs(), nullptr);
	//test11();
#ifdef COMPILE_TO_LL
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	ExecutionEngine *EE = EngineBuilder(std::move(TheModule)).setEngineKind(EngineKind::JIT).create();
	void *main_addr = EE->getPointerToFunction(MainFunction);
	typedef unsigned long (*FuncType)();
	FuncType mainFunc = (FuncType)main_addr;
	EE->finalizeObject();
	std::cout << mainFunc() << std::endl;
#else
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();
	std::string TargetTriple;
#ifdef COMPILE_TO_RISCV
	TargetTriple = "riscv64-unknown-linux-unknown";
	auto CPU = "";
#elifdef COMPILE_TO_X64
	TargetTriple = sys::getDefaultTargetTriple();
	auto CPU = "generic";
#endif
	std::cout << TargetTriple << std::endl;
	TheModule->setTargetTriple(TargetTriple);
	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
	if (!Target) {
		errs() << Error;
		return 1;
	}

	auto Features = "";

	TargetOptions opt;
	auto RM = Optional<Reloc::Model>();
	auto TheTargetMachine = Target->createTargetMachine(TargetTriple,
		       		CPU, Features, opt, RM);

	TheModule->setDataLayout(TheTargetMachine->createDataLayout());

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return 1;
	}

	legacy::PassManager pass;
	auto FileType = CGFT_ObjectFile;

	if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr,
			       			FileType)) {
		errs() << "TheTargetMachine can't emit a file of this type";
		return 1;
	}
	// 
	pass.run(*TheModule);
	dest.flush();
	outs() << "Wrote " << Filename << "\n";
#endif
	return 0;
}
