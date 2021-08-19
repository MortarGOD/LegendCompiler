#include <bits/stdc++.h>
using namespace std;

#define TEXTLEN 8848	// 变量名最大长度
#define NSYMBOLS 888888	// 变量最大数量
#define INF 999999999 // 非const变量的值
#define NOREG	-1 
#define NOLABEL	0

const int push_reg = 32; //保存r4——r10八个寄存器的值
const int L_field = 3072; //ldr跳转预警值3KB
const int OF = 2;
const int Threshold = 4; //寄存器替换阈值

const int imm8 = 255;
const int imm12 = 4095;
const int imm16 = 65535;

const int P_2[31] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 
  131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456,
  536870912, 1073741824};

// Token types
enum {
  T_EOF,

  // Binary operators
  T_ASSIGN, T_LOGOR, T_LOGAND, 
  T_EQ, T_NE,
  T_LT, T_GT, T_LE, T_GE,
  T_PLUS, T_MINUS, T_STAR, T_SLASH, T_PER,

  // Other operators
  T_LOGNOT,

  // Type keywords
  T_VOID, T_INT,

  // Other keywords
  T_IF, T_ELSE, T_WHILE, T_CONTINUE, T_RETURN, T_BREAK, T_CONST,

  // Structural tokens
  T_INTCONST, T_STRCONST, T_SEMI, T_IDENT, T_NOTE,
  T_LB, T_RB, T_LP, T_RP,
  T_LBK, T_RBK, T_COMMA
};

// Token structure
struct token {
  int token; //单词标签
  int intvalue; //单词数值
  string strvalue; //单词变量名
  int line; //单词所在行号
};

vector<token> token_stream; //单词序列

enum {
  A_ASSIGN= 1, A_LOGOR, A_LOGAND,
  A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE,
  A_ADD, A_SUBTRACT, A_MULTIPLY, A_DIVIDE, A_MOD,
  A_INTLIT, A_STRLIT, A_IDENT, A_GLUE,
  A_IF, A_WHILE, A_FUNCTION, A_RETURN, A_CONTINUE, A_BREAK,
  A_FUNCCALL, A_ADDR,
  A_LOGNOT, A_DEFINE, A_RSB, A_INIT
};

// Primitive types
enum {
  P_NONE, P_VOID, P_INT
};

// Abstract Syntax Tree structure
struct ASTnode {
  int op;			// AST树节点类型
  int type;			// 变量类型
  int rvalue;			// 是否是右值
  int Dimension_num;  // 如果是数组, 表示该数组共有几维
  bool istop;  // 对短路语句, 判断是否是最后一项
  ASTnode *left;		// 左、中、右、next子树
  ASTnode *mid;
  ASTnode *right;
  ASTnode *next;
  union {
    int intvalue;		// 对一个变量, 代表在symtable数组中的下标
    int id;			// 对一个函数, 代表在symtable数组中的下标
  } v;
  string strvalue;
  int line;
};

// Structural types
enum {
  S_VARIABLE, S_FUNCTION, S_ARRAY
};

// Storage classes
enum {
  C_GLOBAL = 1,		// 全局变量
  C_LOCAL,		// 局部变量
  C_PARAM			// 函数参数
};

// Symbol table structure
struct symtable {
  string name;			// 变量名
  int type;			// 变量类型(int)
  int stype;			// 变量名类型(函数、变量、数组)
  int ctype;			// 变量名类型(全局、局部、参数)
  int endlabel;			// 对一个函数, 它的结束标签
  int size;			// 对一个数组, 它的大小
  int posn;	

#define nelems posn		// 对一个函数, 它的参数数量

  int st_address;   //对一个局部变量, 它在栈帧中的起始地址
  int intvalue;  //对一个数值常量, 它的数值
  int paramnum;  //对一个函数参数, 它是第几个参数
  vector<int> Dimens; //对一个数组, 它每一维分别有多大
  vector<int> conNum;  //对一个const数组, 它的初始值
};

typedef struct keyword
{
	int id;
	char name[16];
} KEYWORD;

static KEYWORD keywords[16] =
{
	{T_INT, "int"},
	{T_RETURN, "return"},
	{T_CONTINUE, "continue"},
	{T_BREAK, "break"},
	{T_IF, "if"},
	{T_ELSE, "else"},
	{T_WHILE, "while"},
	{T_VOID, "void"},
	{T_CONST, "const"}
};

// AST node types
enum {
  ADD, SUBTRACT, MULTIPLY, DIVIDE, MOD, INTCONST
};

// 用于编译时能求值的表达式的AST树结构体
struct ASnode {
  int op;				// 常量类型
  ASnode *left;			// 左、右子树
  ASnode *right;
  int intvalue;				// 常量的值
};

void new_field();

int scan(token *t);

ASTnode *newASTnode(int op, int type, ASTnode *left, ASTnode *mid, ASTnode *right, int intvalue);
ASTnode *newASTleaf(int op, int type, int intvalue);
ASTnode *newASTunary(int op, int type, ASTnode *left, int intvalue);

int genlabel(void);
int genLC(void);
int CodeGen(ASTnode *n, int label, int looptoplabel, int loopendlabel, int parentASTop);
void printAST(ASTnode *root);
void genpreamble();
void genpostamble();
void genfreeregs();
void genglobsym(int id, int num);
void genglobsym_plus(int id, vector<int> &x);
int genglobstr(string strvalue);
void genreturn(int reg, int id);

void freeall_registers(void);
void ARM_preamble();
void ARM_funcpre(int id);
void ARM_funcpos(int id);
int ARM_loadint(int value);
int ARM_loadglob(int id);
int ARM_loadlocal(int id);
int ARM_add(int r1, int r2);
int ARM_sub(int r1, int r2);
int ARM_mul(int r1, int r2);
int ARM_div(int r1, int r2);
int ARM_mod(int r1, int r2);
int ARM_funccall(int r, int id);
int ARM_str_glob(int r, int id);
int ARM_str_local(int r, int id);
void ARM_globsym(int id, int num);
void ARM_globsym_plus(int id, vector<int> &x);
int ARM_cmp_jump(int r, int label);
void ARM_label(int l);
void ARM_jump(int l);
int ARM_cmp_jmp(int ASTop, int r1, int r2, int label, int parentASTop);
void ARM_return(int reg, int id);

ASTnode *Exp_Stmt();
ASTnode *Cond();
ASTnode *AddExp();
ASTnode *MulExp();
ASTnode *UnaryExp();
ASTnode *LOrExp();
ASTnode *LAndExp();
ASTnode *EqExp();
ASTnode *RelExp();

ASTnode *Block(void);

void match(int t, string what);
void semi(void);
void lbrace(void);
void rbrace(void);
void lparen(void);
void rparen(void);
void ident(void);
void fatal(string s);
void fatals(string s1, string s2);
void fatald(string s, int d);
void fatalc(string s, int c);

int findglob(string s);
int findlocl(string s);
int findsymbol(string s);
int addglob(string name, int type, int stype, int endlabel, int size);
int addlocl(string name, int type, int stype, int isparam, int size);
void freeloclsyms(void);

void VarDef(int type, int islocal, int isparam);
ASTnode *FuncDef(int type);
void CompUnit(void);

int FuncType(void);

ASnode *newNode(int op, ASnode *left, ASnode *right, int intvalue);
ASnode *newLeaf(int op, int intvalue);
ASnode *newUnary(int op, ASnode *left, int intvalue);
ASnode *Exp(int rbp);
int interpretAST(ASnode *n);


int Inchar;  //当前输入字符
int Line;  //当前行号
int Putback;
int Functionid;		// 函数在symtable中的id
int Globs;		// 全局变量数
int Locls;		// 局部变量数
stack<int>Locls_save;  // 保存局部变量作用域
int Local_v;  // 一个函数领域内的局部变量数
int func_v;  // 由函数调用生成的临时变量数
int Looplevel;  // while语句嵌套深度
int max_param;  // 函数调用最大参数数量
bool con;  // 是否const定义
bool last_or;  // 是否是逻辑或语句的最后一项
bool last_and;  // 是否是逻辑与语句的最后一项
FILE *Infile;	
FILE *Outfile;
int token_index; // 当前是第几个单词
int token_size; // 单词数
int pars; // 统计是第几个函数参数
int func_field; // 当前领域
int true_L;  // if和while语句的true标签
int ldr_Byte;  // 当前汇编输出字节数
string nowname;  //上一个读取的标识符
char Text[TEXTLEN + 1];	
symtable Symtable[NSYMBOLS];	// 变量信息集合
char buf[8848];

bool if_PerformanceTest = false; //是否开启性能优化
int while_level; //while嵌套层数
int max_alloc; //while语句块中出现过的最大寄存器编号
int while_st; //while语句块起始汇编下标
int while_ed; //while语句块终止汇编下标
unordered_map<int, int> ump; //键: fp偏移量, 值: 出现次数 
unordered_map<int, int> newReg; //键: fp偏移量, 值: 新分配的寄存器
bool cmp(const pair<int,int> &a, const pair<int,int> &b){
  return a.second > b.second;
}

vector<string> ObjectCode;  //汇编集合

// 用于申请的寄存器集合
static int freereg[7];
static char const *reglist[7] = { "r4", "r5", "r6", "r7", "r8", "r9", "r10"};

// 释放所有寄存器
void freeall_registers(void) {
  freereg[0] = freereg[1] = freereg[2] = freereg[3] = freereg[4] = freereg[5] = freereg[6] = 1;
}

// 申请一个寄存器
static int alloc_register(void) {
  for (int i = 0; i < 7; i++) {
    if (freereg[i]) {
      freereg[i] = 0;
      return (i);
    }
  }
  exit(26);
  fatal("Out of registers");
  return (NOREG);		// Keep -Wall happy
}

// 申请一个寄存器
static int alloc_register_plus(void) {
  for (int i = 6; i > max_alloc; i--) {
    if (freereg[i]) {
      freereg[i] = 0;
      return (i);
    }
  }
  return (NOREG);		// Keep -Wall happy
}

// 释放指定寄存器
static void free_register(int reg) {
  freereg[reg] = 1;
}

void putf(string s){
  string ins = "\t.ascii\t\"";
  int len = s.size();
  for(int i = 0; i < len; ++i){
    if(s[i] == '%'){
      ins += '%';
      ins += '%';
    }else{
      ins += s[i];
    }
  }
  ins += "\\000\"\n";
  ObjectCode.emplace_back(ins);
  return ;
}

#define MAXN 8848
vector<int>Intlist;
int Intslot = 0;

// 计算一个整数在汇编中的字节数
int leng(int a){
  int flag = 0;
  while(a){
    a /= 10;
    flag++;
  }
  return flag;
}

// 从标签中读取一个常量
static void set_int_offset(int r, int val) {
  int offset = -1;

  for (int i = 0; i < Intlist.size(); i++) {
    if (Intlist[i] == val) {
      offset = 4 * i;
      break;
    }
  }

  if (offset == -1) {
    offset = 4 * Intslot;
    Intlist.emplace_back(val);
    ++Intslot;
    ldr_Byte += (8 + leng(val));
  }
  for (int i = 0; i < Globs; i++) {
    if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
      offset += 4;
  }
  
  sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[r], func_field, offset);
  ObjectCode.emplace_back(buf); ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
}

pair<int, string> LClist[MAXN];
static int LCslot = 0;

static void set_lc_offset(int r, string val) {
  int offset = -1;

  for (int i = 0; i < LCslot; i++) {
    if (LClist[i].second == val) {
      offset = 4 * i;
      break;
    }
  }

  if (offset == -1) {
    offset = 4 * LCslot;
    if (LCslot == MAXN)
      fatal("Out of int slots in set_int_offset()");
    LClist[LCslot].first = genLC();
    LClist[LCslot++].second = val;
  }
  for (int i = 0; i < Globs; i++) {
    if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
      offset += 4;
  }
  offset += 4 * Intslot;

  sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[r], func_field, offset);
  ObjectCode.emplace_back(buf); ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
}

void addString(string s){
  int offset = -1;

  for (int i = 0; i < LCslot; i++) {
    if (LClist[i].second == s) {
      offset = 4 * i;
      break;
    }
  }

  if (offset == -1) {
    offset = 4 * LCslot;
    if (LCslot == MAXN)
      fatal("Out of int slots in set_int_offset()");
    LClist[LCslot].first = genLC();
    LClist[LCslot++].second = s;
  }
  return ;
}

//寄存器分配之扫描
void ScanRegPass(string s){
  const int len = s.size();
  if(len > 4){
    string operand = s.substr(1, 3);
    //扫描出最大使用过的寄存器
    int maxReg = 0;
    for(int i = 0; i < len; ++i){
      if(s[i] == 'r' && isdigit(s[i+1])){
        maxReg = max(maxReg, s[i+1]-'0');
      }
    }
    max_alloc = max(maxReg, max_alloc);
    //统计出现次数最多的局部变量地址
    if(operand == "ldr" || operand == "str"){
      for(int i = 0; i < len; ++i){
        if(s[i] == '[' && s[i+1] == 'f' && s[i+2] == 'p'){
          int st = i + 6;
          int ed;
          for(int j = i+3; j < len; ++j){
            if(s[j] == ']'){
              ed = j;
              break;
            }
          }
          int var_address = stoi(s.substr(st, ed-st));
          ump[var_address]++;
        }
      }
    }
  }
  return ;
}

//寄存器分配之修改
string AllocRegPass(string s){
  const int len = s.size();
  if(len > 4){
    string operand = s.substr(1, 3);
    int Reg;
    if(operand == "ldr" || operand == "str"){
      for(int i = 0; i < len; ++i){
        //扫描出写入或者读出的寄存器
        if(s[i] == 'r' && isdigit(s[i+1])){
          Reg = s[i+1]-'0';
        }
        //将局部变量地址替换成寄存器
        if(s[i] == '[' && s[i+1] == 'f' && s[i+2] == 'p'){
          int st = i + 6;
          int ed;
          for(int j = i+3; j < len; ++j){
            if(s[j] == ']'){
              ed = j;
              break;
            }
          }
          int var_address = stoi(s.substr(st, ed-st));
          if(newReg.find(var_address) != newReg.end()){
            int r = newReg[var_address];
            if(operand == "ldr"){
              sprintf(buf, "\tmov	r%d, %s\n", Reg, reglist[r]);
              s = buf;
            }else{
              sprintf(buf, "\tmov	%s, r%d\n", reglist[r], Reg);
              s = buf;
            }
          }
        }
      }
    }
  }
  return s;
}

// 生成一个新的全局变量、常量标签, 更新ldr指令作用域
void new_field(){
  sprintf(buf, "\tb\t.L%d\n", func_field+2);
  ObjectCode.emplace_back(buf);
  sprintf(buf, ".L%d:\n", func_field+1);
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.align 2\n");
  ObjectCode.emplace_back(buf);

  ldr_Byte = 0;

  sprintf(buf, ".L%d:\n", func_field);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  for (int i = 0; i < Globs; i++) {
    if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF)){
      sprintf(buf, "\t.word %s\n", Symtable[i].name.c_str());
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
    }
  }
  for (int i = 0; i < Intlist.size(); i++) {
    sprintf(buf, "\t.word %d\n", Intlist[i]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  }
  for (int i = 0; i < LCslot; i++) {
    sprintf(buf, "\t.word .LC%d\n", LClist[i].first);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  }
  sprintf(buf, ".L%d:\n", func_field+2);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  func_field = genlabel();
  genlabel();
  genlabel();
  Intlist.clear();
  Intslot = 0;
  return ;
}

void ARM_preamble() {
  freeall_registers();
  fputs("\t.text\n", Outfile);
  fprintf(Outfile, "\t.arch\tarmv7ve\n");
}

/*函数声明前缀*/
void ARM_funcpre(int id) {
  for (int i = 0; i < LCslot; i++) {
    sprintf(buf, "\t.align 2\n");
    ObjectCode.emplace_back(buf);
    sprintf(buf, ".LC%d:\n", LClist[i].first);
    ObjectCode.emplace_back(buf);
    putf(LClist[i].second);
  }
  string name = Symtable[id].name;
  sprintf(buf, "\t.text\n");
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.align 2\n");
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.global\t%s\n", name.c_str());
  ObjectCode.emplace_back(buf);
  sprintf(buf, "%s:\n", name.c_str());
  ObjectCode.emplace_back(buf);


  sprintf(buf, "\tpush\t{r4, r5, r6, r7, r8, r9, r10, fp, lr}\n");
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\tadd\tfp, sp, #%d\n", push_reg);
  ObjectCode.emplace_back(buf);
  int subnum = Local_v*4;
  if(max_param > 5){
    subnum += (max_param - 5)*4;
  }
  subnum += 4;
  if(!if_PerformanceTest) subnum += (func_v)*4;
  int r = alloc_register();
  if(subnum <= imm8){
    sprintf(buf, "\tsub\tsp, sp, #%d\n", subnum);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  }else{
    set_int_offset(r, subnum);
    sprintf(buf, "\tsub\tsp, sp, %s\n", reglist[r]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
  }
  free_register(r);
  func_v = 1;
  int sub = -36;
  for(int i = 0; i < Symtable[id].nelems && i < 4; ++i){
    sprintf(buf, "\tstr\tr%d, [fp, #%d]\n", i, sub);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);
    sub -= 4;
  }

  if(if_PerformanceTest){
    max_alloc = 0;
    while_st = ObjectCode.size();
    ump.clear();
    newReg.clear();
  }
}

/*函数声明后缀*/
void ARM_funcpos(int id) {
  if(if_PerformanceTest){
    while_ed = ObjectCode.size();
    for(int i = while_st; i < while_ed; ++i){
      ScanRegPass(ObjectCode[i]);
    }
    max_alloc -= 4;
    vector<pair<int, int> >a;
    for(auto x : ump){
      a.push_back(x);
    }
    sort(a.begin(), a.end(), cmp);
    int now = 0;
    while(1){
      if(now >= a.size()) break;
      if(a[now].second <= Threshold) break;
      int reg = alloc_register_plus();
      if(reg == -1) break;
      newReg[a[now].first] = reg;
      ++now;
    }
    for(int i = while_st; i < while_ed; ++i){
      ObjectCode[i] = AllocRegPass(ObjectCode[i]);
    } 
    for(auto &x : newReg){
      int address = x.first;
      int r = x.second;
      sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], address);
      ObjectCode.emplace(ObjectCode.begin() + while_ed, buf);
    }
    for(auto &x : newReg){
      int address = x.first;
      int r = x.second;
      sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[r], address);
      ObjectCode.emplace(ObjectCode.begin() + while_st, buf);
    }
  }

  ARM_label(Symtable[id].endlabel);

  sprintf(buf, "\tsub\tsp, fp, #%d\n", push_reg);
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\tpop\t{r4, r5, r6, r7, r8, r9, r10, fp, pc}\n");
  ObjectCode.emplace_back(buf);

  sprintf(buf, ".L%d:\n", func_field+1);
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.align 2\n");
  ObjectCode.emplace_back(buf);

  sprintf(buf, ".L%d:\n", func_field);
  ObjectCode.emplace_back(buf);
  for (int i = 0; i < Globs; i++) {
    if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF)){
      sprintf(buf, "\t.word %s\n", Symtable[i].name.c_str());
      ObjectCode.emplace_back(buf);
    }
  }
  for (int i = 0; i < Intlist.size(); i++) {
    sprintf(buf, "\t.word %d\n", Intlist[i]);
    ObjectCode.emplace_back(buf);
  }
  for (int i = 0; i < LCslot; i++) {
    sprintf(buf, "\t.word .LC%d\n", LClist[i].first);
    ObjectCode.emplace_back(buf);
  }
  Intlist.clear();
  Intslot = 0;
}

/*将一个立即数放到一个新的寄存器*/
int ARM_loadint(int value) {
  int reg = alloc_register();
  if (value <= imm16){
    sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg], value);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
  }else {
    set_int_offset(reg, value);
  }
  return (reg);
}

/*将一个变量放到一个新的寄存器*/
int ARM_var_offset(int id) {
  int offset = 0;
  int reg = alloc_register();
  for (int i = 0; i < id; i++) {
    if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
      offset += 4;
  }
  sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[reg], func_field, offset);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
  return reg;
}

/*读取全局变量*/
int ARM_loadglob(int id) {
  int reg = alloc_register();
  int reg1 = ARM_var_offset(id);
  sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
  free_register(reg1);
  return (reg);
}

/*读取局部变量*/
int ARM_loadlocal(int id) {
  int reg = alloc_register();
  int subnum = abs(Symtable[id].st_address);
  int reg1 = alloc_register();
  if(subnum <= imm8){
    sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], Symtable[id].st_address);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
  }else{
    set_int_offset(reg1, subnum);
    sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
  }
  free_register(reg1);
  return (reg);
}

/*读取函数参数*/
int ARM_loadparam(int id) {
  int reg = alloc_register();
  if(Symtable[id].paramnum < 4){
    sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], Symtable[id].st_address);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
  }else{
    int subnum = (Symtable[id].paramnum - 3)*4;
    int reg1 = alloc_register();
    if(subnum <= imm8){
      sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], subnum);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }else{
      set_int_offset(reg1, subnum);
      sprintf(buf, "\tadd\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
      sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }
    free_register(reg1);
  }
  return (reg);
}

int ARM_add(int r1, int r2) {
  sprintf(buf, "\tadd\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
  free_register(r2);
  return (r1);
}

int ARM_sub(int r1, int r2) {
  sprintf(buf, "\tsub\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  return (r1);
}

int ARM_mla(int r1, int r2, int r3) {
  sprintf(buf, "\tmla\t%s, %s, %s, %s\n", reglist[r1], reglist[r2], reglist[r3], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  free_register(r3);
  return (r1);
}

int ARM_mul(int r1, int r2) {
  sprintf(buf, "\tmul\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  return (r1);
}

int ARM_lsl(int r1, int r2, int val){
  sprintf(buf, "\tlsl\t%s, %s, #%d\n", reglist[r1], reglist[r1], val);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  return (r1);
}

int ARM_rsb(int r){
  sprintf(buf, "\trsb\t%s, %s, #0\n", reglist[r], reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
  return (r);
}

int ARM_div(int r1, int r2) {
  sprintf(buf, "\tsdiv\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  return (r1);
}

int ARM_asr(int r1, int r2, int val){
  int reg = alloc_register();
  if(P_2[val] - 1 <= imm8){
    sprintf(buf, "\tadd\t%s, %s, #%d\n", reglist[reg], reglist[r1], P_2[val] - 1);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  }else{
    int reg1 = alloc_register();
    set_int_offset(reg1, P_2[val] - 1);
    sprintf(buf, "\tadd\t%s, %s, %s\n", reglist[reg], reglist[r1], reglist[reg1]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    free_register(reg1);
  }
  sprintf(buf, "\tcmp\t%s, #0\n", reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmovlt\t%s, %s\n", reglist[r1], reglist[reg]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tasr\t%s, %s, #%d\n", reglist[r1], reglist[r1], val);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(reg);
  free_register(r2);
  return (r1);
}

int ARM_mod(int r1, int r2) {
  int reg = alloc_register();
  sprintf(buf, "\tsdiv\t%s, %s, %s\n", reglist[reg], reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmls\t%s, %s, %s, %s\n", reglist[r1], reglist[reg], reglist[r2], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(reg);
  free_register(r2);
  return (r1);
}

/*函数调用*/
int ARM_funccall(int r, int id) {
  sprintf(buf, "\tbl\t%s\n", Symtable[id].name.c_str());
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  if(Symtable[id].type != P_VOID){
    sprintf(buf, "\tmov\t%s, r0\n", reglist[r]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    return (r);
  }else{
    free_register(r);
    return (NOREG);
  }
}

/*写入数组变量*/
int ARM_str_array(int r1, int r2, int id) { //r1要赋的值, r2偏移量
  if(Symtable[id].ctype == C_GLOBAL){
    int r = ARM_var_offset(id);
    sprintf(buf, "\tstr\t%s, [%s, %s, lsl #2]\n", reglist[r1], reglist[r], reglist[r2]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    free_register(r);
  }else if(Symtable[id].ctype == C_LOCAL){
    sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[r2], reglist[r2]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    int reg = alloc_register();
    sprintf(buf, "\tsub\t%s, fp, #%d\n", reglist[reg], push_reg);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    ARM_add(r2, reg);
    int subnum = abs(Symtable[id].st_address + push_reg);
    int reg1 = alloc_register();
    if(subnum <= imm8){
      sprintf(buf, "\tstr\t%s, [%s, #%d]\n", reglist[r1], reglist[r2], Symtable[id].st_address + push_reg);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }else{
      set_int_offset(reg1, subnum);
      ARM_sub(r2, reg1);
      sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r1], reglist[r2]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }
  }else{
    int reg = alloc_register();
    if(Symtable[id].paramnum < 4){
      sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[r2], reglist[r2]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], Symtable[id].st_address);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      ARM_add(r2, reg);
      sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r1], reglist[r2]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }else{
      sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[r2], reglist[r2]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      int subnum = (Symtable[id].paramnum - 3)*4;
      int reg1 = alloc_register();
      if(subnum <= imm8){
        sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], subnum);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        set_int_offset(reg1, subnum);
        sprintf(buf, "\tadd\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
        sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
      }
      free_register(reg1);
      ARM_add(r2, reg);
      sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r1], reglist[r2]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }
  }
  free_register(r2);
  return r1;
}

/*写入全局变量*/
int ARM_str_glob(int r, int id) {
  int r1 = ARM_var_offset(id);
  sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r1);
  return (r);
}

/*写入局部变量*/
int ARM_str_local(int r, int id) {
  int subnum = abs(Symtable[id].st_address);
  int reg = alloc_register();
  if(subnum <= imm8){
    sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], Symtable[id].st_address);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
  }else{
    set_int_offset(reg, subnum);
    sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg], reglist[reg]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r], reglist[reg]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
  }
  free_register(reg);
  return (r);
}

/*写入函数参数*/
int ARM_str_param(int r, int id) {
  if(Symtable[id].paramnum < 4){
    sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], Symtable[id].st_address);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  }else{
    int subnum = (Symtable[id].paramnum - 3)*4;
    int reg = alloc_register();
    if(subnum <= imm8){
      sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], subnum);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }else{
      set_int_offset(reg, subnum);
      sprintf(buf, "\tadd\t%s, fp, %s\n", reglist[reg], reglist[reg]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
      sprintf(buf, "\tstr\t%s, [%s]\n", reglist[r], reglist[reg]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF);if(ldr_Byte >= L_field){ new_field(); }
    }
    free_register(reg);
  }
  return (r);
}

/*未初始化的全局变量声明*/
void ARM_globsym(int id, int num) {
  sprintf(buf, "\t.comm\t%s,%d,4\n", Symtable[id].name.c_str(), 4*num);
  ObjectCode.emplace_back(buf);
}

/*已初始化的全局变量声明*/
void ARM_globsym_plus(int id, vector<int> &x){
  sprintf(buf, "\t.global\t%s\n", Symtable[id].name.c_str());
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.data\n");
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.align\t2\n");
  ObjectCode.emplace_back(buf);
  sprintf(buf, "\t.size\t%s, %d\n", Symtable[id].name.c_str(), (int)(x.size())*4);
  ObjectCode.emplace_back(buf);
  sprintf(buf, "%s:\n", Symtable[id].name.c_str());
  ObjectCode.emplace_back(buf);
  for(int i = 0; i < x.size(); i++){
    sprintf(buf, "\t.word\t%d\n", x[i]);
    ObjectCode.emplace_back(buf);
  }
}

/*生成新的标签*/
void ARM_label(int l) {
  sprintf(buf, ".L%d:\n", l);
  ObjectCode.emplace_back(buf);
}

/*跳转到指定标签*/
void ARM_jump(int l) {
  sprintf(buf, "\tb\t.L%d\n", l);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
}

// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char const *brlist[] = { "bne", "beq", "bge", "ble", "bgt", "blt" };

// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char const *brlist_plus[] = { "beq", "bne", "blt", "bgt", "ble", "bge" };

/*短路语句跳转*/
int ARM_cmp_jmp(int ASTop, int r1, int r2, int label, int parentASTop) {
  //在逻辑或语句里但不是最后一个条件判断元素 || 在逻辑与语句里且是最后一个条件判断元素, 但不包含在最后一个逻辑或条件判断元素中
  if((parentASTop == A_LOGOR && !last_or) || (parentASTop == A_LOGAND && last_and && !last_or)){
    sprintf(buf, "\tcmp\t%s, %s\n", reglist[r1], reglist[r2]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\t%s\t.L%d\n", brlist_plus[ASTop - A_EQ], true_L);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  }else{
    sprintf(buf, "\tcmp\t%s, %s\n", reglist[r1], reglist[r2]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
    sprintf(buf, "\t%s\t.L%d\n", brlist[ASTop - A_EQ], label);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
  }
  freeall_registers();
  return (NOREG);
}

/*对单个元素, 直接和0比较判断*/
int ARM_cmp_jump(int r, int label) {
  sprintf(buf, "\tcmp\t%s, #0\n", reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tbeq\t.L%d\n", label);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  freeall_registers();
  return (NOREG);
}

// 比较指令表,即mov指令的条件 
// AST类型顺序: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
char const *cmplist1[] ={"eq", "ne", "lt", "gt", "le", "ge"};
char const *cmplist2[] ={"ne", "eq", "ge", "le", "gt", "lt"};

// 比较两个寄存器并设置寄存器值
int ARM_compare_and_set(int ASTop, int r1, int r2){
  sprintf(buf, "\tcmp\t%s, %s\n", reglist[r1], reglist[r2]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmov%s\t%s, #1\n", cmplist1[ASTop - A_EQ], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmov%s\t%s, #0\n", cmplist2[ASTop - A_EQ], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tuxtb\t%s, %s\n", reglist[r1], reglist[r1]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  free_register(r2);
  return r1;
}

/*把逻辑非语句转换成数值0和1*/
int ARM_lognot(int r, int label, int parentASTop) {
  sprintf(buf, "\tcmp	%s, #0\n", reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmoveq	%s, #1\n", reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tmovne	%s, #0\n", reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  sprintf(buf, "\tuxtb	%s, %s\n", reglist[r], reglist[r]);
  ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  return (r);
}

/*函数调用返回语句*/
void ARM_return(int reg, int id) {
  if(Symtable[id].type != P_VOID){
    sprintf(buf, "\tmov\tr0, %s\n", reglist[reg]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
  }
  ARM_jump(Symtable[id].endlabel);
  freeall_registers();
}

int FuncType(void) {
  int type;
  switch (token_stream[token_index].token) {
    case T_VOID:
      type = P_VOID;
      break;
    case T_INT:
      type = P_INT;
      break;
  }
  ++token_index;
  return (type);
}

/*构造常量表达式AST树*/
static struct ASnode *primary(void) {
  struct ASnode *n;
  int id;
  switch (token_stream[token_index].token) {
  case T_INTCONST:
    n = newLeaf(INTCONST, token_stream[token_index].intvalue);
    match(T_INTCONST, "intconst");
    return (n);
  case T_IDENT:
    {
      if(token_stream[token_index+1].token == T_LBK){
        id = findsymbol(token_stream[token_index].strvalue);
        ++token_index;
        int offset = 0;
        int len = Symtable[id].Dimens.size();
        int cnt = 1;
        while(token_stream[token_index].token == T_LBK){
          match(T_LBK, "[");
          int temp = 1;
          for(int i = cnt; i < len; ++i){
            temp = temp*Symtable[id].Dimens[i];
          }
          ASnode *IntExp = Exp(0);
          int nowvalue = interpretAST(IntExp);
          delete IntExp;
          offset += temp*nowvalue;
          match(T_RBK, "]");
          cnt++;
        }
        if(offset >= Symtable[id].conNum.size()){
          fprintf(stderr, "Not Const Array!\n");
          exit(2);
        }
        n = newLeaf(INTCONST, Symtable[id].conNum[offset]);
        return (n);
      }else{
        id = findsymbol(token_stream[token_index].strvalue);
        if(Symtable[id].intvalue == INF){
          fprintf(stderr, "Not Const!\n");
          exit(3);
        }
        n = newLeaf(INTCONST, Symtable[id].intvalue);
        ++token_index;
        return (n);
      }
    }
  case T_MINUS:
    {
      int cnt = 0;
      int value;
      while((token_stream[token_index].token != T_INTCONST) && (token_stream[token_index].token != T_IDENT) && (token_stream[token_index].token != T_LP)){
        if(token_stream[token_index].token == T_MINUS) ++cnt;
        ++token_index;
      }
      if(token_stream[token_index].token == T_INTCONST){
        value = token_stream[token_index].intvalue;
        ++token_index;
      }else if(token_stream[token_index].token == T_IDENT){
        if(token_stream[token_index+1].token == T_LBK){
          id = findsymbol(token_stream[token_index].strvalue);
          ++token_index;
          int offset = 0;
          int len = Symtable[id].Dimens.size();
          int cnt = 1;
          while(token_stream[token_index].token == T_LBK){
            match(T_LBK, "[");
            int temp = 1;
            for(int i = cnt; i < len; ++i){
              temp = temp*Symtable[id].Dimens[i];
            }
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            offset += temp*nowvalue;
            match(T_RBK, "]");
            cnt++;
          }
          if(offset >= Symtable[id].conNum.size()){
            fprintf(stderr, "Not Const Array!\n");
            exit(4);
          }
          value = Symtable[id].conNum[offset];
        }else{
          id = findsymbol(token_stream[token_index].strvalue);
          if(Symtable[id].intvalue == INF){
            fprintf(stderr, "Not Const!\n");
            exit(5);
          }
          value = Symtable[id].intvalue;
          ++token_index;
        }
      }else{
        fprintf(stderr, "gg - !\n");
        exit(6);
      }
      if(cnt%2 == 1) value = 0-value;
      n = newLeaf(INTCONST, value);
      return (n);
    }
  case T_PLUS:
    {
      int cnt = 0;
      int value;
      while((token_stream[token_index].token != T_INTCONST) && (token_stream[token_index].token != T_IDENT) && (token_stream[token_index].token != T_LP)){
        if(token_stream[token_index].token == T_MINUS) ++cnt;
        ++token_index;
      }
      if(token_stream[token_index].token == T_INTCONST){
        value = token_stream[token_index].intvalue;
        ++token_index;
      }else if(token_stream[token_index].token == T_IDENT){
        if(token_stream[token_index+1].token == T_LBK){
          id = findsymbol(token_stream[token_index].strvalue);
          ++token_index;
          int offset = 0;
          int len = Symtable[id].Dimens.size();
          int cnt = 1;
          while(token_stream[token_index].token == T_LBK){
            match(T_LBK, "[");
            int temp = 1;
            for(int i = cnt; i < len; ++i){
              temp = temp*Symtable[id].Dimens[i];
            }
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            offset += temp*nowvalue;
            match(T_RBK, "]");
            cnt++;
          }
          if(offset >= Symtable[id].conNum.size()){
            fprintf(stderr, "Not Const Array!\n");
            exit(7);
          }
          value = Symtable[id].conNum[offset];
        }else{
          id = findsymbol(token_stream[token_index].strvalue);
          if(Symtable[id].intvalue == INF){
            fprintf(stderr, "Not Const!\n");
            exit(8);
          }
          value = Symtable[id].intvalue;
          ++token_index;
        }
      }else{
        fprintf(stderr, "gg + !\n");
        exit(9);
      }
      if(cnt%2 == 1) value = 0-value;
      n = newLeaf(INTCONST, value);
      return (n);
    }
  case T_LP:
    match(T_LP, "(");
    n = Exp(0);
    match(T_RP, ")");
    return (n);
  default:
    fprintf(stderr, "syntax error on line %d, token %d\n", token_stream[token_index].line, token_stream[token_index].token);
    exit(10);
  }
}

int arithop(int tokentype) {
  switch (tokentype) {
  case T_PLUS:
    return (ADD);
  case T_MINUS:
    return (SUBTRACT);
  case T_STAR:
    return (MULTIPLY);
  case T_SLASH:
    return (DIVIDE);
  case T_PER:
    return (MOD);
  default:
    fprintf(stderr, "syntax error on line %d, token %d\n", Line, tokentype);
    exit(11);
  }
}

static int OpPrec_plus[] = { 
  0,
  0, 0, 0, 
  0, 0,
  0, 0, 0, 0,
  10, 10, 20, 20, 20,
  0,
  0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0 ,0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0 
  };

static int op_precedence_plus(int tokentype) {
  int prec = OpPrec_plus[tokentype];
  if (prec == 0) {
    fprintf(stderr, "syntax error on line %d, token %d\n", Line, tokentype);
    exit(12);
  }
  return (prec);
}

bool check(){
  switch (token_stream[token_index].token) {
    case T_PLUS:
      return true;
    case T_MINUS:
      return true;
    case T_STAR:
      return true;
    case T_SLASH:
      return true;
    case T_PER:
      return true;
    case T_INTCONST:
      return true;
    case T_LP:
      return true;
    case T_RP:
      return true;
    case T_IDENT:
      return true;
    default:
      return false;
  }
  return false;
}

struct ASnode *Exp(int ptp) {
  struct ASnode *left, *right;
  int tokentype;
  left = primary();
  if (!check() || token_stream[token_index].token == T_RP)
    return (left);
  tokentype = token_stream[token_index].token;
  while (op_precedence_plus(tokentype) > ptp) {
    ++token_index;
    right = Exp(OpPrec_plus[tokentype]);
    left = newNode(arithop(tokentype), left, right, 0);
    if (!check()  || token_stream[token_index].token == T_RP)
      return (left);
    tokentype = token_stream[token_index].token;
  }
  return (left);
}

// Build and return a generic AST node
ASnode *newNode(int op, ASnode *left, ASnode *right, int intvalue) {
  ASnode *n = new ASnode;
  n->op = op;
  n->left = left;
  n->right = right;
  n->intvalue = intvalue;
  return (n);
}

ASnode *newLeaf(int op, int intvalue) {
  return (newNode(op, NULL, NULL, intvalue));
}

ASnode *newUnary(int op, ASnode *left, int intvalue) {
  return (newNode(op, left, NULL, intvalue));
}

// List of AST operators
static char const *ASTop[] = { "+", "-", "*", "/", "%" };

int interpretAST(ASnode *n) {
  int leftval, rightval;
  if (n->left)
    leftval = interpretAST(n->left);
  if (n->right)
    rightval = interpretAST(n->right);
  switch (n->op) {
    case ADD:
      return (leftval + rightval);
    case SUBTRACT:
      return (leftval - rightval);
    case MULTIPLY:
      return (leftval * rightval);
    case DIVIDE:
      return (leftval / rightval);
    case MOD:
      return (leftval % rightval);
    case INTCONST:
      return (n->intvalue);
    default:
      fprintf(stderr, "Unknown AST operator %d\n", n->op);
      exit(13);
  }
}

// 变量定义 VarDef → Ident {'[' ConstExp ']'} | Ident {'[' ConstExp ']'} '=' InitVal
void VarDef(int type, int islocal, int isparam) {
  int id;
  vector<int> D;
  if(islocal == 0){
    if (token_stream[token_index].token == T_LBK) {
      int Array_size = 1; //数组大小
      int Dimension = 0; //数组维数
      while(token_stream[token_index].token == T_LBK){
        match(T_LBK, "[");
        ASnode *IntExp = Exp(0);
        int nowvalue = interpretAST(IntExp);
        delete IntExp;
        Array_size = Array_size * nowvalue;
        D.emplace_back(nowvalue);
        match(T_RBK, "]");
        ++Dimension;
      }
      if(token_stream[token_index].token == T_SEMI || token_stream[token_index].token == T_COMMA){
        id = addglob(nowname, type, S_ARRAY, 0, Array_size);
        Symtable[id].Dimens.clear();
        Symtable[id].Dimens = D;
        genglobsym(id, Array_size);
      }else{
        match(T_ASSIGN, "=");  //匹配 =
        match(T_LB, "{");  //匹配 {
        id = addglob(nowname, type, S_ARRAY, 0, Array_size);
        Symtable[id].Dimens.clear();
        Symtable[id].Dimens = D;
        vector<int> v(Dimension + 1);
        for(int i = 1; i <= Dimension; ++i){
          int expect = 1;
          for(int j = i-1; j < D.size(); ++j){
            expect = expect * D[j];
          }
          v[i] = expect;  //每一维度期望匹配的元素数量
        }
        vector<int> initnum;  //数组初始化的值
        vector<int> already(Dimension + 1, 0);  //每一维度已经匹配到的元素数量
        int k = 1;  //当前'{'层数, 代表数组维度
        while(token_stream[token_index].token != T_SEMI){
          if(token_stream[token_index].token == T_INTCONST){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_IDENT){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_MINUS){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_PLUS){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_LP){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_LB){
            ++k;
            match(T_LB, "{");
          }else if(token_stream[token_index].token == T_RB){
            while(already[k] < v[k]){
              initnum.emplace_back(0);
              ++already[k];
            }
            already[k] = 0;
            --k;
            already[k] += v[k+1];
            if(k == 0){
              match(T_RB, "}");
              break;
            }
            match(T_RB, "}");
          }else{
            ++token_index;
          }
        }
        genglobsym_plus(id, initnum);
        if(con) Symtable[id].conNum = initnum;
      }
    } else {
      if(token_stream[token_index].token == T_SEMI || token_stream[token_index].token == T_COMMA){
        id = addglob(nowname, type, S_VARIABLE, 0, 1);
        genglobsym(id, 1);
      }else{
        match(T_ASSIGN, "=");
        id = addglob(nowname, type, S_VARIABLE, 0, 1);
        vector<int> v;
        ASnode *IntExp = Exp(0);
        int nowvalue = interpretAST(IntExp);
        delete IntExp;
        if(con){
          Symtable[id].intvalue = nowvalue;
        }else{
          v.emplace_back(nowvalue);
          genglobsym_plus(id, v);
        }
      }
    }
  }else{
    if (token_stream[token_index].token == T_LBK) {
      int Array_size = 1;
      int Dimension = 0;
      while(token_stream[token_index].token == T_LBK){
        match(T_LBK, "[");
        if(token_stream[token_index].token == T_RBK){
          match(T_RBK, "]");
          D.emplace_back(1);
          ++Dimension;
          continue;
        }
        ASnode *IntExp = Exp(0);
        int nowvalue = interpretAST(IntExp);
        delete IntExp;
        Array_size = Array_size * nowvalue;
        D.emplace_back(nowvalue);
        match(T_RBK, "]");
        ++Dimension;
      }
      if(isparam == 1){
        Array_size = 1;
      }
      Local_v += Array_size;
      id = addlocl(nowname, type, S_ARRAY, isparam, Array_size);
      if(isparam == 1){
        Symtable[id].paramnum = pars++;
      }
      Symtable[id].Dimens.clear();
      Symtable[id].Dimens = D;
      Symtable[id].st_address = -4*Local_v - push_reg;
      if(con && if_PerformanceTest){
        match(T_ASSIGN, "=");
        match(T_LB, "{");  //匹配 {
        vector<int> v(Dimension + 1);
        for(int i = 1; i <= Dimension; ++i){
          int expect = 1;
          for(int j = i-1; j < D.size(); ++j){
            expect = expect * D[j];
          }
          v[i] = expect;  //每一维度期望匹配的元素数量
        }
        vector<int> initnum;  //数组初始化的值
        vector<int> already(Dimension + 1, 0);  //每一维度已经匹配到的元素数量
        int k = 1;  //当前'{'层数, 代表数组维度
        while(token_stream[token_index].token != T_SEMI){
          if(token_stream[token_index].token == T_INTCONST){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_IDENT){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_MINUS){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_PLUS){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_LP){
            ASnode *IntExp = Exp(0);
            int nowvalue = interpretAST(IntExp);
            delete IntExp;
            initnum.emplace_back(nowvalue);
            ++already[k];
          }else if(token_stream[token_index].token == T_LB){
            ++k;
            match(T_LB, "{");
          }else if(token_stream[token_index].token == T_RB){
            while(already[k] < v[k]){
              initnum.emplace_back(0);
              ++already[k];
            }
            already[k] = 0;
            --k;
            already[k] += v[k+1];
            if(k == 0){
              match(T_RB, "}");
              break;
            }
            match(T_RB, "}");
          }else{
            ++token_index;
          }
        }
        Symtable[id].conNum = initnum;
      }
    } else {
      Local_v++;
      id = addlocl(nowname, type, S_VARIABLE, isparam, 1);
      if(isparam == 1){
        Symtable[id].paramnum = pars++;
      }
      Symtable[id].st_address = -4*Local_v - push_reg;
      if(con){
        match(T_ASSIGN, "=");
        ASnode *IntExp = Exp(0);
        int nowvalue = interpretAST(IntExp);
        Symtable[id].intvalue = nowvalue;
      }
    }
  }
}

// 函数形参表 FuncFParams → FuncFParam {',' FuncFParam}
static int FuncFParams(void) {
  int type;
  int paramcnt=0;
  pars = 0;

  while (token_stream[token_index].token != T_RP) {
    type = FuncType();
    ident();
    VarDef(type, 1, 1);
    paramcnt++;

    switch (token_stream[token_index].token) {
      case T_COMMA: ++token_index; break;
      case T_RP: break;
      default:
        exit(14);
        fatald("Unexpected token in parameter list", token_stream[token_index].token);
    }
  }

  return(paramcnt);
}

// 函数定义 FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
ASTnode *FuncDef(int type) {
  ASTnode *tree = NULL;
  int nameslot, endlabel, paramcnt;

  endlabel = genlabel();
  nameslot = addglob(nowname, type, S_FUNCTION, endlabel, 0);
  Functionid = nameslot;
  func_field = genlabel();
  genlabel();
  genlabel();

  lparen();
  paramcnt= FuncFParams();
  Symtable[nameslot].nelems= paramcnt;
  rparen();

  tree = Block();

  return (newASTunary(A_FUNCTION, type, tree, nameslot));
}

// 编译单元  CompUnit → [ CompUnit ] ( Decl | FuncDef )
void CompUnit(void) {
  ASTnode *tree;
  int type;
  while (token_index < token_size) {
    tree = NULL;
    if(token_stream[token_index].token == T_CONST){
      ++token_index;
      con = true;
    }
    type = FuncType();
    ident();
    Local_v = 0;
    max_param = 0;
    if (token_stream[token_index].token == T_LP) {
      func_v = 0;
      LCslot = 0;
      ldr_Byte = 0;
      tree = FuncDef(type);
      CodeGen(tree, NOLABEL, NOLABEL, NOLABEL, 0);
      freeloclsyms();
    } else {
      VarDef(type, 0, 0);
      while(token_stream[token_index].token == T_COMMA){
        match(T_COMMA, ",");
        ident();
        VarDef(type, 0, 0);
      }
      semi();
    }
    con = false;
  }
}

static ASTnode *funccall(void) {
  ASTnode *root = NULL;
  ASTnode *temp;
  int id;
  if ((id = findsymbol(nowname)) == -1 || Symtable[id].stype != S_FUNCTION) {
    fatals("Undeclared function", nowname);
  }
  int parnum = 0;
  int nowline = token_stream[token_index].line;
  // Get the '('
  lparen();
  if(token_stream[token_index].token != T_RP){
    if(token_stream[token_index].token == T_STRCONST){
      root =newASTleaf(A_STRLIT, P_NONE, 0);
      addString(token_stream[token_index].strvalue);
      root->strvalue = token_stream[token_index].strvalue;
      match(T_STRCONST, "String");
    }else{
      root = Exp_Stmt();
    }
    temp = root;
    ++parnum;
    while(token_stream[token_index].token == T_COMMA){
      ++token_index;
      ASTnode *a;
      if(token_stream[token_index].token == T_STRCONST){
        a =newASTleaf(A_STRLIT, P_NONE, 0);
        a->strvalue = token_stream[token_index].strvalue;
        match(T_STRCONST, "String");
      }else{
        a = Exp_Stmt();
      } 
      temp->next = a;
      temp = temp->next;
      ++parnum;
    }
  }
  root = newASTunary(A_FUNCCALL, Symtable[id].type, root, id);
  root->line = nowline;
  func_v += parnum;
  // Get the ')'
  max_param = max(max_param, parnum);
  rparen();
  return (root);
}

static ASTnode *array_access(void) {
  ASTnode *tree;
  int id;
  int Dimension_num = 0;
  if ((id = findsymbol(nowname)) == -1 || Symtable[id].stype != S_ARRAY) {
    fatals("Undeclared array", nowname);
  }
  if(if_PerformanceTest && Symtable[id].conNum.size() > 0){
    int cnt = 1;
    int index = 0;
    const int len = Symtable[id].Dimens.size();
    while(token_stream[token_index].token == T_LBK){
      int k = 1;
      for(int i = cnt; i < len; ++i){
        k = k*Symtable[id].Dimens[i];
      }
      match(T_LBK, "[");
      ASnode *IntExp = Exp(0);
      int nowvalue = interpretAST(IntExp);
      delete IntExp;
      index += nowvalue * k;
      match(T_RBK, "]");
    }
    tree = newASTleaf(A_INTLIT, P_INT, Symtable[id].conNum[index]);
  }else{
    match(T_LBK, "[");
    tree = Exp_Stmt();
    ++Dimension_num;
    match(T_RBK, "]");
    ASTnode *temp = tree;
    while(token_stream[token_index].token == T_LBK) {
      match(T_LBK, "[");
      ASTnode *a = Exp_Stmt();
      temp->next = a;
      temp = temp->next;
      ++Dimension_num;
      match(T_RBK, "]");
    }
    tree = newASTunary(A_ADDR, P_INT, tree, id);
    tree->Dimension_num = Dimension_num;
  }
  return (tree);
}

static ASTnode *LVal(void) {
  ASTnode *n;
  int id;
  ident();
  if (token_stream[token_index].token == T_LP)
    return (funccall());
  if (token_stream[token_index].token == T_LBK)
    return (array_access());
  id = findsymbol(nowname);
  if (id == -1)
    fatals("Unknown variable", nowname);
  if(Symtable[id].intvalue != INF){
    n = newASTleaf(A_INTLIT, P_INT, Symtable[id].intvalue);
  }else{
    n = newASTleaf(A_IDENT, Symtable[id].type, id);
  }
  return (n);
}

static ASTnode *PrimaryExp(void) {
  ASTnode *n;
  int id;
  switch (token_stream[token_index].token) {
    case T_INTCONST:
	    n = newASTleaf(A_INTLIT, P_INT, token_stream[token_index].intvalue);
      break;
    case T_IDENT:
      return (LVal());
    case T_LP:
      lparen();
      n = Exp_Stmt();
      rparen();
      return (n);
    default:
      exit(15);
      fatald("Expecting a PrimaryExp expression, got token", token_stream[token_index].token);
  }
  ++token_index;
  return (n);
}

ASTnode *UnaryExp(void) {
  ASTnode *tree;
  switch (token_stream[token_index].token) {
    case T_LOGNOT:
    case T_MINUS:
    case T_PLUS:
      {
        int cnt = 0, cnt1 = 0;
        while((token_stream[token_index].token == T_LOGNOT) || (token_stream[token_index].token == T_PLUS) || (token_stream[token_index].token == T_MINUS)){
          if(token_stream[token_index].token == T_LOGNOT) ++cnt;
          if(token_stream[token_index].token == T_MINUS) ++cnt1;
          ++token_index;
        }
        tree = UnaryExp();
        tree->rvalue = 1;
        if(cnt%2 == 1){
          tree = newASTunary(A_LOGNOT, tree->type, tree, 0);
        }
        if(cnt1%2 == 1){
          tree = newASTunary(A_RSB, tree->type, tree, 0);
        }
        break;
      }
    default:
      tree = PrimaryExp();
  }
  return (tree);
}

// 加减表达式 AddExp → MulExp | AddExp ('+' | '-') MulExp
ASTnode *AddExp() {
  ASTnode *left, *right;
  left = MulExp();
  left->rvalue = 1;
  while((token_stream[token_index].token == T_PLUS) || (token_stream[token_index].token == T_MINUS)){
    if(token_stream[token_index].token == T_PLUS){
      match(T_PLUS, "+");
      right = MulExp();
      right->rvalue = 1;
      left = newASTnode(A_ADD, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else if(token_stream[token_index].token == T_MINUS){
      match(T_MINUS, "-");
      right = MulExp();
      right->rvalue = 1;
      left = newASTnode(A_SUBTRACT, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }
  }
  return left;
}

// 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
ASTnode *MulExp(){
  ASTnode *left, *right;
  left = UnaryExp();
  left->rvalue = 1;
  while((token_stream[token_index].token == T_STAR) || (token_stream[token_index].token == T_SLASH) || (token_stream[token_index].token == T_PER)){
    if(token_stream[token_index].token == T_STAR){
      match(T_STAR, "*");
      right = UnaryExp();
      right->rvalue = 1;
      left = newASTnode(A_MULTIPLY, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else if(token_stream[token_index].token == T_SLASH){
      match(T_SLASH, "/");
      right = UnaryExp();
      right->rvalue = 1;
      left = newASTnode(A_DIVIDE, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else if(token_stream[token_index].token == T_PER){
      match(T_PER, "%");
      right = UnaryExp();
      right->rvalue = 1;
      left = newASTnode(A_MOD, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }
  }
  return left;
}

ASTnode *Exp_Stmt() {
  return AddExp();
}

// Prototypes
static ASTnode *BlockItem(void);
static ASTnode *Stmt(void);

// 条件表达式 Cond → LOrExp
ASTnode *Cond() {
  return LOrExp();
}

// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
ASTnode *LOrExp() {
  ASTnode *left, *right;
  left = LAndExp();
  left->rvalue = 1;
  while(token_stream[token_index].token == T_LOGOR){
    match(T_LOGOR, "||");
    right = LAndExp();
    right->rvalue = 1;
    left = newASTnode(A_LOGOR, P_INT, left, NULL, right, 0);
    left->rvalue = 1;
  }
  return left;
}

// 逻辑与表达式 AndExp → EqExp | AndExp '&&' EqExp
ASTnode *LAndExp() {
  ASTnode *left, *right;
  left = EqExp();
  left->rvalue = 1;
  while(token_stream[token_index].token == T_LOGAND){
    match(T_LOGAND, "&&");
    right = EqExp();
    right->rvalue = 1;
    left = newASTnode(A_LOGAND, P_INT, left, NULL, right, 0);
    left->rvalue = 1;
  }
  return left;
}

// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
ASTnode *EqExp() {
  ASTnode *left, *right;
  left = RelExp();
  left->rvalue = 1;
  while((token_stream[token_index].token == T_EQ) || (token_stream[token_index].token == T_NE)){
    if(token_stream[token_index].token == T_EQ){
      match(T_EQ, "==");
      right = RelExp();
      right->rvalue = 1;
      left = newASTnode(A_EQ, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else{
      match(T_NE, "!=");
      right = RelExp();
      right->rvalue = 1;
      left = newASTnode(A_NE, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }
  }
  return left;
}

// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
ASTnode *RelExp() {
  ASTnode *left, *right;
  left = AddExp();
  left->rvalue = 1;
  while((token_stream[token_index].token == T_LT) || (token_stream[token_index].token == T_GT) || (token_stream[token_index].token == T_LE) || (token_stream[token_index].token == T_GE)){
    if(token_stream[token_index].token == T_LT){
      match(T_LT, "<");
      right = AddExp();
      right->rvalue = 1;
      left = newASTnode(A_LT, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else if(token_stream[token_index].token == T_GT){
      match(T_GT, ">");
      right = AddExp();
      right->rvalue = 1;
      left = newASTnode(A_GT, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else if(token_stream[token_index].token == T_LE){
      match(T_LE, "<=");
      right = AddExp();
      right->rvalue = 1;
      left = newASTnode(A_LE, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }else{
      match(T_GE, ">=");
      right = AddExp();
      right->rvalue = 1;
      left = newASTnode(A_GE, P_INT, left, NULL, right, 0);
      left->rvalue = 1;
    }
  }
  return left;
}


static ASTnode *if_Stmt(void) {
  ASTnode *condAST, *trueAST, *falseAST = NULL;
  match(T_IF, "if");
  lparen();
  condAST = Cond();
  rparen();
  trueAST = Stmt();
  if (token_stream[token_index].token == T_ELSE) {
    ++token_index;
    falseAST = Stmt();
  }
  return (newASTnode(A_IF, P_NONE, condAST, trueAST, falseAST, 0));
}

static ASTnode *while_Stmt(void) {
  ASTnode *condAST, *bodyAST;
  match(T_WHILE, "while");
  lparen();
  condAST = Cond();
  rparen();
  Looplevel++;
  bodyAST = Stmt();
  Looplevel--;
  return (newASTnode(A_WHILE, P_NONE, condAST, NULL, bodyAST, 0));
}

static ASTnode *return_Stmt(void) {
  ASTnode *tree = NULL;
  match(T_RETURN, "return");
  if (Symtable[Functionid].type == P_INT){
    tree = Exp_Stmt();
    if (tree == NULL)
      fatal("Incompatible type to return");
  }
  tree = newASTunary(A_RETURN, P_NONE, tree, 0);
  semi();
  return (tree);
}

static ASTnode *break_Stmt(void) {
  if (Looplevel == 0)
    fatal("no loop to break out from");
  match(T_BREAK, "break");
  semi();
  return (newASTleaf(A_BREAK, 0, 0));
}

static ASTnode *continue_Stmt(void) {
  if (Looplevel == 0)
    fatal("no loop to continue to");
  match(T_CONTINUE, "continue");
  semi();
  return (newASTleaf(A_CONTINUE, 0, 0));
}

/* 语句
Stmt → LVal '=' Exp ';' | [Exp]';' | Block
| 'if' '(' Cond ')' Stmt ['else' Stmt]
| 'while' '(' Cond ')' Stmt
| 'break' ';' | 'continue' ';'
| 'return' [Exp] ';'
*/
static ASTnode *Stmt(void){
  ASTnode *left, *right;
  switch (token_stream[token_index].token) {
    case T_IF:
      return (if_Stmt());
    case T_WHILE:
      return (while_Stmt());
    case T_RETURN:
      return (return_Stmt());
    case T_BREAK:
      return (break_Stmt());
    case T_CONTINUE:
      return (continue_Stmt());
    case T_LB:
      return (Block());
    case T_SEMI:
      semi();
      return (NULL);
    default:
      {
        bool flag = false;
        int index = token_index;
        while(token_stream[index].token != T_SEMI){
          if(token_stream[index].token == T_ASSIGN){
            flag = true;
            break;
          }
          ++index;
        }
        if(flag){
          right = LVal();
          match(T_ASSIGN, "=");
          left = Exp_Stmt();
          semi();
          ASTnode *tree = newASTnode(A_ASSIGN, P_INT, left, NULL, right, 0);
          tree->rvalue = 1;
          return tree;
        }else{
          left = Exp_Stmt();
          semi();
          return left;
        }
      }
  }
  return (NULL);
}

// 语句块项 BlockItem → Decl | Stmt
static ASTnode *BlockItem(void) {
  int type;
  if(token_stream[token_index].token == T_CONST){
    ++token_index;
    con = true;
  }
  switch (token_stream[token_index].token) {
    case T_INT:
      {
        ASTnode *root = newASTunary(A_DEFINE, P_NONE, NULL, 0);
        ASTnode *temp = root;
        type = FuncType();
        int tindex = token_index;
        string str = token_stream[token_index].strvalue;
        ident();
        VarDef(type, 1, 0);
        while(token_stream[token_index].token != T_SEMI){
          if(token_stream[token_index].token == T_COMMA){
            match(T_COMMA, ",");
            tindex = token_index;
            str = token_stream[token_index].strvalue;
            ident();
            VarDef(type, 1, 0);
          }else if(token_stream[token_index].token == T_ASSIGN && token_stream[token_index+1].token == T_LB){
            token_index = tindex;
            ASTnode *right = LVal();
            match(T_ASSIGN, "=");
            match(T_LB, "{");
            int id = findsymbol(str);
            ASTnode *left = newASTnode(A_INIT, Symtable[id].size, NULL, NULL, NULL, Symtable[id].st_address);
            ASTnode *temp1 = left;
            int Dimension = Symtable[id].Dimens.size();
            vector<int> D = Symtable[id].Dimens;
            vector<int> v(Dimension + 1);
            for(int i = 1; i <= Dimension; ++i){
              int expect = 1;
              for(int j = i-1; j < D.size(); ++j){
                expect = expect * D[j];
              }
              v[i] = expect;
            }
            vector<int> already(Dimension + 1, 0);
            int k = 1;
            while(token_stream[token_index].token != T_SEMI){
              if(token_stream[token_index].token == T_INTCONST){
                ASTnode *tree1 = newASTleaf(A_INTLIT, P_INT, token_stream[token_index].intvalue);
                tree1->rvalue = 1;
                temp1->next = tree1;
                temp1 = temp1->next;
                ++already[k];
                ++token_index;
              }else if(token_stream[token_index].token == T_IDENT){
                ASTnode *tree1 = LVal();
                tree1->rvalue = 1;
                temp1->next = tree1;
                temp1 = temp1->next;
                ++already[k];
              }else if(token_stream[token_index].token == T_LB){
                ++k;
                match(T_LB, "{");
              }else if(token_stream[token_index].token == T_RB){
                while(already[k] < v[k] && k != 1){
                  ASTnode *tree1 = newASTleaf(A_INTLIT, P_INT, 0);
                  tree1->rvalue = 1;
                  temp1->next = tree1;
                  temp1 = temp1->next;
                  ++already[k];
                }
                already[k] = 0;
                --k;
                already[k] += v[k+1];
                if(k == 0){
                  match(T_RB, "}");
                  break;
                }
                match(T_RB, "}");
              }else{
                ++token_index;
              }
            }
            ASTnode *tree = newASTnode(A_ASSIGN, P_INT, left, NULL, right, 0);
            tree->rvalue = 1;
            temp->next = tree;
            temp = temp->next;
          }else if(token_stream[token_index].token == T_ASSIGN){
            token_index = tindex;
            ASTnode *right = LVal();
            match(T_ASSIGN, "=");
            ASTnode *left = Exp_Stmt();
            ASTnode *tree = newASTnode(A_ASSIGN, P_INT, left, NULL, right, 0);
            tree->rvalue = 1;
            temp->next = tree;
            temp = temp->next;
          }else{
            fprintf(stderr, "ERROR!\n");
            exit(16);
          }
        }
        semi();
        con = false;
        return (root);
      }
    default:
      return (Stmt());
  }
  return (NULL);
}

// 语句块 Block → '{' {BlockItem} '}'
ASTnode *Block(void) {
  ASTnode *left = NULL;
  ASTnode *tree;
  lbrace();
  if (token_stream[token_index].token == T_RB) {
    rbrace();
    return (left);
  }
  while (1) {
    tree = BlockItem();
    if (tree != NULL) {
      if (left == NULL)
	      left = tree;
      else
	      left = newASTnode(A_GLUE, P_NONE, left, NULL, tree, 0);
    }
    if (token_stream[token_index].token == T_RB) {
      rbrace();
      return (left);
    }
  }
}

static int genIF(ASTnode *n, int looptoplabel, int loopendlabel) {
  int Ltrue, Lfalse, Lend;
  Ltrue = genlabel();
  true_L = Ltrue;
  Lfalse = genlabel();
  if (n->right)
    Lend = genlabel();
  if(n->left->op == A_LOGOR) n->left->istop = true;
  int reg = CodeGen(n->left, Lfalse, looptoplabel, loopendlabel, n->op);
  if(reg != NOREG){
    ARM_cmp_jump(reg, Lfalse);
  }
  genfreeregs();
  ARM_label(Ltrue);
  if(n->mid) CodeGen(n->mid, NOLABEL, looptoplabel, loopendlabel, NOLABEL);
  genfreeregs();
  if (n->right)
    ARM_jump(Lend);
  ARM_label(Lfalse);
  if (n->right) {
    CodeGen(n->right, NOLABEL, looptoplabel, loopendlabel, NOLABEL);
    genfreeregs();
    ARM_label(Lend);
  }
  return (NOREG);
}

static int genWHILE(ASTnode *n) {
  int Lstart, Ltrue, Lend;
  Lstart = genlabel();
  Ltrue = genlabel();
  true_L = Ltrue;
  Lend = genlabel();
  if(if_PerformanceTest){
    if(while_level == 0){
      while_ed = ObjectCode.size();
      for(int i = while_st; i < while_ed; ++i){
        ScanRegPass(ObjectCode[i]);
      }
      max_alloc -= 4;
      vector<pair<int, int> >a;
      for(auto x : ump){
        a.push_back(x);
      }
      sort(a.begin(), a.end(), cmp);
      int now = 0;
      while(1){
        if(now >= a.size()) break;
        if(a[now].second <= Threshold) break;
        int reg = alloc_register_plus();
        if(reg == -1) break;
        newReg[a[now].first] = reg;
        ++now;
      }
      for(int i = while_st; i < while_ed; ++i){
        ObjectCode[i] = AllocRegPass(ObjectCode[i]);
      }
      for(auto &x : newReg){
        int address = x.first;
        int r = x.second;
        sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], address);
        ObjectCode.emplace(ObjectCode.begin() + while_ed, buf);
      }
      for(auto &x : newReg){
        int address = x.first;
        int r = x.second;
        sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[r], address);
        ObjectCode.emplace(ObjectCode.begin() + while_st, buf);
      }
      max_alloc = 0;
      while_st = ObjectCode.size();
      ump.clear();
      newReg.clear();
    }
    while_level++;
  }
  ARM_label(Lstart);
  if(n->left->op == A_LOGOR) n->left->istop = true;
  int reg = CodeGen(n->left, Lend, Lstart, Lend, n->op);
  if(reg != NOREG){
    ARM_cmp_jump(reg, Lend);
  }
  genfreeregs();
  ARM_label(Ltrue);
  if(n->right) CodeGen(n->right, NOLABEL, Lstart, Lend, NOLABEL);
  genfreeregs();
  ARM_jump(Lstart);
  ARM_label(Lend);
  if(if_PerformanceTest){
    while_level--;
    if(while_level == 0){
      while_ed = ObjectCode.size();
      for(int i = while_st; i < while_ed; ++i){
        ScanRegPass(ObjectCode[i]);
      }
      max_alloc -= 4;
      vector<pair<int, int> >a;
      for(auto x : ump){
        a.push_back(x);
      }
      sort(a.begin(), a.end(), cmp);
      int now = 0;
      while(1){
        if(now >= a.size()) break;
        int reg = alloc_register_plus();
        if(reg == -1) break;
        newReg[a[now].first] = reg;
        ++now;
      }
      for(int i = while_st; i < while_ed; ++i){
        ObjectCode[i] = AllocRegPass(ObjectCode[i]);
      }
      for(auto &x : newReg){
        int address = x.first;
        int r = x.second;
        sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[r], address);
        ObjectCode.emplace(ObjectCode.begin() + while_ed, buf);
      }
      for(auto &x : newReg){
        int address = x.first;
        int r = x.second;
        sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[r], address);
        ObjectCode.emplace(ObjectCode.begin() + while_st, buf);
      }
      max_alloc = 0;
      while_st = ObjectCode.size();
      ump.clear();
      newReg.clear();
    }
  }
  return (NOREG);
}

vector<pair<int, int> > nested_calls[MAXN];
int nested_index = 0;

int OutputFunccall(ASTnode* L, int ri){
  ASTnode *r;
  int reg;
  r = L->next;
  if(r != NULL){
    OutputFunccall(r, ri+1);
  }
  if(L->op != A_INTLIT && Symtable[L->v.id].stype == S_ARRAY && L->Dimension_num < Symtable[L->v.id].Dimens.size()){
    int len = Symtable[L->v.id].Dimens.size();
    int cnt = 1;
    ASTnode *test = L->left;
    bool flag = true;
    int off_size = 0;
    while(test != NULL){
      if(test->op == A_INTLIT){
        int k = 1;
        for(int i = cnt; i < len; ++i){
          k = k*Symtable[L->v.id].Dimens[i];
        }
        off_size += k * (test->v.intvalue);
        test = test->next;
        ++cnt;
      }else{
        flag = false;
        break;
      }
    }
    int reg1 = alloc_register();
    if(flag){
      if(off_size <= imm16){
        sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg1], off_size);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        set_int_offset(reg1, off_size);
      }
    }else if(len == 1){
      free_register(reg);
      ASTnode *a = L->left;
      reg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, L->op);
    }else{
      int rightreg;
      ASTnode *a = L->left;
      cnt = 1;
      while(a != NULL){
        rightreg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, L->op);
        a = a->next;
        int k = 1;
        for(int i = cnt; i < len; ++i){
          k = k*Symtable[L->v.id].Dimens[i];
        }
        if(cnt < len){
          int reg2 = alloc_register();
          if (k <= imm16){
            sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg2], k);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
          }else {
            int offset = -1;

            for (int i = 0; i < Intlist.size(); i++) {
              if (Intlist[i] == k) {
                offset = 4 * i;
                break;
              }
            }

            if (offset == -1) {
              offset = 4 * Intslot;
              Intlist.emplace_back(k);
              ++Intslot;
              ldr_Byte += (8 + leng(k));
            }
            for (int i = 0; i < Globs; i++) {
              if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
                offset += 4;
            }
            sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[reg2], func_field, offset);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }
          if(cnt == 1){
            sprintf(buf, "\tmul\t%s, %s, %s\n", reglist[reg1], reglist[reg2], reglist[rightreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(rightreg);
            free_register(reg2);
          }else{
            reg1 = ARM_mla(reg1, rightreg, reg2);
          }
        }else{
          ARM_add(reg1, rightreg);
        }
        ++cnt;
      }
    }
    sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[reg1], reglist[reg1]);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    reg = alloc_register();
    int address = -4*Local_v - push_reg - 4*func_v;
    if(Symtable[L->v.id].ctype == C_GLOBAL){
      int offset = 0;
      for (int i = 0; i < L->v.id; i++) {
        if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
          offset += 4;
      }
      sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[reg], func_field, offset);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      if(if_PerformanceTest){
        if(ri < 4){
          sprintf(buf, "\tadd\tr%d, %s, %s\n", ri, reglist[reg], reglist[reg1]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          free_register(reg1);
        }else{
          int off = (ri-4)*4;
          ARM_add(reg, reg1);
          sprintf(buf, "\tstr\t%s, [sp, #%d]\n", reglist[reg], off);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
      }else{
        ARM_add(reg, reg1);
        int off = abs(address);
        int reg2 = alloc_register();
        if(off <= imm8){
          sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[reg], address);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }else{
          set_int_offset(reg2, off);
          sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg2], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tstr\t%s, [%s]\n", reglist[reg], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
        free_register(reg2); 
      }
    }else if(Symtable[L->v.id].ctype == C_LOCAL){
      int subnum = abs(Symtable[L->v.id].st_address);
      int reg2 = alloc_register();
      if(subnum <= imm12){
        sprintf(buf, "\tsub\t%s, fp, #%d\n", reglist[reg], subnum);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        set_int_offset(reg2, subnum);
        sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg], reglist[reg2]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }
      free_register(reg2);
      if(if_PerformanceTest){
        if(ri < 4){
          sprintf(buf, "\tadd\tr%d, %s, %s\n", ri, reglist[reg], reglist[reg1]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          free_register(reg1);
        }else{
          int off = (ri-4)*4;
          ARM_add(reg, reg1);
          sprintf(buf, "\tstr\t%s, [sp, #%d]\n", reglist[reg], off);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
      }else{
        ARM_add(reg, reg1);
        int off = abs(address);
        int reg3 = alloc_register();
        if(off <= imm8){
          sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[reg], address);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }else{
          set_int_offset(reg3, off);
          sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg3], reglist[reg3]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tstr\t%s, [%s]\n", reglist[reg], reglist[reg3]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
        free_register(reg3);
      }
    }else{
      if(Symtable[L->v.id].paramnum < 4){
        sprintf(buf, "\tldr	%s, [fp, #%d]\n", reglist[reg], Symtable[L->v.id].st_address);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        int subnum = (Symtable[L->v.id].paramnum-3)*4;
        if(subnum <= imm8){
          sprintf(buf, "\tldr	%s, [fp, #%d]\n", reglist[reg], subnum);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }else{
          int reg2 = alloc_register();
          set_int_offset(reg2, subnum);
          sprintf(buf, "\tadd\t%s, fp, %s\n", reglist[reg2], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tldr	%s, [%s]\n", reglist[reg], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          free_register(reg2);
        }
      }
      if(if_PerformanceTest){
        if(ri < 4){
          sprintf(buf, "\tadd\tr%d, %s, %s\n", ri, reglist[reg], reglist[reg1]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          free_register(reg1);
        }else{
          int off = (ri-4)*4;
          ARM_add(reg, reg1);
          sprintf(buf, "\tstr\t%s, [sp, #%d]\n", reglist[reg], off);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
      }else{
        ARM_add(reg, reg1);
        int off = abs(address);
        int reg2 = alloc_register();
        if(off <= imm8){
          sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[reg], address);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }else{
          set_int_offset(reg2, off);
          sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg2], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tstr\t%s, [%s]\n", reglist[reg], reglist[reg2]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
        free_register(reg2);
      }
    }
    nested_calls[nested_index].emplace_back(address, ri);
    func_v++;
    free_register(reg);
  }else{
    reg = CodeGen(L, NOLABEL, NOLABEL, NOLABEL, A_FUNCCALL);
    int address = -4*Local_v - push_reg - 4*func_v;
    if(if_PerformanceTest){
      if(ri < 4){
        sprintf(buf, "\tmov\tr%d, %s\n", ri, reglist[reg]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        int off = (ri-4)*4;
        sprintf(buf, "\tstr\t%s, [sp, #%d]\n", reglist[reg], off);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }
    }else{
      int off = abs(address);
      int reg1 = alloc_register();
      if(off <= imm8){
        sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[reg], address);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        set_int_offset(reg1, off);
        sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        sprintf(buf, "\tstr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }
      free_register(reg1);
    }
    nested_calls[nested_index].emplace_back(address, ri);
    func_v++;
    free_register(reg);
  }
  return reg;
}

int CodeGen(ASTnode *n, int label, int looptoplabel, int loopendlabel, int parentASTop) {
  if(n == NULL) return NOREG;
  int leftreg = -1, rightreg = -1;
  if((n->op == A_FUNCCALL) && (Symtable[n->v.id].name == "starttime")){
    sprintf(buf, "\tmov\tr0, #%d\n", n->line);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tbl	_sysy_starttime\n");
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    return (NOREG);
  }
  if((n->op == A_FUNCCALL) && (Symtable[n->v.id].name == "stoptime")){
    sprintf(buf, "\tmov\tr0, #%d\n", n->line);
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tbl	_sysy_stoptime\n");
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    return (NOREG);
  }
  if(n->op == A_STRLIT){
    leftreg = alloc_register();
    set_lc_offset(leftreg, n->strvalue);
    return leftreg;
  }
  /*局部数组初始化*/
  if(n->op == A_ASSIGN && n->left->op == A_INIT){
    n = n->left;
    int address = n->v.intvalue;
    int num = n->type*4;
    int subnum = abs(address);
    int reg = alloc_register();
    if(subnum <= imm8){
      sprintf(buf, "\tsub\tr3, fp, #%d\n", subnum);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }else{
      set_int_offset(reg, subnum);
      sprintf(buf, "\tsub\tr3, fp, %s\n", reglist[reg]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }
    if(num <= imm16){
      sprintf(buf, "\tmov\tr2, #%d\n", num);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }else{
      set_int_offset(reg, num);
      sprintf(buf, "\tmov\tr2, %s\n", reglist[reg]);
      ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    }
    sprintf(buf, "\tmov\tr1, #0\n");
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tmov\tr0, r3\n");
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    sprintf(buf, "\tbl\tmemset\n");
    ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
    free_register(reg);
    int cnt = 0;
    ASTnode *a = n->next;
    while(a != NULL){
      leftreg = CodeGen(a, label, looptoplabel, loopendlabel, A_GLUE);
      a = a->next;
      int off = abs(address + 4*cnt);
      if(off <= imm8){
        sprintf(buf, "\tstr\t%s, [fp, #%d]\n", reglist[leftreg], address + 4*cnt);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }else{
        int reg1 = alloc_register();
        set_int_offset(reg1, off);
        sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        sprintf(buf, "\tstr\t%s, [%s]\n", reglist[leftreg], reglist[reg1]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        free_register(reg1);
      }
      genfreeregs();
      ++cnt;
    }
    return (NOREG);
  }
  switch (n->op) {
    /*局部变量初始化*/
    case A_DEFINE:
      {
      ASTnode *a = n->next;
      while(a != NULL){
        CodeGen(a, label, looptoplabel, loopendlabel, A_GLUE);
        genfreeregs();
        a = a->next;
      }
      return (NOREG);
      }
    /*if-else语句*/
    case A_IF:
      return (genIF(n, looptoplabel, loopendlabel));
    /*while语句*/
    case A_WHILE:
      return (genWHILE(n));
    /*语句直接的连接枢纽*/
    case A_GLUE:
      CodeGen(n->left, label, looptoplabel, loopendlabel, n->op);
      genfreeregs();
      CodeGen(n->right, label, looptoplabel, loopendlabel, n->op);
      genfreeregs();
      return (NOREG);
    /*函数声明*/
    case A_FUNCTION:
      ARM_funcpre(n->v.id);
      CodeGen(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
      ARM_funcpos(n->v.id);
      genfreeregs();
      return (NOREG);
    /*数组*/
    case A_ADDR:
      {
        int len = Symtable[n->v.id].Dimens.size();
        int cnt = 1;
        ASTnode *test = n->left;
        bool flag = true;
        int off_size = 0;
        while(test != NULL){
          if(test->op == A_INTLIT){
            int k = 1;
            for(int i = cnt; i < len; ++i){
              k = k*Symtable[n->v.id].Dimens[i];
            }
            off_size += k * (test->v.intvalue);
            test = test->next;
            ++cnt;
          }else{
            flag = false;
            break;
          }
        }
        
        int reg = alloc_register(); //计算偏移量
        if(flag){
          if(off_size <= imm16){
            sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg], off_size);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }else{
            set_int_offset(reg, off_size);
          }
        }else if(len == 1){
          free_register(reg);
          ASTnode *a = n->left;
          reg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, n->op);
        }else{
          cnt = 1;
          ASTnode *a = n->left;
          while(a != NULL){
            rightreg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, n->op);
            a = a->next;
            int k = 1;
            for(int i = cnt; i < len; ++i){
              k = k*Symtable[n->v.id].Dimens[i];
            }
            if(cnt < len){
              int reg1 = alloc_register();
              if (k <= imm16){
                sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg1], k);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }else {
                int offset = -1;
                for (int i = 0; i < Intlist.size(); i++) {
                  if (Intlist[i] == k) {
                    offset = 4 * i;
                    break;
                  }
                }
                if (offset == -1) {
                  offset = 4 * Intslot;
                  Intlist.emplace_back(k);
                  ++Intslot;
                  ldr_Byte += (8 + leng(k));
                }
                for (int i = 0; i < Globs; i++) {
                  if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
                    offset += 4;
                }
                sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[reg1], func_field, offset);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }
              if(cnt == 1){
                sprintf(buf, "\tmul\t%s, %s, %s\n", reglist[reg], reglist[reg1], reglist[rightreg]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
                free_register(rightreg);
                free_register(reg1);
              }else{
                reg = ARM_mla(reg, rightreg, reg1);
              }
            }else{
              ARM_add(reg, rightreg);
            }
            ++cnt;
          }
        }
        int reg1 = alloc_register();
        if(Symtable[n->v.id].ctype == C_GLOBAL){
          int reg2 = ARM_var_offset(n->v.id);
          sprintf(buf, "\tldr\t%s, [%s, %s, lsl #2]\n", reglist[reg1], reglist[reg2], reglist[reg]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          free_register(reg2);
        }else if(Symtable[n->v.id].ctype == C_LOCAL){
          int reg2 = alloc_register();
          sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[reg], reglist[reg]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tsub\t%s, fp, #%d\n", reglist[reg2], push_reg);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          ARM_add(reg, reg2);
          int off = abs(Symtable[n->v.id].st_address + push_reg);
          if(off <= imm8){
            sprintf(buf, "\tldr\t%s, [%s, #%d]\n", reglist[reg1], reglist[reg], Symtable[n->v.id].st_address + push_reg);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }else{
            int reg3 = alloc_register();
            set_int_offset(reg3, off);
            sprintf(buf, "\tsub\t%s, %s, %s\n", reglist[reg], reglist[reg], reglist[reg3]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg1], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(reg3);
          }
        }else{
          int reg2 = alloc_register();
          if(Symtable[n->v.id].paramnum < 4){
            sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[reg], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg2], Symtable[n->v.id].st_address);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            ARM_add(reg, reg2);
            sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg1], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();}
          }else{
            sprintf(buf, "\tlsl\t%s, %s, #2\n", reglist[reg], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            int off = (Symtable[n->v.id].paramnum - 3)*4;
            if(off <= imm8){
              sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg2], off);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }else{
              int reg3 = alloc_register();
              set_int_offset(reg3, off);
              sprintf(buf, "\tadd\t%s, fp, %s\n", reglist[reg3], reglist[reg3]);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg2], reglist[reg3]);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              free_register(reg3);
            }
            ARM_add(reg, reg2);
            sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg1], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }
        }
        free_register(reg);
        return reg1;
      }
    case A_LOGAND:
      leftreg = CodeGen(n->left, label, looptoplabel, loopendlabel, n->op);
      if(leftreg != NOREG){
        sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        sprintf(buf, "\tbeq\t.L%d\n", label);
        ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
      }
      genfreeregs();
      if(n->istop) last_and = true;
      leftreg = CodeGen(n->right, label, looptoplabel, loopendlabel, n->op);
      if(leftreg != NOREG){
        if(last_and && !last_or){
          sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          sprintf(buf, "\tbne\t.L%d\n", true_L);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
        }else{
          sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
          sprintf(buf, "\tbeq\t.L%d\n", label);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
        }
      }
      if(n->istop) last_and = false;
      genfreeregs();
      return (NOREG);
    case A_LOGOR:
      {
        int Lfalse = genlabel();
        if(n->left->op == A_LOGAND) n->left->istop = true;
        leftreg = CodeGen(n->left, Lfalse, looptoplabel, loopendlabel, n->op);
        if(leftreg != NOREG){
          sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
          sprintf(buf, "\tbne\t.L%d\n", true_L);
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
        }
        genfreeregs();
        ARM_label(Lfalse);
        if(n->istop) last_or = true;
        if(n->right->op == A_LOGAND) n->right->istop = true;
        leftreg = CodeGen(n->right, label, looptoplabel, loopendlabel, n->op);
        if(leftreg != NOREG){
          if(last_or){
            sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\tbeq\t.L%d\n", label);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }else{
            sprintf(buf, "\tcmp\t%s, #0\n", reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\tbne\t.L%d\n", true_L);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }
        }
        if(n->istop) last_or = false;
        genfreeregs();
        return (NOREG);
      }
  }

  if((n->op == A_FUNCCALL) && (n->left == NULL)){
    leftreg = alloc_register();
    ++nested_index;
  }

  if (n->left){
    if(n->op == A_FUNCCALL){
      ++nested_index;
      leftreg = OutputFunccall(n->left, 0);
      freereg[leftreg] = 0;
    }else{
      leftreg = CodeGen(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
    }
  }
  if (n->right){
    if(n->op == A_ASSIGN){
      if(n->right->op == A_ADDR){
        ASTnode *t = n->right;
        ASTnode *test = t->left;
        bool flag = true;
        int len = Symtable[t->v.intvalue].Dimens.size();
        int cnt = 1;
        int off_size = 0;
        while(test != NULL){
          if(test->op == A_INTLIT){
            int k = 1;
            for(int i = cnt; i < len; ++i){
              k = k*Symtable[t->v.id].Dimens[i];
            }
            off_size += k * (test->v.intvalue);
            test = test->next;
            ++cnt;
          }else{
            flag = false;
            break;
          }
        }
        int reg = alloc_register();
        if(flag){
          if(off_size <= imm16){
            sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg], off_size);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          }else{
            set_int_offset(reg, off_size);
          }
        }else if(len == 1){
          free_register(reg);
          ASTnode *a = t->left;
          reg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, t->op);
        }else{
          ASTnode *a = t->left;
          cnt = 1;
          while(a != NULL){
            rightreg = CodeGen(a, NOLABEL, NOLABEL, NOLABEL, t->op);
            a = a->next;
            int k = 1;
            for(int i = cnt; i < len; ++i){
              k = k*Symtable[t->v.id].Dimens[i];
            }
            if(cnt < len){
              int reg1 = alloc_register();
              if (k <= imm16){
                sprintf(buf, "\tmov\t%s, #%d\n", reglist[reg1], k);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }else {
                int offset = -1;
                for (int i = 0; i < Intlist.size(); i++) {
                  if (Intlist[i] == k) {
                    offset = 4 * i;
                    break;
                  }
                }
                if (offset == -1) {
                  offset = 4 * Intslot;
                  Intlist.emplace_back(k);
                  ++Intslot;
                  ldr_Byte += (8 + leng(k));
                }
                for (int i = 0; i < Globs; i++) {
                  if ((Symtable[i].stype == S_VARIABLE || Symtable[i].stype == S_ARRAY) && (Symtable[i].intvalue == INF))
                    offset += 4;
                }
                sprintf(buf, "\tldr\t%s, .L%d+%d\n", reglist[reg1], func_field, offset);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }
              if(cnt == 1){
                sprintf(buf, "\tmul\t%s, %s, %s\n", reglist[reg], reglist[reg1], reglist[rightreg]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
                free_register(rightreg);
                free_register(reg1);
              }else{
                reg = ARM_mla(reg, rightreg, reg1);
              }
            }else{
              ARM_add(reg, rightreg);
            }
            ++cnt;
          }
        }
        rightreg = reg;
      }
    }else{
      rightreg = CodeGen(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
    }
  }

  switch (n->op) {
    case A_ADD:
      return (ARM_add(leftreg, rightreg));
    case A_SUBTRACT:
      return (ARM_sub(leftreg, rightreg));
    case A_MULTIPLY:
      {
        int val = -1;
        if(n->right->op == A_INTLIT){
          int value = n->right->v.intvalue;
          for(int ii = 0; ii < 31 && value>=P_2[ii]; ++ii){
            if(value == P_2[ii]){
              val = ii;
              break;
            }
          }
        }
        if(val != -1 && if_PerformanceTest){
          return (ARM_lsl(leftreg, rightreg, val));
        }else{
          return (ARM_mul(leftreg, rightreg));
        }
      }
    case A_DIVIDE:
      {
        int val = -1;
        if(n->right->op == A_INTLIT){
          int value = n->right->v.intvalue;
          for(int ii = 0; ii < 31 && value>=P_2[ii]; ++ii){
            if(value == P_2[ii]){
              val = ii;
              break;
            }
          }
        }
        if(val != -1 && if_PerformanceTest){
          return (ARM_asr(leftreg, rightreg, val));
        }else if(n->right->op == A_INTLIT && if_PerformanceTest){
          int value = n->right->v.intvalue;
          if(value == 1){
            free_register(rightreg);
            return leftreg;
          }else if(value == -1){
            free_register(rightreg);
            return ARM_rsb(leftreg);
          }else{
            int val = abs(value);
            int k = 0;
            while((2 << k) < val) ++k;
            --k;
            unsigned long long t = pow(2, k+32);
            int factor = ceil(1.0 * t / val);
            set_int_offset(rightreg, factor);
            int reg = alloc_register();
            sprintf(buf, "\tsmull\t%s, %s, %s, %s\n", reglist[reg], reglist[rightreg], reglist[rightreg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(reg);
            sprintf(buf, "\tasr\t%s, %s, #31\n", reglist[leftreg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\trsb\t%s, %s, %s, asr #%d\n", reglist[leftreg], reglist[leftreg], reglist[rightreg], k);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(rightreg);
            return leftreg;
          }
        }else{
          return (ARM_div(leftreg, rightreg));
        }
      }
    case A_MOD:
      {
        if(n->right->op == A_INTLIT && if_PerformanceTest){
          int value = n->right->v.intvalue;
          int p = -1;
          for(int ii = 0; ii < 31 && value>=P_2[ii]; ++ii){
            if(value == P_2[ii]){
              p = ii;
              break;
            }
          }
          if(value == 1 || value == -1){
            sprintf(buf, "\tmov\t%s, #0\n", reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(rightreg);
            return leftreg;
          }else if(p != -1){
            int reg = alloc_register();
            sprintf(buf, "\trsbs\t%s, %s, #0\n", reglist[reg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            if(P_2[p]-1 <= imm8){
              sprintf(buf, "\tand\t%s, %s, #%d\n", reglist[reg], reglist[reg], P_2[p]-1);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }else{
              set_int_offset(rightreg, P_2[p]-1);
              sprintf(buf, "\tand\t%s, %s, %s\n", reglist[reg], reglist[reg], reglist[rightreg]);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }
            if(P_2[p]-1 <= imm8){
              sprintf(buf, "\tand\t%s, %s, #%d\n", reglist[leftreg], reglist[leftreg], P_2[p]-1);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }else{
              set_int_offset(rightreg, P_2[p]-1);
              sprintf(buf, "\tand\t%s, %s, %s\n", reglist[leftreg], reglist[leftreg], reglist[rightreg]);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }
            sprintf(buf, "\trsbpl\t%s, %s, #0\n", reglist[leftreg], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(rightreg);
            return leftreg;
          }else{
            int reg = alloc_register();
            sprintf(buf, "\tmov\t%s, %s\n", reglist[reg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            int val = abs(value);
            int k = 0;
            while((2 << k) < val) ++k;
            --k;
            unsigned long long t = pow(2, k+32);
            int factor = ceil(1.0 * t / val);
            set_int_offset(rightreg, factor);
            int reg1 = alloc_register();
            sprintf(buf, "\tsmull\t%s, %s, %s, %s\n", reglist[reg1], reglist[rightreg], reglist[rightreg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(reg1);
            sprintf(buf, "\tasr\t%s, %s, #31\n", reglist[leftreg], reglist[leftreg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            sprintf(buf, "\trsb\t%s, %s, %s, asr #%d\n", reglist[leftreg], reglist[leftreg], reglist[rightreg], k);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            if(-255 <= value && value <= imm16){
              sprintf(buf, "\tmov\t%s, #%d\n", reglist[rightreg], value);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            }else{
              set_int_offset(rightreg, value);
            }
            sprintf(buf, "\tmls\t%s, %s, %s, %s\n", reglist[leftreg], reglist[rightreg], reglist[leftreg], reglist[reg]);
            ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
            free_register(reg);
            free_register(rightreg);
            return leftreg;
          }
        }else{
          return (ARM_mod(leftreg, rightreg));
        }
      }
    case A_EQ:
    case A_NE:
    case A_LT:
    case A_GT:
    case A_LE: 
    case A_GE:
      if(parentASTop == A_EQ || parentASTop == A_NE || parentASTop == A_LT || parentASTop == A_GT || parentASTop == A_LE || parentASTop == A_GE)
      {
        return ARM_compare_and_set(n->op, leftreg, rightreg);
      }
      else
      {
        return (ARM_cmp_jmp(n->op, leftreg, rightreg, label, parentASTop));
      }
    case A_INTLIT:
      return (ARM_loadint(n->v.intvalue));
    case A_IDENT:
      if (n->rvalue) {
        int r;
	      if (Symtable[n->v.id].ctype == C_GLOBAL) {
	        r = ARM_loadglob(n->v.id);
	      } else if(Symtable[n->v.id].ctype == C_LOCAL){
	        r = ARM_loadlocal(n->v.id);
	      }else{
          r = ARM_loadparam(n->v.id);
        }
        return r;
      } else
	      return (NOREG);
    case A_ASSIGN:
      switch (n->right->op) {
	      case A_IDENT:
	        if (Symtable[n->right->v.id].ctype == C_GLOBAL){
            return (ARM_str_glob(leftreg, n->right->v.id));
          }else if(Symtable[n->right->v.id].ctype == C_LOCAL){
            return (ARM_str_local(leftreg, n->right->v.id));
          }else{
            return (ARM_str_param(leftreg, n->right->v.id));
          }
	      case A_ADDR:
	        return (ARM_str_array(leftreg, rightreg, n->right->v.id));
	      default:
	        fatald("Can't A_ASSIGN in CodeGen(), op", n->op);
      }
    case A_RETURN:
      ARM_return(leftreg, Functionid);
      return (NOREG);
    case A_FUNCCALL:
      {
        if(nested_calls[nested_index].size() > 0 && !if_PerformanceTest){
          int reg = alloc_register();
          for(auto &x:nested_calls[nested_index]){
            int address = x.first, ri = x.second;
            if(ri < 4){
              int off = abs(address);
              int reg1 = alloc_register();
              if(off <= imm8){
                sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], address);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }else{
                set_int_offset(reg1, off);
                sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
                sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }
              free_register(reg1);
              sprintf(buf, "\tmov\tr%d, %s\n", ri, reglist[reg]);
              ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field();  }
            }else{
              int off = abs(address);
              int reg1 = alloc_register();
              if(off <= imm8){
                sprintf(buf, "\tldr\t%s, [fp, #%d]\n", reglist[reg], address);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }else{
                set_int_offset(reg1, off);
                sprintf(buf, "\tsub\t%s, fp, %s\n", reglist[reg1], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
                sprintf(buf, "\tldr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }
              off = (ri-4)*4;
              if(off <= imm8){
                sprintf(buf, "\tstr\t%s, [sp, #%d]\n", reglist[reg], off);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }else{
                set_int_offset(reg1, off);
                sprintf(buf, "\tadd\t%s, sp, %s\n", reglist[reg1], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
                sprintf(buf, "\tstr\t%s, [%s]\n", reglist[reg], reglist[reg1]);
                ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
              }
              free_register(reg1);
            }
          }
          free_register(reg);
          nested_calls[nested_index].clear();
        }
        --nested_index;
        if(Symtable[n->v.id].name == "putf"){
          sprintf(buf, "\tbl\tprintf\n");
          ObjectCode.emplace_back(buf);ldr_Byte += (strlen(buf) + OF); if(ldr_Byte >= L_field){ new_field(); }
          return (NOREG);
        }else{
          int r = ARM_funccall(leftreg, n->v.id);
          return r;
        }
      }
    case A_BREAK:
      ARM_jump(loopendlabel);
      return (NOREG);
    case A_CONTINUE:
      ARM_jump(looptoplabel);
      return (NOREG);
    case A_RSB:
      return ARM_rsb(leftreg);
    case A_LOGNOT:
      return (ARM_lognot(leftreg, label, parentASTop));
    default:
      exit(17);
      fatald("Unknown AST operator", n->op);
  }
  return (NOREG);
}

void genpreamble() {
  ARM_preamble();
}
void genfreeregs() {
  freeall_registers();
}
void genglobsym(int id, int num) {
  ARM_globsym(id, num);
}
void genglobsym_plus(int id, vector<int> &x) {
  ARM_globsym_plus(id, x);
}

void match(int t, string what) {
  if (token_stream[token_index].token == t) {
    ++token_index;
  } else {
    exit(1);
    fprintf(stderr, "This is %d\n", token_stream[token_index].token);
    fatals("Expected", what);
  }
}

void semi(void) {
  match(T_SEMI, ";");
}

void lbrace(void) {
  match(T_LB, "{");
  Locls_save.push(Locls);
}

void rbrace(void) {
  match(T_RB, "}");
  Locls = Locls_save.top();
  Locls_save.pop();
}

void lparen(void) {
  match(T_LP, "(");
}

void rparen(void) {
  match(T_RP, ")");
}

void ident(void) {
  nowname = token_stream[token_index].strvalue;
  match(T_IDENT, "identifier");
}

void fatal(string s) {
  fprintf(stderr, "%s on line %d\n", s.c_str(), token_stream[token_index].line);
  exit(18);
}

void fatals(string s1, string s2) {
  fprintf(stderr, "%s:%s on line %d\n", s1.c_str(), s2.c_str(), token_stream[token_index].line);
  exit(19);
}

void fatald(string s, int d) {
  fprintf(stderr, "%s:%d on line %d\n", s.c_str(), d, token_stream[token_index].line);
  exit(20);
}

void fatalc(string s, int c) {
  fprintf(stderr, "%s:%c on line %d\n", s.c_str(), c, token_stream[token_index].line);
  exit(21);
}

static int chrpos(char *s, int c) {
  char *p;

  p = strchr(s, c);
  return (p ? p - s : -1);
}

static int next(void) {
  if (Putback) {		
    Inchar = Putback;		
    Putback = 0;
    return Inchar;
  }

  Inchar = fgetc(Infile);		
  if ('\n' == Inchar)
    Line++;			
  return Inchar;
}

static void putback(int c) {
  Putback = c;
}

static int skip(void) {
  Inchar = next();
  while (isspace(Inchar)) {
    Inchar = next();
  }
  return (Inchar);
}

static int get_an_escape(void)
{
	Inchar = next();
	if (Inchar == '\\')
	{
		return '\\';
	}
	if (Inchar == 'n')
	{
		return '\n';
	}
	if (Inchar == 't')
	{
		return '\t';
	}
	if (Inchar == 'b')
	{
		return '\b';
	}
	return 0;
}

static int iskeyword()
{
	int i = 0;
	while (strcmp(keywords[i].name, ""))
	{
		if (!strcmp(keywords[i].name, Text))
		{
			return keywords[i].id;
		}
		++i;
	}
	return 0;
}

static int scan_alpha()
{
	int i = 0, num;
	while (isalpha(Inchar) || isdigit(Inchar) || Inchar == '_')
	{
		Text[i++] = Inchar;
		Inchar = next();
	}

	putback(Inchar);
  Text[i] = '\0';

	num = iskeyword();
	if (num == 0)
	{
		return T_IDENT;
	}
	else
	{
		return num;
	}
}

static int getdight(char c){
  if(isdigit(c)){
    return c - '0';
  }else{
    if('a' <= c && c <= 'z'){
      return (c-97)+10;
    }else{
      return (c-65)+10;
    }
  }
}

static int scan_digit() {
  int val = 0;
	if (Inchar != '0') //Decimal
	{
    int k;
		while ((k = chrpos((char*)"0123456789", Inchar)) >= 0) {
      val = val * 10 + k;
      Inchar = next();
    }
	}
	else
	{
		Inchar = next();
		if (Inchar == 'x' || Inchar == 'X') //Hexadecimal
		{
			Inchar = next();
	    while (isxdigit(Inchar))
	    {
		    val = val * 16 + getdight(Inchar);
		    Inchar = next();
	    }
		}
		else if (Inchar == 'b' || Inchar == 'B') //Binary
		{
			Inchar = next();
	    while (Inchar >= '0' && Inchar <= '1')
	    {
		    val = val * 2 + getdight(Inchar);
		    Inchar = next();
	    }
		}
		else //Octal
		{
	    while (Inchar >= '0' && Inchar <= '7')
	    {
		    val = val * 8 + getdight(Inchar);
		    Inchar = next();
	    }
		}
	}
  putback(Inchar);
	return val;
}

static int scan_text() {
  int index = 0;
	Inchar = next();
	while (Inchar != '"' && Inchar != EOF)
	{
		if (Inchar == '\\')
		{
			Text[index] = get_an_escape();
		}
		else
		{
			Text[index] = Inchar;
		}
		Inchar = next();
		index++;
	}
	if (Inchar == EOF)
	{
		printf("lex error: \" expected\n");
		exit(22);
	}
	Text[index] = 0;
  return T_STRCONST;
}

int scan(token *t) {
  Inchar = skip();
  if(Inchar == EOF)
  {
    t->token = T_EOF;
    return (0);
  }
  else if (isalpha(Inchar) || Inchar == '_')
	{
    t->token = scan_alpha();
	}
	else if (isdigit(Inchar))
	{
    t->token = T_INTCONST;
    t->intvalue = scan_digit();
	}
	else if (Inchar == '"')
	{
		t->token = scan_text();
	}
	else if (Inchar == '=')
	{
		Inchar = next();
    if(Inchar == '='){
      t->token = T_EQ;
    }else{
      t->token = T_ASSIGN;
      putback(Inchar);
    }
	}
	else if (Inchar == '!')
	{
		Inchar = next();
    if(Inchar == '='){
      t->token = T_NE;
    }else{
      t->token = T_LOGNOT;
      putback(Inchar);
    }
	}
	else if (Inchar == '>')
	{
		Inchar = next();
    if(Inchar == '='){
      t->token = T_GE;
    }else{
      t->token = T_GT;
      putback(Inchar);
    }
	}
	else if (Inchar == '<')
	{
		Inchar = next();
    if(Inchar == '='){
      t->token = T_LE;
    }else{
      t->token = T_LT;
      putback(Inchar);
    }
	}
	else if (Inchar == '&')
	{
		Inchar = next();
    t->token = T_LOGAND;
	}
	else if (Inchar == '|')
	{
		Inchar = next();
    t->token = T_LOGOR;
	}
	else if (Inchar == '/')
	{
		Inchar = next();
    if(Inchar == '/'){
      t->token = T_NOTE;
      while(Inchar != EOF){
        Inchar = next();
        if(Inchar == '\n'){
          break;
        }
      }
    }else if(Inchar == '*'){
      t->token = T_NOTE;
      while(Inchar != EOF){
        Inchar = next();
        if(Inchar == '*'){
          Inchar = next();
          if(Inchar == '/') break;
        }
      }
    }else{
      t->token = T_SLASH;
      putback(Inchar);
    }
	}
	else
	{
    switch (Inchar) {
    case '+':
      t->token = T_PLUS;
      break;
    case '-':
      t->token = T_MINUS;
      break;
    case '*':
      t->token = T_STAR;
      break;
    case '%':
      t->token = T_PER;
      break;
    case '(':
      t->token = T_LP;
      break;
    case ')':
      t->token = T_RP;
      break;
    case '[':
      t->token = T_LBK;
      break;
    case ']':
      t->token = T_RBK;
      break;
    case '{':
      t->token = T_LB;
      break;
    case '}':
      t->token = T_RB;
      break;
    case ',':
      t->token = T_COMMA;
      break;
    case ';':
      t->token = T_SEMI;
      break;
    default:
      printf("Unrecognised character %c on line %d\n", Inchar, Line);
      exit(23);
    }
	}
  return (1);
}

static void Scanner() {
  struct token T;
  while (scan(&T)) {
    if (T.token == T_INTCONST){
      token_stream.push_back({T.token, T.intvalue, "", Line});
    }else if(T.token == T_IDENT || T.token == T_STRCONST){
      token_stream.push_back({T.token, 0, Text, Line});
    }else if(T.token != T_NOTE){
      token_stream.push_back({T.token, 0, "", Line});
    }
  }
}

int findglob(string s) {
  int i;
  for (i = 0; i < Globs; i++) {
    if (s == Symtable[i].name)
      return (i);
  }
  return (-1);
}

static int newglob(void) {
  int p;
  if ((p = Globs++) >= Locls){
    exit(25);
    fatal("Too many global symbols");
  }
  return (p);
}

int findlocl(string s) {
  int i;
  for (i = Locls + 1; i < NSYMBOLS; i++) {
    if (s == Symtable[i].name)
      return (i);
  }
  return (-1);
}

static int newlocl(void) {
  int p;
  if ((p = Locls--) <= Globs){
    exit(24);
    fatal("Too many local symbols");
  }
  return (p);
}

void freeloclsyms(void) {
  Locls = NSYMBOLS - 1;
}

static void updatesym(int slot, string name, int type, int stype, int ctype, int endlabel, int size, int posn) {
  if (slot < 0 || slot >= NSYMBOLS){
    exit(23);
    fatal("Invalid symbol slot number in updatesym()");
  }
  Symtable[slot].name = name;
  Symtable[slot].type = type;
  Symtable[slot].stype = stype;
  Symtable[slot].ctype = ctype;
  Symtable[slot].endlabel = endlabel;
  Symtable[slot].size = size;
  Symtable[slot].posn = posn;
  Symtable[slot].intvalue = INF;
  Symtable[slot].paramnum = 0;
  Symtable[slot].Dimens.clear();
  Symtable[slot].conNum.clear();
}

int addglob(string name, int type, int stype, int endlabel, int size) {
  int slot;
  slot = newglob();
  updatesym(slot, name, type, stype, C_GLOBAL, endlabel, size, 0);
  ldr_Byte += (8 + name.size());
  return (slot);
}

int addlocl(string name, int type, int stype, int isparam, int size) {
  int localslot, globalslot;
  localslot = newlocl();
  if (isparam) {
    updatesym(localslot, name, type, stype, C_PARAM, 0, size, 0);
  } else {
    updatesym(localslot, name, type, stype, C_LOCAL, 0, size, 0);
  }

  return (localslot);
}

int findsymbol(string s) {
  int slot;
  slot = findlocl(s);
  if (slot == -1)
    slot = findglob(s);
  return (slot);
}

ASTnode *newASTnode(int op, int type, ASTnode *left, ASTnode *mid, ASTnode *right, int intvalue) {
  ASTnode *n = new ASTnode;
  n->op = op;
  n->type = type;
  n->left = left;
  n->mid = mid;
  n->right = right;
  n->next = NULL;
  n->istop = false;
  n->Dimension_num = 0;
  n->v.intvalue = intvalue;
  n->strvalue = "";
  return (n);
}

ASTnode *newASTleaf(int op, int type, int intvalue) {
  return (newASTnode(op, type, NULL, NULL, NULL, intvalue));
}

ASTnode *newASTunary(int op, int type, ASTnode *left, int intvalue) {
  return (newASTnode(op, type, left, NULL, NULL, intvalue));
}

static void init() {
  Line = 1;
  Putback = '\n';
  Globs = 0;
  Locls = NSYMBOLS - 1;
  Local_v = 0;
  func_v = 0;
  max_param = 0;
  Looplevel = 0;
  token_index = 0;
  con = false;
  last_or = false;
  last_and = false;
  pars = 0;
  func_field = -1;
  ldr_Byte = 0;
  true_L = -1;
  while_level = 0;
  max_alloc = 0;
}

int genlabel(void) {
  static int id = 1;
  return (id++);
}

int genLC(void) {
  static int lc = 1;
  return (lc++);
}

int main(int argc, char *argv[]) {

  if(argc == 6){
    if_PerformanceTest = true; 
  }
  
  init();
  // Open up the input file
  Infile = fopen(argv[4], "r");
  // Create the output file
  Outfile = fopen(argv[3], "w");

  // 添加宏定义
  addglob("getint", P_INT, S_FUNCTION, 0, 0);
  addglob("getch", P_INT, S_FUNCTION, 0, 0);
  addglob("getarray", P_INT, S_FUNCTION, 0, 0);
  addglob("putint", P_VOID, S_FUNCTION, 0, 0);
  addglob("putch", P_VOID, S_FUNCTION, 0, 0);
  addglob("putarray", P_VOID, S_FUNCTION, 0, 0);
  addglob("before_main", P_VOID, S_FUNCTION, 0, 0);
  addglob("after_main", P_VOID, S_FUNCTION, 0, 0);
  addglob("_sysy_starttime", P_VOID, S_FUNCTION, 0, 0);
  addglob("_sysy_stoptime", P_VOID, S_FUNCTION, 0, 0);
  addglob("starttime", P_VOID, S_FUNCTION, 0, 0);
  addglob("stoptime", P_VOID, S_FUNCTION, 0, 0);
  addglob("putf", P_VOID, S_FUNCTION, 0, 0);

  Scanner();

  token_size = token_stream.size();

  genpreamble();
  CompUnit();
  for(string &x : ObjectCode){
    fprintf(Outfile, "%s", x.c_str());
  }
  fclose(Outfile);
  return 0;
}
