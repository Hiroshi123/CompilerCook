

static const volatile unsigned int f1() {
  
  register const int a = 1;
  auto int x = 4;
  return 1;
  
}; // {return 1;};

const extern int f2(){
  
  int a=1;
  return a;
  
};

//
static const volatile struct {} s1_,*s2_;

typedef const volatile struct {} s3_,*s4_;

struct ss {};

void f3(int a1);

struct s3;

// struct SStudent student;

typedef const int int_;


struct {
  int a;
} s3,s4;

union s2{};

struct A {
  int a1,a2;
  int const volatile * h ;
  
};


int main(){
  
  f1();
  f2();
  static const int ar[1];
  int a = 3;
  int b = 4;
  int c = a+b;
  int const volatile * h ;
  static const int volatile d=2;
  return 0;
  
};
