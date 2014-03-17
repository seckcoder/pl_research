#include <iostream>

// polymorphism/virtual function in C++ provides
// dynamic dispatch for self. Compare this to
// self_dispatch.cc

using namespace std;

class c1 {
 public:
  virtual void m1() {
    cout << "c1:m1" << endl;
  }
  void m2() {
    this->m1(); // m1 is virtual.
  }
};

class c2:public c1 {
 public:
  virtual void m1() {
    cout << "c2:m1" << endl;
  }
};

int main(int argc, const char *argv[])
{
  // What matters is the class of this pointer,
  // instead of pointer type.
  c2* p2 = new c2();
  p2->m2();
  c1* p1 = new c2();
  p1->m2();
  return 0;
}
