#include <iostream>

using namespace std;

class c1 {
 public:
  void m1() {
    cout << "c1:m1" << endl;
  }
  void m2() {
    this->m1(); // C++ use static dispatch for self!
  }
};

class c2:public c1 {
 public:
  void m1() {
    cout << "c2:m1" << endl;
  }
};

int main(int argc, const char *argv[])
{
  c2* p = new c2();
  p->m2();
  return 0;
}
