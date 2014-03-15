#include <iostream>

using namespace std;


class c1 {
 public:
  void m1() {
    m2(); // static display for self
  }
  void m2() {
    cout << "c1:m2" << endl;
  }
};

class c2: public c1 {
 public:
  void m1() {
    cout << "c2:m1" << endl;
  }
  void m2() {
    cout << "c2:m2" << endl;
  }
  void m3() {
    c1::m1(); // c++ explicitly specify the super class.
  }
};

class c3: public c2 {
  void m1() {
  }
  void m2() {
  }
};

int main(int argc, const char *argv[])
{
  c3* o3 = new c3();
  o3->m3();
  return 0;
}
