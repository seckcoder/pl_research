#include <iostream>


// another demo of polymorphism


using namespace std;


class c1 {
};

class c2:public c1 {
 public:
  virtual void m() {
    cout << "c2:m" << endl;
  }
};


int main(int argc, const char *argv[])
{
  c1* p = new c2();
  p->m(); // This won't compile since type checking is static, it checks according
          // to pointer type.
  return 0;
}
